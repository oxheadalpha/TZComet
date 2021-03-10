open Import

module Uri = struct
  let validate uri_code =
    let open Tezos_contract_metadata.Metadata_uri in
    let errors = ref [] in
    let error w src e = errors := (w, src, e) :: !errors in
    let validate_kt1_address s =
      ( try ignore (B58_hashes.check_b58_kt1_hash s) with
      | Failure f -> error `Address s f
      | e -> Fmt.kstr (error `Address s) "%a" Exn.pp e ) ;
      Ok () in
    let validate_network = function
      | "mainnet" | "carthagenet" | "delphinet" | "dalphanet" | "zeronet" ->
          Ok ()
      | s ->
          ( try ignore (B58_hashes.check_b58_chain_id_hash s) with
          | Failure f -> error `Network s f
          | e -> Fmt.kstr (error `Network s) "%a" Exn.pp e ) ;
          Ok () in
    let uri =
      Uri.of_string uri_code |> of_uri ~validate_kt1_address ~validate_network
    in
    (uri, List.rev !errors)

  module Fetcher = struct
    type t = {current_contract: string option Reactive.var}

    let create () = {current_contract= Reactive.var None}
    let get (ctxt : < fetcher: t ; .. > Context.t) = ctxt#fetcher
    let current_contract ctxt = (get ctxt).current_contract

    let set_current_contract ctxt s =
      Reactive.set (get ctxt).current_contract (Some s)

    let unset_current_contract ctxt =
      Reactive.set (get ctxt).current_contract None
  end

  let rec needs_context_address =
    let open Tezos_contract_metadata.Metadata_uri in
    function
    | Storage {address= None; _} -> true
    | Web _ | Storage _ | Ipfs _ -> false
    | Hash {target; _} -> needs_context_address target

  let fetch ?(log = dbgf "Uri.fetch.log: %s") ctxt uri =
    let open Lwt.Infix in
    let logf fmt = Fmt.kstr (fun s -> dbgf "Uri.fetch: %s" s ; log s) fmt in
    let ni s = Fmt.failwith "Not Implemented: %s" s in
    dbgf "FETCCHINGG ============== " ;
    System.slow_step ctxt
    >>= fun () ->
    let rec resolve =
      let open Tezos_contract_metadata.Metadata_uri in
      function
      | Web http ->
          logf "HTTP %S (may fail because of origin policy)" http ;
          Js_of_ocaml_lwt.XmlHttpRequest.(
            perform_raw ~response_type:ArrayBuffer http
            >>= fun frame ->
            dbgf "%s -> code: %d" http frame.code ;
            match frame.code with
            | 200 ->
                let res =
                  Js_of_ocaml.Js.Opt.get frame.content (fun () ->
                      Fmt.failwith "Getting %S gave no content" http) in
                let as_string =
                  Js_of_ocaml.Typed_array.String.of_arrayBuffer res in
                logf "HTTP success (%d bytes)" (String.length as_string) ;
                Lwt.return as_string
            | other -> Fmt.failwith "Getting %S returned code: %d" http other)
          >>= fun content -> Lwt.return content
      | Ipfs {cid; path} ->
          let gateway = "https://gateway.ipfs.io/ipfs/" in
          let gatewayed = Fmt.str "%s%s%s" gateway cid path in
          logf "IPFS CID %S path %S, adding gateway %S" cid path gateway ;
          resolve (Web gatewayed)
      | Storage {network= None; address; key} ->
          let addr =
            match address with
            | Some s -> s
            | None -> (
              match Reactive.peek (Fetcher.current_contract ctxt) with
              | None -> Fmt.failwith "Missing current contract"
              | Some s -> s ) in
          logf "Using address %S (key = %S)" addr key ;
          Query_nodes.metadata_value ctxt ~address:addr ~key ~log
      | Storage {network= Some network; address; key} ->
          logf "storage %s %a %S" network Fmt.Dump.(option string) address key ;
          Fmt.kstr ni "storage uri with network = %s" network
      | Hash {kind= `Sha256; value; target} -> (
          let expected =
            match Digestif.of_raw_string_opt Digestif.sha256 value with
            | Some s -> s
            | None ->
                Fmt.failwith "%a is not a valid SHA256 hash" Hex.pp
                  (Hex.of_string value) in
          logf "sha256: %a" (Digestif.pp Digestif.sha256) expected ;
          resolve target
          >>= fun content ->
          let obtained = Digestif.digest_string Digestif.sha256 content in
          logf "hash of content: %a" (Digestif.pp Digestif.sha256) obtained ;
          match Digestif.unsafe_compare Digestif.sha256 expected obtained with
          | 0 -> Lwt.return content
          | _ ->
              Fmt.failwith "Hash of content %a is different from expected %a"
                (Digestif.pp Digestif.sha256)
                obtained
                (Digestif.pp Digestif.sha256)
                expected ) in
    resolve uri
end

module Content = struct
  let of_json s =
    try
      let warnings = ref [] in
      let jsonm =
        let j = Ezjsonm.value_from_string s in
        let rec fix = function
          | (`String _ | `Float _ | `Bool _ | `Null) as v -> v
          | `A l -> `A (List.map l ~f:fix)
          | `O kvl ->
              let f (k, v) =
                let fix_warn o k =
                  ( match
                      List.exists !warnings ~f:(function
                          | `Fixed_legacy (a, _) -> String.equal a o)
                    with
                  | true -> ()
                  | false -> warnings := `Fixed_legacy (o, k) :: !warnings ) ;
                  (k, fix v) in
                match k with
                | "michelson-storage-view" -> fix_warn k "michelsonStorageView"
                | "return-type" -> fix_warn k "returnType"
                | other -> (other, fix v) in
              `O (List.map kvl ~f) in
        fix j in
      let contents =
        Json_encoding.destruct
          Tezos_contract_metadata.Metadata_contents.encoding jsonm in
      Ok (!warnings, contents)
    with e -> Tezos_error_monad.Error_monad.error_exn e

  module Permissions_descriptor = struct
    type address = string

    type operator_transfer_policy =
      | No_transfer
      | Owner_transfer
      | Owner_or_operator_transfer

    type owner_hook_policy =
      | Owner_no_hook
      | Optional_owner_hook
      | Required_owner_hook

    type custom_permission_policy = {tag: string; config_api: address option}

    type t =
      { operator: operator_transfer_policy
      ; receiver: owner_hook_policy
      ; sender: owner_hook_policy
      ; custom: custom_permission_policy option }

    let encoding =
      let open Json_encoding in
      let operator_transfer_policy =
        string_enum
          [ ("no-transfer", No_transfer)
          ; ("owner-transfer", Owner_transfer)
          ; ("owner-or-operator-transfer", Owner_or_operator_transfer) ] in
      let owner_transfer_policy =
        string_enum
          [ ("owner-no-hook", Owner_no_hook)
          ; ("optional-owner-hook", Optional_owner_hook)
          ; ("required-owner-hook", Required_owner_hook) ] in
      let custom_permission_policy =
        conv
          (fun {tag; config_api} -> (tag, config_api))
          (fun (tag, config_api) -> {tag; config_api})
          (obj2 (req "tag" string) (opt "config-api" string)) in
      conv
        (fun {operator; receiver; sender; custom} ->
          (operator, receiver, sender, custom))
        (fun (operator, receiver, sender, custom) ->
          {operator; receiver; sender; custom})
        (obj4
           (req "operator" operator_transfer_policy)
           (req "receiver" owner_transfer_policy)
           (req "sender" owner_transfer_policy)
           (opt "custom" custom_permission_policy))

    let of_json jsonm =
      try
        let contents = Json_encoding.destruct encoding jsonm in
        Ok contents
      with e -> Tezos_error_monad.Error_monad.error_exn e
  end

  open Tezos_contract_metadata

  type metadata = Metadata_contents.t
  type view = Metadata_contents.View.t

  type michelson_implementation =
    Metadata_contents.View.Implementation.Michelson_storage.t

  type view_validation =
    | Invalid of
        { view: view
        ; implementation: michelson_implementation
        ; parameter_status:
            [`Missing_parameter | `Ok | `Unchecked_Parameter | `Wrong]
            * Michelson.Partial_type.t option
        ; return_status: [`Ok | `Wrong] * Michelson.Partial_type.t option }
    | Missing
    | No_michelson_implementation of view
    | Valid of view * michelson_implementation

  type classified =
    | Tzip_16 of metadata
    | Tzip_12 of
        { metadata: metadata
        ; interface_claim:
            [`Invalid of string | `Just_interface | `Version of string] option
        ; get_balance: view_validation
        ; total_supply: view_validation
        ; all_tokens: view_validation
        ; is_operator: view_validation
        ; permissions_descriptor:
            ( Permissions_descriptor.t
            , Tezos_error_monad.Error_monad.tztrace )
            Result.t
            Option.t
        ; token_metadata: view_validation
        ; token_metadata_big_map: Z.t option }

  let is_valid ~ignore_token_metadata_big_map = function
    | Tzip_16 _ -> true
    | Tzip_12 t12 -> (
        ( match t12.interface_claim with
        | Some `Just_interface | Some (`Version _) -> true
        | _ -> false )
        && List.for_all
             [t12.get_balance; t12.total_supply; t12.all_tokens; t12.is_operator]
             ~f:(function
             | Missing | Valid _ -> true
             | _ -> false)
        && ( match (t12.token_metadata, t12.token_metadata_big_map) with
           | Missing, None -> ignore_token_metadata_big_map
           | Missing, Some _ -> true
           | Valid _, _ -> true
           | No_michelson_implementation _, Some _ -> true
           | No_michelson_implementation _, None ->
               ignore_token_metadata_big_map
           | Invalid _, _ -> false )
        &&
        match t12.permissions_descriptor with
        | None | Some (Ok _) -> true
        | _ -> false )

  let find_michelson_view metadata ~view_name =
    let open Metadata_contents in
    List.find_map metadata.views ~f:(function
      | view when String.equal view.name view_name -> (
          List.find_map view.implementations ~f:(function
            | Rest_api_query _ -> None
            | Michelson_storage impl -> Some (`Found (view, impl)))
          |> function
          | Some v -> Some v | None -> Some (`No_michelson_implementation view)
          )
      | _ -> None)

  let check_implementation_types ?check_parameter ~check_return impl =
    let open Metadata_contents.View.Implementation.Michelson_storage in
    let open Michelson.Partial_type in
    let param =
      match (Option.map ~f:of_type impl.parameter, check_parameter) with
      | None, None -> (`Ok, None)
      | Some p, Some check when check p.structure -> (`Ok, Some p)
      | Some p, Some _ -> (`Wrong, Some p)
      | Some p, None -> (`Unchecked_Parameter, Some p)
      | None, Some _ -> (`Missing_parameter, None) in
    let result =
      match of_type impl.return_type with
      | p when check_return p.structure -> (`Ok, Some p)
      | p -> (`Wrong, Some p) in
    (param, result)

  let validate_view ?check_parameter metadata ~view_name ~check_return =
    match find_michelson_view metadata ~view_name with
    | None -> Missing
    | Some (`No_michelson_implementation x) -> No_michelson_implementation x
    | Some (`Found (view, impl)) -> (
        let ( ((param_ok, param) as parameter_status)
            , ((result_ok, result) as return_status) ) =
          check_implementation_types impl ?check_parameter ~check_return in
        match (param_ok, result_ok) with
        | `Ok, `Ok -> Valid (view, impl)
        | _ ->
            Invalid {view; implementation= impl; parameter_status; return_status}
        )

  let rec find_token_metadata_big_map ~storage_node ~type_node =
    let open Tezos_micheline.Micheline in
    let go (storage_node, type_node) =
      find_token_metadata_big_map ~storage_node ~type_node in
    let check_annots annotations node =
      if List.mem annotations "%token_metadata" ~equal:String.equal then
        Decorate_error.raise
          Message.(
            t "Wrong %token_metadata annotation:"
            %% kpp ct
                 Tezos_contract_metadata.Micheline_helpers
                 .pp_arbitrary_micheline node) in
    match (storage_node, type_node) with
    | Prim (_, "Pair", [l; r], ans), Prim (_, "pair", [lt; rt], ant) ->
        check_annots ans storage_node ;
        check_annots ant type_node ;
        go (l, lt) @ go (r, rt)
    | ( Int (_, z)
      , Prim
          ( _
          , "big_map"
          , [ Prim (_, "nat", [], _)
            ; Prim
                ( _
                , "pair"
                , [ Prim (_, "nat", [], _)
                  ; Prim
                      ( _
                      , "map"
                      , [Prim (_, "string", [], _); Prim (_, "bytes", [], _)]
                      , _ ) ]
                , _ ) ]
          , annotations ) )
      when List.mem annotations "%token_metadata" ~equal:String.equal ->
        [z]
    | Int (_, _z), Prim (_, "big_map", _, annots) ->
        check_annots annots type_node ;
        []
    | Int (_, _z), _ -> []
    | String (_, _s), _ -> []
    | Bytes (_, _b), _ -> []
    | Prim (_, _prim, _args, annot), _t ->
        check_annots annot storage_node ;
        []
    | Seq (_, _l), _t -> []

  let token_metadata_value ctxt ~address ~key ~(log : string -> unit) =
    let open Lwt in
    let open Query_nodes in
    let logf f = Fmt.kstr log f in
    find_node_with_contract ctxt address
    >>= fun node ->
    logf "Found contract with node %S" node.Node.name ;
    Node.metadata_big_map ctxt node ~address ~log
    >>= fun metacontract ->
    let Node.Contract.{storage_node; type_node; _} = metacontract in
    let tmbm_id =
      match find_token_metadata_big_map ~storage_node ~type_node with
      | [one] -> one
      | other ->
          Decorate_error.raise
            Message.(
              t "Wrong number of %token_metadata big-maps:"
              %% int ct (List.length other)) in
    logf "Token-Metadata big-map: %s" (Z.to_string tmbm_id) ;
    Lwt.return tmbm_id

  let classify : ?token_metadata_big_map:Z.t -> metadata -> classified =
    let open Metadata_contents in
    let looks_like_tzip_12 ?token_metadata_big_map ~found metadata =
      let interface_claim =
        List.find metadata.interfaces ~f:(fun s ->
            String.is_prefix s ~prefix:"TZIP-12"
            || String.is_prefix s ~prefix:"TZIP-012")
        |> Option.map ~f:(function
             | "TZIP-012" -> `Just_interface
             | "TZIP-12" -> `Invalid "TZIP-12"
             | itf -> (
               match String.chop_prefix itf ~prefix:"TZIP-012-" with
               | None -> `Invalid itf
               | Some v -> `Version v )) in
      let find_extra name =
        List.find_map metadata.unknown ~f:(function
          | n, json when String.equal name n -> Some json
          | _ -> None) in
      if Option.is_none interface_claim (* && Option.is_none tokens *) then ()
      else
        let check_nat =
          Michelson.Partial_type.Structure.(
            function Leaf {kind= Nat; _} -> true | _ -> false) in
        let get_balance =
          let check_parameter =
            Michelson.Partial_type.Structure.(
              function
              | Pair {left= Leaf {kind= Address; _}; right= Leaf {kind= Nat; _}}
                ->
                  true
              | _ -> false) in
          validate_view metadata ~view_name:"get_balance" ~check_parameter
            ~check_return:check_nat in
        let total_supply =
          validate_view metadata ~view_name:"total_supply"
            ~check_parameter:check_nat ~check_return:check_nat in
        let all_tokens =
          let check_return =
            Michelson.Partial_type.Structure.(
              function Leaf {kind= List Nat; _} -> true | _ -> false) in
          validate_view metadata ~view_name:"all_tokens" ~check_return in
        let is_operator =
          let check_parameter =
            Michelson.Partial_type.Structure.(
              function
              | Pair
                  { left= Leaf {kind= Address; _}
                  ; right=
                      Pair
                        { left= Leaf {kind= Address; _}
                        ; right= Leaf {kind= Nat; _} } } ->
                  true
              | _ -> false) in
          let check_return =
            Michelson.Partial_type.Structure.(
              function Leaf {kind= Bool; _} -> true | _ -> false) in
          validate_view metadata ~view_name:"is_operator" ~check_parameter
            ~check_return in
        let token_metadata =
          let check_return =
            Michelson.Partial_type.Structure.(
              function
              | Pair
                  { left= Leaf {kind= Nat; _}
                  ; right= Leaf {kind= Map (String, Bytes); _} } ->
                  true
              | _ -> false) in
          validate_view metadata ~view_name:"token_metadata"
            ~check_parameter:check_nat ~check_return in
        let permissions_descriptor =
          match find_extra "permissions" with
          | None -> None
          | Some j -> Some (Permissions_descriptor.of_json j) in
        let metadata =
          { metadata with
            unknown=
              List.filter metadata.unknown ~f:(function
                | "permissions", _ -> false
                | _ -> true) } in
        found
          (Tzip_12
             { metadata
             ; interface_claim
             ; get_balance
             ; total_supply
             ; all_tokens
             ; is_operator
             ; permissions_descriptor
             ; token_metadata
             ; token_metadata_big_map }) in
    let exception Found of classified in
    fun ?token_metadata_big_map metadata ->
      try
        looks_like_tzip_12 ?token_metadata_big_map
          ~found:(fun x -> raise (Found x))
          metadata ;
        Tzip_16 metadata
      with Found x -> x

  let tzip21_claim metadata =
    List.find metadata.Tezos_contract_metadata.Metadata_contents.interfaces
      ~f:(fun claim -> String.is_prefix claim "TZIP-021")
end
