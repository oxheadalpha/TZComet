open Import

module Uri = struct
  let validate uri_code =
    let open Tezai_contract_metadata.Metadata_uri in
    let errors = ref [] in
    let error w src e = errors := (w, src, e) :: !errors in
    let validate_kt1_address s =
      ( try Tezai_base58_digest.Identifier.Kt1_address.check s with
      | Failure f -> error `Address s f
      | e -> Fmt.kstr (error `Address s) "%a" Exn.pp e ) ;
      Ok () in
    let validate_network = function
      | "mainnet" | "carthagenet" | "delphinet" | "dalphanet" | "zeronet" ->
          Ok ()
      | s ->
          ( try Tezai_base58_digest.Identifier.Chain_id.check s with
          | Failure f -> error `Network s f
          | e -> Fmt.kstr (error `Network s) "%a" Exn.pp e ) ;
          Ok () in
    let uri =
      Uri.of_string uri_code |> of_uri ~validate_kt1_address ~validate_network
    in
    (uri, List.rev !errors)

  module Fetcher = struct
    type gateway = {main: string; alternate: string}
    type t = {current_contract: string option Reactive.var; gateway: gateway}

    let create () =
      let main = "https://gateway.ipfs.io/ipfs/" in
      let alternate = "https://dweb.link/ipfs/" in
      {current_contract= Reactive.var None; gateway= {main; alternate}}

    let get (ctxt : < fetcher: t ; .. > Context.t) = ctxt#fetcher
    let current_contract ctxt = (get ctxt).current_contract
    let gateway ctxt = (get ctxt).gateway

    let set_current_contract ctxt s =
      Reactive.set (get ctxt).current_contract (Some s)

    let unset_current_contract ctxt =
      Reactive.set (get ctxt).current_contract None
  end

  let rec needs_context_address =
    let open Tezai_contract_metadata.Metadata_uri in
    function
    | Storage {address= None; _} -> true
    | Web _ | Storage _ | Ipfs _ -> false
    | Hash {target; _} -> needs_context_address target

  let to_ipfs_gateway ?(alt_gateway = false) ctxt ~cid ~path =
    let gateway =
      if alt_gateway then (Fetcher.gateway ctxt).alternate
      else (Fetcher.gateway ctxt).main in
    Fmt.str "%s%s%s" gateway cid path

  let to_web_address ctxt =
    let open Tezai_contract_metadata.Metadata_uri in
    function
    | Web http -> Some http
    | Ipfs {cid; path} -> Some (to_ipfs_gateway ctxt ~cid ~path)
    | Hash {kind= _; value= _; target= Web http} -> Some http
    | _ -> None

  let fetch ?limit_bytes ?(log = dbgf "Uri.fetch.log: %s") ctxt uri =
    let open Lwt.Infix in
    let logf fmt = Fmt.kstr (fun s -> dbgf "Uri.fetch: %s" s ; log s) fmt in
    let ni s = Fmt.failwith "Not Implemented: %s" s in
    dbgf "FETCCHINGG ============== " ;
    System.slow_step ctxt
    >>= fun () ->
    let rec resolve =
      let open Tezai_contract_metadata.Metadata_uri in
      function
      | Web http ->
          logf "HTTP %S (may fail because of origin policy)" http ;
          System.with_timeout ctxt
            ~raise:(fun timeout ->
              Fmt.failwith "HTTP Call Timeouted: %.3f s" timeout )
            ~f:
              Js_of_ocaml_lwt.XmlHttpRequest.(
                fun () ->
                  let headers =
                    Option.map limit_bytes ~f:(fun b ->
                        [("Range", Fmt.str "bytes=0-%d" b)] ) in
                  perform_raw ~response_type:ArrayBuffer ?headers http
                  >>= fun frame ->
                  dbgf "%s -> code: %d" http frame.code ;
                  match frame.code with
                  | ok when ok = 200 || (ok = 206 && Option.is_some limit_bytes)
                    ->
                      let res =
                        Js_of_ocaml.Js.Opt.get frame.content (fun () ->
                            Fmt.failwith "Getting %S gave no content" http )
                      in
                      let as_string =
                        Js_of_ocaml.Typed_array.String.of_arrayBuffer res in
                      logf "HTTP success (%d bytes)" (String.length as_string) ;
                      Lwt.return as_string
                  | other ->
                      Fmt.failwith "Getting %S returned code: %d" http other)
          >>= fun content -> Lwt.return content
      | Ipfs {cid; path} ->
          logf "IPFS CID %S path %S" cid path ;
          let gatewayed = to_ipfs_gateway ctxt ~cid ~path in
          (* resolve (Web gatewayed) *)
          Lwt.catch
            (fun () -> resolve (Web gatewayed))
            (fun _ ->
              dbgf "Trying alternate IPFS gateway..." ;
              let gatewayed_alt =
                to_ipfs_gateway ctxt ~alt_gateway:true ~cid ~path in
              resolve (Web gatewayed_alt) )
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
                          | `Fixed_legacy (a, _) -> String.equal a o )
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
          Tezai_contract_metadata.Metadata_contents.encoding jsonm in
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
          (operator, receiver, sender, custom) )
        (fun (operator, receiver, sender, custom) ->
          {operator; receiver; sender; custom} )
        (obj4
           (req "operator" operator_transfer_policy)
           (req "receiver" owner_transfer_policy)
           (req "sender" owner_transfer_policy)
           (opt "custom" custom_permission_policy) )

    let of_json jsonm =
      try
        let contents = Json_encoding.destruct encoding jsonm in
        Ok contents
      with e -> Tezos_error_monad.Error_monad.error_exn e
  end

  module Q = Query_nodes
  open Tezos_contract_metadata
  open Tezai_contract_metadata
  module Query_nodes = Q

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
             | _ -> false )
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
            | Michelson_storage impl -> Some (`Found (view, impl)) )
          |> function
          | Some v -> Some v | None -> Some (`No_michelson_implementation view)
          )
      | _ -> None )

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
        let ( ((param_ok, _) as parameter_status)
            , ((result_ok, _) as return_status) ) =
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
            %% kpp ct Tezai_michelson.Untyped.pp
                 (Tezai_michelson.Untyped.of_micheline_node node)) in
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

  let token_metadata_value ctxt ~address ~key:_ ~(log : string -> unit) =
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
      match
        find_token_metadata_big_map
          ~storage_node:(Tezai_michelson.Untyped.to_micheline_node storage_node)
          ~type_node:(Tezai_michelson.Untyped.to_micheline_node type_node)
      with
      | [one] -> one
      | other ->
          Decorate_error.raise
            Message.(
              t "Wrong number of %token_metadata big-maps:"
              %% int ct (List.length other)) in
    logf "Token-Metadata big-map: %s" (Z.to_string tmbm_id) ;
    Lwt.return tmbm_id

  let call_view_from_string (ctxt : _ Context.t) view ~address ~parameter_string
      ~log =
    let open Lwt.Infix in
    let parameter =
      Tezai_michelson.Concrete_syntax.parse_exn ~check_indentation:false
        parameter_string ~check_primitives:false in
    Query_nodes.call_off_chain_view ctxt ~log ~address ~view ~parameter
    >>= function
    | Ok (result, _) -> Lwt.return (Ok result) | Error s -> Lwt.return (Error s)

  let call_view_or_fail ctxt view ~parameter_string ~address ~log =
    let open Lwt.Infix in
    call_view_from_string ctxt view ~address ~parameter_string ~log
    >>= function
    | Ok o -> Lwt.return o
    | Error s -> Decorate_error.raise Message.(t "Calling view failed" %% ct s)

  let maybe_call_view ctxt view_validation ~parameter_string ~address ~log =
    let open Lwt.Infix in
    match view_validation with
    | Invalid _ | Missing | No_michelson_implementation _ -> Lwt.return_none
    | Valid (_, view) ->
        call_view_from_string ctxt view ~parameter_string ~address ~log
        >>= fun res -> Lwt.return_some res

  let classify : ?token_metadata_big_map:Z.t -> metadata -> classified =
    let open Metadata_contents in
    let looks_like_tzip_12 ?token_metadata_big_map ~found metadata =
      let interface_claim =
        List.find metadata.interfaces ~f:(fun s ->
            String.is_prefix s ~prefix:"TZIP-12"
            || String.is_prefix s ~prefix:"TZIP-012" )
        |> Option.map ~f:(function
             | "TZIP-012" -> `Just_interface
             | "TZIP-12" -> `Invalid "TZIP-12"
             | itf -> (
               match String.chop_prefix itf ~prefix:"TZIP-012-" with
               | None -> `Invalid itf
               | Some v -> `Version v ) ) in
      let find_extra name =
        List.find_map metadata.unknown ~f:(function
          | n, json when String.equal name n -> Some json
          | _ -> None ) in
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
                | _ -> true ) } in
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
             ; token_metadata_big_map } ) in
    let exception Found of classified in
    fun ?token_metadata_big_map metadata ->
      try
        looks_like_tzip_12 ?token_metadata_big_map
          ~found:(fun x -> raise (Found x))
          metadata ;
        Tzip_16 metadata
      with Found x -> x

  module Tzip_021 = struct
    let claim metadata =
      List.find metadata.Tezai_contract_metadata.Metadata_contents.interfaces
        ~f:(fun claim -> String.is_prefix claim ~prefix:"TZIP-021")

    type uri_format =
      { uri: string option
      ; mime_type: string option
      ; other: (string * Ezjsonm.value) list }

    type t =
      { description: string option
      ; creators: string list option
      ; tags: string list option
      ; transferable: bool option
      ; boolean_amount: bool option
      ; prefers_symbol: bool option
      ; thumbnail: string option
      ; display: string option
      ; artifact: string option
      ; formats: uri_format list option
      ; warnings: Message.t list }

    let is_empty = function
      | { description= None
        ; creators= None
        ; tags= None
        ; transferable= None
        ; boolean_amount= None
        ; prefers_symbol= None
        ; thumbnail= None
        ; display= None
        ; artifact= None
        ; formats= None
        ; warnings= [] } ->
          true
      | _ -> false

    let uri_mime_types tzip21 =
      match tzip21.formats with
      | None -> []
      | Some some ->
          List.filter_map some ~f:(function
            | {uri= Some u; mime_type= Some m; _} -> Some (u, m)
            | _ -> None )

    let from_extras l =
      let kvs, trash =
        List.partition_map l ~f:(function
          | Ok kv -> First kv
          | Error _ as e -> Second e ) in
      let extr = ref kvs in
      let find_remove l ~key =
        let rec go found acc = function
          | [] -> (found, List.rev acc)
          | one :: more when Option.is_some found -> go found (one :: acc) more
          | (kone, vone) :: more ->
              if String.equal kone key then go (Some vone) acc more
              else go found ((kone, vone) :: acc) more in
        go None [] l in
      let find_remove_extr key =
        let v, ex = find_remove !extr ~key in
        extr := ex ;
        v in
      let warnings = ref [] in
      let warn m = warnings := m :: !warnings in
      let find_remove_extr_bool key =
        find_remove_extr key
        |> Option.bind ~f:(function
             | "true" -> Some true
             | "false" -> Some false
             | _ ->
                 warn
                   Message.(
                     t "Key" %% ct key
                     %% t "should be a JSON boolean: "
                     %% ct "true" %% t "or" %% ct "false") ;
                 None ) in
      let find_remove_extr_json key ~parse_json ~expected =
        find_remove_extr key
        |> Option.bind ~f:(fun s ->
               match Ezjsonm.(value_from_string s |> parse_json) with
               | s -> Some s
               | exception e ->
                   warn
                     Message.(
                       t "Key" %% ct key
                       %% Fmt.kstr t "is supposed to be %s but got" expected
                       %% ct s
                       %% parens (t "Exception:" %% Fmt.kstr ct "%a" Exn.pp e)) ;
                   None ) in
      let find_remove_extr_string_list key =
        find_remove_extr_json key ~parse_json:Ezjsonm.get_strings
          ~expected:"an array of strings" in
      let transferable = find_remove_extr_bool "isTransferable" in
      let boolean_amount = find_remove_extr_bool "isBooleanAmount" in
      let prefers_symbol = find_remove_extr_bool "shouldPreferSymbol" in
      let description = find_remove_extr "description" in
      let thumbnail = find_remove_extr "thumbnailUri" in
      let display = find_remove_extr "displayUri" in
      let artifact = find_remove_extr "artifactUri" in
      let creators = find_remove_extr_string_list "creators" in
      let tags = find_remove_extr_string_list "tags" in
      let formats =
        find_remove_extr_json "formats"
          ~parse_json:
            Ezjsonm.(
              fun j ->
                let parse_one j =
                  let d = get_dict j in
                  let get_string_field key =
                    List.find_map d ~f:(function
                      | k, `String s when String.equal key k -> Some s
                      | k, other when String.equal key k ->
                          warn
                            Message.(
                              t "In the" %% ct "\"formats\""
                              %% t "field, the object"
                              %% ct Ezjsonm.(value_to_string ~minify:true j)
                              %% t "at key" %% Fmt.kstr ct "%S" key
                              %% t "should have a string, not"
                              %% ct Ezjsonm.(value_to_string ~minify:true other)
                              %% t ".") ;
                          None
                      | _ -> None ) in
                  let uri = get_string_field "uri" in
                  let mime_type = get_string_field "mimeType" in
                  let other =
                    List.filter d ~f:(fun (k, _) ->
                        not (List.mem ["uri"; "mimeType"] ~equal:String.equal k) )
                  in
                  {uri; mime_type; other} in
                get_list parse_one j)
          ~expected:"an array of objects" in
      let tzip21 =
        (* We've used the side-effects here, we can get the warnings: *)
        { description
        ; creators
        ; tags
        ; transferable
        ; boolean_amount
        ; prefers_symbol
        ; thumbnail
        ; display
        ; artifact
        ; formats
        ; warnings= !warnings } in
      (tzip21, List.map !extr ~f:Result.return @ trash)
  end
end

module Multimedia = struct
  type t = {uri: string; converted_uri: string; sfw: bool; format: Blob.Format.t}

  let pp ppf t =
    let open Fmt in
    pf ppf "{%s@ %s@ %s[%s]}" t.uri
      (Blob.Format.to_mime t.format)
      ( try String.sub t.converted_uri ~pos:0 ~len:10 ^ "..."
        with _ -> t.converted_uri )
      (if t.sfw then "SFW" else "NSFW")

  let prepare_and_guess ~mime_types (ctxt : _ Context.t) ~uri ~log =
    let open Lwt.Infix in
    let found ~format ?(sfw = false) ~converted uri =
      Lwt.return {uri; sfw; format; converted_uri= converted} in
    let known_mime_type = List.Assoc.find mime_types ~equal:String.equal uri in
    let failf fmt =
      Fmt.kstr
        (fun s ->
          Decorate_error.raise
            Message.(
              t "Preparing and guessing format for" %% ct uri % t ":" %% t s) )
        fmt in
    let guess_format content =
      let format = Blob.guess_format content in
      match format with
      | Some format -> Lwt.return format
      | None ->
          let (`Hex hx) = Hex.of_string content in
          failf "could not guess the format of the content: 0x%s"
            (bytes_summary ~left:24 ~right:0 hx) in
    let convert ~format uri =
      let open Uri in
      match validate uri with
      | Ok uri16, _ -> (
        match (to_web_address ctxt uri16, format) with
        | Some s, Some f -> Lwt.return (f, s)
        | Some s, None ->
            (* TODO: fetch for format *)
            fetch ~limit_bytes:128 ctxt uri16 ~log
            >>= fun content ->
            guess_format content >>= fun format -> Lwt.return (format, s)
        | None, _ ->
            Lwt.catch
              (fun () ->
                fetch ctxt uri16 ~log
                >>= fun content ->
                guess_format content
                >>= fun format ->
                let content_type = Blob.Format.to_mime format in
                let src =
                  Fmt.str "data:%s;base64,%s" content_type
                    (Base64.encode_exn ~pad:true
                       ~alphabet:Base64.default_alphabet content ) in
                Lwt.return (format, src) )
              (function
                | Decorate_error.E _ as e -> raise e
                | other -> failf "failed to fetch the URI: %a" Exn.pp other ) )
      | Error e, _ ->
          failf "failed to validate the URI: %a"
            Tezos_error_monad.Error_monad.pp_print_error e in
    State.Metadata_metadata.sfw_multimedia ctxt uri
    >>= fun format ->
    let sfw = Option.is_some format in
    convert uri ~format:(Option.first_some format known_mime_type)
    >>= fun (format, converted) -> found ~converted ~format ~sfw uri
end

module Token = struct
  type warning =
    [ `Fetching_uri of string * exn
    | `Parsing_uri of
      string
      * Tezos_error_monad.Error_monad.error
        Tezos_error_monad.Error_monad.TzTrace.trace
    | `Getting_metadata_field of Message.t ]

  type t =
    { address: string
    ; id: Z.t
    ; network: Network.t option
    ; symbol: string option
    ; name: string option
    ; decimals: string option
    ; total_supply: Z.t option
    ; tzip21: Content.Tzip_021.t
    ; main_multimedia: (string * Multimedia.t, exn) Result.t Option.t
    ; metadata: Tezai_contract_metadata.Metadata_contents.t
    ; special_knowledge: [`Hic_et_nunc of Z.t] list
    ; warnings: (string * warning) list }

  let make ?symbol ?name ?decimals ?network ?main_multimedia ~tzip21
      ?(warnings = []) ~metadata ?total_supply ?(special_knowledge = []) address
      id =
    { address
    ; id
    ; network
    ; warnings
    ; symbol
    ; name
    ; decimals
    ; main_multimedia
    ; metadata
    ; total_supply
    ; special_knowledge
    ; tzip21 }

  let piece_of_metadata ?(json_type = `String) ~warn ~key ~metadata_map
      ~metadata_json () =
    let in_json =
      match metadata_json with
      | None -> None
      | Some (_, `O l) -> (
        match
          ( List.filter_map l ~f:(function
              | k, v when String.equal k key -> Some v
              | _ -> None )
          , json_type )
        with
        | [], _ -> None
        | [`String one], `String -> Some one
        | [`Float one], `Int -> Some (Float.to_int one |> Int.to_string)
        | (`String one :: _ as more), `String ->
            warn
              (Fmt.str "fields-of-object-at-%s" key)
              Message.(
                t "Token-metadata URI objects has"
                %% Fmt.kstr ct "%d" (List.length more)
                %% t "fields called" %% Fmt.kstr ct "%S" key) ;
            Some one
        | (`Float one :: _ as more), `Int ->
            warn
              (Fmt.str "fields-of-object-at-%s" key)
              Message.(
                t "Token-metadata URI objects has"
                %% Fmt.kstr ct "%d" (List.length more)
                %% t "fields called" %% Fmt.kstr ct "%S" key) ;
            Some (Float.to_int one |> Int.to_string)
        | other :: _, _ ->
            warn
              (Fmt.str "type-of-field-of-object-at-%s" key)
              Message.(
                t "Token-metadata URI points a JSON where field"
                %% Fmt.kstr ct "%S" key %% t "has the wrong type:"
                %% ct (Ezjsonm.value_to_string other)) ;
            None )
      | Some (uri, other) ->
          warn
            (Fmt.str "uri-wrong-json-%s" uri)
            Message.(
              t "Metadata URI" %% ct uri
              %% t "does not point at a JSON object, I got:"
              %% ct (Ezjsonm.value_to_string other)) ;
          None in
    match (List.Assoc.find ~equal:String.equal metadata_map key, in_json) with
    | None, Some s -> Some s
    | Some s, None -> Some s
    | Some s, Some same when String.equal s same -> Some s
    | Some s, Some ignored ->
        warn
          (Fmt.str "field-double-%s" key)
          Message.(
            t "Field" %% Fmt.kstr ct "%S" key
            %% t "is defined twice differently:"
            %% Fmt.kstr ct "%S" ignored) ;
        Some s
    | None, None -> None

  let token_fetch (ctxt : _ Context.t) ~address ~id ~log : t Lwt.t =
    let open Lwt.Infix in
    let logs prefix msg = log Message.(t prefix %% t "👉" %% t msg) in
    let warnings = ref [] in
    let warn k e =
      if List.exists !warnings ~f:(fun (key, _) -> String.equal key k) then ()
      else warnings := (k, e) :: !warnings in
    let failm msg =
      Decorate_error.raise
        Message.(Fmt.kstr t "Fetching %s/%s:" address (Z.to_string id) %% msg)
    in
    Uri.Fetcher.set_current_contract ctxt address ;
    Lwt.catch
      (fun () ->
        Content.token_metadata_value ctxt ~address ~key:""
          ~log:(logs "Getting %token_metadata big-map")
        >>= fun token_metadata -> Lwt.return_some token_metadata )
      (fun _exn ->
        log Message.(t "Attempt at getting a %token_metadata big-map failed.") ;
        Lwt.return_none )
    >>= fun token_metadata_big_map ->
    Query_nodes.metadata_value ctxt ~address ~key:"" ~log:(logs "Getting URI")
    >>= (fun metadata_uri ->
          let open Tezai_contract_metadata.Metadata_contents in
          let empty () = Lwt.return (make []) in
          match Uri.validate metadata_uri with
          | Ok uri, _ -> (
              Uri.fetch ctxt uri ~log:(logs "Fetching Metadata")
              >>= fun json_code ->
              match Content.of_json json_code with
              | Ok (_, con) -> Lwt.return con
              | Error error ->
                  log
                    Message.(
                      t "failed to parse/validate the metadata URI:"
                      %% Fmt.kstr ct "%a"
                           Tezos_error_monad.Error_monad.pp_print_error error) ;
                  empty () )
          | Error error, _ ->
              log
                Message.(
                  t "failed to parse/validate the metadata URI:"
                  %% Fmt.kstr ct "%a"
                       Tezos_error_monad.Error_monad.pp_print_error error) ;
              empty () )
    >>= fun metadata_contents ->
    let total_supply_validation, token_metadata_validation =
      match Content.classify ?token_metadata_big_map metadata_contents with
      | Tzip_16 _ ->
          log
            Message.(
              t "This is not a TZIP-012 token at all. See interfaces claimed:"
              %% list
                   (oxfordize_list ~map:ct
                      ~sep:(fun () -> t ", ")
                      ~last_sep:(fun () -> t ", and ")
                      metadata_contents.interfaces )) ;
          Content.(Missing, Missing)
      | Tzip_12
          { (* metadata
               ; interface_claim
               ; get_balance *)
            total_supply
            (* ; all_tokens
               ; permissions_descriptor
                    ; is_operator*)
          ; token_metadata
          ; _ } ->
          (total_supply, token_metadata) in
    let get_token_metadata_map_with_view () =
      Content.maybe_call_view ctxt token_metadata_validation
        ~parameter_string:(Z.to_string id) ~address ~log:(logs "Call View")
    in
    let get_total_supply_with_view () =
      Content.maybe_call_view ctxt total_supply_validation
        ~parameter_string:(Z.to_string id) ~address ~log:(logs "Call View")
      >|= Option.map
            ~f:(Result.map ~f:Tezai_michelson.Untyped.to_micheline_node)
      >>= function
      | Some (Ok (Tezos_micheline.Micheline.Int (_, z))) -> Lwt.return_some z
      | _ -> Lwt.return_none in
    let get_token_metadata_map_with_big_map ~log ~node big_map_id =
      Query_nodes.Node.micheline_value_of_big_map_at_nat ctxt node ~log
        ~big_map_id ~key:id in
    let meta_log = logs "Fetching token-metadata" in
    Query_nodes.find_node_with_contract ctxt address
    >>= fun node ->
    Fmt.kstr meta_log "Using %s" node.Query_nodes.Node.name ;
    begin
      begin
        match (token_metadata_big_map, token_metadata_validation) with
        | _, Valid _ | None, _ -> (
            get_token_metadata_map_with_view ()
            >>= function
            | Some (Ok s) -> Lwt.return s
            | _ -> failm Message.(Fmt.kstr t "Token-metadata view failed.") )
        | Some big_map_id, _ ->
            get_token_metadata_map_with_big_map ~log:meta_log ~node big_map_id
      end
      >>= fun mich ->
      match Tezai_michelson.Untyped.to_micheline_node mich with
      | Prim (_, "Pair", [_; full_map], _) -> (
          let key_values =
            Michelson.Partial_type.micheline_string_bytes_map_exn full_map in
          let get l ~k = List.Assoc.find l k ~equal:String.equal in
          let _get_exn l ~k =
            match get l ~k with
            | Some s -> s
            | None ->
                Decorate_error.raise
                  Message.(t "Could not find piece-of-metadata at key" %% ct k)
          in
          let uri = get key_values ~k:"" in
          ( match uri with
          | None -> Lwt.return_none
          | Some u -> (
            match Uri.validate u with
            | Ok uri, _ ->
                Lwt.catch
                  (fun () ->
                    Uri.fetch ctxt uri ~log:(fun s ->
                        Fmt.kstr meta_log "At %s ‣ %s"
                          (ellipsize_string u ~max_length:16 ~ellipsis:"…")
                          s )
                    >>= fun s -> Lwt.return_some (u, Ezjsonm.value_from_string s)
                    )
                  (fun exn ->
                    warn "fetch-uri" (`Fetching_uri (u, exn)) ;
                    Lwt.return_none )
            | Error error, _ ->
                warn "parsing-uri" (`Parsing_uri (u, error)) ;
                Lwt.return_none ) )
          >>= fun metadata_json ->
          let piece_of_metadata ?json_type key =
            piece_of_metadata ?json_type
              ~warn:(fun k m -> warn k (`Getting_metadata_field m))
              ~key ~metadata_map:key_values ~metadata_json in
          let symbol = piece_of_metadata "symbol" () in
          let name =
            match piece_of_metadata "name" () with
            | Some "" ->
                warn "name-is-empty"
                  (`Getting_metadata_field
                    Message.(
                      t "The" %% ct "name" %% t "field is the empty string.") ) ;
                None
            | o -> o in
          let decimals = piece_of_metadata ~json_type:`Int "decimals" () in
          let tzip21, _ =
            Content.Tzip_021.from_extras
              ( List.map key_values ~f:Result.return
              @
              match metadata_json with
              | None -> []
              | Some (_, `O l) ->
                  let f = function
                    | `String s -> s
                    | `Float f -> Float.to_string f
                    | `Bool b -> Bool.to_string b
                    | other -> Ezjsonm.value_to_string other in
                  List.map l ~f:(fun (k, v) -> Ok (k, f v))
              | Some (_, _) -> [] ) in
          let multimedia_choice =
            match (tzip21.artifact, tzip21.display, tzip21.thumbnail) with
            | Some a, _, _ -> Some ("Artifact", a)
            | _, Some a, _ -> Some ("Display", a)
            | _, _, Some a -> Some ("Thumbnail", a)
            | _ -> None in
          ( match multimedia_choice with
          | None -> Lwt.return_none
          | Some (title, uri) ->
              Lwt.catch
                (fun () ->
                  Multimedia.prepare_and_guess ~uri
                    ~log:(fun s ->
                      Fmt.kstr meta_log "Preparing/Guessing %s ⏩ %s"
                        (ellipsize_string uri ~max_length:16 ~ellipsis:"…")
                        s )
                    ctxt
                    ~mime_types:
                      ( Content.Tzip_021.uri_mime_types tzip21
                      |> List.filter_map ~f:(fun (u, m) ->
                             Option.try_with (fun () ->
                                 (u, Blob.Format.of_mime_exn m) ) ) )
                  >>= fun mm -> Lwt.return_ok (title, mm) )
                (fun exn -> Lwt.return_error exn)
              >|= Option.some )
          >>= fun main_multimedia ->
          get_total_supply_with_view ()
          >>= fun total_supply ->
          let special_knowledge =
            match address with
            | "KT1M2JnD1wsg7w2B4UXJXtKQPuDUpU2L7cJH"
             |"KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton" ->
                [`Hic_et_nunc id]
            | _ -> [] in
          match main_multimedia with
          | Some (Error _) ->
              failm Message.(Fmt.kstr t "Error with the multimedia.")
          | _ ->
              Lwt.return
                (make ?symbol ?name ?decimals ~tzip21 ?main_multimedia
                   ?total_supply ~metadata:metadata_contents
                   ~network:node.Query_nodes.Node.network ~special_knowledge
                   address id ~warnings:!warnings ) )
      | other ->
          Decorate_error.raise
            Message.(
              t "Metadata result has wrong structure:"
              %% ct (Michelson.micheline_node_to_string other))
    end
end
