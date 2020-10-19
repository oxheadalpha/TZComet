open! Import

module Node_status = struct
  type t = Uninitialized | Non_responsive of string | Ready of string
end

open Node_status

module Node = struct
  type t =
    {name: string; prefix: string; status: (float * Node_status.t) Reactive.var}

  let create name prefix =
    {name; prefix; status= Reactive.var (0., Uninitialized)}

  let rpc_get node path =
    let open Lwt in
    let uri = Fmt.str "%s/%s" node.prefix path in
    Js_of_ocaml_lwt.XmlHttpRequest.(
      get uri
      >>= fun frame ->
      dbgf "%s %s code: %d" node.prefix path frame.code ;
      match frame.code with
      | 200 -> return frame.content
      | other -> Fmt.failwith "Getting %S returned code: %d" path other)

  let ping node =
    let open Lwt in
    Js_of_ocaml_lwt.XmlHttpRequest.(
      Fmt.kstr get "%s/chains/main/blocks/head/metadata" node.prefix
      >>= fun frame ->
      dbgf "%s metadata code: %d" node.name frame.code ;
      let new_status =
        match frame.code with
        | 200 ->
            dbgf "%s metadata content: %s" node.name frame.content ;
            Ready frame.content
        | other -> Non_responsive (Fmt.str "Return-code: %d" other) in
      return new_status)

  let metadata_big_map state_handle node ~address ~log =
    let open Lwt in
    let get = rpc_get node in
    let log fmt = Fmt.kstr log fmt in
    Fmt.kstr get "/chains/main/blocks/head/context/contracts/%s/storage" address
    >>= fun storage_string ->
    log "Got raw storage: %s" storage_string ;
    let mich_storage = Michelson.micheline_of_json storage_string in
    log "As concrete: %a"
      Tezos_contract_metadata.Contract_storage.pp_arbitrary_micheline
      mich_storage ;
    System.slow_step state_handle
    >>= fun () ->
    Fmt.kstr get "/chains/main/blocks/head/context/contracts/%s/script" address
    >>= fun script_string ->
    log "Got raw script: %s…" (String.prefix script_string 30) ;
    let mich_storage_type =
      Michelson.micheline_of_json script_string
      |> Tezos_micheline.Micheline.strip_locations
      |> Tezos_contract_metadata.Contract_storage.get_storage_type_exn in
    log "Storage type: %a"
      Tezos_contract_metadata.Contract_storage.pp_arbitrary_micheline
      mich_storage_type ;
    System.slow_step state_handle
    >>= fun () ->
    let bgs =
      Tezos_contract_metadata.Contract_storage.find_metadata_big_maps
        ~storage_node:mich_storage ~type_node:mich_storage_type in
    match bgs with
    | [] -> Fmt.failwith "Contract has no valid %%metadata big-map!"
    | _ :: _ :: _ ->
        Fmt.failwith "Contract has too many %%metadata big-maps: %s"
          ( oxfordize_list bgs ~map:Z.to_string
              ~sep:(fun () -> ",")
              ~last_sep:(fun () -> ", and ")
          |> String.concat ~sep:"" )
    | [one] -> return one

  let bytes_value_of_big_map_at_string node ~big_map_id ~key ~log =
    let open Lwt in
    let hash_string = B58_hashes.b58_script_id_hash_of_michelson_string key in
    Fmt.kstr (rpc_get node) "/chains/main/blocks/head/context/big_maps/%s/%s"
      (Z.to_string big_map_id) hash_string
    >>= fun bytes_raw_value ->
    Fmt.kstr log "bytes raw value: %s" bytes_raw_value ;
    let content =
      (* The code below was throwing a stack-overflow: *)
      (* match Ezjsonm.value_from_string bytes_raw_value with
         | `O [("bytes", `String b)] -> Hex.to_string (`Hex b)
         | _ -> Fmt.failwith "Cannot find bytes in %s" bytes_raw_value
         | exception e -> *)
      let v =
        Js_of_ocaml.Json.unsafe_input (Js_of_ocaml.Js.string bytes_raw_value)
      in
      dbgf "v: %s" v##.bytes ;
      Hex.to_string (`Hex v##.bytes) in
    return content
end

type t =
  { nodes: Node.t Reactive.Table.t
  ; wake_up_call: unit Lwt_condition.t
  ; loop_started: bool Reactive.var
  ; loop_interval: float Reactive.var }

let create () =
  { nodes=
      Reactive.Table.make ()
      (* ~eq:(List.equal Node.(fun na nb -> String.equal na.prefix nb.prefix)) *)
  ; wake_up_call= Lwt_condition.create ()
  ; loop_started= Reactive.var false
  ; loop_interval= Reactive.var 10. }

let get (ctxt : < nodes: t ; .. > Context.t) = ctxt#nodes
let nodes t = (get t).nodes
let add_node ctxt nod = Reactive.Table.append (nodes ctxt) nod

let default_nodes =
  [ Node.create "Carthagenet-GigaNode" "https://testnet-tezos.giganode.io"
  ; Node.create "Mainnet-GigaNode" "https://mainnet-tezos.giganode.io"
  ; Node.create "Dalphanet-GigaNode" "https://dalphanet-tezos.giganode.io"
  ; Node.create "Carthagenet-SmartPy" "https://carthagenet.smartpy.io"
  ; Node.create "Mainnet-SmartPy" "https://mainnet.smartpy.io"
  ; Node.create "Delphinet-SmartPy" "https://delphinet.smartpy.io" ]

let add_default_nodes ctxt = List.iter ~f:(add_node ctxt) default_nodes
let loop_interval ctxt = Reactive.peek (get ctxt).loop_interval

module Update_status_loop = struct
  let wake_up ctxt = Lwt_condition.broadcast (get ctxt).wake_up_call ()
  let wait_for_wake_up t = Lwt_condition.wait (get t).wake_up_call

  let start ctxt =
    let open Lwt.Infix in
    Lwt.ignore_result
      (let rec loop count =
         let sleep_time = loop_interval ctxt in
         dbgf "update-loop %d (%f s)" count sleep_time ;
         Reactive.Table.fold (nodes ctxt) ~init:Lwt.return_unit
           ~f:(fun prevm nod ->
             prevm
             >>= fun () ->
             Lwt.catch
               (fun () ->
                 Lwt.pick
                   [ ( Js_of_ocaml_lwt.Lwt_js.sleep 5.
                     >>= fun () ->
                     dbgf "%s timeout in start_update_loop" nod.Node.name ;
                     Lwt.return (Non_responsive "Time-out while getting status")
                     )
                   ; ( Node.ping nod
                     >>= fun res ->
                     dbgf "%s returned to start_update_loop" nod.name ;
                     Lwt.return res ) ])
               (fun e ->
                 Lwt.return (Non_responsive (Fmt.str "Error: %a" Exn.pp e)))
             >>= fun new_status ->
             dbgf "got status for %s" nod.name ;
             let now = (new%js Js_of_ocaml.Js.date_now)##valueOf in
             Reactive.set nod.status (now, new_status) ;
             Lwt.return ())
         >>= fun () ->
         Lwt.pick
           [Js_of_ocaml_lwt.Lwt_js.sleep sleep_time; wait_for_wake_up ctxt]
         >>= fun () ->
         Reactive.set (get ctxt).loop_interval
           (Float.min (sleep_time *. 1.4) 120.) ;
         loop (count + 1) in
       loop 0)

  let ensure t =
    match Reactive.peek (get t).loop_started with
    | true -> ()
    | false ->
        start t ;
        Reactive.set (get t).loop_started true
end

let find_node_with_contract ctxt addr =
  let open Lwt in
  Reactive.Table.Lwt.find (nodes ctxt) ~f:(fun node ->
      catch
        (fun () ->
          Fmt.kstr (Node.rpc_get node)
            "/chains/main/blocks/head/context/contracts/%s/storage" addr
          >>= fun _ -> return_true)
        (fun _ -> return_false))
  >>= function
  | Some node -> Lwt.return node
  | None -> Fmt.failwith "Cannot find a node that knows about %S" addr

let metadata_value ctxt ~address ~key ~(log : string -> unit) =
  let open Lwt in
  let logf f = Fmt.kstr log f in
  find_node_with_contract ctxt address
  >>= fun node ->
  logf "Found contract with node %S" node.Node.name ;
  Node.metadata_big_map ctxt node ~address ~log
  >>= fun big_map_id ->
  logf "Metadata big-map: %s" (Z.to_string big_map_id) ;
  Node.bytes_value_of_big_map_at_string node ~big_map_id ~key ~log

let call_off_chain_view nodes ~log ~address ~view ~parameter =
  let open Lwt in
  let open Tezos_contract_metadata.Metadata_contents.View.Implementation
           .Michelson_storage in
  let logf f =
    Fmt.kstr
      (fun s ->
        log s ;
        dbgf "call_off_chain_view: %s" s)
      f in
  logf "Calling %s(%a)" address
    Tezos_contract_metadata.Contract_storage.pp_arbitrary_micheline parameter ;
  find_node_with_contract nodes address
  >>= fun node ->
  logf "Found contract with node %S" node.name ;
  catch
    (fun () ->
      Fmt.kstr (Node.rpc_get node)
        "/chains/main/blocks/head/context/contracts/%s/storage" address
      >>= fun storage ->
      logf "Got the storage: %s" storage ;
      Fmt.kstr (Node.rpc_get node)
        "/chains/main/blocks/head/context/contracts/%s/script" address
      >>= fun script ->
      Fmt.kstr (Node.rpc_get node)
        "/chains/main/blocks/head/context/contracts/%s/balance" address
      >>= fun balance ->
      let balance = Ezjsonm.(value_from_string balance |> get_string) in
      Fmt.kstr (Node.rpc_get node) "/chains/main/chain_id"
      >>= fun chain_id ->
      let chain_id = Ezjsonm.(value_from_string chain_id |> get_string) in
      logf "Got the script: %s" script ;
      let contract_storage = Michelson.micheline_of_json storage in
      let `Contract view_contract, `Input view_input, `Storage view_storage =
        let code_mich = Michelson.micheline_of_json script in
        let open Tezos_contract_metadata.Contract_storage in
        let contract_storage_type =
          get_storage_type_exn
            (Tezos_micheline.Micheline.strip_locations code_mich) in
        let contract_parameter_type =
          get_parameter_type_exn
            (Tezos_micheline.Micheline.strip_locations code_mich) in
        let view_parameters =
          Tezos_micheline.Micheline.(strip_locations parameter |> root) in
        let view =
          (* TEMPORARY: this is one macro expansion for the test that is on
             carthagenet *)
          let code =
            match view.code with
            | Micheline mich ->
                let open Tezos_micheline.Micheline in
                let node = root mich in
                let rec go = function
                  | (Int _ | String _ | Bytes _) as ok -> ok
                  | Prim (_loc, "CDAR", [], _annot) ->
                      Seq
                        ( _loc
                        , [ Prim (_loc, "CDR", [], [])
                          ; Prim (_loc, "CAR", [], []) ] )
                  | Prim (_loc, _prim, args, _annot) ->
                      Prim (_loc, _prim, List.map ~f:go args, _annot)
                  | Seq (loc, args) -> Seq (loc, List.map ~f:go args) in
                go node |> strip_locations in
          {view with code= Micheline code} in
        build_off_chain_view_contract view
          ~contract_balance:(Z.of_string balance) ~contract_address:address
          ~contract_storage ~view_parameters ~contract_storage_type
          ~contract_parameter_type in
      logf "Made the view-script: %a"
        Tezos_contract_metadata.Contract_storage.pp_arbitrary_micheline
        view_contract ;
      logf "Made the view-input: %a"
        Tezos_contract_metadata.Contract_storage.pp_arbitrary_micheline
        view_input ;
      logf "Made the view-storage: %a"
        Tezos_contract_metadata.Contract_storage.pp_arbitrary_micheline
        view_storage ;
      let rpc_post node ~body path =
        let open Node in
        let open Lwt in
        let uri = Fmt.str "%s/%s" node.prefix path in
        Js_of_ocaml_lwt.XmlHttpRequest.(
          perform ~contents:(`String body) ~content_type:"application/json"
            (Option.value_exn ~message:"uri-of-string"
               (Js_of_ocaml.Url.url_of_string uri))
          >>= fun frame ->
          dbgf "%s %s code: %d" node.prefix path frame.code ;
          match frame.code with
          | 200 -> return frame.content
          | other ->
              dbgf "CONTENT: %s" frame.content ;
              Fmt.failwith "Getting %S returned code: %d" path other) in
      let constructed =
        let open Ezjsonm in
        dict
          [ ("script", Michelson.micheline_to_ezjsonm view_contract)
          ; ("storage", Michelson.micheline_to_ezjsonm view_storage)
          ; ("input", Michelson.micheline_to_ezjsonm view_input)
          ; ("amount", string "0"); ("chain_id", string chain_id) ] in
      rpc_post node
        ~body:(Ezjsonm.value_to_string constructed)
        "/chains/main/blocks/head/helpers/scripts/run_code"
      >>= fun result ->
      logf "RESULT: %s" result ;
      (*
      POST /chains/main/blocks/head/helpers/scripts/run_code
      "properties":
        { "script":
            { "$ref": "#/definitions/micheline.michelson_v1.expression" },
          "storage":
            { "$ref": "#/definitions/micheline.michelson_v1.expression" },
          "input":
            { "$ref": "#/definitions/micheline.michelson_v1.expression" },
          "amount": { "$ref": "#/definitions/mutez" },
          "chain_id": { "$ref": "#/definitions/Chain_id" },
          "source": { "$ref": "#/definitions/contract_id" },
          "payer": { "$ref": "#/definitions/contract_id" },
          "gas": { "$ref": "#/definitions/bignum" },
          "entrypoint": { "$ref": "#/definitions/unistring" } },
      "required": [ "chain_id", "amount", "input", "storage", "script" ],
     *)
      let actual_result =
        let open Ezjsonm in
        let d = value_from_string result |> get_dict in
        let mich =
          match List.Assoc.find ~equal:String.equal d "storage" with
          | None -> Fmt.failwith "Result has not storage: %S" result
          | Some json -> Michelson.micheline_of_ezjsonm json in
        let open Tezos_micheline.Micheline in
        match mich with
        | Prim (_, "Some", [s], _) -> s
        | other ->
            Fmt.failwith "Result is not (Some _): %a"
              Tezos_contract_metadata.Contract_storage.pp_arbitrary_micheline
              other in
      return (Ok (actual_result, contract_storage)))
    (fun e -> return (Error (Fmt.str "FAILED: %a" Exn.pp e)))