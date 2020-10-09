open! Import

module Node_status = struct
  type t = Uninitialized | Non_responsive of string | Ready of string
end

open Node_status

let micheline_of_ezjsonm json =
  let enc =
    Tezos_micheline.Micheline.canonical_encoding ~variant:"custom"
      Data_encoding.string in
  let mich = Data_encoding.Json.destruct enc json in
  Tezos_micheline.Micheline.root mich

let micheline_of_json s =
  let json =
    match Ezjsonm.value_from_string s with
    | `O (("code", code) :: _) -> code
    | other -> other in
  micheline_of_ezjsonm json

let micheline_to_ezjsonm mich =
  let enc =
    Tezos_micheline.Micheline.canonical_encoding ~variant:"custom"
      Data_encoding.string in
  let json =
    Data_encoding.Json.construct enc
      (Tezos_micheline.Micheline.strip_locations mich) in
  json

module Node = struct
  type t = {name: string; prefix: string; status: (float * Node_status.t) Var.t}

  let create name prefix =
    {name; prefix; status= Var.create "node-status" (0., Uninitialized)}

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
    let mich_storage = micheline_of_json storage_string in
    log "As concrete: %a"
      Tezos_contract_metadata.Contract_storage.pp_arbitrary_micheline
      mich_storage ;
    State.slow_step state_handle
    >>= fun () ->
    Fmt.kstr get "/chains/main/blocks/head/context/contracts/%s/script" address
    >>= fun script_string ->
    log "Got raw script: %s…" (String.prefix script_string 30) ;
    let mich_storage_type =
      micheline_of_json script_string
      |> Tezos_micheline.Micheline.strip_locations
      |> Tezos_contract_metadata.Contract_storage.get_storage_type_exn in
    log "Storage type: %a"
      Tezos_contract_metadata.Contract_storage.pp_arbitrary_micheline
      mich_storage_type ;
    State.slow_step state_handle
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
      match Ezjsonm.value_from_string bytes_raw_value with
      | `O [("bytes", `String b)] -> Hex.to_string (`Hex b)
      | _ -> Fmt.failwith "Cannot find bytes in %s" bytes_raw_value in
    return content
end

type t =
  { nodes: Node.t list Var.t
  ; wake_up_call: unit Lwt_condition.t
  ; loop_started: bool Var.t
  ; loop_interval: float Var.t }

let create nodes =
  { nodes=
      Var.create "list-of-nodes" nodes
        ~eq:(List.equal Node.(fun na nb -> String.equal na.prefix nb.prefix))
  ; wake_up_call= Lwt_condition.create ()
  ; loop_started= Var.create "loop-started" false
  ; loop_interval= Var.create "loop-interval" 10. }

let nodes t = t.nodes

let _global =
  create
    [ Node.create "Carthagenet-GigaNode" "https://testnet-tezos.giganode.io"
    ; Node.create "Mainnet-GigaNode" "https://mainnet-tezos.giganode.io"
    ; Node.create "Dalphanet-GigaNode" "https://dalphanet-tezos.giganode.io"
    ; Node.create "Carthagenet-SmartPy" "https://carthagenet.smartpy.io"
    ; Node.create "Mainnet-SmartPy" "https://mainnet.smartpy.io"
    ; Node.create "Delphinet-SmartPy" "https://delphinet.smartpy.io" ]

let wake_up_update_loop t = Lwt_condition.broadcast t.wake_up_call ()

let start_update_loop t =
  let open Lwt in
  ignore_result
    (let rec loop count =
       let sleep_time = Var.value t.loop_interval in
       dbgf "update-loop %d (%f s)" count sleep_time ;
       Var.value t.nodes
       |> List.fold ~init:return_unit ~f:(fun prevm nod ->
              prevm
              >>= fun () ->
              catch
                (fun () ->
                  pick
                    [ ( Js_of_ocaml_lwt.Lwt_js.sleep 5.
                      >>= fun () ->
                      dbgf "%s timeout in start_update_loop" nod.Node.name ;
                      return (Non_responsive "Time-out while getting status") )
                    ; ( Node.ping nod
                      >>= fun res ->
                      dbgf "%s returned to start_update_loop" nod.name ;
                      return res ) ])
                (fun e ->
                  return (Non_responsive (Fmt.str "Error: %a" Exn.pp e)))
              >>= fun new_status ->
              dbgf "got status for %s" nod.name ;
              let now = (new%js Js_of_ocaml.Js.date_now)##valueOf in
              Var.set nod.status (now, new_status) ;
              return ())
       >>= fun () ->
       pick
         [ Js_of_ocaml_lwt.Lwt_js.sleep sleep_time
         ; Lwt_condition.wait t.wake_up_call ]
       >>= fun () ->
       Var.set t.loop_interval (Float.min (sleep_time *. 1.4) 90.) ;
       loop (count + 1) in
     loop 0)

let ensure_update_loop t =
  match Var.value t.loop_started with
  | true -> ()
  | false ->
      start_update_loop t ;
      Var.set t.loop_started true

let find_node_with_contract node_list addr =
  let open Lwt in
  catch
    (fun () ->
      Lwt_list.find_s
        (fun node ->
          catch
            (fun () ->
              Fmt.kstr (Node.rpc_get node)
                "/chains/main/blocks/head/context/contracts/%s/storage" addr
              >>= fun _ -> return_true)
            (fun _ -> return_false))
        (nodes node_list |> Var.value))
    (fun _ -> Fmt.failwith "Cannot find a node that knows about %S" addr)

let metadata_value state_handle nodes ~address ~key ~log =
  let open Lwt in
  let logf f = Fmt.kstr log f in
  find_node_with_contract nodes address
  >>= fun node ->
  logf "Found contract with node %S" node.name ;
  Node.metadata_big_map state_handle node ~address ~log
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
      let contract_storage = micheline_of_json storage in
      let `Contract view_contract, `Input view_input, `Storage view_storage =
        let code_mich = micheline_of_json script in
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
          [ ("script", micheline_to_ezjsonm view_contract)
          ; ("storage", micheline_to_ezjsonm view_storage)
          ; ("input", micheline_to_ezjsonm view_input); ("amount", string "0")
          ; ("chain_id", string chain_id) ] in
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
          | Some json -> micheline_of_ezjsonm json in
        let open Tezos_micheline.Micheline in
        match mich with
        | Prim (_, "Some", [s], _) -> s
        | other ->
            Fmt.failwith "Result is not (Some _): %a"
              Tezos_contract_metadata.Contract_storage.pp_arbitrary_micheline
              other in
      return (Ok (actual_result, contract_storage)))
    (fun e -> return (Error (Fmt.str "FAILED: %a" Exn.pp e)))

let table_of_statuses node_list =
  let open RD in
  let node_status node =
    let node_metadata _date json =
      let open Ezjsonm in
      try
        let j = value_from_string json in
        let field f j =
          try List.Assoc.find_exn ~equal:String.equal (get_dict j) f
          with _ ->
            Fmt.failwith "Cannot find %S in %s" f
              (value_to_string ~minify:true j) in
        code ~a:[ (* Fmt.kstr a_ "%.03f" date *) ]
          [ Fmt.kstr txt "Level: %d"
              (field "level" j |> field "level" |> get_int) ]
      with e ->
        code [Fmt.kstr txt "Failed to parse the Metadata JSON: %a" Exn.pp e]
    in
    Reactive.div
      (Var.map_to_list node.Node.status
         ~f:
           Node_status.(
             fun (date, status) ->
               let show s = [code [s]] in
               match status with
               | Uninitialized -> show (txt "Uninitialized")
               | Non_responsive reason ->
                   show (Fmt.kstr txt "Non-responsive: %s" reason)
               | Ready metadata -> [node_metadata date metadata])) in
  tablex
    ~a:[a_class ["table"; "table-bordered"; "table-hover"]]
    ~thead:
      (thead
         [ tr
             [ th [txt "Name"]; th [txt "URI-prefix"]; th [txt "Status"]
             ; th [txt "Latest Ping"] ] ])
    [ Reactive.tbody
        (Var.map_to_list (nodes node_list) ~f:(fun nodes ->
             List.map nodes ~f:(fun node ->
                 let open Node in
                 let open Node_status in
                 tr ~a:[a_style "height: 3em"]
                   [ td
                       ~a:
                         [ Reactive.a_class
                             ( Var.signal node.status
                             |> React.S.map (function
                                  | _, Uninitialized -> ["bg-warning"]
                                  | _, Non_responsive _ -> ["bg-danger"]
                                  | _, Ready _ -> ["bg-success"]) ) ]
                       [em [txt node.name]]; td [code [txt node.prefix]]
                   ; td [node_status node]
                   ; td
                       [ Reactive.code
                           (Var.map_to_list node.status ~f:(fun (date, _) ->
                                let date_string =
                                  (new%js Js_of_ocaml.Js.date_fromTimeValue
                                     date)##toISOString
                                  |> Js_of_ocaml__Js.to_string in
                                [txt date_string])) ] ]))) ]
