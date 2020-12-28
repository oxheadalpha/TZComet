open! Import

module Node_status = struct
  type t = Uninitialized | Non_responsive of exn | Ready of string
end

open Node_status

module Rpc_cache = struct
  module Hashtbl = Caml.Hashtbl

  type t = (string, float * string) Hashtbl.t

  let create () : t = Hashtbl.create 42

  let add (t : t) ~rpc ~response =
    let now = System.program_time () in
    dbgf "CACHE-ADD: %s (%.0f)" rpc now ;
    Hashtbl.add t rpc (now, response)

  let get (t : t) ~rpc =
    let now = System.program_time () in
    let best_ts = ref 0. in
    let best = ref None in
    let filter r (ts, v) =
      if Float.(!best_ts < ts) && String.equal rpc r then (
        best_ts := ts ;
        best := Some v ) ;
      if Float.(ts + 120. < now) then None else Some (ts, v) in
    Hashtbl.filter_map_inplace filter t ;
    let age = now -. !best_ts in
    dbgf "CACHE-GET:\n %s\n → now: %.0f\n → age: %.0f\n → %s" rpc now age
      (if Option.is_none !best then "MISS" else "HIT") ;
    (age, !best)
end

module Node = struct
  type t =
    { name: string
    ; prefix: string
    ; status: (float * Node_status.t) Reactive.var
    ; rpc_cache: Rpc_cache.t }

  let create name prefix =
    { name
    ; prefix
    ; status= Reactive.var (0., Uninitialized)
    ; rpc_cache= Rpc_cache.create () }

  let status n = Reactive.get n.status

  let rpc_get ctxt node path =
    let open Lwt in
    let uri = Fmt.str "%s/%s" node.prefix path in
    let fail msg =
      Decorate_error.raise
        Message.(
          t "Calling" %% ct "HTTP-GET" %% ct path %% t "on node" %% ct node.name
          %% msg) in
    let actually_get () =
      System.with_timeout ctxt
        ~f:
          Js_of_ocaml_lwt.XmlHttpRequest.(
            fun () ->
              get uri
              >>= fun frame ->
              dbgf "%s %s code: %d" node.prefix path frame.code ;
              match frame.code with
              | 200 ->
                  Rpc_cache.add node.rpc_cache ~rpc:path ~response:frame.content ;
                  return frame.content
              | other ->
                  fail Message.(t "failed with code" %% Fmt.kstr ct "%d" other))
        ~raise:(fun timeout ->
          dbgf "Node-%S GET %s → TIMEOUT" node.name path ;
          fail Message.(Fmt.kstr t "timeouted (%.03f seconds)" timeout)) in
    match Rpc_cache.get node.rpc_cache ~rpc:path with
    | _, None -> actually_get ()
    | age, Some _ when Float.(age > 120.) -> actually_get ()
    | _, Some s -> Lwt.return s

  let rpc_post ctxt node ~body path =
    let open Lwt in
    let uri = Fmt.str "%s/%s" node.prefix path in
    let fail msg =
      Decorate_error.raise
        Message.(
          t "Calling" %% ct "HTTP-POST" %% ct path %% t "on node"
          %% ct node.name %% t "with" %% code_block body %% msg) in
    System.with_timeout ctxt
      ~f:
        Js_of_ocaml_lwt.XmlHttpRequest.(
          fun () ->
            perform ~contents:(`String body) ~content_type:"application/json"
              (Option.value_exn ~message:"uri-of-string"
                 (Js_of_ocaml.Url.url_of_string uri))
            >>= fun frame ->
            dbgf "%s %s code: %d" node.prefix path frame.code ;
            match frame.code with
            | 200 -> return frame.content
            | other ->
                dbgf "CONTENT: %s" frame.content ;
                fail
                  Message.(
                    Fmt.kstr t "failed with with return code %d:" other
                    %% code_block
                         ( try
                             Ezjsonm.value_from_string frame.content
                             |> Ezjsonm.value_to_string ~minify:false
                           with _ -> frame.content )))
      ~raise:(fun timeout ->
        fail Message.(Fmt.kstr t "timeouted (%03.f seconds)." timeout))

  let ping ctxt node =
    let open Lwt.Infix in
    Lwt.catch
      (fun () ->
        rpc_get ctxt node "/chains/main/blocks/head/metadata"
        >>= fun metadata -> Lwt.return (Ready metadata))
      (fun e -> Lwt.return (Non_responsive e))

  let metadata_big_map state_handle node ~address ~log =
    let open Lwt in
    let get = rpc_get state_handle node in
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
    log "Got raw script: %s…" (ellipsize_string script_string ~max_length:30) ;
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

  let bytes_value_of_big_map_at_string ctxt node ~big_map_id ~key ~log =
    let open Lwt in
    let hash_string = B58_hashes.b58_script_id_hash_of_michelson_string key in
    Decorate_error.(
      reraise
        Message.(
          t "Cannot find any value in the big-map"
          %% ct (Z.to_string big_map_id)
          %% t "at the key" %% ct key %% t "(hash: " % ct hash_string % t ").")
        ~f:(fun () ->
          Fmt.kstr (rpc_get ctxt node)
            "/chains/main/blocks/head/context/big_maps/%s/%s"
            (Z.to_string big_map_id) hash_string))
    >>= fun bytes_raw_value ->
    Fmt.kstr log "bytes raw value: %s"
      (ellipsize_string bytes_raw_value ~max_length:30) ;
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

module Node_list = struct
  type t = (string, Node.t * bool) List.Assoc.t

  let empty : t = []

  let add ?(dev = false) t n =
    List.Assoc.add ~equal:String.equal t n.Node.name (n, dev)

  let remove ?(dev = false) t n =
    List.Assoc.remove ~equal:String.equal t n.Node.name

  let remove_dev t =
    List.filter t ~f:(function _, (_, true) -> false | _ -> true)

  let fold_nodes t ~init ~f = List.fold t ~init ~f:(fun p (_, (n, _)) -> f p n)
  let map t ~f = List.map t ~f:(fun (_, (n, _)) -> f n)
  let concat_map t ~f = List.concat_map t ~f:(fun (_, (n, _)) -> f n)
end

type t =
  { nodes: Node_list.t Reactive.var
  ; wake_up_call: unit Lwt_condition.t
  ; loop_started: bool Reactive.var
  ; loop_interval: float Reactive.var
  ; loop_status: [`Not_started | `In_progress | `Sleeping] Reactive.var }

let create () =
  { nodes= Reactive.var Node_list.empty
  ; wake_up_call= Lwt_condition.create ()
  ; loop_started= Reactive.var false
  ; loop_interval= Reactive.var 10.
  ; loop_status= Reactive.var `Not_started }

let get (ctxt : < nodes: t ; .. > Context.t) = ctxt#nodes
let nodes t = (get t).nodes

let get_nodes t ~map =
  Reactive.pair ((get t).nodes |> Reactive.get) (System.dev_mode t)
  |> Reactive.map ~f:(function
       | l, true -> Node_list.map ~f:map l
       | l, false -> Node_list.map ~f:map (Node_list.remove_dev l))

let add_node ?dev ctxt nod =
  Reactive.set (nodes ctxt)
    (Node_list.add ?dev (Reactive.peek (nodes ctxt)) nod)

let default_nodes =
  List.rev
    [ Node.create "Delphinet-GigaNode" "https://delphinet-tezos.giganode.io"
    ; Node.create "Mainnet-GigaNode" "https://mainnet-tezos.giganode.io"
    ; Node.create "Current-Testnet-GigaNode" "https://testnet-tezos.giganode.io"
    ; Node.create "Edonet-GigaNode" "https://edonet-tezos.giganode.io"
    ; Node.create "Carthagenet-SmartPy" "https://carthagenet.smartpy.io"
    ; Node.create "Mainnet-SmartPy" "https://mainnet.smartpy.io"
    ; Node.create "Delphinet-SmartPy" "https://delphinet.smartpy.io" ]

let dev_nodes =
  List.rev
    [ Node.create "Dev:Wrong-node" "http://example.com/nothing"
    ; Node.create "Dev:Minibox-node" "http://127.0.0.1:20000" ]

let add_default_nodes ctxt =
  List.iter ~f:(add_node ~dev:true ctxt) dev_nodes ;
  List.iter ~f:(add_node ~dev:false ctxt) default_nodes

let loop_interval ctxt = Reactive.peek (get ctxt).loop_interval
let loop_status ctxt = Reactive.get (get ctxt).loop_status
let set_loop_status ctxt = Reactive.set (get ctxt).loop_status

let observe_nodes ctxt =
  let data = Reactive.(get_nodes ~map:Fn.id ctxt) in
  let data_root = Reactive.observe data in
  let nodes = Reactive.quick_sample data_root in
  Reactive.quick_release data_root ;
  nodes

module Update_status_loop = struct
  let wake_up ctxt = Lwt_condition.broadcast (get ctxt).wake_up_call ()
  let wait_for_wake_up t = Lwt_condition.wait (get t).wake_up_call

  let start ctxt =
    let open Lwt.Infix in
    Lwt.ignore_result
      (let rec loop count =
         set_loop_status ctxt `In_progress ;
         let sleep_time = loop_interval ctxt in
         let nodes = observe_nodes ctxt in
         dbgf "update-loop %d (%f s) %d nodes" count sleep_time
           (List.length nodes) ;
         List.fold nodes ~init:Lwt.return_unit ~f:(fun prevm nod ->
             prevm
             >>= fun () ->
             Node.ping ctxt nod
             >>= fun new_status ->
             dbgf "got status for %s" nod.name ;
             let now = System.now () in
             Reactive.set nod.status (now, new_status) ;
             Lwt.return ())
         >>= fun () ->
         set_loop_status ctxt `Sleeping ;
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
  let trace = ref [] in
  catch
    (fun () ->
      Lwt_list.find_s
        (fun node ->
          catch
            (fun () ->
              Fmt.kstr (Node.rpc_get ctxt node)
                "/chains/main/blocks/head/context/contracts/%s/storage" addr
              >>= fun _ -> return_true)
            (fun exn ->
              trace := exn :: !trace ;
              return_false))
        (observe_nodes ctxt)
      >>= fun node -> Lwt.return node)
    (fun exn ->
      Decorate_error.raise ~trace:(List.rev !trace)
        Message.(t "Cannot find a node that knows about address" %% ct addr))

let metadata_value ctxt ~address ~key ~(log : string -> unit) =
  let open Lwt in
  let logf f = Fmt.kstr log f in
  find_node_with_contract ctxt address
  >>= fun node ->
  logf "Found contract with node %S" node.Node.name ;
  Node.metadata_big_map ctxt node ~address ~log
  >>= fun big_map_id ->
  logf "Metadata big-map: %s" (Z.to_string big_map_id) ;
  Node.bytes_value_of_big_map_at_string ctxt node ~big_map_id ~key ~log

let call_off_chain_view ctxt ~log ~address ~view ~parameter =
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
  find_node_with_contract ctxt address
  >>= fun node ->
  logf "Found contract with node %S" node.name ;
  Fmt.kstr (Node.rpc_get ctxt node) "/chains/main/blocks/head/protocols"
  >>= fun protocols ->
  let protocol_kind, protocol_hash =
    let hash =
      match Ezjsonm.value_from_string protocols with
      | `O l ->
          List.find_map l ~f:(function
            | "protocol", `String p -> Some p
            | _ -> None)
      | _ | (exception _) -> None in
    match hash with
    | None ->
        Decorate_error.raise
          Message.(
            t "Cannot understand answer from “protocols” RPC:"
            %% code_block protocols)
    | Some p when String.is_prefix p ~prefix:"PsCARTHA" -> (`Carthage, p)
    | Some p when String.is_prefix p ~prefix:"PsDELPH1" -> (`Delphi, p)
    | Some p when String.is_prefix p ~prefix:"PtEdoTez" -> (`Edo, p)
    | Some p when String.is_prefix p ~prefix:"ProtoALpha" -> (`Edo, p)
    | Some p ->
        logf "Can't recognize protocol: `%s` assuming Delphi-like." p ;
        (`Delphi, p) in
  logf "Protocol is `%s`" protocol_hash ;
  Fmt.kstr (Node.rpc_get ctxt node)
    "/chains/main/blocks/head/context/contracts/%s/storage" address
  >>= fun storage ->
  logf "Got the storage: %s" storage ;
  Fmt.kstr (Node.rpc_get ctxt node)
    "/chains/main/blocks/head/context/contracts/%s/script" address
  >>= fun script ->
  Fmt.kstr (Node.rpc_get ctxt node)
    "/chains/main/blocks/head/context/contracts/%s/balance" address
  >>= fun balance ->
  let balance = Ezjsonm.(value_from_string balance |> get_string) in
  Fmt.kstr (Node.rpc_get ctxt node) "/chains/main/chain_id"
  >>= fun chain_id ->
  let chain_id = Ezjsonm.(value_from_string chain_id |> get_string) in
  logf "Got the script: %s" (ellipsize_string script ~max_length:30) ;
  let contract_storage = Michelson.micheline_of_json storage in
  let `Contract view_contract, `Input view_input, `Storage view_storage =
    let code_mich = Michelson.micheline_of_json script in
    let open Tezos_contract_metadata.Contract_storage in
    let contract_storage_type =
      get_storage_type_exn (Tezos_micheline.Micheline.strip_locations code_mich)
    in
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
                    , [Prim (_loc, "CDR", [], []); Prim (_loc, "CAR", [], [])]
                    )
              | Prim (_loc, _prim, args, _annot) ->
                  Prim (_loc, _prim, List.map ~f:go args, _annot)
              | Seq (loc, args) -> Seq (loc, List.map ~f:go args) in
            go node |> strip_locations in
      {view with code= Micheline code} in
    build_off_chain_view_contract view ~contract_balance:(Z.of_string balance)
      ~contract_address:address ~contract_storage ~view_parameters
      ~contract_storage_type ~contract_parameter_type in
  logf "Made the view-script: %a"
    Tezos_contract_metadata.Contract_storage.pp_arbitrary_micheline
    view_contract ;
  logf "Made the view-input: %a"
    Tezos_contract_metadata.Contract_storage.pp_arbitrary_micheline view_input ;
  logf "Made the view-storage: %a"
    Tezos_contract_metadata.Contract_storage.pp_arbitrary_micheline view_storage ;
  let constructed =
    let open Ezjsonm in
    let normal_fields =
      [ ("script", Michelson.micheline_to_ezjsonm view_contract)
      ; ("storage", Michelson.micheline_to_ezjsonm view_storage)
      ; ("input", Michelson.micheline_to_ezjsonm view_input)
      ; ("amount", string "0"); ("chain_id", string chain_id) ] in
    let fields =
      match protocol_kind with
      | `Edo -> normal_fields @ [("balance", string "0")]
      | _ -> normal_fields in
    dict fields in
  Node.rpc_post ctxt node
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
          Tezos_contract_metadata.Contract_storage.pp_arbitrary_micheline other
  in
  return (Ok (actual_result, contract_storage))
