open Import

module Tezos_nodes = struct
  module Node_status = struct
    type t = Uninitialized | Non_responsive of string | Ready of string
  end

  open Node_status

  module Node = struct
    type t =
      {name: string; prefix: string; status: (float * Node_status.t) Var.t}

    let create name prefix =
      {name; prefix; status= Var.create "node-status" (0., Uninitialized)}

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
                        return (Non_responsive "Time-out while getting status")
                        )
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
end

let gui ?version_string state =
  let nodes = Tezos_nodes._global in
  Tezos_nodes.ensure_update_loop nodes ;
  RD.(txt "Hello world")

let attach_to_page gui =
  let open Js_of_ocaml in
  let base_div = Dom_html.getElementById "attach-ui" in
  base_div##.innerHTML := Js.string "" ;
  base_div##appendChild (Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_node gui)
  |> ignore ;
  Lwt.return ()

let go _ =
  dbg Fmt.(const string "Hello Go!") ;
  Bootstrap_css.ensure () ;
  ignore
    Lwt.(
      catch
        (fun () ->
          Js_of_ocaml_lwt.XmlHttpRequest.(
            get "./VERSION"
            >>= fun frame ->
            dbgf "version: %d" frame.code ;
            if frame.code = 200 then return (Some frame.content)
            else return None)
          >>= fun version_string ->
          attach_to_page (gui ?version_string state) >>= fun () -> return ())
        (fun exn ->
          Printf.ksprintf
            (fun s -> Fmt.epr "ERROR: %s" s ; failwith s)
            "Uncaught Exception: %s" (Exn.to_string exn))) ;
  Js_of_ocaml.Js._true

let _ =
  dbgf "Hello Main!" ;
  let open Js_of_ocaml in
  (Lwt.async_exception_hook := fun e -> dbgf "Async Exn: %s" (Exn.to_string e)) ;
  Dom_html.window##.onload := Dom_html.handler go
