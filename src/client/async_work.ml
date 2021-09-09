open! Import

type log_item = Html_types.div_content_fun Meta_html.H5.elt
type status = Empty | Work_in_progress | Done
type 'a content = ('a, log_item) Result.t list

type 'a t =
  { logs: log_item Reactive.Table.t
  ; status: status Reactive.var
  ; id: int
  ; content: 'a content Reactive.var }

let _id = ref 0

let empty () =
  let id = !_id in
  Caml.incr _id ;
  { logs= Reactive.Table.make ()
  ; status= Reactive.var Empty
  ; id
  ; content= Reactive.var [] }

let logs_div_id t = Fmt.str "logs-of-async-work-%d" t.id

let reinit s =
  Reactive.Table.clear s.logs ;
  Reactive.set s.content [] ;
  Reactive.set s.status Empty

let log t item =
  Reactive.Table.append' t.logs item ;
  Lwt.async
    Lwt.Infix.(
      fun () ->
        Js_of_ocaml.(
          Js_of_ocaml_lwt.Lwt_js.sleep 0.1
          >>= fun () ->
          let divid = logs_div_id t in
          dbgf "Trying to scroll down %s" divid ;
          ( match Dom_html.getElementById_opt divid with
          | Some e -> e##.scrollTop := 100000
          | None -> dbgf "Cannot find: %s" divid ) ;
          Lwt.return_unit)) ;
  ()

let wip t = Reactive.set t.status Work_in_progress
let wip_add_ok t ok = Reactive.set t.content (Ok ok :: Reactive.peek t.content)

let wip_add_error t err =
  Reactive.set t.content (Error err :: Reactive.peek t.content)

let ok t o =
  Reactive.set t.status Done ;
  Reactive.set t.content [Ok o]

let error t o =
  Reactive.set t.status Done ;
  Reactive.set t.content [Error o]

let finish t = Reactive.set t.status Done

let busy {status; _} =
  Reactive.(
    get status |> map ~f:(function Work_in_progress -> true | _ -> false))

let peek_busy {status; _} =
  Reactive.(peek status |> function Work_in_progress -> true | _ -> false)

let is_empty {status; _} =
  Reactive.(get status |> map ~f:(function Empty -> true | _ -> false))

let async_catch :
    type b.
       'a t
    -> exn_to_html:(exn -> log_item)
    -> (mkexn:(log_item -> exn) -> unit -> unit Lwt.t)
    -> unit =
 fun wip ~exn_to_html f ->
  let open Lwt in
  let exception Work_failed of log_item in
  async (fun () ->
      catch
        (fun () -> f ~mkexn:(fun x -> Work_failed x) ())
        Meta_html.(
          function
          | Work_failed l -> error wip l ; return ()
          | exn ->
              error wip (exn_to_html exn) ;
              return ()) )

let default_show_error e =
  let open Meta_html in
  Bootstrap.bordered ~kind:`Danger (div e)

let render ?(done_empty = Meta_html.empty) ?(show_error = default_show_error)
    work_status ~f =
  let open Meta_html in
  let show_logs ?(wip = false) () =
    let make_logs_map _ x = H5.li [x] in
    let logs = Reactive.Table.concat_map ~map:make_logs_map work_status.logs in
    div
      ~a:
        [ H5.a_style (Lwd.pure "max-height: 20em; overflow: auto")
        ; H5.a_id (Lwd.pure (logs_div_id work_status)) ]
      (Bootstrap.terminal_logs
         (H5.ul
            ( if wip then
              [logs; H5.li [Bootstrap.spinner ~kind:`Info (t "Working …")]]
            else [logs] ) ) ) in
  let collapsing_logs () =
    let collapse = Bootstrap.Collapse.make () in
    Bootstrap.Collapse.fixed_width_reactive_button_with_div_below collapse
      ~width:"12em" ~kind:`Secondary
      ~button:(function true -> t "Show Logs" | false -> t "Collapse Logs")
      (fun () -> show_logs ~wip:false ()) in
  let content ~wip =
    Reactive.bind_var work_status.content ~f:(function
      | [] ->
          dbgf "*** Async_work.render inner function match - EMPTY list ***" ;
          if wip then empty () else done_empty ()
      | l ->
          dbgf "*** Async_work.render inner function match - NON-empty list ***" ;
          ( if wip then
            div
              ( it "Work in progress …"
              %% Bootstrap.spinner ~kind:`Info (t "Working …") )
          else empty () )
          % list
              (List.rev_map l ~f:(function
                | Ok o ->
                    dbgf "*** render Ok ***" ;
                    div (f o)
                | Error e ->
                    dbgf "*** render Error ***" ;
                    show_error e ) ) ) in
  Reactive.bind_var work_status.status ~f:(function
    | Empty ->
        dbgf "*** Async_work.render initial function match - Empty ***" ;
        empty ()
    | Work_in_progress ->
        dbgf
          "*** Async_work.render initial function match - Work_in_progress ***" ;
        content ~wip:true %% show_logs ~wip:true ()
    | Done ->
        dbgf "*** Async_work.render initial function match - Done ***" ;
        content ~wip:false %% collapsing_logs () )
