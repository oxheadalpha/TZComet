open! Import

type log_item = Html_types.div_content_fun Meta_html.H5.elt
type 'a status = Empty | Work_in_progress | Done of ('a, log_item) Result.t
type 'a t = {logs: log_item Reactive.Table.t; status: 'a status Reactive.var}

let empty () = {logs= Reactive.Table.make (); status= Reactive.var Empty}

let reinit s =
  Reactive.Table.clear s.logs ;
  Reactive.set s.status Empty

let log t item = Reactive.Table.append' t.logs item
let wip t = Reactive.set t.status Work_in_progress
let ok t o = Reactive.set t.status (Done (Ok o))
let error t o = Reactive.set t.status (Done (Error o))

let busy {status; _} =
  Reactive.(
    get status |> map ~f:(function Work_in_progress -> true | _ -> false))

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
              return ()))

let render work_status ~f =
  let open Meta_html in
  let show_logs ?(wip = false) () =
    let make_logs_map _ x = H5.li [x] in
    let logs = Reactive.Table.concat_map ~map:make_logs_map work_status.logs in
    Bootstrap.terminal_logs
      (H5.ul
         ( if wip then
           [logs; H5.li [Bootstrap.spinner ~kind:`Info (t "Working …")]]
         else [logs] )) in
  let collapsing_logs () =
    let collapse = Bootstrap.Collapse.make () in
    Bootstrap.Collapse.fixed_width_reactive_button_with_div_below collapse
      ~width:"12em" ~kind:`Secondary
      ~button:(function
        | `Hiding | `Showing -> t "..⻎.."
        | `Hidden -> t "Show Logs"
        | `Shown -> t "Collapse Logs")
      (fun () -> show_logs ~wip:false ()) in
  Reactive.bind_var work_status.status ~f:(function
    | Empty -> empty ()
    | Work_in_progress ->
        Bootstrap.alert ~kind:`Secondary (show_logs ~wip:true ())
    | Done (Ok x) -> div (f x) % collapsing_logs ()
    | Done (Error e) ->
        Bootstrap.bordered ~kind:`Danger (div e %% collapsing_logs ()))
