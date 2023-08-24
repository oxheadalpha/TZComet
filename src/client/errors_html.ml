open Import
open Meta_html

type handler = exn -> (Html_types.li_content Meta_html.t * exn list) option

let exception_html ?(handlers : handler list = []) ctxt exn =
  let rec construct = function
    | Decorate_error.E { message; trace } ->
        let trace_part =
          match trace with
          | [] -> empty ()
          | more ->
              let collapse = Bootstrap.Collapse.make () in
              Bootstrap.Collapse.fixed_width_reactive_button_with_div_below
                collapse ~width:"12em" ~kind:`Secondary
                ~button:(function
                  | true -> t "Show Error Trace" | false -> t "Hide Error Trace")
                (fun () -> itemize (List.map more ~f:construct))
        in
        Message_html.render ctxt message % trace_part
    | Failure s -> t "Failure:" %% t s
    | e -> (
        match List.find_map handlers ~f:(fun f -> f e) with
        | Some (m, []) -> m
        | Some (m, more) -> m % itemize (List.map more ~f:construct)
        | None -> t "Exception:" % pre (Fmt.kstr ct "%a" Exn.pp e))
  in
  bt "Error:" %% construct exn
