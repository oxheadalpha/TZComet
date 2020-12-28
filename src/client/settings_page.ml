open! Import

let nodes_form ctxt =
  let open Meta_html in
  Bootstrap.Table.simple
    ~header_row:
      [ t "Name"; t "URI-Prefix"; t "Status"
      ; t "Latest Ping"
        %% Reactive.bind
             (Query_nodes.loop_status ctxt)
             ~f:
               (let m s = i (parens (t s)) in
                function
                | `Not_started -> m "ping-loop not started"
                | `In_progress -> m "ping-loop in progress"
                | `Sleeping -> m "ping-loop sleeping") ]
    (let row l = H5.tr (List.map ~f:td l) in
     let node_status =
       let m kind s = Bootstrap.color kind (Bootstrap.monospace (t s)) in
       Query_nodes.Node_status.(
         function
         | Uninitialized -> m `Warning "Uninitialized"
         | Non_responsive e ->
             let collapse = Bootstrap.Collapse.make () in
             m `Danger "Non-responsive"
             % Bootstrap.Collapse.fixed_width_reactive_button_with_div_below
                 collapse ~width:"12em" ~kind:`Secondary
                 ~button:(function
                   | true -> t "Show Error" | false -> t "Hide Error")
                 (fun () -> Errors_html.exception_html ctxt e)
         | Ready _ -> m `Success "Ready") in
     let ping_date date =
       if Float.(date < 10.) then (* Construction sign: *) t "🚧"
       else
         let date_string =
           (new%js Js_of_ocaml.Js.date_fromTimeValue date)##toISOString
           |> Js_of_ocaml__Js.to_string in
         Bootstrap.monospace (t date_string) in
     let row_of_node n =
       row
         Query_nodes.Node.
           [ it n.name; ct n.prefix
           ; Reactive.bind (status n) ~f:(fun (_, s) -> node_status s)
           ; Reactive.bind (status n) ~f:(fun (f, _) -> ping_date f) ] in
     let last_row =
       let name = Reactive.var "" in
       let nameb = Reactive.Bidirectional.of_var name in
       let prefix = Reactive.var "" in
       let prefixb = Reactive.Bidirectional.of_var prefix in
       row
         [ input_bidirectional nameb
             ~a:
               [ H5.a_placeholder (Reactive.pure "Name")
               ; classes ["form-control"] ]
         ; input_bidirectional prefixb
             ~a:
               [ H5.a_placeholder (Reactive.pure "URL-Prefix")
               ; classes ["form-control"] ]
         ; Bootstrap.button (t "⇐ Add/replace node (by name)")
             ~kind:`Secondary ~action:(fun () ->
               Query_nodes.add_node ctxt
                 (Query_nodes.Node.create (Reactive.peek name)
                    (Reactive.peek prefix)) ;
               Reactive.Bidirectional.set nameb "" ;
               Reactive.Bidirectional.set prefixb "" ;
               ())
         ; Bootstrap.button (t "⇑ Ping'em'all") ~kind:`Secondary
             ~action:(fun () ->
               Query_nodes.Update_status_loop.ensure ctxt ;
               Query_nodes.Update_status_loop.wake_up ctxt) ] in
     Reactive.bind (Query_nodes.get_nodes ctxt ~map:row_of_node)
       ~f:(fun nodes -> list nodes)
     % last_row)

let render ctxt =
  let open Meta_html in
  let open State in
  let timeout_valid_and_changed = Reactive.var None in
  let timeout =
    Reactive.Bidirectional.make
      (System.http_timeout_peek ctxt |> Fmt.str "%f" |> Reactive.pure)
      (fun x ->
        match Float.of_string x with
        | f ->
            System.set_http_timeout ctxt f ;
            Reactive.set timeout_valid_and_changed
              (Some (t "Timeout set to " % Fmt.kstr ct "%f" f))
        | exception _ ->
            Reactive.set timeout_valid_and_changed
              (Some
                 (Bootstrap.color `Danger
                    ( t "Timeout cannot be set to"
                    %% ct x
                    % t ", it should a valid floating-point number." )))) in
  h2 (t "Settings")
  % Bootstrap.Form.(
      make
        [ check_box
            (State.dev_mode_bidirectional ctxt)
            ~label:(t "Dev-mode enabled")
            ~help:
              (t
                 "Shows things that regular users should not see and \
                  artificially slows down the application.")
        ; check_box
            (State.check_micheline_indentation_bidirectional ctxt)
            ~label:(t "Check Micheline Indentation")
            ~help:
              ( t
                  "Make the Micheline parser (in the Editor) also check for \
                   proper indentation like"
              %% ct "tezos-client" %% t "does." )
        ; input
            ~placeholder:(Reactive.pure "Number of seconds (with decimals).")
            ~help:
              (Reactive.bind_var timeout_valid_and_changed ~f:(function
                | None ->
                    t
                      "How long to wait for nodes and gateways to give/accept \
                       data."
                | Some msg -> msg))
            ~label:(t "HTTP-Call Timeout") timeout ])
  % h3 (t "Tezos Nodes")
  % nodes_form ctxt
