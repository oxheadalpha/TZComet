open! Import

let go_action ctxt ~wip =
  let token_id = State.token_id ctxt |> Reactive.peek in
  let address = State.token_address ctxt |> Reactive.peek in
  let _logh msg = Async_work.log wip msg in
  let logm msg = Async_work.log wip (Message_html.render ctxt msg) in
  Async_work.wip wip ;
  Async_work.async_catch wip
    ~exn_to_html:(Errors_html.exception_html ctxt)
    Lwt.Infix.(
      fun ~mkexn () ->
        let id =
          try Int.of_string token_id
          with _ ->
            raise
              (mkexn
                 Meta_html.(
                   t "Hmmm, the token-id is not a nat anymore:" %% ct token_id))
        in
        logm
          Message.(
            t "Fetching metadata for" %% Fmt.kstr ct "%s/%s" address token_id) ;
        Contract_metadata.Token.fetch ctxt ~address ~id ~log:logm
        >>= fun token -> Async_work.ok wip token ; Lwt.return_unit) ;
  ()

let show_token ctxt
    Contract_metadata.Token.
      { address
      ; id
      ; warnings
      ; network
      ; symbol
      ; name
      ; decimals
      ; main_multimedia
      ; tzip21 } =
  let open Meta_html in
  let open Contract_metadata.Content.Tzip_021 in
  let warning = function
    | `Fetching_uri (uri, e) ->
        t "Fetching URI" %% ct uri %% Errors_html.exception_html ctxt e
    | `Parsing_uri (uri, err) ->
        t "Parsing URI" %% ct uri %% Tezos_html.error_trace ctxt err
    | `Getting_metadata_field m ->
        t "Parsing metadata" %% Message_html.render ctxt m in
  let warnings = List.map warnings ~f:(fun (k, v) -> (k, warning v)) in
  let metaname =
    match (name, symbol, tzip21.prefers_symbol) with
    | _, Some s, Some true -> bt s
    | Some n, _, _ -> bt n
    | None, _, _ -> Fmt.kstr ct "%s/%d" address id in
  let or_empty o f = match o with None -> empty () | Some o -> f o in
  let metadescription = or_empty tzip21.description it in
  let multimedia =
    match main_multimedia with
    | None -> Bootstrap.alert ~kind:`Warning (t "There is no multi-media.")
    | Some (Error exn) ->
        Bootstrap.alert ~kind:`Danger
          ( t "Error while getting multimedia content:"
          %% Errors_html.exception_html ctxt exn )
    | Some (Ok (title, mm)) ->
        let open Contract_metadata.Multimedia in
        let maybe_censor f =
          if mm.sfw then f ()
          else
            Bootstrap.Collapse.(
              fixed_width_reactive_button_with_div_below (make ())
                ~kind:`Secondary ~width:"100%")
              ~button:(function
                | true ->
                    Fmt.kstr t "Show %s (potentially NSFW)"
                      ( match mm.format with
                      | `Image, _ -> "Image"
                      | `Video, _ -> "Video" )
                | false -> t "Hide Multimedia")
              f in
        maybe_censor (fun () ->
            match mm.format with
            | `Image, "svg+xml" ->
                link ~target:mm.converted_uri
                  (H5.object_
                     ~a:
                       [ H5.a_mime_type (Lwd.pure "image/svg+xml")
                       ; H5.a_data (Lwd.pure mm.converted_uri) ]
                     [ H5.img ~a:[style "max-width: 100%"]
                         ~alt:
                           (Fmt.kstr Lwd.pure "%s at %s" title mm.converted_uri)
                         ~src:(Lwd.pure mm.converted_uri)
                         () ])
            | `Image, _ ->
                link ~target:mm.converted_uri
                  (H5.img ~a:[style "max-width: 100%"]
                     ~alt:(Fmt.kstr Lwd.pure "%s at %s" title mm.converted_uri)
                     ~src:(Lwd.pure mm.converted_uri)
                     ())
            | `Video, _ ->
                H5.video
                  ~a:[H5.a_controls (); style "max-width: 100%"]
                  ~src:(Lwd.pure mm.converted_uri)
                  [])
    (* Tezos_html.multimedia_from_tzip16_uri ctxt ~title
       ~mime_types:(uri_mime_types tzip21) ~uri) *) in
  let creators =
    or_empty tzip21.creators (function
      | [] -> bt "Creators list is explicitly empty."
      | [one] -> bt "Creator:" %% it one
      | sl -> bt "Creators:" %% itemize (List.map sl ~f:it)) in
  let tags =
    or_empty tzip21.tags (fun sl ->
        bt "Tags:"
        %% list
             (oxfordize_list sl
                ~map:(fun t -> ct t)
                ~sep:(fun () -> t ", ")
                ~last_sep:(fun () -> t ", and "))) in
  let main_content =
    h3
      ( metaname
      %% i
           (parens
              ( t "on"
              %% t
                   (Option.value_map ~f:Network.to_string
                      ~default:"unknown network" network) )) )
    % multimedia
    (* % p
        (Fmt.kstr t "mm: %a"
           Fmt.(
             option
               (result
                  ~ok:(pair string Contract_metadata.Multimedia.pp)
                  ~error:Exn.pp))
           main_multimedia) *)
    % Bootstrap.p_lead metadescription
    % div creators % div tags in
  Bootstrap.bordered
    ~a:[style "padding: 3em; border: solid 3px #aaa"; classes ["col-8"]]
    ~kind:`Primary main_content
  % Bootstrap.Collapse.(
      fixed_width_reactive_button_with_div_below (make ()) ~kind:`Secondary
        ~width:(* Same as async_work *) "12em")
      ~button:(function
        | true -> t "Show token details" | false -> t "Hide details")
      (fun () ->
        Tezos_html.show_one_token ctxt ?symbol ?name ?decimals ~tzip_021:tzip21
          ~id ~warnings)

let render ctxt =
  let open Meta_html in
  let result = Async_work.empty () in
  let token_id = State.token_id ctxt in
  let token_id_bidi = Reactive.Bidirectional.of_var token_id in
  let token_address = State.token_address ctxt in
  let token_address_bidi = Reactive.Bidirectional.of_var token_address in
  let is_address_valid k =
    match B58_hashes.check_b58_kt1_hash k with
    | _ -> true
    | exception _ -> false in
  let address_valid ctxt token_address =
    Reactive.(map (get token_address) ~f:is_address_valid) in
  let is_token_id_valid i =
    match Int.of_string i with _ -> true | exception _ -> false in
  let token_id_valid ctxt token_id =
    Reactive.(map (get token_id) ~f:is_token_id_valid) in
  let input_valid ctxt =
    Reactive.(
      map
        (address_valid ctxt token_address ** token_id_valid ctxt token_id)
        ~f:(fun (add, tid) -> add && tid)) in
  let make_help ~validity ~input content =
    Reactive.(
      bind
        (validity ctxt input ** get input)
        ~f:(function
          | true, more -> content %% t "👍"
          | false, "" -> content
          | false, more ->
              content %% Bootstrap.color `Danger (ct more %% t "is wrong.")))
  in
  let enter_action () = go_action ctxt ~wip:result in
  let _once_in_tab =
    if
      is_token_id_valid (Reactive.peek token_id)
      && is_address_valid (Reactive.peek token_address)
    then enter_action () in
  h2 (t "Token Viewer")
  % Bootstrap.Form.(
      State.if_explorer_should_go ctxt enter_action ;
      make ~enter_action
        [ row
            [ cell 2
                (submit_button (t "Pick A Random Token") (fun () ->
                     Reactive.set token_address "KT1todotododod" ;
                     Reactive.set token_id "1.3"))
            ; cell 4
                (input
                   ~placeholder:(Reactive.pure "Contract address")
                   token_address_bidi
                   ~help:
                     (make_help ~validity:address_valid ~input:token_address
                        (t "A valid KT1 address on any known network.")))
            ; cell 2
                (input ~placeholder:(Reactive.pure "Token ID") token_id_bidi
                   ~help:
                     (make_help ~validity:token_id_valid ~input:token_id
                        (t "A natural number.")))
            ; cell 3
                (submit_button (t "Go!")
                   ~active:
                     Reactive.(
                       map
                         (input_valid ctxt ** Async_work.busy result)
                         ~f:(function
                           | false, _ -> false | _, true -> false | _ -> true))
                   enter_action)
              (* ; cell 1
                  (magic (t " – or – ")) *) ] ])
  % Async_work.render result ~f:(show_token ctxt)
