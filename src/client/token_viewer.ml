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

let show_token ctxt Contract_metadata.Token.{address; id; warnings; network} =
  let open Meta_html in
  h3
    ( t "Token"
    %% Fmt.kstr ct "%s/%d" address id
    %% parens
         ( t "on"
         %% t
              (Option.value_map ~f:Network.to_string ~default:"unknown network"
                 network) ) )

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
