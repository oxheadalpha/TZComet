open Import

module State = struct
  module Page = struct
    type t = Explorer | Settings | About

    let to_string = function
      | Explorer -> "Explorer"
      | Settings -> "Settings"
      | About -> "About"

    let all_in_order = [Explorer; Settings; About]
  end

  open Page

  type t =
    { page: Page.t Reactive.var
    ; version_string: string option
    ; dev_mode: bool Reactive.var
    ; explorer_input: string Reactive.var }

  (* type 'a context = 'a Context.t constraint 'a = < gui: t ; .. > *)

  let get (state : < gui: t ; .. > Context.t) = state#gui

  module Fragment = struct
    type parsed = {page: Page.t; dev_mode: bool; explorer_input: string}

    let make {page; dev_mode; explorer_input} =
      let query =
        match explorer_input with
        | "" -> []
        | more -> [("explorer-input", [more])] in
      let query = if not dev_mode then query else ("dev", ["true"]) :: query in
      Uri.make ()
        ~path:(Fmt.str "/%s" (Page.to_string page |> String.lowercase))
        ~query
      |> Uri.to_string

    let parse fragment =
      let uri = Uri.of_string (Uri.pct_decode fragment) in
      let pagename = Uri.path uri |> String.chop_prefix_if_exists ~prefix:"/" in
      let page =
        List.find all_in_order ~f:(fun page ->
            String.equal
              (String.lowercase (Page.to_string page))
              (pagename |> String.lowercase))
        |> Option.value ~default:Explorer in
      let query = Uri.query uri in
      let in_query = List.Assoc.find ~equal:String.equal query in
      let dev_mode =
        match in_query "dev" with Some ["true"] -> true | _ -> false in
      let explorer_input =
        match in_query "explorer-input" with Some [one] -> one | _ -> "" in
      {page; dev_mode; explorer_input}
  end

  let create () =
    let {Fragment.page; dev_mode; explorer_input} =
      let fragment = Js_of_ocaml.Url.Current.get_fragment () in
      Fragment.parse fragment in
    { page= Reactive.var page
    ; version_string= None
    ; dev_mode= Reactive.var dev_mode
    ; explorer_input= Reactive.var explorer_input }

  let version_string state = (get state).version_string
  let set_page state p () = Reactive.set (get state).page p
  let page state = (get state).page |> Reactive.get

  let current_page_is_not state p =
    Reactive.get (get state).page |> Reactive.map ~f:Poly.(( <> ) p)

  let dev_mode state = (get state).dev_mode |> Reactive.get

  let dev_mode_bidirectional state =
    (get state).dev_mode |> Reactive.Bidirectrional.of_var

  let explorer_input state = (get state).explorer_input |> Reactive.get
  let explorer_input_value state = (get state).explorer_input |> Reactive.peek
  let set_explorer_input state = (get state).explorer_input |> Reactive.set

  let explorer_input_bidirectional state =
    (get state).explorer_input |> Reactive.Bidirectrional.of_var

  let make_fragment state =
    (* WARNING: for now it is important for this to be attached "somewhere"
       in the DOM. *)
    let open Js_of_ocaml.Url in
    let state = get state in
    let dev = Reactive.get state.dev_mode in
    let page = Reactive.get state.page in
    let explorer_input = Reactive.get state.explorer_input in
    Reactive.(dev ** page ** explorer_input)
    |> Reactive.map ~f:(fun (dev_mode, (page, explorer_input)) ->
           let current = Js_of_ocaml.Url.Current.get_fragment () in
           let now = Fragment.(make {page; dev_mode; explorer_input}) in
           dbgf "Updating %S → %S" current now ;
           Current.set_fragment now ;
           now)
end

let tzcomet_link () =
  let open Meta_html in
  link ~target:"https://github.com/tqtezos/TZComet" (t "TZComet")

let navigation_menu state =
  let open State in
  let open Page in
  let open Meta_html in
  Bootstrap.Navigation_bar.(
    make
      ~brand:
        (Bootstrap.label `Dark
           ( tzcomet_link ()
           %% small
                (Reactive.bind (make_fragment state) (fun f ->
                     link (t "ʘ") ~target:("#" ^ f)))
           %% Reactive.bind (State.dev_mode state) (function
                | true -> it "(dev)"
                | false -> empty ()) ))
      (let of_page p =
         let fragment = make_fragment state in
         item
           (bt (Page.to_string p))
           ~active:(State.current_page_is_not state p)
           ~action:(State.set_page state p) ~fragment in
       List.map ~f:of_page all_in_order))

let about_page state =
  let open Meta_html in
  let open State in
  let p = p_lead in
  h2 (t "TZComet")
  % p
      ( t "This is" %% tzcomet_link ()
      %% ( match State.version_string state with
         | None -> it "unknown version"
         | Some vs ->
             t "version "
             %% link
                  ~target:
                    (Fmt.str "https://github.com/tqtezos/TZComet/commit/%s" vs)
                  (it vs) )
      % Reactive.bind (State.dev_mode state) (function
          | true -> t " (in “dev” mode)."
          | false -> t ".") )
  % p (t "An explorer/editor/validator/visualizer for Tezos contract metadata.")
  % p
      ( t "The source for this webpage is available on Github:"
      %% link ~target:"https://github.com/tqtezos/TZComet"
           (ct "tqtezos/TZComet")
      % t "." )
  % p
      ( t "The Contract Metadata standard, a.k.a. TZIP-16, is at: "
      % url ct
          "https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-16/tzip-16.md"
      % t "." )
  % Reactive.bind (State.dev_mode state) (function
      | false -> empty ()
      | true ->
          h2 (t "Dev-mode Junk:")
          % p (t "This is also a test/experiment in UI writing …")
          % Meta_html.Example.e1 ())

let settings_page state =
  let open Meta_html in
  let open State in
  h2 (t "Settings")
  % Bootstrap.Form.(
      make
        [ check_box
            (State.dev_mode_bidirectional state)
            ~label:(t "Dev-mode enabled")
            ~help:
              (t
                 "Shows things that regular users should not see and \
                  artificially slows down the application.") ])

module Explorer = struct
  let input_valid state =
    Reactive.map (State.explorer_input state) ~f:(fun input_value ->
        match B58_hashes.check_b58_kt1_hash input_value with
        | _ -> `KT1 input_value
        | exception _ -> (
          match Contract_metadata.Uri.validate input_value with
          | Ok uri -> `Uri (input_value, uri)
          | Error e -> `Error (input_value, e) ))

  let input_validation_status state =
    let open Meta_html in
    let cct txt = Bootstrap.monospace (Fmt.kstr t "‘%s’" txt) in
    Reactive.bind (input_valid state) ~f:(function
      | `KT1 k ->
          cct k %% t "is a valid KT1 address" |> Bootstrap.color `Success
      | `Uri (txt, _) ->
          cct txt %% t "is a valid TZIP-16 URI" |> Bootstrap.color `Success
      | `Error ("", _) -> t "Can be a metadata URI or a contract address."
      | `Error (txt, _) ->
          cct txt %% t "is a not a valid address nor a TZIP-16 URI"
          |> Bootstrap.color `Danger)

  let page state =
    let open Meta_html in
    let contract_examples =
      [ ( "KT1XRT495WncnqNmqKn4tkuRiDJzEiR4N2C9"
        , "Contract with metadata on Carthagenet." )
      ; ("KT1PcrG22mRhK6A8bTSjRhk2wV1o5Vuum2S2", "Should not exist any where.")
      ] in
    let uri_examples =
      [ ( "tezos-storage://KT1XRT495WncnqNmqKn4tkuRiDJzEiR4N2C9/here"
        , "An on-chain pointer to metadata." )
      ; ( "ipfs://QmWDcp3BpBjvu8uJYxVqb7JLfr1pcyXsL97Cfkt3y1758o"
        , "An IPFS URI to metadata JSON." ) ] in
    h2 (t "Contract Metadata Explorer")
    % Bootstrap.Form.(
        let enter_action () =
          dbgf "Form submitted with %s" (State.explorer_input_value state) in
        make
          [ row
              [ cell 2
                  (magic
                     Bootstrap.Dropdown_menu.(
                       let example (v, msg) =
                         item
                           (ct v %% t "→" %% it msg)
                           ~action:(fun () -> State.set_explorer_input state v)
                       in
                       button (t "Examples 💡 ")
                         ( [header (t "KT1 Contracts")]
                         @ List.map contract_examples ~f:example
                         @ [header (t "TZIP-16 URIs")]
                         @ List.map uri_examples ~f:example )))
              ; cell 8
                  (input
                     ~placeholder:
                       (Reactive.pure
                          "Enter a contract address or a metadata URI")
                     (State.explorer_input_bidirectional state)
                     ~help:(input_validation_status state))
              ; cell 2
                  (submit_button (t "Go!")
                     ~active:
                       ( input_valid state
                       |> Reactive.map ~f:(function
                            | `Error _ -> false
                            | _ -> true) )
                     enter_action) ] ])
end

let root_document state =
  let open Meta_html in
  Bootstrap.container ~suffix:"-fluid"
    ( navigation_menu state
    % Reactive.bind (State.page state)
        State.Page.(
          function
          | Explorer -> Explorer.page state
          | Settings -> settings_page state
          | About -> about_page state) )
