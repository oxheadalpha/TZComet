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
    ; dev_mode: bool Reactive.var }

  (* type 'a context = 'a Context.t constraint 'a = < gui: t ; .. > *)

  let get (state : < gui: t ; .. > Context.t) = state#gui

  module Fragment = struct
    type parsed = {page: Page.t; dev_mode: bool}

    let make {page; dev_mode} =
      let query = if not dev_mode then [] else [("dev", ["true"])] in
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
      let dev_mode =
        match List.Assoc.find ~equal:String.equal query "dev" with
        | Some ["true"] -> true
        | _ -> false in
      {page; dev_mode}
  end

  let create () =
    let {Fragment.page; dev_mode} =
      let fragment = Js_of_ocaml.Url.Current.get_fragment () in
      Fragment.parse fragment in
    { page= Reactive.var page
    ; version_string= None
    ; dev_mode= Reactive.var dev_mode }

  let version_string state = (get state).version_string
  let set_page state p () = Reactive.set (get state).page p
  let page state = (get state).page |> Reactive.get

  let current_page_is_not state p =
    Reactive.get (get state).page |> Reactive.map ~f:Poly.(( <> ) p)

  let dev_mode state = (get state).dev_mode |> Reactive.get

  let dev_mode_bidirectional state =
    (get state).dev_mode |> Reactive.Bidirectrional.of_var

  let make_fragment state =
    (* WARNING: for now it is important for this to be attached "somewhere"
       in the DOM. *)
    let open Js_of_ocaml.Url in
    let state = get state in
    let dev = Reactive.get state.dev_mode in
    let page = Reactive.get state.page in
    Reactive.map2' dev page (fun dev_mode page ->
        let current = Js_of_ocaml.Url.Current.get_fragment () in
        let now = Fragment.(make {page; dev_mode}) in
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
            ~checked:(State.dev_mode_bidirectional state)
            (t "Dev-mode enabled")
            ~help:
              (t
                 "Shows things that regular users should not see and \
                  artificially slows down the application.") ])

let root_document state =
  let open Meta_html in
  Bootstrap.container ~suffix:"-fluid"
    ( navigation_menu state
    % Reactive.bind (State.page state)
        State.Page.(
          function
          | Explorer -> t "Welcome/explorer page TODO"
          | Settings -> settings_page state
          | About -> about_page state) )
