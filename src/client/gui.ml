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
    {page: Page.t Lwd.var; version_string: string option; dev_mode: bool Lwd.var}

  let create () =
    let start_page =
      let fragment = Js_of_ocaml.Url.Current.get_fragment () in
      List.find all_in_order ~f:(fun page ->
          String.equal
            (String.lowercase (Page.to_string page))
            ( String.chop_prefix_if_exists fragment ~prefix:"#"
            |> String.lowercase ))
      |> Option.value ~default:Explorer in
    let dev_mode =
      match
        List.Assoc.find ~equal:String.equal Js_of_ocaml.Url.Current.arguments
          "dev"
      with
      | Some "true" -> true
      | _ -> false in
    {page= Lwd.var start_page; version_string= None; dev_mode= Lwd.var dev_mode}

end

let tzcomet_link () =
  let open Meta_html in
  link ~target:"https://github.com/tqtezos/TZComet" (t "TZComet")

let navigation_menu state =
  let open State in
  let open Page in
  let open Meta_html in
  let current_is_not p = Lwd.get state.page |> Lwd.map Poly.(( <> ) p) in
  let act p () = Lwd.set state.page p in
  Bootstrap.Navigation_bar.(
    make
      ~brand:
        (Bootstrap.label `Dark
           ( tzcomet_link ()
           %% bind_var state.dev_mode ~f:(function
                | true -> it "(dev)"
                | false -> empty ()) ))
      (let of_page p =
         let fragment = Page.to_string p in
         item (bt fragment) ~active:(current_is_not p) ~action:(act p) ~fragment
       in
       List.map ~f:of_page all_in_order))

let about_page state =
  let open Meta_html in
  let open State in
  let p = p_lead in
  h2 (t "TZComet")
  % p
      ( t "This is" %% tzcomet_link ()
      %% ( match state.version_string with
         | None -> it "unknown version"
         | Some vs ->
             t "version "
             %% link
                  ~target:
                    (Fmt.str "https://github.com/tqtezos/TZComet/commit/%s" vs)
                  (it vs) )
      % bind_var state.State.dev_mode ~f:(function
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
  % bind_var state.dev_mode ~f:(function
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
        [ check_box state.dev_mode (t "Dev-mode enabled")
            ~help:
              (t
                 "Shows things that regular users should not see and \
                  artificially slows down the application.") ])

let root_document state =
  let open Meta_html in
  Bootstrap.container ~suffix:"-fluid"
    ( navigation_menu state
    % bind_var state.page
        ~f:
          State.Page.(
            function
            | Explorer -> t "Welcome/explorer page TODO"
            | Settings -> settings_page state
            | About -> about_page state) )
