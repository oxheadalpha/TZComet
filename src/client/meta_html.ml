open Import
module H5 = Tyxml_lwd.Html

type 'a t = 'a H5.elt (* list Lwd.t *)

let t (s : string) : _ t = H5.(txt (Lwd.pure s))
let ( % ) a b : _ t = Lwd.map2 Lwd_seq.concat a b
let ( %% ) a b : _ t = a % t " " % b
let singletize f ?a x = f ?a [x]
let p ?a l = singletize H5.p ?a l
let i ?a l = singletize H5.i ?a l
let b ?a l = singletize H5.b ?a l
let code ?a l = singletize H5.code ?a l
let it s = i (t s)
let bt s = b (t s)
let ct s = code (t s)

module H = struct
  let button ?a l = singletize H5.button ?a l
  let span ?a l = singletize H5.span ?a l
  let p ?a l = singletize H5.p ?a l
  let div ?a l = singletize H5.div ?a l
end

let button ?(a = []) ~action k =
  H.button
    ~a:(H5.a_onclick (Tyxml_lwd.Lwdom.attr (fun _ -> action () ; false)) :: a)
    k

let classes l = H5.a_class (Lwd.pure l)

let onclick_action action =
  H5.a_onclick (Tyxml_lwd.Lwdom.attr (fun _ -> action () ; true))

let bind_var : 'a Lwd.var -> f:('a -> 'b t) -> 'b t =
 fun v ~f -> Lwd.bind (Lwd.get v) f

module Bootstrap = struct
  module Label_kind = struct
    type t =
      [ `Primary
      | `Secondary
      | `Success
      | `Danger
      | `Warning
      | `Info
      | `Light
      | `Dark ]

    let to_string : t -> string = function
      | `Primary -> "primary"
      | `Secondary -> "secondary"
      | `Success -> "success"
      | `Danger -> "danger"
      | `Warning -> "warning"
      | `Info -> "info"
      | `Light -> "light"
      | `Dark -> "dark"
  end

  let label kind content =
    H5.span
      ~a:[classes ["alert"; Fmt.str "alert-%s" (Label_kind.to_string kind)]]
      [content]

  let button ?(kind = `Light) content ~action =
    button ~action
      ~a:[classes ["btn"; Fmt.str "btn-%s" (Label_kind.to_string kind)]]
      content

  let container ?(suffix = "-md") c =
    H.div ~a:[classes [Fmt.str "container%s" suffix]] c

  module Fresh_id = struct
    let _ids = ref 0

    let make prefix =
      Caml.incr _ids ;
      Fmt.str "%s-%06d" prefix !_ids

    let of_option prefix = function Some s -> s | None -> make prefix
  end

  module Dropdown_menu = struct
    let item ~action content = `Menu_item (content, action)
    let header content = `Menu_header content

    let button ?(kind = `Light) ?id content items =
      let id = Fresh_id.of_option "dropdown" id in
      let div_items =
        let open H5 in
        List.map items ~f:(function
          | `Menu_item (content, action) ->
              button
                ~a:
                  [ a_class (Lwd.pure ["dropdown-item"])
                  ; a_onclick (Tyxml_lwd.Lwdom.attr (fun _ -> action () ; true))
                  ]
                [content]
          | `Menu_header content ->
              h6 ~a:[a_class (Lwd.pure ["dropdown-header"])] [content]) in
      H5.(
        let open Tyxml_lwd.Lwdom in
        div
          ~a:[a_class (Lwd.pure ["dropdown"])]
          [ button
              ~a:
                [ a_class
                    (Lwd.pure
                       [ "btn"; Fmt.str "btn-%s" (Label_kind.to_string kind)
                       ; "dropdown-toggle" ])
                  (* ; a_type (Lwd.pure `Button) *); a_id (Lwd.pure id)
                ; a_user_data "toggle" (Lwd.pure "dropdown")
                ; a_aria "haspopup" (Lwd.pure ["true"])
                ; a_aria "expanded" (Lwd.pure ["false"]) ]
              [content]
          ; div
              ~a:
                [ a_class (Lwd.pure ["dropdown-menu"])
                ; a_aria "labelledby" (Lwd.pure [id]) ]
              div_items ])
  end

  module Navigation_bar = struct
    (* https://getbootstrap.com/docs/4.5/components/navbar/#toggler *)
    let item ?(active = Lwd.pure true) ?fragment c ~action =
      `Item (c, action, active, fragment)

    let make ?(aria_label = "Show/Hide Navigation") ?id ~brand items =
      let open H5 in
      let toggler_id = Fresh_id.of_option "dropdown" id in
      let content =
        [ button
            ~a:
              [ classes ["navbar-toggler"]
              ; a_user_data "toggle" (Lwd.pure "collapse")
              ; a_user_data "target" (Fmt.kstr Lwd.pure "#%s" toggler_id)
              ; a_aria "controls" (Lwd.pure [toggler_id])
              ; a_aria "expanded" (Lwd.pure ["false"])
              ; a_aria "label" (Lwd.pure [aria_label]) ]
            [span ~a:[classes ["navbar-toggler-icon"]] []]
        ; a ~a:[classes ["navbar-brand"]] [brand]
        ; div
            ~a:
              [ classes ["collapse"; "navbar-collapse"]
              ; a_id (Lwd.pure toggler_id) ]
            [ ul
                ~a:[classes ["navbar-nav"; "mr-auto"; "mt-2"; "mt-lg-0"]]
                (List.map items ~f:(function
                     | `Item (content, action, active, fragment) ->
                     (* a_class
                           (Lwd.map
                              (function
                                | true -> ["nav-item"; "active"]
                                | false -> ["nav-item"])
                              active) *)
                     Lwd.bind active (function
                       | true ->
                           li
                             ~a:[classes ["nav-item"; "active"]]
                             [ a
                                 ~a:
                                   [ classes ["nav-link"]
                                   ; a_href
                                       (Fmt.kstr Lwd.pure "#%a"
                                          Fmt.(option string)
                                          fragment); onclick_action action ]
                                 [content] ]
                       | false ->
                           li
                             ~a:[classes ["nav-item"]]
                             [a ~a:[classes ["nav-link"]] [content]]))) ] ]
      in
      nav content
        ~a:[classes ["navbar"; "navbar-expand-lg"; "navbar-light"; "bg-light"]]
  end
end

module Example = struct
  let e0 () = t "Hello" %% it "World"

  let e1 () =
    let button_calls = Lwd.var 0 in
    p (e0 ())
    % Bootstrap.container
        ( p (t "This is in a bootstrap container.")
        % p
            (Bootstrap.button ~kind:`Primary
               ~action:(fun () ->
                 Lwd.set button_calls (Lwd.peek button_calls + 1))
               (bind_var button_calls ~f:(fun count ->
                    H5.span
                      [ Fmt.kstr
                          (if Stdlib.( mod ) count 2 = 0 then it else bt)
                          "Click %d" count ])))
        % p
            (Bootstrap.label `Danger
               (bind_var button_calls ~f:(fun count ->
                    Fmt.kstr t "Button above clicked %d time%s." count
                      (if count = 1 then "" else "s"))))
        % p (t "A dropdown menu:")
        % Bootstrap.Dropdown_menu.(
            button
              (t "This is a" %% ct "Dropdown" %% t "menu")
              [ item (t "The First") ~action:(fun () ->
                    dbgf "Hello from the first")
              ; header (t "This is a dropdown" %% it "header")
              ; item (t "The Second") ~action:(fun () ->
                    dbgf "Hellow from the second") ])
        % p (t "A Nav-bar …")
        % Bootstrap.Navigation_bar.(
            make
              ~brand:(it "Examples of Meta_html")
              [ item (t "One")
                  ~action:(fun () -> dbgf "one from nav bar")
                  ~fragment:"page-one"
              ; item ~active:(Lwd.pure false) (t "One-inactive")
                  ~action:(fun () -> assert false) ]) )
end
