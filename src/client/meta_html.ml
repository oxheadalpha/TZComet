open Import
module H5 = Tyxml_lwd.Html

type 'a t = 'a H5.elt (* list Lwd.t *)

let t (s : string) : _ t = H5.(txt (Lwd.pure s))
let empty () = t ""
let ( % ) a b : _ t = Lwd.map2 Lwd_seq.concat a b
let ( %% ) a b : _ t = a % t " " % b
let singletize f ?a x = f ?a [x]

module H = struct
  let p ?a l = singletize H5.p ?a l
  let i ?a l = singletize H5.i ?a l
  let b ?a l = singletize H5.b ?a l
  let code ?a l = singletize H5.code ?a l
  let button ?a l = singletize H5.button ?a l
  let span ?a l = singletize H5.span ?a l
  let small ?a l = singletize H5.span ?a l
  let a ?a l = singletize H5.a ?a l
  let div ?a l = singletize H5.div ?a l
  let h1 ?a l = singletize H5.h1 ?a l
  let h2 ?a l = singletize H5.h2 ?a l
  let h3 ?a l = singletize H5.h3 ?a l
  let h4 ?a l = singletize H5.h4 ?a l
  let h5 ?a l = singletize H5.h5 ?a l
  let h6 ?a l = singletize H5.h6 ?a l
end

include H

let classes l = H5.a_class (Lwd.pure l)
let it s = i (t s)
let bt s = b (t s)
let ct s = code (t s)
let p_lead ?(a = []) c = p ~a:(classes ["lead"] :: a) c

let link ~target ?(a = []) content =
  H.a ~a:(H5.a_href (Lwd.pure target) :: a) content

let url ?a t u = link ?a ~target:u (t u)

let button ?(a = []) ~action k =
  H.button
    ~a:(H5.a_onclick (Tyxml_lwd.Lwdom.attr (fun _ -> action () ; false)) :: a)
    k

let onclick_action action =
  H5.a_onclick (Tyxml_lwd.Lwdom.attr (fun _ -> action () ; true))

let bind_var : 'a Reactive.var -> f:('a -> 'b t) -> 'b t =
 fun v ~f -> Reactive.bind (Reactive.get v) f

let itemize ?(numbered = false) ?a_ul ?a_li l =
  (if numbered then H5.ul else H5.ol)
    ?a:a_ul
    (List.map l ~f:(fun item -> H5.li ?a:a_li [item]))

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

  let container_fluid c = container ~suffix:"-fluid" c

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
                  [ a_class (Reactive.pure ["dropdown-item"])
                  ; a_onclick (Tyxml_lwd.Lwdom.attr (fun _ -> action () ; true))
                  ]
                [content]
          | `Menu_header content ->
              h6 ~a:[a_class (Reactive.pure ["dropdown-header"])] [content])
      in
      H5.(
        let open Tyxml_lwd.Lwdom in
        div
          ~a:[a_class (Reactive.pure ["dropdown"])]
          [ button
              ~a:
                [ a_class
                    (Reactive.pure
                       [ "btn"; Fmt.str "btn-%s" (Label_kind.to_string kind)
                       ; "dropdown-toggle" ])
                  (* ; a_type (Reactive.pure `Button) *)
                ; a_id (Reactive.pure id)
                ; a_user_data "toggle" (Reactive.pure "dropdown")
                ; a_aria "haspopup" (Reactive.pure ["true"])
                ; a_aria "expanded" (Reactive.pure ["false"]) ]
              [content]
          ; div
              ~a:
                [ a_class (Reactive.pure ["dropdown-menu"])
                ; a_aria "labelledby" (Reactive.pure [id]) ]
              div_items ])
  end

  module Navigation_bar = struct
    (* https://getbootstrap.com/docs/4.5/components/navbar/#toggler *)
    let item ?(active = Reactive.pure true) ?fragment c ~action =
      `Item
        ( c
        , (action : unit -> unit)
        , active
        , (fragment : string Reactive.t option) )

    let make ?(aria_label = "Show/Hide Navigation") ?id ~brand items =
      let open H5 in
      let toggler_id = Fresh_id.of_option "dropdown" id in
      let content =
        [ button
            ~a:
              [ classes ["navbar-toggler"]
              ; a_user_data "toggle" (Reactive.pure "collapse")
              ; a_user_data "target" (Fmt.kstr Reactive.pure "#%s" toggler_id)
              ; a_aria "controls" (Reactive.pure [toggler_id])
              ; a_aria "expanded" (Reactive.pure ["false"])
              ; a_aria "label" (Reactive.pure [aria_label]) ]
            [span ~a:[classes ["navbar-toggler-icon"]] []]
        ; a ~a:[classes ["navbar-brand"]] [brand]
        ; div
            ~a:
              [ classes ["collapse"; "navbar-collapse"]
              ; a_id (Reactive.pure toggler_id) ]
            [ ul
                ~a:[classes ["navbar-nav"; "mr-auto"; "mt-2"; "mt-lg-0"]]
                (List.map items ~f:(function
                     | `Item (content, action, active, fragment) ->
                     (* a_class
                           (Reactive.map
                              (function
                                | true -> ["nav-item"; "active"]
                                | false -> ["nav-item"])
                              active) *)
                     Reactive.bind active (function
                       | true ->
                           li
                             ~a:[classes ["nav-item"; "active"]]
                             [ a
                                 ~a:
                                   ( [ classes ["nav-link"]
                                     ; onclick_action action ]
                                   @
                                   match fragment with
                                   | None -> []
                                   | Some frg ->
                                       [ a_href
                                           (Reactive.map ~f:(Fmt.str "#%s") frg)
                                       ] )
                                 [content] ]
                       | false ->
                           li
                             ~a:[classes ["nav-item"]]
                             [a ~a:[classes ["nav-link"]] [content]]))) ] ]
      in
      nav content
        ~a:[classes ["navbar"; "navbar-expand-lg"; "navbar-light"; "bg-light"]]
  end

  module Form = struct
    module Item = struct
      type input =
        { label: Html_types.label_content_fun H5.elt
        ; id: string option
        ; help: Html_types.small_content_fun H5.elt option }

      type t =
        | Input of {input: input; content: string Reactive.Bidirectrional.t}
        | Check_box of {input: input; checked: bool Reactive.Bidirectrional.t}
        | Button of
            {label: Html_types.button_content_fun H5.elt; action: unit -> unit}

      let to_div =
        let open H5 in
        let generic_input ?id ?help ~kind lbl more_a =
          let the_id = Fresh_id.of_option "input-item" id in
          let help_id = the_id ^ "Help" in
          let full_label =
            label
              ~a:
                [ (* shoud be for="<id>" *)
                  classes
                    ( match kind with
                    | `Text -> []
                    | `Checkbox -> ["form-check-label"] ) ]
              [lbl] in
          let full_input =
            input
              ~a:
                ( [ classes
                      [ ( match kind with
                        | `Text -> "form-control"
                        | `Checkbox -> "form-check-input" ) ]
                  ; a_id (Reactive.pure the_id)
                  ; a_aria "describedBy" (Reactive.pure [help_id])
                  ; a_input_type (Reactive.pure kind) ]
                @ more_a )
              () in
          let full_help =
            small
              ~a:
                [ a_id (Reactive.pure help_id)
                ; classes ["form-text"; "text-muted"] ]
              (match help with None -> [] | Some h -> [h]) in
          let div_content =
            match kind with
            | `Text -> [full_label; full_input; full_help]
            | `Checkbox -> [full_input; full_label; full_help] in
          let div_classes =
            match kind with `Text -> [] | `Checkbox -> ["form-check"] in
          div ~a:[classes ("form-group" :: div_classes)] div_content in
        function
        | Input {input= {label= lbl; id; help}; content} ->
            generic_input ?id ?help ~kind:`Text lbl
              [ a_value (Reactive.Bidirectrional.get content)
              ; a_oninput
                  (Tyxml_lwd.Lwdom.attr
                     Js_of_ocaml.(
                       fun ev ->
                         Js.Opt.iter ev##.target (fun input ->
                             Js.Opt.iter (Dom_html.CoerceTo.input input)
                               (fun input ->
                                 let v = input##.value |> Js.to_string in
                                 dbgf "TA inputs: %d bytes: %S"
                                   (String.length v) v ;
                                 Reactive.Bidirectrional.set content v)) ;
                         true)) ]
        | Check_box {input= {label= lbl; id; help}; checked} ->
            Reactive.Bidirectrional.get checked
            |> Reactive.bind ~f:(fun init_checked ->
                   let initstatus =
                     if init_checked then [a_checked ()] else [] in
                   generic_input ?id ?help ~kind:`Checkbox lbl
                     ( initstatus
                     @ [ a_onclick
                           (Tyxml_lwd.Lwdom.attr
                              Js_of_ocaml.(
                                fun ev ->
                                  Js.Opt.iter ev##.target (fun input ->
                                      Js.Opt.iter
                                        (Dom_html.CoerceTo.input input)
                                        (fun input ->
                                          let v =
                                            input##.checked |> Js.to_bool in
                                          dbgf "checkbox → %b" v ;
                                          Reactive.Bidirectrional.set checked v)) ;
                                  true)) ] ))
        | Button {label= lbl; action} ->
            button
              ~a:
                [ a_button_type (Reactive.pure `Submit)
                ; classes ["btn"; "btn-primary"]
                ; a_onclick (Tyxml_lwd.Lwdom.attr (fun _ -> action () ; false))
                ]
              [lbl]
    end

    open Item

    let input ?id ?help content label = Input {input= {label; id; help}; content}

    let check_box ?id ?help label ~checked =
      Check_box {input= {label; id; help}; checked}

    let submit_button label action = Button {action; label}

    let make items =
      let div_items = List.map ~f:Item.to_div items in
      H5.form div_items
  end
end

module Example = struct
  let e0 () = t "Hello" %% it "World"

  let e1 () =
    let button_calls = Reactive.var 0 in
    p (e0 ())
    % Bootstrap.container_fluid
        ( p (t "This is in a bootstrap" %% ct "container-fluid.")
        % p
            (Bootstrap.button ~kind:`Primary
               ~action:(fun () ->
                 Reactive.set button_calls (Reactive.peek button_calls + 1))
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
                  ~fragment:(Reactive.pure "page-one")
              ; item ~active:(Reactive.pure false) (t "One-inactive")
                  ~action:(fun () -> assert false) ])
        %
        let hello = Reactive.var "is it me …" in
        let checkboxed = Reactive.var false in
        let submissions = Reactive.var [] in
        p (t "And now some forms")
        % Bootstrap.Form.(
            make
              [ input (Reactive.Bidirectrional.of_var hello) (t "Say Hello")
              ; check_box (t "Check this box")
                  ~checked:(Reactive.Bidirectrional.of_var checkboxed)
              ; submit_button (t "Submit This!") (fun () ->
                    Reactive.set submissions
                      ( (Reactive.peek hello, Reactive.peek checkboxed)
                      :: Reactive.peek submissions )) ])
        % p
            ( t "Form results:"
            %% bind_var hello ~f:(fun v -> t "Hello:" %% ct v)
            % t ", checkbox is "
            %% bind_var checkboxed ~f:(function
                 | false -> bt "not"
                 | true -> empty ())
            %% t "checked." )
        % itemize
            [ t "Some item"; t "Some other item"
            ; t "truc" %% it "bidule" %% bt "chouette"
            ; t "Form submissions:"
              %% bind_var submissions ~f:(fun subs ->
                     itemize ~numbered:true
                       (List.rev_map subs ~f:(fun (h, c) ->
                            t "Submission:" %% ct h % t ","
                            %% if c then it "checked" else it "unchecked"))) ]
        %
        let content = Reactive.var "content" in
        H5.div
          [ ( p (t "more input experiemnt" %% bind_var content ~f:ct)
            %% H5.(
                 input
                   ~a:
                     [ a_input_type (Reactive.pure `Text)
                     ; a_value (Reactive.pure "hello")
                     ; a_oninput
                         (Tyxml_lwd.Lwdom.attr
                            Js_of_ocaml.(
                              fun ev ->
                                Js.Opt.iter ev##.target (fun input ->
                                    Js.Opt.iter (Dom_html.CoerceTo.input input)
                                      (fun input ->
                                        let v = input##.value |> Js.to_string in
                                        dbgf "TA inputs: %d bytes: %S"
                                          (String.length v) v ;
                                        Reactive.set content v)) ;
                                false)) ]
                   ()) ) ] )
end
