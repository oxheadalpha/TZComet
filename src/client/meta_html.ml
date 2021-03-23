open Import
module H5 = Tyxml_lwd.Html

type 'a t = 'a H5.elt (* list Lwd.t *)

let t (s : string) : _ t = H5.(txt (Lwd.pure s))
let empty () = Lwd.pure Lwd_seq.empty
let ( % ) a b : _ t = Lwd.map2 ~f:Lwd_seq.concat a b
let ( %% ) a b : _ t = a % t " " % b
let singletize f ?a x = f ?a [x]

let list = function
  | [] -> empty ()
  | one :: more -> List.fold more ~init:one ~f:( % )

let parens c = t "(" % c % t ")"

module H = struct
  let p ?a l = singletize H5.p ?a l
  let i ?a l = singletize H5.i ?a l
  let b ?a l = singletize H5.b ?a l
  let em ?a l = singletize H5.em ?a l
  let code ?a l = singletize H5.code ?a l
  let button ?a l = singletize H5.button ?a l
  let span ?a l = singletize H5.span ?a l
  let sub ?a l = singletize H5.sub ?a l
  let sup ?a l = singletize H5.sup ?a l
  let small ?a l = singletize H5.small ?a l
  let strong ?a l = singletize H5.strong ?a l
  let abbr ?a l = singletize H5.abbr ?a l
  let a ?a l = singletize H5.a ?a l
  let div ?a l = singletize H5.div ?a l
  let pre ?a l = singletize H5.pre ?a l
  let h1 ?a l = singletize H5.h1 ?a l
  let h2 ?a l = singletize H5.h2 ?a l
  let h3 ?a l = singletize H5.h3 ?a l
  let h4 ?a l = singletize H5.h4 ?a l
  let h5 ?a l = singletize H5.h5 ?a l
  let h6 ?a l = singletize H5.h6 ?a l
  let tr ?a l = singletize H5.tr ?a l
  let td ?a l = singletize H5.td ?a l
  let th ?a l = singletize H5.th ?a l
  let blockquote ?a l = singletize H5.blockquote ?a l
end

let hr = H5.hr
let br = H5.br

include H

let classes l = H5.a_class (Lwd.pure l)
let style s = H5.a_style (Lwd.pure s)
let it s = i (t s)
let bt s = b (t s)
let ct s = code (t s)

let link ~target ?(a = []) content =
  H.a ~a:(H5.a_href (Lwd.pure target) :: a) content

let url ?a t u = link ?a ~target:u (t u)
let abbreviation title c = H5.abbr ~a:[H5.a_title (Lwd.pure title)] [c]

let button ?(a = []) ~action k =
  H.button
    ~a:(H5.a_onclick (Tyxml_lwd.Lwdom.attr (fun _ -> action () ; false)) :: a)
    k

let onclick_action action =
  H5.a_onclick (Tyxml_lwd.Lwdom.attr (fun _ -> action () ; true))

let itemize ?(numbered = false) ?a_ul ?a_li l =
  (if not numbered then H5.ul else H5.ol)
    ?a:a_ul
    (List.map l ~f:(fun item -> H5.li ?a:a_li [item]))

let input_bidirectional ?(a = []) bidi =
  H5.input
    ~a:
      ( a
      @ [ H5.a_value (Reactive.Bidirectional.get bidi)
        ; H5.a_oninput
            (Tyxml_lwd.Lwdom.attr
               Js_of_ocaml.(
                 fun ev ->
                   Js.Opt.iter ev##.target (fun input ->
                       Js.Opt.iter (Dom_html.CoerceTo.input input) (fun input ->
                           let v = input##.value |> Js.to_string in
                           Reactive.Bidirectional.set bidi v)) ;
                   true)) ] )
    ()

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

  let color kind content =
    H5.span
      ~a:[classes [Fmt.str "text-%s" (Label_kind.to_string kind)]]
      [content]

  let muted f content =
    (* Using `?a:Some` to force the type inference to see the optional argument. *)
    f ?a:(Some [classes ["text-muted"]]) content

  let spinner ?(kind = `Primary) content =
    H5.div
      ~a:[classes ["spinner-border"]; H5.a_role (Lwd.pure ["status"])]
      [H5.span ~a:[classes ["sr-only"]] [content]]

  let alert ?(kind = `Primary) content =
    H5.div
      ~a:
        [ classes ["alert"; Fmt.str "alert-%s" (Label_kind.to_string kind)]
        ; H5.a_role (Lwd.pure ["alert"]) ]
      [content]

  let p_lead ?(a = []) c = p ~a:(classes ["lead"] :: a) c
  let div_lead ?(a = []) c = div ~a:(classes ["lead"] :: a) c

  let bordered ?(a = []) ?(rounded = `Default) ?(kind = `Primary) content =
    let a =
      [ classes
          ( ["border"; Fmt.str "border-%s" (Label_kind.to_string kind)]
          @ match rounded with `Default -> ["rounded-sm"] | `No -> [] ) ]
      @ a in
    H5.div ~a [content]

  let monospace content = H5.span ~a:[classes ["text-monospace"]] [content]
  let terminal_logs content = div ~a:[classes ["bg-dark"; "text-white"]] content
  let _raw_button = button

  let button ?(outline = false) ?(disabled = false) ?(size = `Normal)
      ?(kind = `Light) content ~action =
    let a =
      [ classes
          ( [ "btn"
            ; Fmt.str "btn-%s%s"
                (if outline then "outline-" else "")
                (Label_kind.to_string kind) ]
          @
          match size with
          | `Normal -> []
          | `Small -> ["btn-sm"]
          | `Large -> ["btn-lg"] ) ]
      @ if disabled then [H5.a_disabled ()] else [] in
    _raw_button ~action ~a content

  let close_button ~action =
    let a = [classes ["close"]; H5.a_aria "label" (Lwd.pure ["Close"])] in
    _raw_button ~action ~a (t "×")

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
                  ; a_onclick
                      (Tyxml_lwd.Lwdom.attr (fun _ -> action () ; false)) ]
                [content]
          | `Menu_header content ->
              h6 ~a:[a_class (Reactive.pure ["dropdown-header"])] [content])
      in
      H5.(
        let open Tyxml_lwd.Lwdom in
        div
          ~a:
            [ a_class (Reactive.pure ["dropdown"])
            ; a_style (Reactive.pure "display: inline-block") ]
          [ button
              ~a:
                [ a_class
                    (Reactive.pure
                       [ "btn"
                       ; Fmt.str "btn-%s" (Label_kind.to_string kind)
                       ; "dropdown-toggle" ])
                  (* ; a_type (Reactive.pure `Button) *)
                ; a_id (Reactive.pure id)
                ; a_user_data "toggle" (Reactive.pure "dropdown")
                ; a_aria "haspopup" (Reactive.pure ["true"])
                ; a_aria "expanded" (Reactive.pure ["false"]) ]
              [content]
          ; div
              ~a:
                [ a_class
                    (Reactive.pure ["dropdown-menu"; "dropdown-menu-lg-right"])
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

  module Tab_bar = struct
    type item =
      { label: Html_types.a_content_fun H5.elt
      ; active: bool Reactive.t
      ; action: unit -> unit }

    let item ~active ~action label = {label; action; active}

    let make items =
      H5.ul
        ~a:[classes ["nav"; "nav-tabs"]]
        (List.map items ~f:(fun {label; active; action} ->
             let li = H5.li ~a:[classes ["nav-item"]] in
             Reactive.bind active ~f:(function
               | true ->
                   li
                     [ a
                         ~a:
                           [ classes ["nav-link"]
                           ; H5.a_onclick
                               (Tyxml_lwd.Lwdom.attr (fun ev ->
                                    action () ; true)) ]
                         label ]
               | false ->
                   (* in bootstrap active means currently already activated *)
                   li [a ~a:[classes ["active"; "nav-link"]] label])))
  end

  module Form = struct
    module Item = struct
      type input =
        { label: Html_types.label_content_fun H5.elt option
        ; id: string option
        ; active: bool Reactive.t
        ; help: Html_types.small_content_fun H5.elt option }

      type t =
        | Row of (int * t) list
        | Any of Html_types.div_content_fun H5.elt
        | Input of
            { input: input
            ; placeholder: string Reactive.t option
            ; content: string Reactive.Bidirectional.t }
        | Check_box of {input: input; checked: bool Reactive.Bidirectional.t}
        | Button of
            { label: Html_types.button_content_fun H5.elt
            ; active: bool Reactive.t
            ; action: unit -> unit }

      let rec to_div ?(enter_action = fun () -> ()) ?cols =
        let open H5 in
        let generic_input ~active ?id ?help ?placeholder ~kind lbl more_a =
          let the_id = Fresh_id.of_option "input-item" id in
          let help_id = the_id ^ "Help" in
          let full_label =
            Option.value_map ~default:(empty ()) lbl ~f:(fun lbl ->
                label
                  ~a:
                    [ (* shoud be for="<id>" *)
                      classes
                        ( match kind with
                        | `Text -> []
                        | `Checkbox -> ["form-check-label"] ) ]
                  [lbl]) in
          let full_input =
            let a_base =
              [ classes
                  [ ( match kind with
                    | `Text -> "form-control"
                    | `Checkbox -> "form-check-input" ) ]
              ; a_id (Reactive.pure the_id)
              ; a_aria "describedBy" (Reactive.pure [help_id])
              ; a_onkeydown
                  (Tyxml_lwd.Lwdom.attr (fun ev ->
                       dbgf "keycode: %d" ev##.keyCode ;
                       match ev##.keyCode with
                       | 13 when not (Js_of_ocaml.Js.to_bool ev##.shiftKey) ->
                           enter_action () ; false
                       | _ -> true))
              ; a_input_type (Reactive.pure kind) ]
              @ Option.value_map placeholder ~default:[] ~f:(fun plc ->
                    [a_placeholder plc])
              @ more_a in
            Reactive.bind active ~f:(function
              | true -> input ~a:a_base ()
              | false -> input ~a:(a_disabled () :: a_base) ()) in
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
          let cols_class =
            match cols with
            | None -> []
            | Some n when 1 <= n && n <= 12 -> [Fmt.str "col-md-%d" n]
            | Some _ -> assert false in
          let div_classes =
            match kind with
            | `Text -> cols_class
            | `Checkbox -> "form-check" :: cols_class in
          div ~a:[classes ("form-group" :: div_classes)] div_content in
        function
        | Row l ->
            div
              ~a:[classes ["form-row"]]
              (List.map l ~f:(fun (cols, item) ->
                   to_div ~enter_action ~cols item))
        | Input {input= {label= lbl; id; help; active}; placeholder; content} ->
            generic_input ?id ?help ~kind:`Text lbl ~active ?placeholder
              [ a_value (Reactive.Bidirectional.get content)
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
                                 Reactive.Bidirectional.set content v)) ;
                         true)) ]
        | Check_box {input= {label= lbl; id; help; active}; checked} ->
            Reactive.Bidirectional.get checked
            |> Reactive.bind ~f:(fun init_checked ->
                   let initstatus =
                     if init_checked then [a_checked ()] else [] in
                   generic_input ?id ?help ~kind:`Checkbox lbl ~active
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
                                          Reactive.Bidirectional.set checked v)) ;
                                  true)) ] ))
        | Button {label= lbl; active; action} ->
            let btn = ["btn"; "btn-primary"] in
            let cls =
              match cols with
              | None -> Fn.id
              | Some n -> fun x -> div ~a:[classes [Fmt.str "col-md-%d" n]] [x]
            in
            Reactive.bind active ~f:(fun is_active ->
                let a =
                  let base =
                    [a_button_type (Reactive.pure `Submit); classes btn] in
                  match is_active with
                  | true ->
                      a_onclick
                        (Tyxml_lwd.Lwdom.attr (fun _ -> action () ; false))
                      :: base
                  | false -> a_disabled () :: base in
                button ~a [lbl] |> cls)
        | Any the_div ->
            let cls =
              let base = ["form-group"] in
              match cols with
              | None -> base
              | Some n -> Fmt.str "col-md-%d" n :: base in
            div ~a:[classes cls] [the_div]
    end

    open Item

    let input ?(active = Reactive.pure true) ?id ?placeholder ?help ?label
        content =
      Input {input= {label; id; help; active}; placeholder; content}

    let check_box ?(active = Reactive.pure true) ?id ?help ?label checked =
      Check_box {input= {label; id; help; active}; checked}

    let submit_button ?(active = Reactive.pure true) label action =
      Button {action; active; label}

    let cell i item = (i, item)
    let row l = Row l
    let magic d = Any d

    let make ?enter_action items =
      let div_items = List.map ~f:(Item.to_div ?enter_action) items in
      H5.form div_items
  end

  module Collapse = struct
    type state = [`Hidden | `Hiding | `Shown | `Showing]

    module Global_jquery_communication = struct
      (** This module brings the ["*.bs.collapse"] jQuery events into the Lwd
          realm, using each a global handler for traditional JS events.

          See the generation of the events forwarding in
          ["src/gen-web/main.ml"]:

          {v
           $(document).on('hidden.bs.collapse', function (e) {
              var ev = new CustomEvent('collapse-hidden', { detail: e.target.id } );
              document.body.dispatchEvent(ev);
           })
          v} *)

      let done_once = ref false
      let ids_and_states : (string * state Reactive.var) list ref = ref []

      let ensure_handlers () =
        match !done_once with
        | true -> ()
        | false ->
            let open Js_of_ocaml in
            (* This adds one listener per collapse element … *)
            let the_div =
              (Dom_html.document##.body :> Js_of_ocaml.Dom_html.element Js.t)
            in
            List.iter
              [ ("shown", `Shown)
              ; ("show", `Showing)
              ; ("hide", `Hiding)
              ; ("hidden", `Hidden) ]
              ~f:(fun (evname, resulting_status) ->
                let ev_type = Fmt.kstr Dom.Event.make "collapse-%s" evname in
                let _id =
                  Dom_html.addEventListener the_div ev_type
                    (Dom_html.handler (fun ev ->
                         dbgf "html-handler (%d): %s -- %s"
                           (List.length !ids_and_states)
                           (Js.to_string ev##._type)
                           (Js.to_string ev##.detail) ;
                         List.iter !ids_and_states ~f:(fun (the_id, state) ->
                             if String.equal (Js.to_string ev##.detail) the_id
                             then Reactive.set state resulting_status) ;
                         Js._true))
                    Js._true in
                ()) ;
            done_once := true

      let register id state = ids_and_states := (id, state) :: !ids_and_states

      let unregister id =
        ids_and_states :=
          List.Assoc.remove !ids_and_states id ~equal:String.equal
    end

    type t = {id: string Reactive.t; state: state Reactive.var}

    let make ?id () =
      let open H5 in
      let (state : state Reactive.var) = Reactive.var `Hidden in
      let the_id_prim =
        Reactive.prim
          ~acquire:(fun _ ->
            let the_id = Fresh_id.of_option "collapse" id in
            Global_jquery_communication.ensure_handlers () ;
            Global_jquery_communication.register the_id state ;
            the_id)
          ~release:(fun _ id -> Global_jquery_communication.unregister id) in
      let the_id = Reactive.get_prim the_id_prim in
      {id= the_id; state}

    let full_state t = Reactive.get t.state

    let collapsed_state t =
      full_state t
      |> Reactive.map ~f:(function
           | `Hiding | `Hidden -> true
           | `Showing | `Shown -> false)

    let make_button ?(kind = `Primary) ?style ?more_classes t content =
      let more_a =
        Option.value_map style ~default:[] ~f:(fun s -> [H5.a_style s]) in
      H5.button
        ~a:
          ( more_a
          @ H5.
              [ classes
                  [ "btn"
                  ; "btn-sm"
                  ; Fmt.str "btn-outline-%s" (Label_kind.to_string kind) ]
              ; a_user_data "toggle" (Lwd.pure "collapse")
              ; a_user_data "target" (Reactive.map ~f:(Fmt.str "#%s") t.id)
              ; a_aria "expanded"
                  (Reactive.map (collapsed_state t) ~f:(function
                    | true -> ["false"]
                    | false -> ["true"]))
              ; a_aria "controls" (Reactive.map ~f:(fun x -> [x]) t.id) ] )
        [content]

    let make_div t content =
      Reactive.bind (collapsed_state t) ~f:(function
        | true -> div ~a:[classes ["collapse"]; H5.a_id t.id] (empty ())
        | false ->
            div ~a:[classes ["collapse"; "show"]; H5.a_id t.id] (content ()))

    let fixed_width_reactive_button_with_div_below ?kind t ~width ~button
        content =
      make_button ?kind t
        ~style:(Reactive.pure (Fmt.str "width: %s" width))
        (Reactive.bind (collapsed_state t) ~f:button)
      % make_div t content
  end

  module Table = struct
    let simple ?header_row content =
      let open H5 in
      let thead =
        Option.map header_row ~f:(fun hl ->
            thead [tr (List.map hl ~f:(fun x -> th [x]))]) in
      tablex
        ~a:[classes ["table"; "table-bordered"; "table-hover"]]
        ?thead
        [tbody [content]]
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
               (Reactive.bind_var button_calls ~f:(fun count ->
                    H5.span
                      [ Fmt.kstr
                          (if Stdlib.( mod ) count 2 = 0 then it else bt)
                          "Click %d" count ])))
        % p
            (Bootstrap.label `Danger
               (Reactive.bind_var button_calls ~f:(fun count ->
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
              [ input
                  (Reactive.Bidirectional.of_var hello)
                  ~label:(t "Say Hello")
              ; check_box ~label:(t "Check this box")
                  (Reactive.Bidirectional.of_var checkboxed)
              ; submit_button (t "Submit This!") (fun () ->
                    Reactive.set submissions
                      ( (Reactive.peek hello, Reactive.peek checkboxed)
                      :: Reactive.peek submissions )) ])
        % p
            ( t "Form results:"
            %% Reactive.bind_var hello ~f:(fun v -> t "Hello:" %% ct v)
            % t ", checkbox is "
            %% Reactive.bind_var checkboxed ~f:(function
                 | false -> bt "not"
                 | true -> empty ())
            %% t "checked." )
        % itemize
            [ t "Some item"
            ; t "Some other item"
            ; t "truc" %% it "bidule" %% bt "chouette"
            ; t "Form submissions:"
              %% Reactive.bind_var submissions ~f:(fun subs ->
                     itemize ~numbered:true
                       (List.rev_map subs ~f:(fun (h, c) ->
                            t "Submission:" %% ct h % t ","
                            %% if c then it "checked" else it "unchecked"))) ]
        %
        let content = Reactive.var "content" in
        H5.div
          [ ( p (t "more input experiemnt" %% Reactive.bind_var content ~f:ct)
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
