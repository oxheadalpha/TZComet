open Base

let dbg fmt = Fmt.pf Fmt.stdout "@[comevitz-debug: %a@]%!" fmt ()
let dbgf fmt = Fmt.(kstr (fun s -> dbg (const string s))) fmt

module Var = struct
  type 'a t =
    {name: string; signal: 'a React.S.t; set: ?step:React.step -> 'a -> unit}

  let create ?eq name v =
    let signal, set = React.S.create ?eq v in
    {name; signal; set}

  let set var v = var.set v
  let signal v = v.signal
  let value v = React.S.value v.signal

  let map ?name v ~f =
    let sgn = signal v in
    let new_signal =
      React.S.map f sgn
      |> React.S.trace (fun b -> dbgf "%s.mapped -> %b" v.name b) in
    let name = Option.value name ~default:(v.name ^ "-m") in
    {name; signal= new_signal; set= (fun ?step:_ _ -> failwith "not setable")}

  let map_to_list v ~f =
    let sgn = signal v in
    React.S.map f sgn |> ReactiveData.RList.from_signal
end

module RD = struct
  include Js_of_ocaml_tyxml.Tyxml_js.Html

  module Reactive = struct
    include Js_of_ocaml_tyxml.Tyxml_js.R.Html

    let div_of_var v ~f = Var.map_to_list v ~f |> div
  end
end

module State = struct
  module View = struct
    type t =
      | Welcome
      | Metadata_examples of {current: int option Var.t}
      | Metadata_json_editor
      | Metadata_uri_editor
  end

  type t = {current_view: View.t Var.t}

  let init () = {current_view= Var.create "current-view" View.Welcome}
end

module Bootstrap_css = struct
  let uri =
    "https://stackpath.bootstrapcdn.com/bootstrap/3.4.1/css/bootstrap.min.css"

  let ensure () =
    let (_ : unit) =
      Fmt.kstr Js_of_ocaml.Js.Unsafe.eval_string
        {js|
// Create new link Element 
var link = document.createElement('link');  
link.rel = 'stylesheet';  
link.type = 'text/css'; 
link.href = '%s';  
// Get HTML head element to append  
// link element to it  
document.getElementsByTagName('HEAD')[0].appendChild(link);  
|js}
        uri in
    ()
end

module Text_editor = struct
  let code_mirror = "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.39.2"

  type status = Non_initialized | Initialized

  type t =
    {id: string; language: string; code: string Var.t; status: status Var.t}

  let create ?(language = "mllike") id ~code =
    { id
    ; language
    ; code
    ; status= Var.create (Fmt.str "text-editor-%s-status" id) Non_initialized }

  let ensure te =
    match Var.value te.status with
    | Initialized -> ()
    | Non_initialized ->
        dbgf "Initializing %S" te.id ;
        let (_ : unit) =
          Fmt.kstr Js_of_ocaml.Js.Unsafe.eval_string
            {js|
// Create new link Element 
var link = document.createElement('link');  
link.rel = 'stylesheet';  
link.type = 'text/css'; 
link.href = '%s/codemirror.css';  
// Get HTML head element to append  
// link element to it  
document.getElementsByTagName('HEAD')[0].appendChild(link);  
// Now the JS:  
var main_script = document.createElement('script');
main_script.src = '%s/codemirror.min.js';
document.head.appendChild(main_script);
main_script.onload = function () {
var lang_script = document.createElement('script');
lang_script.src = '%s/mode/%s/%s.min.js';
document.head.appendChild(lang_script);
lang_script.onload = function () {
  var editor = CodeMirror.fromTextArea(document.getElementById(%S),
  {
  mode: %S,
  onChange: function(cm){
     console.log("Cm save" + cm);
     cm.save();
     document.getElementById(%S).change()
  },
  lineWrapping: true,
  lineNumbers: true
  });
  window.%s = editor;
  editor.on('change', editor => {
      editor.save();
      var evt = document.createEvent("HTMLEvents");
      evt.initEvent("change", false, true);
      document.getElementById(%S)
         .dispatchEvent(evt);
  });
  /* See https://codemirror.net/demo/indentwrap.html */
  var charWidth = editor.defaultCharWidth(), basePadding = 4;
  editor.on("renderLine", function(cm, line, elt) {
      var off = (2 + CodeMirror.countColumn(line.text, null, cm.getOption("tabSize"))) * charWidth;
      elt.style.textIndent = "-" + off + "px";
          elt.style.paddingLeft = (basePadding + off) + "px";
      });
  editor.refresh();
}};
|js}
            code_mirror code_mirror code_mirror te.language te.language te.id
            te.language te.id te.id te.id in
        Var.set te.status Initialized

  let text_area te =
    let open RD in
    let css =
      {css|
.editorcontainer { height: 50% }
@media (min-width: 992px) {
    .editorcontainer { height: 90% }
}
.CodeMirror { height: auto }
|css}
    in
    div
      [ style [txt css]
      ; div
          ~a:[a_class ["editorcontainer"]]
          [ textarea
              ~a:
                [ a_id te.id; a_class ["form-control"]
                ; a_style "font-family: monospace"; a_rows 80
                ; a_onchange
                    Js_of_ocaml.(
                      fun ev ->
                        Js.Opt.iter ev##.target (fun input ->
                            Js.Opt.iter (Dom_html.CoerceTo.textarea input)
                              (fun input ->
                                let v = input##.value |> Js.to_string in
                                dbgf "TA inputs: %d bytes" (String.length v) ;
                                Var.set te.code v)) ;
                        false) ]
              (txt (Var.value te.code)) ] ]

  let editor_command_button te ~text command_name =
    let open RD in
    button [txt text]
      ~a:
        [ a_class ["btn"; "btn-secondary"]
        ; a_onclick
            Js_of_ocaml.(
              fun _ ->
                let _ =
                  Js.Unsafe.meth_call
                    (Js.Unsafe.get Dom_html.window (Js.string te.id))
                    "execCommand"
                    [|Js.string command_name |> Js.Unsafe.inject|] in
                let _ =
                  Js.Unsafe.meth_call
                    (Js.Unsafe.get Dom_html.window (Js.string te.id))
                    "focus" [||] in
                true) ]
end

module Menu = struct
  type item =
    { message: Html_types.flow5_without_interactive RD.elt
    ; long_message: Html_types.flow5_without_interactive RD.elt option
    ; description: Html_types.p RD.elt option
    ; active: bool Var.t
    ; action: unit -> unit }

  let item ?long_message ?active ?description message action =
    { message
    ; action
    ; long_message
    ; description
    ; active=
        ( match active with
        | None -> Var.create (Fmt.str "item-active") true
        | Some a -> a ) }

  let to_html ?(kind = `Short) items =
    let open RD in
    dbgf "menu to html, %d items" (List.length items) ;
    ul
      ~a:[a_class [(match kind with `Short -> "nav nav-tabs" | `Long -> "")]]
      (List.map items
         ~f:(fun {message; long_message; description; active; action} ->
           dbgf "menu to html map" ;
           let actual_text =
             match kind with
             | `Short -> message
             | `Long -> Option.value ~default:message long_message in
           let active_class =
             (* in bootstrap active means currently already activated *)
             Reactive.a_class
               ( Var.signal active
               |> React.S.map (function false -> ["active"] | true -> []) )
           in
           let desc (* : [< Html_types.p] elt *) =
             match (kind, description) with
             | _, None | `Short, _ -> span []
             | `Long, Some d -> div [d] in
           try
             Reactive.li ~a:[active_class]
               (Var.map_to_list active ~f:(function
                 | true ->
                     [ a [actual_text]
                         ~a:[a_href "#"; a_onclick (fun _ -> action () ; true)]
                     ; desc ]
                 | false ->
                     [a [actual_text]; desc (* :> [> Html_types.p ] elt *)]))
           with e ->
             dbgf "exn in map: %a" Exn.pp e ;
             ( try dbgf "active: %b" (Var.value active)
               with _ -> dbgf "so, it's “active”" ) ;
             li [txt "error"]))
end

module Contract_metadata = struct
  module Content = struct
    let to_html metadata =
      let open RD in
      let open Tezos_contract_metadata.Metadata_contents in
      let option_field name field f =
        match field with
        | None -> []
        | Some s -> [li [b [Fmt.kstr txt "%s: " name]; f s]] in
      let normal_field name x = option_field name (Some ()) (fun () -> x) in
      let code_string t = code [txt t] in
      let paragraphs t =
        let rec go l acc =
          match List.split_while l ~f:(function "" -> false | _ -> true) with
          | ll, [] -> String.concat ~sep:" " ll :: acc
          | ll, _ :: lr -> go lr (String.concat ~sep:" " ll :: acc) in
        go (String.split t ~on:'\n') []
        |> List.rev_map ~f:(fun x -> span [br (); txt x])
        |> span in
      let license l =
        let open License in
        span
          [ i [txt l.name]
          ; span
              (Option.value_map ~default:[] l.details ~f:(fun d ->
                   [Fmt.kstr txt " → %s" d])) ] in
      let list_field name field f =
        option_field name (match field with [] -> None | more -> Some more) f
      in
      let authors l =
        List.map l ~f:code_string |> List.intersperse ~sep:(txt ", ") |> span
      in
      let interfaces l =
        let interface s =
          let r = Re.Posix.re "TZIP-([0-9]+)" in
          let normal s = i [txt s] in
          let tok = function
            | `Text s -> normal s
            | `Delim g -> (
                let the_text = normal (Re.Group.get g 0) in
                try
                  let tzip_nb = Re.Group.get g 1 |> Int.of_string in
                  a
                    ~a:
                      [ Fmt.kstr a_href
                          "https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-%d/tzip-%d.md"
                          tzip_nb tzip_nb ]
                    [the_text]
                  (* Fmt.kstr txt "{%a --- %d}" Re.Group.pp g (Re.Group.nb_groups g)] *)
                with e ->
                  dbgf "Error in interface html: %a" Exn.pp e ;
                  the_text ) in
          Re.split_full (Re.compile r) s |> List.map ~f:tok |> span in
        List.map l ~f:interface |> List.intersperse ~sep:(txt ", ") |> span
      in
      let _todo l = Fmt.kstr txt "todo: %d items" (List.length l) in
      let views (views : View.t list) =
        let view v =
          let open View in
          let purity =
            if v.is_pure then span ~a:[a_class ["bg-success"]] [txt "pure"]
            else span ~a:[a_class ["bg-warning"]] [txt "inpure"] in
          let implementations impls =
            let open Implementation in
            ul
              (List.map impls ~f:(function
                | Michelson_storage ms ->
                    let open Michelson_storage in
                    let mich (Micheline m) =
                      let open Tezos_micheline in
                      Fmt.str "%a" Micheline_printer.print_expr
                        (Micheline_printer.printable Base.Fn.id m) in
                    li
                      [ b [txt "Michelson-storage-view:"]
                      ; ul
                          ( option_field "Michelson-Version" ms.version
                              (fun s ->
                                Fmt.kstr code_string "%s" (String.prefix s 12))
                          @ normal_field "Type"
                              (code
                                 [ Fmt.kstr txt "%s<contract-storage> → %s"
                                     ( match ms.parameter with
                                     | None -> ""
                                     | Some p -> mich p ^ " × " )
                                     (mich ms.return_type) ])
                          @ normal_field "Code"
                              (details
                                 (summary [txt "Expand"])
                                 [pre [code [txt (mich ms.code)]]])
                          @ list_field "Annotations" ms.human_annotations
                              (fun anns ->
                                ul
                                  (List.map anns ~f:(fun (k, v) ->
                                       li
                                         [ code_string k; txt " → "
                                         ; em [txt v] ]))) ) ]
                | Rest_api_query raq ->
                    li
                      [ b [txt "REST-API Query:"]
                      ; ul
                          ( normal_field "OpenAPI Spec"
                              (code_string raq.specification_uri)
                          @ option_field "Base-URI Override" raq.base_uri
                              code_string
                          @ normal_field "Path" (code_string raq.path)
                          @ normal_field "Method"
                              (Fmt.kstr code_string "%s"
                                 (Cohttp.Code.string_of_method raq.meth)) ) ]))
          in
          div
            [ b [txt v.name]; txt " ("; purity; txt "):"
            ; ul
                ( option_field "Description" v.description paragraphs
                @ list_field "Implementation(s)" v.implementations
                    implementations ) ] in
        ul (List.map views ~f:(fun v -> li [view v])) in
      let unknown_extras kv =
        ul
          (List.map kv ~f:(fun (k, v) ->
               li
                 [ code_string k; txt " "
                 ; pre [code [txt (Ezjsonm.value_to_string ~minify:false v)]] ]))
      in
      ul
        ( option_field "Name" metadata.name code_string
        @ option_field "Version" metadata.version code_string
        @ option_field "Description" metadata.description paragraphs
        @ option_field "License" metadata.license license
        @ list_field "Authors" metadata.authors authors
        @ list_field "Interfaces" metadata.interfaces interfaces
        @ list_field "Views" metadata.views views
        @ list_field "Extra/Unknown" metadata.unknown unknown_extras )
  end
end

let gui state =
  let all_examples =
    let open Tezos_contract_metadata.Metadata_contents in
    let rec go n = try (n, Example.build n) :: go (n + 1) with _ -> [] in
    go 0 in
  let ex =
    let open Tezos_contract_metadata.Metadata_contents in
    Fmt.str "Example 5:\n%a" pp (Example.build 5) in
  RD.(
    let menu which =
      let items =
        [ Menu.item
            (txt "Metadata JSON Vali-Editor")
            ~long_message:(txt "Start the Metadata JSON editor.")
            ~description:
              (p
                 [ txt
                     "It is a text editor that parses and tries to validate \
                      the JSON metadata that is input. There examples one can \
                      load too." ])
            ~active:
              (Var.map state.State.current_view ~f:(function
                | Metadata_json_editor -> false
                | _ -> true))
            (fun () -> Var.set state.State.current_view Metadata_json_editor)
        ; Menu.item (txt "URI Vali-Editor")
            ~long_message:(txt "Start the Metadata URI editor.")
            ~active:
              (Var.map state.State.current_view ~f:(function
                | Metadata_uri_editor -> false
                | _ -> true))
            (fun () -> Var.set state.State.current_view Metadata_uri_editor) ]
      in
      let home =
        Menu.item (txt "Home")
          ~active:
            (Var.map state.State.current_view ~f:(function
              | Welcome -> false
              | _ -> true))
          (fun () ->
            dbgf "Going back home" ;
            Var.set state.State.current_view Welcome) in
      let kind, all_items =
        match which with
        | `Top -> (`Short, home :: items)
        | `Welcome -> (`Long, items) in
      let m =
        try Menu.to_html ~kind all_items
        with e -> dbgf "exn: %a" Exn.pp e ; div [] in
      dbgf "menu done" ; m in
    let menu_welcome =
      (* This is up here because it fails if it is within the
         map-to-list function below. *)
      menu `Welcome in
    let metadata_json_code =
      let open Tezos_contract_metadata.Metadata_contents in
      Var.create "metadata-json-code" (Example.build 5 |> to_json) in
    let metadata_json_editor =
      Text_editor.create "metadatajsoneditor" ~code:metadata_json_code
        ~language:"yaml" in
    let metadata_json_editor_area = Text_editor.text_area metadata_json_editor in
    let metadata_uri_code = Var.create "metadata-uri-code" "tezos-storage:foo" in
    let metadata_uri_editor =
      Text_editor.create "metadataurieditor" ~code:metadata_uri_code
        ~language:"css" in
    let metadata_uri_editor_area = Text_editor.text_area metadata_uri_editor in
    let editor_with_preview editor editor_area result_div =
      Text_editor.ensure editor ;
      div
        ~a:[a_class ["col-md-12"]]
        [ div ~a:[a_class ["col-md-6"]] [editor_area]
        ; div ~a:[a_class ["col-md-6"]] [result_div] ] in
    div
      ~a:[a_class ["container-fluid"]]
      [ div ~a:[a_class ["col-md-12"]] [menu `Top]; hr ()
      ; Reactive.div
          (Var.map_to_list state.State.current_view ~f:(function
            | Welcome ->
                dbgf "Showing welc-home" ;
                [div [h3 [txt "Welcome, what do you want to do?"]; menu_welcome]]
            | Metadata_examples {current} ->
                [ div
                    [h3 [txt "Examples from the library:"]; pre [code [txt ex]]]
                ; Reactive.div_of_var current ~f:(function
                    | None ->
                        [ txt "Current: none"
                        ; ul
                            (List.map all_examples ~f:(fun (ith, _ex) ->
                                 li
                                   [ button
                                       ~a:
                                         [ a_onclick (fun _ ->
                                               Var.set current (Some ith) ; true)
                                         ]
                                       [Fmt.kstr txt "Example #%d" ith] ])) ]
                    | Some ith -> [Fmt.kstr txt "Current: %d" ith]) ]
            | Metadata_uri_editor ->
                let result_div =
                  Reactive.div_of_var metadata_uri_code ~f:(fun uri_code ->
                      let open Tezos_contract_metadata.Metadata_uri in
                      match Uri.of_string uri_code |> of_uri with
                      | Ok o ->
                          [Fmt.kstr (fun s -> pre [code [txt s]]) "%a" pp o]
                      | Error el ->
                          [ Fmt.kstr
                              (fun s -> pre [code [txt s]])
                              "%a" Tezos_error_monad.Error_monad.pp_print_error
                              el ]) in
                [ editor_with_preview metadata_uri_editor
                    metadata_uri_editor_area result_div ]
            | Metadata_json_editor ->
                let result_div =
                  Reactive.div_of_var metadata_json_code ~f:(fun json_code ->
                      let open Tezos_contract_metadata.Metadata_contents in
                      match of_json json_code with
                      | Ok ex ->
                          [ div [Contract_metadata.Content.to_html ex]
                          ; Fmt.kstr (fun s -> pre [code [txt s]]) "%a" pp ex ]
                      | Error el ->
                          [ Fmt.kstr
                              (fun s -> pre [code [txt s]])
                              "%a" Tezos_error_monad.Error_monad.pp_print_error
                              el ]) in
                [ editor_with_preview metadata_json_editor
                    metadata_json_editor_area result_div ]))
        (*  ; Text_editor.text_area test_editor *) ])

let attach_to_page gui =
  let open Js_of_ocaml in
  let base_div = Dom_html.getElementById "attach-ui" in
  base_div##.innerHTML := Js.string "" ;
  base_div##appendChild (Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_node gui)
  |> ignore ;
  Lwt.return ()

let go _ =
  dbg Fmt.(const string "Hello Go!") ;
  Bootstrap_css.ensure () ;
  ignore
    Lwt.(
      let state = State.init () in
      catch
        (fun () -> attach_to_page (gui state) >>= fun () -> return ())
        (fun exn ->
          Printf.ksprintf
            (fun s -> Fmt.epr "ERROR: %s" s ; failwith s)
            "Uncaught Exception: %s" (Exn.to_string exn))) ;
  Js_of_ocaml.Js._true

let _ =
  dbgf "Hello Main!" ;
  let open Js_of_ocaml in
  (Lwt.async_exception_hook := fun e -> dbgf "Async Exn: %s" (Exn.to_string e)) ;
  Dom_html.window##.onload := Dom_html.handler go
