open Base

let dbg fmt = Fmt.pf Fmt.stdout "@[comevitz-debug: %a@]%!" fmt ()
let dbgf fmt = Fmt.(kstr (fun s -> dbg (const string s))) fmt

module Var = struct
  type 'a t = {signal: 'a React.S.t; set: ?step:React.step -> 'a -> unit}

  let create ?eq v =
    let signal, set = React.S.create ?eq v in
    {signal; set}

  let set var v = var.set v
  let signal v = v.signal
  let value v = React.S.value v.signal

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
    type t = Welcome | Metadata_examples of {current: int option Var.t}
  end

  type t = {current_view: View.t Var.t}

  let init () = {current_view= Var.create View.Welcome}
end

module Text_editor = struct
  let code_mirror = "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.39.2"

  type status = Non_initialized | Initialized

  type t =
    {id: string; language: string; code: string Var.t; status: status Var.t}

  let create ?(language = "mllike") id ~code =
    {id; language; code; status= Var.create Non_initialized}

  let ensure te =
    match Var.value te.status with
    | Initialized -> ()
    | Non_initialized ->
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
    textarea
      ~a:
        [ a_id te.id; a_class ["form-control"]; a_style "font-family: monospace"
        ; a_rows 24
        ; a_onchange
            Js_of_ocaml.(
              fun ev ->
                Js.Opt.iter ev##.target (fun input ->
                    Js.Opt.iter (Dom_html.CoerceTo.textarea input) (fun input ->
                        let v = input##.value |> Js.to_string in
                        dbgf "TA inputs: %d bytes" (String.length v) ;
                        Var.set te.code v)) ;
                false) ]
      (txt (Var.value te.code))

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

let gui state =
  let all_examples =
    let open Tezos_contract_metadata.Metadata_contents in
    let rec go n = try (n, Example.build n) :: go (n + 1) with _ -> [] in
    go 0 in
  let ex =
    let open Tezos_contract_metadata.Metadata_contents in
    Fmt.str "Example 5:\n%a" pp (Example.build 5) in
  let test_code =
    let open Tezos_contract_metadata.Metadata_contents in
    Var.create (Example.build 5 |> to_json) in
  let test_editor =
    Text_editor.create "testeditor" ~code:test_code ~language:"yaml" in
  Text_editor.ensure test_editor ;
  RD.(
    div
      [ Reactive.div
          (Var.map_to_list state.State.current_view ~f:(function
            | Welcome ->
                [ txt "Welcome, what do you want to do?"
                ; ul
                    [ li
                        [ button
                            ~a:
                              [ a_onclick (fun _ ->
                                    Var.set state.State.current_view
                                      (Metadata_examples
                                         {current= Var.create None}) ;
                                    true) ]
                            [ txt
                                "Check-out metadata (meaningless) examples \
                                 from the library." ] ] ] ]
            | Metadata_examples {current} ->
                [ Reactive.div_of_var current ~f:(function
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
                    | Some ith -> [Fmt.kstr txt "Current: %d" ith]) ]))
      ; h3 [txt "Examples from the library:"]; pre [code [txt ex]]
      ; Text_editor.text_area test_editor ])

let attach_to_page gui =
  let open Js_of_ocaml in
  let base_div = Dom_html.getElementById "attach-ui" in
  base_div##.innerHTML := Js.string "" ;
  base_div##appendChild (Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_node gui)
  |> ignore ;
  Lwt.return ()

let go _ =
  dbg Fmt.(const string "Hello Go!") ;
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
