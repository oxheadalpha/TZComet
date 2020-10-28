open Import

let code_mirror = "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.39.2"

type status = Non_initialized | Initialized

type t =
  { id: string
  ; language: string
  ; code: string Reactive.Bidirectrional.t
  ; status: status Reactive.var
  ; mutable text_area: Html_types.div Meta_html.t option }

let create ?(language = "mllike") id ~code =
  {id; language; code; text_area= None; status= Reactive.var Non_initialized}

let ensure te =
  match Reactive.peek te.status with
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
      Reactive.set te.status Initialized

let text_area te =
  match te.text_area with Some s -> s | None -> Fmt.failwith "TODO"

(*
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
      let area =
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
                  (txt (Var.value te.code)) ] ] in
      te.text_area <- Some area ;
      area
      
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

 *)
let set_code te ~code =
  Reactive.Bidirectrional.set te.code code ;
  let _ =
    let open Js_of_ocaml in
    Js.Unsafe.meth_call
      (Js.Unsafe.get Dom_html.window (Js.string te.id))
      "setValue"
      [|Js.string code |> Js.Unsafe.inject|] in
  ()
