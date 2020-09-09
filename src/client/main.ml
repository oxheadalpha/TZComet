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
    let new_signal = React.S.map f sgn in
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
      | Metadata_json_editor
      | Metadata_uri_editor
      | Michelson_bytes_parser
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

  let set_code te ~code =
    Var.set te.code code ;
    let _ =
      let open Js_of_ocaml in
      Js.Unsafe.meth_call
        (Js.Unsafe.get Dom_html.window (Js.string te.id))
        "setValue"
        [|Js.string code |> Js.Unsafe.inject|] in
    ()
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
  module Uri = struct
    let rec to_html uri =
      let open RD in
      let open Tezos_contract_metadata.Metadata_uri in
      let li_field name content = li (em [txt name; txt ": "] :: content) in
      match uri with
      | Web u -> [txt "Web URL: "; a ~a:[a_href u] [code [txt u]]]
      | Ipfs {cid; path} ->
          let gatewayed = Fmt.str "https://gateway.ipfs.io/ipfs/%s%s" cid path in
          [ txt "IPFS URI: "
          ; ul
              [ li_field "CID" [code [txt cid]]
              ; li_field "Path" [code [txt path]]
              ; li
                  [txt "(Try "; a ~a:[a_href gatewayed] [txt gatewayed]; txt ")"]
              ] ]
      | Storage {network; address; key} ->
          [ txt "In Storage:"
          ; ul
              [ li_field "Network"
                  [ Option.value_map network ~default:(txt "“Current”.")
                      ~f:(fun s -> code [txt s]) ]
              ; li_field "Address"
                  [ Option.value_map address
                      ~default:(txt "“Same contract”.") ~f:(fun s ->
                        code [txt s]) ]
              ; li_field "Key in the big-map" [code [txt key]] ] ]
      | Hash {kind= `Sha256; value; target} ->
          [ txt "Hash checked URI:"
          ; ul
              [ li_field "Target" (to_html target)
              ; li_field "… should SHA256-hash to"
                  [code [Fmt.kstr txt "%a" Hex.pp (Hex.of_string value)]] ] ]
  end

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

module Michelson_bytes = struct
  (** See src/proto_alpha/lib_protocol/michelson_v1_primitives.ml *)
  let prim_encoding =
    let open Data_encoding in
    def "michelson.v1.primitives"
    @@ string_enum
         [ (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, "FOR" BACKWARD COMPATIBILITY OF THE ENCODING. *)
           ("parameter", "K_parameter"); ("storage", "K_storage")
         ; ("code", "K_code"); ("False", "False"); ("Elt", "Elt")
         ; ("Left", "Left"); ("None", "None"); ("Pair", "Pair")
         ; ("Right", "Right"); ("Some", "Some")
         ; (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, "FOR" BACKWARD COMPATIBILITY OF THE ENCODING. *)
           ("True", "True"); ("Unit", "Unit"); ("PACK", "PACK")
         ; ("UNPACK", "UNPACK"); ("BLAKE2B", "BLAKE2B"); ("SHA256", "SHA256")
         ; ("SHA512", "SHA512"); ("ABS", "ABS"); ("ADD", "ADD")
         ; ("AMOUNT", "AMOUNT")
         ; (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, "FOR" BACKWARD COMPATIBILITY OF THE ENCODING. *)
           ("AND", "AND"); ("BALANCE", "BALANCE"); ("CAR", "CAR"); ("CDR", "CDR")
         ; ("CHECK_SIGNATURE", "CHECK_SIGNATURE"); ("COMPARE", "COMPARE")
         ; ("CONCAT", "CONCAT"); ("CONS", "CONS")
         ; ("CREATE_ACCOUNT", "CREATE_ACCOUNT")
         ; ("CREATE_CONTRACT", "CREATE_CONTRACT")
         ; (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, "FOR" BACKWARD COMPATIBILITY OF THE ENCODING. *)
           ("IMPLICIT_ACCOUNT", "IMPLICIT_ACCOUNT"); ("DIP", "DIP")
         ; ("DROP", "DROP"); ("DUP", "DUP"); ("EDIV", "EDIV")
         ; ("EMPTY_MAP", "EMPTY_MAP"); ("EMPTY_SET", "EMPTY_SET"); ("EQ", "EQ")
         ; ("EXEC", "EXEC"); ("FAILWITH", "FAILWITH")
         ; (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, "FOR" BACKWARD COMPATIBILITY OF THE ENCODING. *)
           ("GE", "GE"); ("GET", "GET"); ("GT", "GT"); ("HASH_KEY", "HASH_KEY")
         ; ("IF", "IF"); ("IF_CONS", "IF_CONS"); ("IF_LEFT", "IF_LEFT")
         ; ("IF_NONE", "IF_NONE"); ("INT", "INT"); ("LAMBDA", "LAMBDA")
         ; (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, "FOR" BACKWARD COMPATIBILITY OF THE ENCODING. *)
           ("LE", "LE"); ("LEFT", "LEFT"); ("LOOP", "LOOP"); ("LSL", "LSL")
         ; ("LSR", "LSR"); ("LT", "LT"); ("MAP", "MAP"); ("MEM", "MEM")
         ; ("MUL", "MUL"); ("NEG", "NEG")
         ; (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, "FOR" BACKWARD COMPATIBILITY OF THE ENCODING. *)
           ("NEQ", "NEQ"); ("NIL", "NIL"); ("NONE", "NONE"); ("NOT", "NOT")
         ; ("NOW", "NOW"); ("OR", "OR"); ("PAIR", "PAIR"); ("PUSH", "PUSH")
         ; ("RIGHT", "RIGHT"); ("SIZE", "SIZE")
         ; (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, "FOR" BACKWARD COMPATIBILITY OF THE ENCODING. *)
           ("SOME", "SOME"); ("SOURCE", "SOURCE"); ("SENDER", "SENDER")
         ; ("SELF", "SELF"); ("STEPS_TO_QUOTA", "STEPS_TO_QUOTA"); ("SUB", "SUB")
         ; ("SWAP", "SWAP"); ("TRANSFER_TOKENS", "TRANSFER_TOKENS")
         ; ("SET_DELEGATE", "SET_DELEGATE"); ("UNIT", "UNIT")
         ; (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, "FOR" BACKWARD COMPATIBILITY OF THE ENCODING. *)
           ("UPDATE", "UPDATE"); ("XOR", "XOR"); ("ITER", "ITER")
         ; ("LOOP_LEFT", "LOOP_LEFT"); ("ADDRESS", "ADDRESS")
         ; ("CONTRACT", "CONTRACT"); ("ISNAT", "ISNAT"); ("CAST", "CAST")
         ; ("RENAME", "RENAME"); ("bool", "bool")
         ; (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, "FOR" BACKWARD COMPATIBILITY OF THE ENCODING. *)
           ("contract", "contract"); ("int", "int"); ("key", "key")
         ; ("key_hash", "key_hash"); ("lambda", "lambda"); ("list", "list")
         ; ("map", "map"); ("big_map", "big_map"); ("nat", "nat")
         ; ("option", "option")
         ; (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, "FOR" BACKWARD COMPATIBILITY OF THE ENCODING. *)
           ("or", "or"); ("pair", "pair"); ("set", "set")
         ; ("signature", "signature"); ("string", "string"); ("bytes", "bytes")
         ; ("mutez", "mutez"); ("timestamp", "timestamp"); ("unit", "unit")
         ; ("operation", "operation")
         ; (* /!\ NEW INSTRUCTIONS MUST BE ADDED AT THE END OF THE STRING_ENUM, "FOR" BACKWARD COMPATIBILITY OF THE ENCODING. *)
           ("address", "address"); (* Alpha_002 addition *) ("SLICE", "SLICE")
         ; (* Alpha_005 addition *) ("DIG", "DIG"); ("DUG", "DUG")
         ; ("EMPTY_BIG_MAP", "EMPTY_BIG_MAP"); ("APPLY", "APPLY")
         ; ("chain_id", "chain_id"); ("CHAIN_ID", "CHAIN_ID")
           (* New instructions must be added here, "for" backward compatibility of the encoding. *)
         ]

  let expr_encoding =
    Tezos_micheline.Micheline.canonical_encoding_v1 ~variant:"michelson_v1"
      (* Data_encoding.Encoding.string *)
      prim_encoding

  let parse_bytes bytes =
    try
      let mich =
        Data_encoding.Binary.of_bytes_exn
          (* Tezos_micheline.Micheline.canonical_location_encoding *)
          expr_encoding
          (Hex.to_bytes (`Hex bytes)) in
      let json =
        Data_encoding.Json.construct expr_encoding
          (* Tezos_micheline.Micheline.canonical_location_encoding *)
          mich in
      Ok
        ( json
        , let open Tezos_micheline in
          Fmt.str "%a" Micheline_printer.print_expr
            (Micheline_printer.printable Base.Fn.id mich) )
    with
    | Data_encoding.Binary.Read_error e ->
        Error (Fmt.str "readerror: %a" Data_encoding.Binary.pp_read_error e)
    | e -> Error (Fmt.str "exn: %a" Exn.pp e)

  let example =
    let bytes = "0707002a002a" in
    let to_display =
      try
        let mich =
          Data_encoding.Binary.of_bytes_exn
            (* Tezos_micheline.Micheline.canonical_location_encoding *)
            expr_encoding
            (Hex.to_bytes (`Hex bytes)) in
        let json =
          Data_encoding.Json.construct expr_encoding
            (* Tezos_micheline.Micheline.canonical_location_encoding *)
            mich in
        Ezjsonm.value_to_string ~minify:false json
      with
      | Data_encoding.Binary.Read_error e ->
          Fmt.str "readerror: %a" Data_encoding.Binary.pp_read_error e
      | e -> Fmt.str "exn: %a" Exn.pp e in
    to_display
end

let sizing_table sizes =
  let open RD in
  (* carthage: 1 militez / byte
     http://mainnet.smartpy.io/chains/main/blocks/head/context/constants
     "cost_per_byte":"1000"
     http://delphinet.smartpy.io/chains/main/blocks/head/context/constants
     "cost_per_byte":"250"
  *)
  tablex
    ~a:[a_class ["table"; "table-bordered"; "table-hover"]]
    ~thead:
      (thead
         [ tr
             [ th []; th [txt "Size"]; th [txt "Carthage Burn"]
             ; th [txt "Delphi Burn"] ] ])
    [ tbody
        (List.map sizes ~f:(fun (label, bytes) ->
             let ppbig ppf i =
               let open Fmt in
               pf ppf "%s" (Int.to_string_hum i ~delimiter:' ') in
             tr
               [ th [txt label]; td [Fmt.kstr txt "%a B" ppbig bytes]
               ; td [Fmt.kstr txt "%a μꜩ" ppbig (bytes * 1000)]
               ; td [Fmt.kstr txt "%a μꜩ" ppbig (bytes * 250)] ])) ]

let gui ?version_string state =
  let all_examples =
    let open Tezos_contract_metadata.Metadata_contents in
    let rec go n = try (n, Example.build n) :: go (n + 1) with _ -> [] in
    go 0 in
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
            (fun () -> Var.set state.State.current_view Metadata_uri_editor)
        ; Menu.item
            (txt "Michelson PACK Parser")
            ~long_message:(txt "Start the Michelson-bytes editor.")
            ~description:
              (p
                 [ txt
                     "It parses hexadecimal encodings of byte-sequences \
                      representing Michelson expression." ])
            ~active:
              (Var.map state.State.current_view ~f:(function
                | Michelson_bytes_parser -> false
                | _ -> true))
            (fun () -> Var.set state.State.current_view Michelson_bytes_parser)
        ] in
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
    let michbytes_code =
      Var.create "michbytes-code"
        "0x050707010000000c48656c6c6f2057\n6f726c6421002a" in
    let michbytes_editor =
      Text_editor.create "michbytesditor" ~code:michbytes_code ~language:"css"
    in
    let michbytes_editor_area = Text_editor.text_area michbytes_editor in
    let editor_with_preview ?(examples = []) editor editor_area result_div =
      Text_editor.ensure editor ;
      div
        ~a:[a_class ["col-md-12"]]
        [ div
            ~a:[a_class ["col-md-6"]]
            [ Text_editor.editor_command_button editor ~text:"Undo" "undo"
            ; Text_editor.editor_command_button editor ~text:"Redo" "redo"
            ; Text_editor.editor_command_button editor ~text:"Select All"
                "selectAll"
            ; ( match examples with
              | [] -> span []
              | _more ->
                  let expanded = Var.create "examples-menu-expanded" false in
                  div
                    [ button
                        ~a:
                          [ a_class ["btn"; "btn-secondary"; "dropdown-toggle"]
                          ; a_onclick (fun _ ->
                                dbgf "examples menu" ;
                                Var.set expanded (not (Var.value expanded)) ;
                                true) ]
                        [txt "Load Examples "; span [] ~a:[a_class ["caret"]]]
                    ; Reactive.ul
                        ~a:
                          [ a_class []
                          ; Reactive.a_style
                              ( Var.map expanded ~f:(function
                                  | false -> "display: none"
                                  | true ->
                                      "position: absolute; z-index: 10; \
                                       background-color: white;padding: 10px; \
                                       list-style-type: none; border: solid \
                                       1px black;")
                              |> Var.signal ) ]
                        (Var.map_to_list expanded ~f:(function
                          | true ->
                              List.map examples ~f:(fun (k, code) ->
                                  li
                                    [ a
                                        ~a:
                                          [ a_href "#"
                                          ; a_onclick (fun _ ->
                                                Text_editor.set_code editor
                                                  ~code ;
                                                Var.set expanded false ;
                                                true) ]
                                        [txt k] ])
                          | false -> [])) ] ); div [editor_area] ]
        ; div
            ~a:
              [ a_class ["col-md-6"]
              ; a_style "border-left: solid 2px grey; height: 90%" ]
            [result_div] ] in
    let big_answer level content =
      let bgclass =
        match level with `Ok -> "bg-success" | `Error -> "bg-danger" in
      p ~a:[a_class ["lead"; bgclass]] content in
    let rec show_tezos_error = function
      | [] -> []
      | Tezos_error_monad.Error_monad.Exn
          (Ezjsonm.Parse_error (json_value, msg))
        :: more ->
          [ Fmt.kstr txt "JSON Parsing Error: %s, JSON:" msg
          ; pre [code [txt (Ezjsonm.value_to_string ~minify:false json_value)]]
          ]
          @ show_tezos_error more
      | Tezos_error_monad.Error_monad.Exn (Failure text) :: more ->
          [Fmt.kstr txt "Error: %a" Fmt.text text] @ show_tezos_error more
      | Tezos_error_monad.Error_monad.Exn other_exn :: more ->
          [ Fmt.kstr txt "Error: %a"
              (Json_encoding.print_error ~print_unknown:Exn.pp)
              other_exn ]
          @ show_tezos_error more
      | other ->
          [ pre
              [ code
                  [ Fmt.kstr txt "%a"
                      Tezos_error_monad.Error_monad.pp_print_error other ] ] ]
    in
    div
      ~a:[a_class ["container-fluid"]]
      [ div ~a:[a_class ["col-md-12"]] [menu `Top]; hr ()
      ; Reactive.div
          (Var.map_to_list state.State.current_view ~f:(function
            | Welcome ->
                dbgf "Showing welc-home" ;
                [ div
                    [ h3 [txt "Welcome"]
                    ; p
                        [ txt "This is "
                        ; a
                            ~a:[a_href "https://github.com/smondet/comevitz"]
                            [txt "Comevitz "]
                        ; ( match version_string with
                          | None -> i [txt "unknown version"]
                          | Some vs ->
                              span
                                [ txt "version "
                                ; a
                                    ~a:
                                      [ Fmt.kstr a_href
                                          "https://github.com/smondet/comevitz/commit/%s"
                                          vs ]
                                    [i [txt vs]] ] ); txt "." ]
                    ; h3 [Fmt.kstr txt "What would you like to do?"]
                    ; menu_welcome; h3 [txt "Further Reading"]
                    ; p
                        [ txt
                            "The source for this webpage is available on \
                             Github: "
                        ; a
                            ~a:[a_href "https://github.com/smondet/comevitz"]
                            [code [txt "smondet/comevitz"]]; txt "." ]
                    ; p
                        [ txt
                            "The Contract Metadata standard, a.k.a. TZIP-16, \
                             is currently being drafted in the merge-request: "
                        ; a
                            ~a:
                              [ a_href
                                  "https://gitlab.com/tzip/tzip/-/merge_requests/76"
                              ]
                            [txt "tzip/tzip!76"]; txt "." ] ] ]
            | Metadata_uri_editor ->
                let examples =
                  let ex name u = (name, Uri.to_string u) in
                  [ ex "Simple in storage"
                      (Uri.make ~scheme:"tezos-storage" ~path:"foo" ())
                  ; ex "HTTPS"
                      (Uri.of_string
                         "https://example.com/path/to/metadata.json")
                  ; ex "IPFS"
                      (Uri.of_string
                         "ipfs://QmXfrS3pHerg44zzK6QKQj6JDk8H6cMtQS7pdXbohwNQfK/pages/hello.json")
                  ; ex "SHA256-checked HTTPS"
                      (Uri.of_string
                         "sha256://0xeaa42ea06b95d7917d22135a630e65352cfd0a721ae88155a1512468a95cb750/https:%2F%2Fexample.com%2Fmetadata.json")
                  ] in
                let result_div =
                  Reactive.div_of_var metadata_uri_code ~f:(fun uri_code ->
                      let open Tezos_contract_metadata.Metadata_uri in
                      match Uri.of_string uri_code |> of_uri with
                      | Ok o ->
                          [ big_answer `Ok
                              [txt "This metadata URI is VALID 👍"]
                          ; div (Contract_metadata.Uri.to_html o)
                          ; div [sizing_table [("URI", String.length uri_code)]]
                          ]
                      | Error el ->
                          [ big_answer `Error
                              [txt "There were parsing/validation errors:"]
                          ; div (show_tezos_error el) ]) in
                [ editor_with_preview metadata_uri_editor ~examples
                    metadata_uri_editor_area result_div ]
            | Metadata_json_editor ->
                let examples =
                  List.map all_examples ~f:(fun (ith, v) ->
                      ( Fmt.str "Meaningless Example #%d" ith
                      , Tezos_contract_metadata.Metadata_contents.to_json v ))
                in
                let result_div =
                  Reactive.div_of_var metadata_json_code ~f:(fun json_code ->
                      let open Tezos_contract_metadata.Metadata_contents in
                      match of_json json_code with
                      | Ok
                          { name= None
                          ; description= None
                          ; version= None
                          ; license= None
                          ; authors= []
                          ; interfaces= []
                          ; views= []
                          ; unknown= [] } ->
                          [ big_answer `Ok
                              [ txt
                                  "This piece of metadata, while valid, is \
                                   completely empty!" ] ]
                      | Ok ex ->
                          [ big_answer `Ok
                              [txt "This metadata blob is VALID 👍"]
                          ; div [Contract_metadata.Content.to_html ex]
                          ; div
                              [ sizing_table
                                  [ ("Current JSON", String.length json_code)
                                  ; ( "Minimized JSON"
                                    , Ezjsonm.value_from_string json_code
                                      |> Ezjsonm.value_to_string ~minify:true
                                      |> String.length ) ] ] ]
                      | Error el ->
                          [ big_answer `Error
                              [txt "There were parsing/validation errors:"]
                          ; div (show_tezos_error el) ]) in
                [ editor_with_preview metadata_json_editor ~examples
                    metadata_json_editor_area result_div ]
            | Michelson_bytes_parser ->
                let examples =
                  [ ("The Unit value", "0x05030b")
                  ; ( "With a (map string string)"
                    , "050707010000000c486\n\
                       56c6c6f20576f726c64\n\
                       2102000000260704010\n\
                       0000003666f6f010000\n\
                       0003626172070401000\n\
                       0000474686973010000\n\
                       000474686174" ) ] in
                let result_div =
                  Reactive.div_of_var michbytes_code ~f:(fun bytes_code ->
                      let with_zero_x, bytes =
                        let prefix = "0x" in
                        if String.is_prefix bytes_code ~prefix then
                          (true, String.chop_prefix_exn bytes_code ~prefix)
                        else (false, bytes_code) in
                      let with_zero_five, bytes =
                        let prefix = "05" in
                        if String.is_prefix bytes ~prefix then
                          (true, String.chop_prefix_exn bytes ~prefix)
                        else (false, bytes) in
                      let bytes =
                        String.filter bytes ~f:(function
                          | ' ' | '\n' | '\t' -> false
                          | _ -> true) in
                      let items =
                        ( if with_zero_x then
                          [ li
                              [ code [txt "0x"]
                              ; txt " just means “this is hexadecimal”." ]
                          ]
                        else [] )
                        @
                        if with_zero_five then
                          [ li
                              [ code [txt "05"]
                              ; txt
                                  " is the standard prefix/watermark Michelson \
                                   expressions." ] ]
                        else [] in
                      match Michelson_bytes.parse_bytes bytes with
                      | Ok (json, concrete) ->
                          [ big_answer `Ok
                              [ txt
                                  "This hexa-blob was successfully parsed 🏆"
                              ]; ul items; h4 [txt "As Concrete Michelson:"]
                          ; div [pre [code [txt concrete]]]; h4 [txt "As JSON:"]
                          ; div
                              [ pre
                                  [ code
                                      [ txt
                                          (Ezjsonm.value_to_string ~minify:false
                                             json) ] ] ] ]
                      | Error s ->
                          [ big_answer `Error
                              [txt "There were parsing/validation errors:"]
                          ; pre [code [txt s]] ]) in
                [ editor_with_preview michbytes_editor ~examples
                    michbytes_editor_area result_div ])) ])

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
        (fun () ->
          Js_of_ocaml_lwt.XmlHttpRequest.(
            get "./VERSION"
            >>= fun frame ->
            dbgf "version: %d" frame.code ;
            if frame.code = 200 then return (Some frame.content)
            else return None)
          >>= fun version_string ->
          attach_to_page (gui ?version_string state) >>= fun () -> return ())
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
