open Base

let dbg fmt = Fmt.pf Fmt.stdout "@[comevitz-debug: %a@]%!" fmt ()
let dbgf fmt = Fmt.(kstr (fun s -> dbg (const string s))) fmt

let rec oxfordize_list l ~map ~sep ~last_sep =
  match l with
  | [] -> []
  | [one] -> [map one]
  | [one; two] -> [map one; last_sep (); map two]
  | one :: more -> map one :: sep () :: oxfordize_list more ~map ~sep ~last_sep

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
      | Metadata_explorer
  end

  type t = {dev_mode: bool; current_view: View.t Var.t}

  let init ?(dev_mode = false) () =
    {current_view= Var.create "current-view" View.Welcome; dev_mode}
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
    { id: string
    ; language: string
    ; code: string Var.t
    ; status: status Var.t
    ; mutable text_area: Html_types.div RD.elt option }

  let create ?(language = "mllike") id ~code =
    { id
    ; language
    ; code
    ; text_area= None
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
    match te.text_area with
    | Some s -> s
    | None ->
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
                                      dbgf "TA inputs: %d bytes"
                                        (String.length v) ;
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
                         ~a:
                           [ (* a_href "#"; *)
                             a_onclick (fun _ -> action () ; true) ]; desc ]
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

  let encode_michelson_string s =
    Data_encoding.Binary.to_bytes_exn expr_encoding
      Tezos_micheline.Micheline.(String (0, s) |> strip_locations)
    |> Bytes.to_string

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

module B58_hashes = struct
  module B58_crypto = struct
    let sha256 s = Digestif.SHA256.(to_raw_string (digest_string s))
  end

  let script_expr_hash =
    (* Taken from src/proto_006_PsCARTHA/lib_protocol/script_expr_hash.ml *)
    (* expr(54) *)
    "\013\044\064\027"

  let blake2b x =
    Digestif.digest_string (Digestif.blake2b 32) x
    |> Digestif.to_raw_string (Digestif.blake2b 32)

  let b58 s = Base58.of_bytes (module B58_crypto) s |> Base58.to_string
  let b58_script_id_hash s = b58 (script_expr_hash ^ blake2b s)

  let crypto_test () =
    dbgf "TRYING BLAKE2B: %s"
      (let dgst = Digestif.digest_string (Digestif.blake2b 32) "" in
       Digestif.to_hex (Digestif.blake2b 32) dgst) ;
    dbgf "TRYING base58: %a %S"
      Fmt.(Dump.option Base58.pp)
      (Base58.of_string
         (module B58_crypto)
         "expru5X1yxJG6ezR2uHMotwMLNmSzQyh5t1vUnhjx4cS6Pv9qE1Sdo")
      ( Base58.of_string_exn
          (module B58_crypto)
          "expru5X1yxJG6ezR2uHMotwMLNmSzQyh5t1vUnhjx4cS6Pv9qE1Sdo"
      |> Base58.to_bytes (module B58_crypto)
      |> Option.value_map ~default:"EEEERRRROR" ~f:(fun x ->
             let (`Hex h) = Hex.of_string x in
             h) ) ;
    let michelson_string_expr_hash s =
      dbgf "mseh: %S" s ;
      let bytes = Michelson_bytes.encode_michelson_string s in
      let ppb ppf b =
        let (`Hex hx) = Hex.of_string b in
        Fmt.pf ppf "0x%s" hx in
      dbgf "bytes: %a" ppb bytes ;
      let dgst x =
        Digestif.digest_string (Digestif.blake2b 32) x
        |> Digestif.to_raw_string (Digestif.blake2b 32) in
      let b58 s = Base58.of_bytes (module B58_crypto) s |> Base58.to_string in
      dbgf "digest raw: %a -> %s (%s)" ppb (dgst bytes)
        (b58 (dgst bytes))
        (Base58.raw_encode (dgst bytes)) ;
      let with05 = "\x05" ^ bytes in
      dbgf "digest-05: %a → %s [%s]" ppb (dgst with05)
        (b58 (dgst with05))
        (Base58.raw_encode (dgst with05)) ;
      dbgf "digest-pfx: %a → %s" ppb (dgst with05)
        (b58 (script_expr_hash ^ dgst with05)) in
    michelson_string_expr_hash "" ;
    michelson_string_expr_hash "foo" ;
    ()
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

let editor_with_preview ?(examples = []) editor result_div =
  let open RD in
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
                                   list-style-type: none; border: solid 1px \
                                   black;")
                          |> Var.signal ) ]
                    (Var.map_to_list expanded ~f:(function
                      | true ->
                          List.map examples ~f:(fun (k, code) ->
                              li
                                [ a
                                    ~a:
                                      [ (* a_href "#"
                                           ; *)
                                        a_onclick (fun _ ->
                                            Text_editor.set_code editor ~code ;
                                            Var.set expanded false ;
                                            true) ]
                                    [txt k] ])
                      | false -> [])) ] ); div [Text_editor.text_area editor] ]
    ; div
        ~a:
          [ a_class ["col-md-6"]
          ; a_style "border-left: solid 2px grey; height: 90%" ]
        [result_div] ]

let welcome_page ?version_string state ~menu_welcome =
  let open RD in
  div
    [ h3 [txt "Welcome"]
    ; p
        [ txt "This is "
        ; a ~a:[a_href "https://github.com/smondet/comevitz"] [txt "Comevitz "]
        ; ( match version_string with
          | None -> i [txt "unknown version"]
          | Some vs ->
              span
                [ txt "version "
                ; a
                    ~a:
                      [ Fmt.kstr a_href
                          "https://github.com/smondet/comevitz/commit/%s" vs ]
                    [i [txt vs]] ] )
        ; Fmt.kstr txt "%s."
            (if state.State.dev_mode then " (in “dev” mode)" else "") ]
    ; h3 [Fmt.kstr txt "What would you like to do?"]; menu_welcome
    ; h3 [txt "Further Reading"]
    ; p
        [ txt "The source for this webpage is available on Github: "
        ; a
            ~a:[a_href "https://github.com/smondet/comevitz"]
            [code [txt "smondet/comevitz"]]; txt "." ]
    ; p
        [ txt
            "The Contract Metadata standard, a.k.a. TZIP-16, is currently \
             being drafted in the merge-request: "
        ; a
            ~a:[a_href "https://gitlab.com/tzip/tzip/-/merge_requests/76"]
            [txt "tzip/tzip!76"]; txt "." ] ]

let rec show_tezos_error =
  let open RD in
  function
  | [] -> []
  | Tezos_error_monad.Error_monad.Exn (Ezjsonm.Parse_error (json_value, msg))
    :: more ->
      [ Fmt.kstr txt "JSON Parsing Error: %s, JSON:" msg
      ; pre [code [txt (Ezjsonm.value_to_string ~minify:false json_value)]] ]
      @ show_tezos_error more
  | Tezos_error_monad.Error_monad.Exn (Failure text) :: more ->
      [Fmt.kstr txt "Error: %a" Fmt.text text] @ show_tezos_error more
  | Tezos_error_monad.Error_monad.Exn other_exn :: more ->
      [ Fmt.kstr txt "Error: %a"
          (Json_encoding.print_error ~print_unknown:Exn.pp)
          other_exn ]
      @ show_tezos_error more
  | Tezos_contract_metadata.Metadata_uri.Contract_metadata_uri_parsing
      parsing_error
    :: more ->
      let open Tezos_contract_metadata.Metadata_uri.Parsing_error in
      let details =
        let sha256_host_advice =
          span
            [ txt "The host should look like: "
            ; code
                [ txt
                    "0x5891b5b522d5df086d0ff0b110fbd9d21bb4fc7163af34d08286a2e846f6be03"
                ]; txt "." ] in
        let scheme_advice =
          span
            [ txt "The URI should start with one of: "
            ; span
                (oxfordize_list
                   ["tezos-storage"; "http"; "https"; "sha256"; "ipfs"]
                   ~map:(fun sch -> code [Fmt.kstr txt "%s:" sch])
                   ~sep:(fun () -> txt ",")
                   ~last_sep:(fun () -> txt ", or ")); txt "." ] in
        match parsing_error.error_kind with
        | Wrong_scheme None -> [txt "Missing URI scheme. "; scheme_advice]
        | Wrong_scheme (Some scheme) ->
            [ txt "Unknown URI scheme: "; code [txt scheme]; txt ". "
            ; scheme_advice ]
        | Missing_cid_for_ipfs ->
            [ txt
                "Missing content identifier in IPFS URI, it should be the host."
            ]
        | Wrong_tezos_storage_host str ->
            [ txt "Cannot parse the “host” part of the URI: "; code [txt str]
            ; txt ", should look like "; code [txt "<network>.<address>"]
            ; txt " or just "; code [txt "<address>"] ]
        | Forbidden_slash_in_tezos_storage_path path ->
            [ txt "For "; code [txt "tezos-storage"]
            ; txt " URIs, the “path” cannot contain any "; code [txt "/"]
            ; txt " (“slash”) character: "; code [txt path] ]
        | Missing_host_for_hash_uri `Sha256 ->
            [ txt "Missing “host” in "; code [txt "sha256://"]; txt " URI. "
            ; sha256_host_advice ]
        | Wrong_hex_format_for_hash {hash= `Sha256; host; message} ->
            [ txt "Failed to parse the “host” "; code [txt host]
            ; txt " in this "; code [txt "sha256://"]; txt " URI: "; txt message
            ; txt " → "; sha256_host_advice ] in
      let exploded_uri =
        let u = Uri.of_string parsing_error.input in
        let item name opt =
          [ li
              [ em [txt name; txt ": "]
              ; (match opt with None -> txt "<empty>" | Some s -> code [txt s])
              ] ] in
        let item_some name s = item name (Some s) in
        [ txt "The URI is understood this way: "
        ; ul
            ( item "Scheme" (Uri.scheme u)
            @ item "Host" (Uri.host u)
            @ item "User-info" (Uri.userinfo u)
            @ item "Port" (Uri.port u |> Option.map ~f:Int.to_string)
            @ item_some "Path" (Uri.path u)
            @ item "Query" (Uri.verbatim_query u)
            @ item "Fragment" (Uri.fragment u) ) ] in
      [ txt "Failed to parse URI: "; code [txt parsing_error.input]; txt ":"
      ; br () ]
      @ details @ [br ()] @ exploded_uri @ show_tezos_error more
  | other ->
      [ pre
          [ code
              [ Fmt.kstr txt "%a" Tezos_error_monad.Error_monad.pp_print_error
                  other ] ] ]

let big_answer level content =
  let open RD in
  let bgclass = match level with `Ok -> "bg-success" | `Error -> "bg-danger" in
  p ~a:[a_class ["lead"; bgclass]] content

let metadata_uri_editor_page _state ~metadata_uri_editor ~metadata_uri_code =
  let open RD in
  let examples =
    let ex name u = (name, Uri.to_string u) in
    [ ex "Simple in storage" (Uri.make ~scheme:"tezos-storage" ~path:"foo" ())
    ; ex "HTTPS" (Uri.of_string "https://example.com/path/to/metadata.json")
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
            [ big_answer `Ok [txt "This metadata URI is VALID 👍"]
            ; div (Contract_metadata.Uri.to_html o)
            ; div [sizing_table [("URI", String.length uri_code)]] ]
        | Error el ->
            [ big_answer `Error [txt "There were parsing/validation errors:"]
            ; div (show_tezos_error el) ]) in
  [editor_with_preview metadata_uri_editor ~examples result_div]

let metadata_json_editor_page _state ~metadata_json_editor ~metadata_json_code =
  let open RD in
  let examples =
    let all_examples =
      let open Tezos_contract_metadata.Metadata_contents in
      let rec go n = try (n, Example.build n) :: go (n + 1) with _ -> [] in
      go 0 in
    List.map all_examples ~f:(fun (ith, v) ->
        ( Fmt.str "Meaningless Example #%d" ith
        , Tezos_contract_metadata.Metadata_contents.to_json v )) in
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
                [txt "This piece of metadata, while valid, is completely empty!"]
            ]
        | Ok ex ->
            [ big_answer `Ok [txt "This metadata blob is VALID 👍"]
            ; div [Contract_metadata.Content.to_html ex]
            ; div
                [ sizing_table
                    [ ("Current JSON", String.length json_code)
                    ; ( "Minimized JSON"
                      , Ezjsonm.value_from_string json_code
                        |> Ezjsonm.value_to_string ~minify:true
                        |> String.length ) ] ] ]
        | Error el ->
            [ big_answer `Error [txt "There were parsing/validation errors:"]
            ; div (show_tezos_error el) ]) in
  [editor_with_preview metadata_json_editor ~examples result_div]

let michelson_bytes_editor_page _state ~michelson_bytes_editor
    ~michelson_bytes_code =
  let open RD in
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
    Reactive.div_of_var michelson_bytes_code ~f:(fun bytes_code ->
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
            [li [code [txt "0x"]; txt " just means “this is hexadecimal”."]]
          else [] )
          @
          if with_zero_five then
            [ li
                [ code [txt "05"]
                ; txt
                    " is the standard prefix/watermark for Michelson \
                     expressions." ] ]
          else [] in
        match Michelson_bytes.parse_bytes bytes with
        | Ok (json, concrete) ->
            [ big_answer `Ok [txt "This hexa-blob was successfully parsed 🏆"]
            ; ul items; h4 [txt "As Concrete Michelson:"]
            ; div [pre [code [txt concrete]]]; h4 [txt "As JSON:"]
            ; div [pre [code [txt (Ezjsonm.value_to_string ~minify:false json)]]]
            ; h4 [txt "Useful Hashes"]
            ; div
                [ ul
                    (let hash title v =
                       li [em [Fmt.kstr txt "%s: " title]; code [txt v]] in
                     let actual = Hex.to_string (`Hex bytes) in
                     let actual05 = "\x05" ^ actual in
                     [ hash "Ledger-hash"
                         (Base58.raw_encode (B58_hashes.blake2b actual05))
                     ; hash "Script-ID-hash (big-map access)"
                         (B58_hashes.b58_script_id_hash actual05) ]) ] ]
        | Error s ->
            [ big_answer `Error [txt "There were parsing/validation errors:"]
            ; pre [code [txt s]] ]) in
  [editor_with_preview michelson_bytes_editor ~examples result_div]

module Tezos_nodes = struct
  module Node_status = struct
    type t = Uninitialized | Non_responsive of string | Ready of string
  end

  open Node_status

  module Node = struct
    type t = {prefix: string; status: (float * Node_status.t) Var.t}

    let create prefix =
      {prefix; status= Var.create "node-status" (0., Uninitialized)}

    let ping node =
      let open Lwt in
      Js_of_ocaml_lwt.XmlHttpRequest.(
        Fmt.kstr get "%s/chains/main/blocks/head/metadata" node.prefix
        >>= fun frame ->
        dbgf "%s metadata code: %d" node.prefix frame.code ;
        let new_status =
          match frame.code with
          | 200 -> Ready frame.content
          | other -> Non_responsive (Fmt.str "Return-code: %d" other) in
        return new_status)
  end

  type t =
    { nodes: Node.t list Var.t
    ; loop_started: bool Var.t
    ; loop_interval: float Var.t }

  let create nodes =
    { nodes= Var.create "list-of-nodes" nodes
    ; loop_started= Var.create "loop-started" false
    ; loop_interval= Var.create "loop-interval" 10. }

  let nodes t = t.nodes

  let _global =
    create
      [ Node.create "https://testnet-tezos.giganode.io"
      ; Node.create "https://carthagenet.smartpy.io" ]

  let start_update_loop t =
    let open Lwt in
    ignore_result
      (let rec loop count =
         let sleep_time = Var.value t.loop_interval in
         dbgf "update-loop %d (%f s)" count sleep_time ;
         Var.value t.nodes
         |> List.fold ~init:return_unit ~f:(fun prevm nod ->
                prevm
                >>= fun () ->
                pick
                  [ ( Js_of_ocaml_lwt.Lwt_js.sleep 5.
                    >>= fun () ->
                    return (Non_responsive "Time-out while getting status") )
                  ; (Node.ping nod >>= fun res -> return res) ]
                >>= fun new_status ->
                let now = (new%js Js_of_ocaml.Js.date_now)##valueOf in
                Var.set nod.status (now, new_status) ;
                return ())
         >>= fun () ->
         Js_of_ocaml_lwt.Lwt_js.sleep sleep_time
         >>= fun () ->
         Var.set t.loop_interval (Float.min (sleep_time *. 1.4) 90.) ;
         loop (count + 1) in
       loop 0)

  let ensure_update_loop t =
    match Var.value t.loop_started with
    | true -> ()
    | false ->
        start_update_loop t ;
        Var.set t.loop_started true
end

let metadata_explorer _state =
  let open RD in
  let nodes = Tezos_nodes._global in
  Tezos_nodes.ensure_update_loop nodes ;
  let node_status node =
    let node_metadata json =
      let open Ezjsonm in
      try
        let j = value_from_string json in
        let field f j =
          try List.Assoc.find_exn ~equal:String.equal (get_dict j) f
          with _ ->
            Fmt.failwith "Cannot find %S in %s" f
              (value_to_string ~minify:true j) in
        div
          [ Fmt.kstr txt "Level: %d"
              (field "level" j |> field "level" |> get_int) ]
      with e ->
        div
          [ Fmt.kstr txt "Failed to parse the JSON: %a" Exn.pp e
          ; pre [code [txt json]] ] in
    Reactive.div
      (Var.map_to_list node.Tezos_nodes.Node.status
         ~f:
           Tezos_nodes.Node_status.(
             fun (date, status) ->
               let show s = [code [Fmt.kstr txt "%.2f: " date]; s] in
               match status with
               | Uninitialized -> show (txt "Uninitialized")
               | Non_responsive reason ->
                   show (Fmt.kstr txt "Non-responsive: %s" reason)
               | Ready metadata ->
                   show (div [txt "All OK"; node_metadata metadata]))) in
  let address =
    Var.create "contract-address" "KT1XRT495WncnqNmqKn4tkuRiDJzEiR4N2C9" in
  let result = Var.create "contract-exploration" `Not_started in
  let get_metadata () =
    let open Lwt in
    let node = List.hd_exn (Tezos_nodes.nodes nodes |> Var.value) in
    let addr = Var.value address in
    let log_stack = ref [] in
    let log fmt =
      Fmt.kstr
        (fun s ->
          log_stack := s :: !log_stack ;
          Var.set result
            (`Fetching (String.concat ~sep:"\n" (List.rev !log_stack))))
        fmt in
    let micheline_of_json s =
      let json =
        match Ezjsonm.value_from_string s with
        | `O (("code", code) :: _) -> code
        | other -> other in
      let enc =
        Tezos_micheline.Micheline.canonical_encoding ~variant:"custom"
          Data_encoding.string in
      let mich = Data_encoding.Json.destruct enc json in
      Tezos_micheline.Micheline.root mich in
    let get path =
      let uri = Fmt.str "%s/%s" node.prefix path in
      Js_of_ocaml_lwt.XmlHttpRequest.(
        get uri
        >>= fun frame ->
        dbgf "%s %s code: %d" node.prefix path frame.code ;
        match frame.code with
        | 200 -> return frame.content
        | other -> Fmt.failwith "Getting %S returned code: %d" path other) in
    let slow_step () = Js_of_ocaml_lwt.Lwt_js.sleep 0.4 in
    catch
      (fun () ->
        Fmt.kstr get "/chains/main/blocks/head/context/contracts/%s/storage"
          addr
        >>= fun storage_string ->
        log "Got raw storage: %s" storage_string ;
        let mich_storage = micheline_of_json storage_string in
        log "As concrete: %a"
          Tezos_contract_metadata.Contract_storage.pp_arbitrary_micheline
          mich_storage ;
        slow_step ()
        >>= fun () ->
        Fmt.kstr get "/chains/main/blocks/head/context/contracts/%s/script" addr
        >>= fun script_string ->
        log "Got raw script: %s…" (String.prefix script_string 30) ;
        let mich_storage_type =
          micheline_of_json script_string
          |> Tezos_micheline.Micheline.strip_locations
          |> Tezos_contract_metadata.Contract_storage.get_storage_type_exn in
        log "Storage type: %a"
          Tezos_contract_metadata.Contract_storage.pp_arbitrary_micheline
          mich_storage_type ;
        slow_step ()
        >>= fun () ->
        let bgs =
          Tezos_contract_metadata.Contract_storage.find_metadata_big_maps
            ~storage_node:mich_storage ~type_node:mich_storage_type in
        match bgs with
        | [] -> Fmt.failwith "Contract has no valid %%metadata big-map!"
        | _ :: _ :: _ ->
            Fmt.failwith "Contract has too many %%metadata big-maps: %s"
              ( oxfordize_list bgs ~map:Z.to_string
                  ~sep:(fun () -> ",")
                  ~last_sep:(fun () -> ", and ")
              |> String.concat ~sep:"" )
        | [one] -> (
            log "Metadata big-map: %s" (Z.to_string one) ;
            let empty_string =
              "expru5X1yxJG6ezR2uHMotwMLNmSzQyh5t1vUnhjx4cS6Pv9qE1Sdo" in
            Fmt.kstr get "/chains/main/blocks/head/context/big_maps/%s/%s"
              (Z.to_string one) empty_string
            >>= fun uri_raw_value ->
            log "URI raw value: %s" uri_raw_value ;
            let uri =
              match Ezjsonm.value_from_string uri_raw_value with
              | `O [("bytes", `String b)] -> Hex.to_string (`Hex b)
              | _ -> Fmt.failwith "Cannot find URI bytes in %s" uri_raw_value
            in
            log "URI: `%s`" uri ;
            match
              Tezos_contract_metadata.Metadata_uri.of_uri (Uri.of_string uri)
            with
            | Ok mu ->
                log "Parsed uri: %a" Tezos_contract_metadata.Metadata_uri.pp mu ;
                slow_step ()
                >>= fun () ->
                Var.set result (`Done_uri (List.rev !log_stack, uri, mu)) ;
                return ()
            | Error e ->
                Fmt.failwith "Error parsing URI: %a"
                  Tezos_error_monad.Error_monad.pp_print_error e ))
      (function
        | Json_encoding.Cannot_destruct (path, e) ->
            Var.set result
              (`Failed
                ( List.rev !log_stack
                , Fmt.str "JSON-parsing: At %a: %a"
                    (Json_query.print_path_as_json_path ?wildcards:None)
                    path Exn.pp e )) ;
            return ()
        | Failure s ->
            Var.set result (`Failed (List.rev !log_stack, s)) ;
            return ()
        | e ->
            Var.set result
              (`Failed (List.rev !log_stack, Fmt.str "Exception: %a" Exn.pp e)) ;
            return ()) in
  [ div [txt "WIP"]
  ; div
      [ Reactive.ul
          (Var.map_to_list (Tezos_nodes.nodes nodes) ~f:(fun nodes ->
               List.map nodes ~f:(fun s ->
                   li
                     [ txt "Node: "; code [txt s.prefix]; txt " → "
                     ; node_status s ]))) ]
  ; div
      [ textarea
          ~a:
            [ a_style "font-family: monospace"; a_rows 1; a_cols 40
            ; a_onchange
                Js_of_ocaml.(
                  fun ev ->
                    Js.Opt.iter ev##.target (fun input ->
                        Js.Opt.iter (Dom_html.CoerceTo.textarea input)
                          (fun input ->
                            let v = input##.value |> Js.to_string in
                            dbgf "TA inputs: %d bytes: %S" (String.length v) v ;
                            Var.set address v)) ;
                    false) ]
          (txt "KT1XRT495WncnqNmqKn4tkuRiDJzEiR4N2C9")
      ; button
          ~a:
            [ a_onclick (fun _ ->
                  Var.set result (`Fetching "Start fetching data …") ;
                  Lwt.async get_metadata ;
                  true)
            ; Reactive.a_class
                ( Var.signal result
                |> React.S.map (function
                     | `Not_started | `Failed _ | `Done_uri _ ->
                         ["btn"; "btn-primary"]
                     | _ -> ["btn"; "btn-default"; "disabled"]) ) ]
          [ Reactive.span ~a:[a_style "width: 4em"]
              (Var.map_to_list result ~f:(function
                | `Not_started | `Failed _ | `Done_uri _ -> [txt "Go!"]
                | _ ->
                    [ img ~src:"loading.gif" ~a:[a_width 30] ~alt:"LOAADDDINGGG"
                        () ])) ]
      ; Reactive.div
          (Var.map_to_list result ~f:(function
            | `Not_started -> []
            | `Done_uri (log, uri_code, muri) ->
                [ big_answer `Ok [txt "This metadata URI is VALID 👍"]
                ; div [txt "→ "; code [txt uri_code]]
                ; div (Contract_metadata.Uri.to_html muri)
                ; div [sizing_table [("URI", String.length uri_code)]]
                ; details
                    (summary [txt "See logs …"])
                    [ pre
                        ~a:[a_style "color: #999; font-size: 140%"]
                        [span [txt (String.concat ~sep:"\n" log)]] ] ]
            | `Fetching msg ->
                [ pre
                    ~a:[a_style "color: #999; font-size: 140%"]
                    [span [txt msg]] ]
            | `Failed (log, msg) ->
                [ pre
                    ~a:[a_style "color: #999; font-size: 140%"]
                    [ txt (String.concat ~sep:"\n" log)
                    ; span ~a:[a_style "color: #900"] [txt ("\n" ^ msg)] ] ]))
      ] ]

let gui ?version_string state =
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
        ]
        @
        if state.dev_mode then
          [ Menu.item (txt "Metadata Explorer")
              ~long_message:(txt "Explore metadata in existing contracts.")
              ~description:(p [txt "This is WIP."])
              ~active:
                (Var.map state.State.current_view ~f:(function
                  | Metadata_explorer -> false
                  | _ -> true))
              (fun () -> Var.set state.State.current_view Metadata_explorer) ]
        else [] in
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
    let metadata_uri_code = Var.create "metadata-uri-code" "tezos-storage:foo" in
    let metadata_uri_editor =
      Text_editor.create "metadataurieditor" ~code:metadata_uri_code
        ~language:"css" in
    let michelson_bytes_code =
      Var.create "michbytes-code"
        "0x050707010000000c48656c6c6f2057\n6f726c6421002a" in
    let michelson_bytes_editor =
      Text_editor.create "michbytesditor" ~code:michelson_bytes_code
        ~language:"css" in
    div
      ~a:[a_class ["container-fluid"]]
      [ div ~a:[a_class ["col-md-12"]] [menu `Top]; hr ()
      ; Reactive.div
          (Var.map_to_list state.State.current_view ~f:(function
            | Welcome ->
                dbgf "Showing welc-home" ;
                [welcome_page ?version_string state ~menu_welcome]
            | Metadata_uri_editor ->
                metadata_uri_editor_page state ~metadata_uri_editor
                  ~metadata_uri_code
            | Metadata_json_editor ->
                metadata_json_editor_page state ~metadata_json_editor
                  ~metadata_json_code
            | Michelson_bytes_parser ->
                michelson_bytes_editor_page state ~michelson_bytes_editor
                  ~michelson_bytes_code
            | Metadata_explorer -> metadata_explorer state)) ])

let attach_to_page gui =
  let open Js_of_ocaml in
  let base_div = Dom_html.getElementById "attach-ui" in
  base_div##.innerHTML := Js.string "" ;
  base_div##appendChild (Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_node gui)
  |> ignore ;
  Lwt.return ()

let get_fragment () =
  let open Js_of_ocaml in
  let frag = Dom_html.window##.location##.hash |> Js.to_string in
  dbgf "fragment  → %s" frag ;
  String.chop_prefix_if_exists frag ~prefix:"#"

let go _ =
  dbg Fmt.(const string "Hello Go!") ;
  Bootstrap_css.ensure () ;
  ignore
    Lwt.(
      let frag = get_fragment () in
      let state = State.init ~dev_mode:(String.equal frag "dev") () in
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
  B58_hashes.crypto_test () ;
  Js_of_ocaml.Js._true

let _ =
  dbgf "Hello Main!" ;
  let open Js_of_ocaml in
  (Lwt.async_exception_hook := fun e -> dbgf "Async Exn: %s" (Exn.to_string e)) ;
  Dom_html.window##.onload := Dom_html.handler go
