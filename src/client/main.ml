open Import

module State = struct
  module View = struct
    type t =
      | Welcome
      | Metadata_json_editor
      | Metadata_uri_editor
      | Michelson_bytes_parser
      | Metadata_explorer

    let to_string = function
      | Welcome -> "Welcome"
      | Metadata_json_editor -> "Metadata_json_editor"
      | Metadata_uri_editor -> "Metadata_uri_editor"
      | Michelson_bytes_parser -> "Michelson_bytes_parser"
      | Metadata_explorer -> "Metadata_explorer"

    let of_string = function
      | "Welcome" -> Welcome
      | "Metadata_json_editor" -> Metadata_json_editor
      | "Metadata_uri_editor" -> Metadata_uri_editor
      | "Michelson_bytes_parser" -> Michelson_bytes_parser
      | "Metadata_explorer" -> Metadata_explorer
      | other -> Fmt.failwith "View.of_string: %S" other
  end

  type t =
    { dev_mode: bool
    ; current_view: View.t Var.t
    ; explorer_address_input: string Var.t
    ; start_fetching_address: bool Var.t
    ; explorer_uri_input: string Var.t
    ; start_fetching_uri: bool Var.t }

  let kt1_with_metadata = "KT1XRT495WncnqNmqKn4tkuRiDJzEiR4N2C9"

  let init ~arguments () =
    let arg s = List.Assoc.find arguments ~equal:String.equal s in
    let is_true s =
      let ( = ) = Option.equal String.equal in
      arg s = Some "true" in
    let dev_mode = is_true "dev" in
    let fetch_uri = is_true "fetch_uri" in
    let fetch_address = is_true "fetch_address" in
    let initial_view =
      match arg "tab" with
      | Some s -> (
        try View.of_string s
        with e ->
          dbgf "Wrong view name: %a" Exn.pp e ;
          View.Welcome )
      | None -> View.Welcome in
    let initial_explorer_address =
      arg "explorer_address" |> Option.value ~default:kt1_with_metadata in
    let initial_explorer_uri =
      arg "explorer_uri"
      |> Option.value ~default:"https://example.com/my_contract/metadata.json"
    in
    let explorer_address_input =
      Var.create "explorer_address_input" initial_explorer_address in
    let explorer_uri_input =
      Var.create "explorer_uri_input" initial_explorer_uri in
    { current_view= Var.create "current-view" initial_view
    ; dev_mode
    ; explorer_address_input
    ; explorer_uri_input
    ; start_fetching_uri= Var.create "start_fetching_uri" fetch_uri
    ; start_fetching_address= Var.create "start_fetching_address" fetch_address
    }

  let go_to_explorer state ?uri ?address () =
    Option.iter address ~f:(fun a ->
        Var.set state.explorer_address_input a ;
        Var.set state.start_fetching_address true) ;
    Option.iter uri ~f:(fun u ->
        Var.set state.explorer_uri_input u ;
        Var.set state.start_fetching_uri true) ;
    Var.set state.current_view View.Metadata_explorer

  let should_start_fetching_address state =
    let should_i = Var.value state.start_fetching_address in
    Var.set state.start_fetching_address false ;
    should_i

  let should_start_fetching_uri state =
    let should_i = Var.value state.start_fetching_uri in
    Var.set state.start_fetching_uri false ;
    should_i

  let slow_step s =
    if s.dev_mode then Js_of_ocaml_lwt.Lwt_js.sleep 0.5 else Lwt.return ()
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

  let b58_script_id_hash_of_michelson_string s =
    b58_script_id_hash ("\x05" ^ Michelson_bytes.encode_michelson_string s)

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

let metadata_uri_editor_page state ~metadata_uri_editor ~metadata_uri_code =
  let open RD in
  let examples =
    let https_ok =
      "https://raw.githubusercontent.com/smondet/comevitz/master/data/metadata_example0.json"
    in
    let hash_of_https_ok =
      (* `sha256sum data/metadata_example0.json` → Achtung, the URL
         above takes about 5 minutes to be up to date with `master` *)
      "7baf4143a2afbd2682395cda14c9d29e78dee2b5cf7bb544bb434a6a4ac31794" in
    let ex name u = (name, Uri.to_string u) in
    [ ex "In KT1 Storage"
        (Uri.make ~scheme:"tezos-storage" ~host:State.kt1_with_metadata
           ~path:"here" ()); ex "HTTPS" (Uri.of_string https_ok)
    ; ex "IPFS"
        (Uri.of_string "ipfs://QmWDcp3BpBjvu8uJYxVqb7JLfr1pcyXsL97Cfkt3y1758o")
    ; ex "SHA256-checked HTTPS"
        (Uri.of_string
           (Fmt.str "sha256://0x%s/%s" hash_of_https_ok
              (Uri.pct_encode https_ok))) ] in
  let result_div =
    Reactive.div_of_var metadata_uri_code ~f:(fun uri_code ->
        let open Tezos_contract_metadata.Metadata_uri in
        match Uri.of_string uri_code |> of_uri with
        | Ok o ->
            [ big_answer `Ok [txt "This metadata URI is VALID 👍"]
            ; div (Contract_metadata.Uri.to_html o)
            ; div [sizing_table [("URI", String.length uri_code)]]
            ; div
                [ button
                    ~a:
                      [ a_class ["btn"; "btn-primary"]
                      ; a_onclick (fun _ ->
                            State.go_to_explorer state ~uri:uri_code () ;
                            true) ]
                    [txt "Try it in the explorer!"] ] ]
        | Error el ->
            [ big_answer `Error [txt "There were parsing/validation errors:"]
            ; div (show_tezos_error el) ]) in
  [editor_with_preview metadata_uri_editor ~examples result_div]

let metadata_json_to_html json_code =
  let open RD in
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
          [txt "This piece of metadata, while valid, is completely empty!"] ]
  | Ok ex ->
      let errs, warns = Validation.validate ex in
      let thumbdsup = "👍" in
      let thumbdsdown = "👎" in
      let warning = "⚠" in
      let actual_status, msg =
        match (errs, warns) with
        | [], [] -> (`Ok, Fmt.str "is valid %s" thumbdsup)
        | _ :: _, _ ->
            (`Error, Fmt.str "parses correctly but is invalid %s" thumbdsdown)
        | [], _ :: _ -> (`Ok, Fmt.str "is okay, but with warnings %s" warning)
      in
      let events title list to_html =
        match list with
        | [] -> []
        | more ->
            [ h4 [Fmt.kstr txt "%s: " title]
            ; ul (List.map more ~f:(fun ev -> li [to_html ev])) ] in
      let instr s =
        a
          ~a:[Fmt.kstr a_href "https://michelson.nomadic-labs.com/#instr-%s" s]
          [txt s] in
      let error_to_html =
        let open Validation.Error in
        function
        | Forbidden_michelson_instruction {view; instruction} ->
            span
              [ txt "The off-chain-view “"; code [txt view]; txt "” uses a "
              ; strong [txt "forbidden Michelson instruction: "]
              ; instr instruction
              ; txt " (the other forbidden instructions are: "
              ; span
                  (oxfordize_list
                     (List.filter
                        ~f:String.(( <> ) instruction)
                        Validation.Data.forbidden_michelson_instructions)
                     ~sep:(fun () -> txt ", ")
                     ~last_sep:(fun () -> txt ", and ")
                     ~map:instr); txt ")." ] in
      let warning_to_html =
        let open Validation.Warning in
        function
        | Wrong_author_format author ->
            span
              [ txt "The author "; code [txt author]
              ; txt " has a wrong format, it should look like "
              ; code [txt "Print Name <contact-url-or-email>"]; txt "." ]
        | Unexpected_whitespace {field; value} ->
            span
              [ txt "The field "; code [txt field]; txt " (= "
              ; code [Fmt.kstr txt "%S" value]
              ; txt ") uses confusing white-space characters." ]
        | Self_unaddressed {view; instruction} ->
            span
              [ txt "The off-chain-view "; code [txt view]
              ; txt " uses the instruction "; instr "SELF"
              ; ( match instruction with
                | None -> txt " not followed by any instruction."
                | Some i -> span [txt " followed by "; instr i; txt "."] )
              ; txt
                  " The current recommendation is to only use the combination "
              ; code [instr "SELF"; txt "; "; instr "ADDRESS"]
              ; txt " in off-chain-views." ] in
      [ big_answer actual_status [Fmt.kstr txt "This metadata blob %s" msg]
      ; div (events "Errors" errs error_to_html)
      ; div (events "Warnings" warns warning_to_html)
      ; div [h4 [txt "Content:"]; Contract_metadata.Content.to_html ex]
      ; div
          [ sizing_table
              [ ("Current JSON", String.length json_code)
              ; ( "Minimized JSON"
                , Ezjsonm.value_from_string json_code
                  |> Ezjsonm.value_to_string ~minify:true
                  |> String.length ) ] ] ]
  | Error el ->
      [ big_answer `Error [txt "There were parsing/validation errors:"]
      ; div (show_tezos_error el) ]

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
    Reactive.div_of_var metadata_json_code ~f:metadata_json_to_html in
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
    type t =
      {name: string; prefix: string; status: (float * Node_status.t) Var.t}

    let create name prefix =
      {name; prefix; status= Var.create "node-status" (0., Uninitialized)}

    let rpc_get node path =
      let open Lwt in
      let uri = Fmt.str "%s/%s" node.prefix path in
      Js_of_ocaml_lwt.XmlHttpRequest.(
        get uri
        >>= fun frame ->
        dbgf "%s %s code: %d" node.prefix path frame.code ;
        match frame.code with
        | 200 -> return frame.content
        | other -> Fmt.failwith "Getting %S returned code: %d" path other)

    let ping node =
      let open Lwt in
      Js_of_ocaml_lwt.XmlHttpRequest.(
        Fmt.kstr get "%s/chains/main/blocks/head/metadata" node.prefix
        >>= fun frame ->
        dbgf "%s metadata code: %d" node.name frame.code ;
        let new_status =
          match frame.code with
          | 200 ->
              dbgf "%s metadata content: %s" node.name frame.content ;
              Ready frame.content
          | other -> Non_responsive (Fmt.str "Return-code: %d" other) in
        return new_status)

    let micheline_of_json s =
      let json =
        match Ezjsonm.value_from_string s with
        | `O (("code", code) :: _) -> code
        | other -> other in
      let enc =
        Tezos_micheline.Micheline.canonical_encoding ~variant:"custom"
          Data_encoding.string in
      let mich = Data_encoding.Json.destruct enc json in
      Tezos_micheline.Micheline.root mich

    let metadata_big_map state_handle node ~address ~log =
      let open Lwt in
      let get = rpc_get node in
      let log fmt = Fmt.kstr log fmt in
      Fmt.kstr get "/chains/main/blocks/head/context/contracts/%s/storage"
        address
      >>= fun storage_string ->
      log "Got raw storage: %s" storage_string ;
      let mich_storage = micheline_of_json storage_string in
      log "As concrete: %a"
        Tezos_contract_metadata.Contract_storage.pp_arbitrary_micheline
        mich_storage ;
      State.slow_step state_handle
      >>= fun () ->
      Fmt.kstr get "/chains/main/blocks/head/context/contracts/%s/script"
        address
      >>= fun script_string ->
      log "Got raw script: %s…" (String.prefix script_string 30) ;
      let mich_storage_type =
        micheline_of_json script_string
        |> Tezos_micheline.Micheline.strip_locations
        |> Tezos_contract_metadata.Contract_storage.get_storage_type_exn in
      log "Storage type: %a"
        Tezos_contract_metadata.Contract_storage.pp_arbitrary_micheline
        mich_storage_type ;
      State.slow_step state_handle
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
      | [one] -> return one

    let bytes_value_of_big_map_at_string node ~big_map_id ~key ~log =
      let open Lwt in
      let hash_string = B58_hashes.b58_script_id_hash_of_michelson_string key in
      Fmt.kstr (rpc_get node) "/chains/main/blocks/head/context/big_maps/%s/%s"
        (Z.to_string big_map_id) hash_string
      >>= fun bytes_raw_value ->
      Fmt.kstr log "bytes raw value: %s" bytes_raw_value ;
      let content =
        match Ezjsonm.value_from_string bytes_raw_value with
        | `O [("bytes", `String b)] -> Hex.to_string (`Hex b)
        | _ -> Fmt.failwith "Cannot find bytes in %s" bytes_raw_value in
      return content
  end

  type t =
    { nodes: Node.t list Var.t
    ; wake_up_call: unit Lwt_condition.t
    ; loop_started: bool Var.t
    ; loop_interval: float Var.t }

  let create nodes =
    { nodes=
        Var.create "list-of-nodes" nodes
          ~eq:(List.equal Node.(fun na nb -> String.equal na.prefix nb.prefix))
    ; wake_up_call= Lwt_condition.create ()
    ; loop_started= Var.create "loop-started" false
    ; loop_interval= Var.create "loop-interval" 10. }

  let nodes t = t.nodes

  let _global =
    create
      [ Node.create "Carthagenet-GigaNode" "https://testnet-tezos.giganode.io"
      ; Node.create "Mainnet-GigaNode" "https://mainnet-tezos.giganode.io"
      ; Node.create "Dalphanet-GigaNode" "https://dalphanet-tezos.giganode.io"
      ; Node.create "Carthagenet-SmartPy" "https://carthagenet.smartpy.io"
      ; Node.create "Mainnet-SmartPy" "https://mainnet.smartpy.io"
      ; Node.create "Delphinet-SmartPy" "https://delphinet.smartpy.io" ]

  let wake_up_update_loop t = Lwt_condition.broadcast t.wake_up_call ()

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
                catch
                  (fun () ->
                    pick
                      [ ( Js_of_ocaml_lwt.Lwt_js.sleep 5.
                        >>= fun () ->
                        dbgf "%s timeout in start_update_loop" nod.Node.name ;
                        return (Non_responsive "Time-out while getting status")
                        )
                      ; ( Node.ping nod
                        >>= fun res ->
                        dbgf "%s returned to start_update_loop" nod.name ;
                        return res ) ])
                  (fun e ->
                    return (Non_responsive (Fmt.str "Error: %a" Exn.pp e)))
                >>= fun new_status ->
                dbgf "got status for %s" nod.name ;
                let now = (new%js Js_of_ocaml.Js.date_now)##valueOf in
                Var.set nod.status (now, new_status) ;
                return ())
         >>= fun () ->
         pick
           [ Js_of_ocaml_lwt.Lwt_js.sleep sleep_time
           ; Lwt_condition.wait t.wake_up_call ]
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

  let find_node_with_contract node_list addr =
    let open Lwt in
    catch
      (fun () ->
        Lwt_list.find_s
          (fun node ->
            catch
              (fun () ->
                Fmt.kstr (Node.rpc_get node)
                  "/chains/main/blocks/head/context/contracts/%s/storage" addr
                >>= fun _ -> return_true)
              (fun _ -> return_false))
          (nodes node_list |> Var.value))
      (fun _ -> Fmt.failwith "Cannot find a node that knows about %S" addr)

  let metadata_value state_handle nodes ~address ~key ~log =
    let open Lwt in
    let logf f = Fmt.kstr log f in
    find_node_with_contract nodes address
    >>= fun node ->
    logf "Found contract with node %S" node.name ;
    Node.metadata_big_map state_handle node ~address ~log
    >>= fun big_map_id ->
    logf "Metadata big-map: %s" (Z.to_string big_map_id) ;
    Node.bytes_value_of_big_map_at_string node ~big_map_id ~key ~log

  let table_of_statuses node_list =
    let open RD in
    let node_status node =
      let node_metadata _date json =
        let open Ezjsonm in
        try
          let j = value_from_string json in
          let field f j =
            try List.Assoc.find_exn ~equal:String.equal (get_dict j) f
            with _ ->
              Fmt.failwith "Cannot find %S in %s" f
                (value_to_string ~minify:true j) in
          code ~a:[ (* Fmt.kstr a_ "%.03f" date *) ]
            [ Fmt.kstr txt "Level: %d"
                (field "level" j |> field "level" |> get_int) ]
        with e ->
          code [Fmt.kstr txt "Failed to parse the Metadata JSON: %a" Exn.pp e]
      in
      Reactive.div
        (Var.map_to_list node.Node.status
           ~f:
             Node_status.(
               fun (date, status) ->
                 let show s = [code [s]] in
                 match status with
                 | Uninitialized -> show (txt "Uninitialized")
                 | Non_responsive reason ->
                     show (Fmt.kstr txt "Non-responsive: %s" reason)
                 | Ready metadata -> [node_metadata date metadata])) in
    tablex
      ~a:[a_class ["table"; "table-bordered"; "table-hover"]]
      ~thead:
        (thead
           [ tr
               [ th [txt "Name"]; th [txt "URI-prefix"]; th [txt "Status"]
               ; th [txt "Latest Ping"] ] ])
      [ Reactive.tbody
          (Var.map_to_list (nodes node_list) ~f:(fun nodes ->
               List.map nodes ~f:(fun node ->
                   let open Node in
                   let open Node_status in
                   tr ~a:[a_style "height: 3em"]
                     [ td
                         ~a:
                           [ Reactive.a_class
                               ( Var.signal node.status
                               |> React.S.map (function
                                    | _, Uninitialized -> ["bg-warning"]
                                    | _, Non_responsive _ -> ["bg-danger"]
                                    | _, Ready _ -> ["bg-success"]) ) ]
                         [em [txt node.name]]; td [code [txt node.prefix]]
                     ; td [node_status node]
                     ; td
                         [ Reactive.code
                             (Var.map_to_list node.status ~f:(fun (date, _) ->
                                  let date_string =
                                    (new%js Js_of_ocaml.Js.date_fromTimeValue
                                       date)##toISOString
                                    |> Js_of_ocaml__Js.to_string in
                                  [txt date_string])) ] ]))) ]
end

let metadata_explorer state_handle =
  let open RD in
  let nodes = Tezos_nodes._global in
  Tezos_nodes.ensure_update_loop nodes ;
  let contract_address = state_handle.State.explorer_address_input in
  let uri_result = Var.create "contract-exploration-uri" `Not_started in
  let get_metadata_uri () =
    let open Lwt in
    let log_stack = ref [] in
    let log s =
      log_stack := s :: !log_stack ;
      Var.set uri_result
        (`Fetching (String.concat ~sep:"\n" (List.rev !log_stack))) in
    let logf fmt = Fmt.kstr log fmt in
    let addr = Var.value contract_address in
    catch
      (fun () ->
        Tezos_nodes.metadata_value state_handle nodes ~address:addr ~key:"" ~log
        >>= fun uri ->
        logf "URI: `%s`" uri ;
        match
          Tezos_contract_metadata.Metadata_uri.of_uri (Uri.of_string uri)
        with
        | Ok mu ->
            logf "Parsed uri: %a" Tezos_contract_metadata.Metadata_uri.pp mu ;
            State.slow_step state_handle
            >>= fun () ->
            Var.set uri_result (`Done_uri (List.rev !log_stack, uri, mu)) ;
            return ()
        | Error e ->
            Fmt.failwith "Error parsing URI: %a"
              Tezos_error_monad.Error_monad.pp_print_error e)
      (function
        | Json_encoding.Cannot_destruct (path, e) ->
            Var.set uri_result
              (`Failed
                ( List.rev !log_stack
                , Fmt.str "JSON-parsing: At %a: %a"
                    (Json_query.print_path_as_json_path ?wildcards:None)
                    path Exn.pp e )) ;
            return ()
        | Failure s ->
            Var.set uri_result (`Failed (List.rev !log_stack, s)) ;
            return ()
        | e ->
            Var.set uri_result
              (`Failed (List.rev !log_stack, Fmt.str "Exception: %a" Exn.pp e)) ;
            return ()) in
  let uri_input = state_handle.State.explorer_uri_input in
  let metadata_result = Var.create "explorer-metadata" `Not_started in
  let fetch_uri () =
    let open Lwt in
    dbgf "fetching uri" ;
    let log_stack = ref [] in
    let log s =
      log_stack := s :: !log_stack ;
      Var.set metadata_result
        (`Fetching (String.concat ~sep:"\n" (List.rev !log_stack))) in
    let logf fmt = Fmt.kstr log fmt in
    let ni s = Fmt.failwith "Not Implemented: %s" s in
    catch
      (fun () ->
        let uri = Var.value uri_input |> Uri.of_string in
        match Tezos_contract_metadata.Metadata_uri.of_uri uri with
        | Ok mu ->
            logf "Parsed uri: %a" Tezos_contract_metadata.Metadata_uri.pp mu ;
            State.slow_step state_handle
            >>= fun () ->
            let rec resolve =
              let open Tezos_contract_metadata.Metadata_uri in
              function
              | Web http ->
                  logf "HTTP %S, may fail because of origin policy" http ;
                  Js_of_ocaml_lwt.XmlHttpRequest.(
                    get http
                    >>= fun frame ->
                    dbgf "%s -> code: %d" http frame.code ;
                    match frame.code with
                    | 200 ->
                        logf "HTTP success (%d bytes)"
                          (String.length frame.content) ;
                        return frame.content
                    | other ->
                        Fmt.failwith "Getting %S returned code: %d" http other)
                  >>= fun content -> return content
              | Ipfs {cid; path} ->
                  let gateway = "https://gateway.ipfs.io/ipfs/" in
                  let gatewayed = Fmt.str "%s%s%s" gateway cid path in
                  logf "IPFS CID %S path %S, adding gateway %S" cid path gateway ;
                  resolve (Web gatewayed)
              | Storage {network= None; address; key} ->
                  let addr =
                    match address with
                    | Some s -> s
                    | None -> Var.value contract_address in
                  logf "Using address %S (key = %S)" addr key ;
                  Tezos_nodes.metadata_value state_handle nodes ~address:addr
                    ~key ~log
              | Storage {network= Some network; address; key} ->
                  logf "storage %s %a %S" network
                    Fmt.Dump.(option string)
                    address key ;
                  Fmt.kstr ni "storage uri with network = %s" network
              | Hash {kind= `Sha256; value; target} -> (
                  let expected =
                    match Digestif.of_raw_string_opt Digestif.sha256 value with
                    | Some s -> s
                    | None ->
                        Fmt.failwith "%a is not a valid SHA256 hash" Hex.pp
                          (Hex.of_string value) in
                  logf "sha256: %a" (Digestif.pp Digestif.sha256) expected ;
                  resolve target
                  >>= fun content ->
                  let obtained =
                    Digestif.digest_string Digestif.sha256 content in
                  logf "hash of content: %a"
                    (Digestif.pp Digestif.sha256)
                    obtained ;
                  match
                    Digestif.unsafe_compare Digestif.sha256 expected obtained
                  with
                  | 0 -> return content
                  | _ ->
                      Fmt.failwith
                        "Hash of content %a is different from expected %a"
                        (Digestif.pp Digestif.sha256)
                        obtained
                        (Digestif.pp Digestif.sha256)
                        expected ) in
            resolve mu
            >>= fun data ->
            logf "Got data: %S" data ;
            Var.set metadata_result (`Done_metadata (data, List.rev !log_stack)) ;
            return ()
        | Error e ->
            Fmt.failwith "Error parsing URI: %a"
              Tezos_error_monad.Error_monad.pp_print_error e
            >>= fun () -> return ())
      (function
        | e ->
            Var.set metadata_result
              (`Failed
                ( List.rev !log_stack
                , match e with
                  | Failure s -> s
                  | e -> Fmt.str "Exception: %a" Exn.pp e )) ;
            return ()) in
  let inputs_and_button ?(a = []) ?(button_text = "Go!") ?(more_items = [])
      content ~active ~action =
    let inputs =
      oxfordize_list
        ~sep:(fun () -> txt "  ")
        ~last_sep:(fun () -> txt "  ")
        content
        ~map:(fun (lbl, var) ->
          div
            ~a:[a_class ["form-group"]]
            [ label [txt lbl]; txt " "
            ; Reactive.textarea
                ~a:
                  ( a
                  @ [ a_style "font-family: monospace"; a_rows 1; a_cols 60
                    ; a_class ["form-control"]
                    ; a_onkeypress (fun ev ->
                          dbgf "keycode: %d" ev##.keyCode ;
                          match ev##.keyCode with
                          | 13 when not (Js_of_ocaml.Js.to_bool ev##.shiftKey)
                            ->
                              action () ; false
                          | _ -> true)
                    ; a_onchange
                        Js_of_ocaml.(
                          fun ev ->
                            Js.Opt.iter ev##.target (fun input ->
                                Js.Opt.iter (Dom_html.CoerceTo.textarea input)
                                  (fun input ->
                                    let v = input##.value |> Js.to_string in
                                    dbgf "TA inputs: %d bytes: %S"
                                      (String.length v) v ;
                                    Var.set var v)) ;
                            false) ] )
                (Var.signal var |> React.S.map txt) ]) in
    div
      ~a:[a_class ["form-inline"]]
      ( inputs
      @ [ button
            ~a:
              [ a_onclick (fun _ -> action () ; true)
              ; Reactive.a_class
                  ( Var.signal active
                  |> React.S.map (function
                       | true -> ["btn"; "btn-primary"]
                       | false -> ["btn"; "btn-default"; "disabled"]) ) ]
            [ Reactive.span ~a:[a_style "width: 4em"]
                (Var.map_to_list active ~f:(function
                  | true -> [txt button_text]
                  | false ->
                      [ img ~src:"loading.gif" ~a:[a_width 30]
                          ~alt:"LOAADDDINGGG" () ])) ] ]
      @ more_items ) in
  let fetch_uri_activable =
    Var.map metadata_result ~f:(function
      | `Not_started | `Failed _ | `Done_metadata _ -> true
      | _ -> false) in
  let fetch_metadata_uri_action () =
    Var.set uri_result (`Fetching "Start fetching data …") ;
    Lwt.async get_metadata_uri in
  if State.should_start_fetching_address state_handle then
    fetch_metadata_uri_action () ;
  let fetch_uri_action () =
    Var.set metadata_result (`Fetching "Start fetching metadatadata …") ;
    Lwt.async fetch_uri in
  let add_node_name = Var.create "node-name" "User1" in
  let add_node_rpc_prefix = Var.create "node-rpc" "http://betanet.example.com" in
  if State.should_start_fetching_uri state_handle then fetch_uri_action () ;
  [ div [h2 [txt "Work-In-Progress: Metadata Explorer"]]
  ; div
      [ h3 [txt "Tezos Nodes"]; Tezos_nodes.table_of_statuses nodes
      ; inputs_and_button ~button_text:"Add Node"
          [("Name:", add_node_name); ("RPC-Prefix:", add_node_rpc_prefix)]
          ~active:(Var.create "add-node-active" true) ~action:(fun () ->
            let nodes_var = nodes.Tezos_nodes.nodes in
            let current = Var.value nodes_var in
            Var.set nodes_var
              ( current
              @ [ Tezos_nodes.Node.create (Var.value add_node_name)
                    (Var.value add_node_rpc_prefix) ] ) ;
            Tezos_nodes.wake_up_update_loop nodes) ]; hr ()
  ; div
      [ h3 [txt "Find The Metadata URI of A Contract"]
      ; div
          [ inputs_and_button
              [("KT1 Address:", contract_address)]
              ~active:
                (Var.map uri_result ~f:(function
                  | `Not_started | `Failed _ | `Done_uri _ -> true
                  | _ -> false))
              ~action:fetch_metadata_uri_action
              ~more_items:
                (let link text ~f =
                   a
                     ~a:
                       [ Reactive.a_href
                           (Var.signal contract_address |> React.S.map f) ]
                     [txt text] in
                 [ txt " ("
                 ; link "BCD"
                     ~f:(Fmt.str "https://better-call.dev/search?text=%s")
                 ; txt ", "
                 ; link "SmartPy"
                     ~f:
                       (Fmt.str
                          "https://smartpy.io/dev/explorer.html?address=%s")
                 ; txt ")" ]) ]
      ; Reactive.div
          (Var.map_to_list uri_result ~f:(function
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
                        [span [txt (String.concat ~sep:"\n" log)]] ]
                ; button
                    ~a:
                      [ a_onclick (fun _ ->
                            Var.set uri_input uri_code ;
                            fetch_uri_action () ;
                            true)
                      ; Reactive.a_class
                          ( Var.signal fetch_uri_activable
                          |> React.S.map (function
                               | true -> ["btn"; "btn-primary"]
                               | false -> ["btn"; "btn-default"; "disabled"]) )
                      ]
                    [span [txt "Set It as Input And Fetch It ↴"]] ]
            | `Fetching msg ->
                [ pre
                    ~a:[a_style "color: #999; font-size: 140%"]
                    [span [txt msg]] ]
            | `Failed (log, msg) ->
                [ pre
                    ~a:[a_style "color: #999; font-size: 140%"]
                    [ txt (String.concat ~sep:"\n" log)
                    ; span ~a:[a_style "color: #900"] [txt ("\n" ^ msg)] ] ]))
      ]; hr ()
  ; div
      [ h3 [txt "Resolve/Fetch Metadata URIs"]
      ; div
          [ inputs_and_button [("URI:", uri_input)] ~active:fetch_uri_activable
              ~action:fetch_uri_action ]
      ; Reactive.div
          (Var.map_to_list metadata_result ~f:(function
            | `Not_started -> []
            | `Done_metadata (json, log) ->
                [ div (metadata_json_to_html json)
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
        @ [ Menu.item (txt "Metadata Explorer")
              ~long_message:(txt "Explore metadata in existing contracts.")
              ~description:
                (p
                   [ txt
                       "This is a work-in-progress metadata explorer that \
                        fetches metadata from KT1-contracts using public \
                        nodes …" ])
              ~active:
                (Var.map state.State.current_view ~f:(function
                  | Metadata_explorer -> false
                  | _ -> true))
              (fun () -> Var.set state.State.current_view Metadata_explorer) ]
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

let go _ =
  dbg Fmt.(const string "Hello Go!") ;
  Bootstrap_css.ensure () ;
  ignore
    Lwt.(
      let state = State.init ~arguments:Js_of_ocaml.Url.Current.arguments () in
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
