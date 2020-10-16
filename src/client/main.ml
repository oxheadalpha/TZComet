open Import

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
        ; a ~a:[a_href "https://github.com/tqtezos/TZComet"] [txt "TZComet "]
        ; ( match version_string with
          | None -> i [txt "unknown version"]
          | Some vs ->
              span
                [ txt "version "
                ; a
                    ~a:
                      [ Fmt.kstr a_href
                          "https://github.com/tqtezos/TZComet/commit/%s" vs ]
                    [i [txt vs]] ] )
        ; Fmt.kstr txt "%s."
            (if state.State.dev_mode then " (in “dev” mode)" else "") ]
    ; h3 [Fmt.kstr txt "What would you like to do?"]; menu_welcome
    ; h3 [txt "Further Reading"]
    ; p
        [ txt "The source for this webpage is available on Github: "
        ; a
            ~a:[a_href "https://github.com/tqtezos/TZComet"]
            [code [txt "tqtezos/TZComet"]]; txt "." ]
    ; p
        [ txt
            "The Contract Metadata standard, a.k.a. TZIP-16, is currently \
             currently work-in-progress: "
        ; a
            ~a:
              [ a_href
                  "https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-16/tzip-16.md"
              ]
            [ txt
                "https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-16/tzip-16.md"
            ]; txt "." ] ]

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
      "https://raw.githubusercontent.com/tqtezos/TZComet/master/data/metadata_example0.json"
    in
    let hash_of_https_ok =
      (* `sha256sum data/metadata_example0.json` → Achtung, the URL
         above takes about 5 minutes to be up to date with `master` *)
      "7d961916f05d72afc765389a695458a9b451954419b41fa3cdd5fa816128b744" in
    let ex name u = (name, Uri.to_string u) in
    [ ex "In KT1 Storage"
        (Uri.make ~scheme:"tezos-storage" ~host:State.kt1_with_metadata
           ~path:"here" ()); ex "HTTPS" (Uri.of_string https_ok)
    ; ex "IPFS"
        (Uri.of_string "ipfs://QmWDcp3BpBjvu8uJYxVqb7JLfr1pcyXsL97Cfkt3y1758o")
    ; ex "SHA256-checked HTTPS"
        (Uri.of_string
           (Fmt.str "sha256://0x%s/%s" hash_of_https_ok
              (Uri.pct_encode https_ok)))
    ; ex "In storage with chain-id"
        (Uri.make ~scheme:"tezos-storage"
           ~host:(State.kt1_with_metadata ^ ".NetXrtZMmJmZSeb")
           ~path:"here" ()) ] in
  let result_div =
    Reactive.div_of_var metadata_uri_code ~f:(fun uri_code ->
        let open Tezos_contract_metadata.Metadata_uri in
        let errors = ref [] in
        let error w src e = errors := (w, src, e) :: !errors in
        let validate_kt1_address s =
          ( try ignore (B58_hashes.check_b58_kt1_hash s) with
          | Failure f -> error `Address s f
          | e -> Fmt.kstr (error `Address s) "%a" Exn.pp e ) ;
          Ok () in
        let validate_network = function
          | "mainnet" | "carthagenet" | "delphinet" | "dalphanet" | "zeronet" ->
              Ok ()
          | s ->
              ( try ignore (B58_hashes.check_b58_chain_id_hash s) with
              | Failure f -> error `Network s f
              | e -> Fmt.kstr (error `Network s) "%a" Exn.pp e ) ;
              Ok () in
        match
          Uri.of_string uri_code
          |> of_uri ~validate_kt1_address ~validate_network
        with
        | Ok o ->
            let header =
              match !errors with
              | [] -> big_answer `Ok [txt "This metadata URI is VALID 👍"]
              | _ ->
                  big_answer `Error
                    [txt "This metadata URI parses OK but is not VALID 😞"]
            in
            let errors_div =
              match !errors with
              | [] -> []
              | more ->
                  [ txt "Validation errors:"
                  ; ul
                      (List.map more ~f:(fun (which, source, error) ->
                           li
                             [ strong
                                 [ txt
                                     ( match which with
                                     | `Address -> "Contract address"
                                     | `Network -> "Network" ) ]; txt " ("
                             ; code [txt source]; txt "): "; em [txt error] ]))
                  ] in
            [ header; div errors_div; div (Contract_metadata.Uri.to_html o)
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
      ; homepage= None
      ; interfaces= []
      ; views= []
      ; unknown= [] } ->
      [ big_answer `Ok
          [txt "This piece of metadata, while valid, is completely empty!"] ]
  | Ok ex ->
      let errs, warns =
        Validation.validate ex ~protocol_hash_is_valid:(fun s ->
            try
              let _ = B58_hashes.check_b58_protocol_hash s in
              true
            with e ->
              dbgf "Protocol hash problem: %a" Exn.pp e ;
              false) in
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
        let the_off_chain_view view =
          span [txt "The off-chain-view “"; code [txt view]; txt "”"] in
        function
        | Forbidden_michelson_instruction {view; instruction} ->
            span
              [ the_off_chain_view view; txt " uses a "
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
                     ~map:instr); txt ")." ]
        | Michelson_version_not_a_protocol_hash {view; value} ->
            span
              [ the_off_chain_view view
              ; txt " references a wrong version of Michelson ("
              ; code [txt value]
              ; txt "), it should be a valid protocol hash, like "
              ; code [txt "PsDELPH1Kxsxt8f9eWbxQeRxkjfbxoqM52jvs5Y5fBxWWh4ifpo"]
              ; txt "." ] in
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

let metadata_explorer state_handle =
  let open RD in
  let _state_handle =
    object
      method system = System.{dev_mode= false}
    end in
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
        Tezos_nodes.metadata_value _state_handle nodes ~address:addr ~key:""
          ~log
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
                  Tezos_nodes.metadata_value _state_handle nodes ~address:addr
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
        ~map:(fun (lbl, var, cols) ->
          div
            ~a:[a_class ["form-group"]]
            [ label [txt lbl]; txt " "
            ; Reactive.textarea
                ~a:
                  ( a
                  @ [ a_style "font-family: monospace"; a_rows 1; a_cols cols
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
          [ ("Name:", add_node_name, 25)
          ; ("RPC-Prefix:", add_node_rpc_prefix, 50) ]
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
              [("KT1 Address:", contract_address, 60)]
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
          [ inputs_and_button [("URI:", uri_input, 100)]
              ~active:fetch_uri_activable ~action:fetch_uri_action ]
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
      ]
  ; div
      [ h3 [txt "Call Off-Chain-Views"]
      ; Reactive.div
          (Var.map_to_list metadata_result ~f:(function
            | `Done_metadata (json, _) -> (
                let open Tezos_contract_metadata.Metadata_contents in
                match of_json json with
                | Error _ | Ok {views= []; _} ->
                    [ big_answer `Error
                        [txt "This piece of metadata does not have any views."]
                    ]
                | Ok ex ->
                    let implementation_form impl =
                      let open Tezos_contract_metadata.Metadata_contents.View
                               .Implementation in
                      match impl with
                      | Michelson_storage michview -> (
                          let call_result = Var.create "view-result" `None in
                          let open Tezos_contract_metadata.Metadata_contents
                                   .View
                                   .Implementation
                                   .Michelson_storage in
                          match michview.parameter with
                          | None ->
                              div [txt "TODO: Michelson parameter-less form"]
                          | Some (Micheline m) ->
                              let parameter = Var.create "view-param" "" in
                              div
                                [ txt "Please, provide the parameter of type "
                                ; code
                                    [ Fmt.kstr txt "%a"
                                        Tezos_contract_metadata.Contract_storage
                                        .pp_arbitrary_micheline
                                        (Tezos_micheline.Micheline.root m)
                                    ; txt ": "
                                    ; input
                                        ~a:
                                          [ a_input_type `Text; a_value ""
                                          ; a_oninput
                                              Js_of_ocaml.(
                                                fun ev ->
                                                  Js.Opt.iter ev##.target
                                                    (fun input ->
                                                      Js.Opt.iter
                                                        (Dom_html.CoerceTo.input
                                                           input) (fun input ->
                                                          let v =
                                                            input##.value
                                                            |> Js.to_string
                                                          in
                                                          dbgf
                                                            "TA inputs: %d \
                                                             bytes: %S"
                                                            (String.length v) v ;
                                                          Var.set parameter v)) ;
                                                  false) ]
                                        ()
                                    ; Reactive.span
                                        (Var.map_to_list parameter ~f:(fun c ->
                                             match
                                               Tezos_micheline.Micheline_parser
                                               .tokenize c
                                             with
                                             | tokens, [] -> (
                                               match
                                                 Tezos_micheline
                                                 .Micheline_parser
                                                 .parse_expression tokens
                                               with
                                               | node, [] ->
                                                   [ button
                                                       ~a:
                                                         [ a_class
                                                             [ "btn"
                                                             ; "btn-primary" ]
                                                         ; a_onclick (fun _ ->
                                                               dbgf
                                                                 "Call \
                                                                  off-chain \
                                                                  view: %s"
                                                                 c ;
                                                               Var.set
                                                                 call_result
                                                                 (`In_progress
                                                                   []) ;
                                                               Lwt.async
                                                                 Lwt.(
                                                                   fun () ->
                                                                     Tezos_nodes
                                                                     .call_off_chain_view
                                                                       nodes
                                                                       ~address:
                                                                         (Var
                                                                          .value
                                                                            contract_address)
                                                                       ~view:
                                                                         michview
                                                                       ~parameter:
                                                                         node
                                                                       ~log:(fun
                                                                              line
                                                                            ->
                                                                         Var.set
                                                                           call_result
                                                                           (`In_progress
                                                                             ( line
                                                                             ::
                                                                             ( match
                                                                                Var
                                                                                .value
                                                                                call_result
                                                                               with
                                                                             | `In_progress
                                                                                lines
                                                                               ->
                                                                                lines
                                                                             | _
                                                                               ->
                                                                                []
                                                                             )
                                                                             )))
                                                                     >>= fun res ->
                                                                     Var.set
                                                                       call_result
                                                                       (`Done
                                                                         res) ;
                                                                     return ()) ;
                                                               true) ]
                                                       [txt "OK Go!"] ]
                                               | _, errs ->
                                                   [ Fmt.kstr txt
                                                       "errors parsing: %a"
                                                       Tezos_error_monad
                                                       .Error_monad
                                                       .pp_print_error errs ] )
                                             | _, errs ->
                                                 [ Fmt.kstr txt
                                                     "errors tokenizing: %a"
                                                     Tezos_error_monad
                                                     .Error_monad
                                                     .pp_print_error errs ])) ]
                                ; Reactive.div
                                    (Var.map_to_list call_result ~f:(function
                                      | `None -> []
                                      | `In_progress lines ->
                                          [ txt "Working on it …"
                                          ; pre
                                              [code (List.rev_map lines ~f:txt)]
                                          ]
                                      | `Done (Ok (mich_result, mich_storage))
                                        ->
                                          [ txt "OK, done:"
                                          ; ul
                                              [ li
                                                  [ txt "Result: "
                                                  ; code
                                                      [ Fmt.kstr txt "%a"
                                                          Tezos_contract_metadata
                                                          .Contract_storage
                                                          .pp_arbitrary_micheline
                                                          mich_result ] ]
                                              ; li
                                                  [ txt "Current storage: "
                                                  ; code
                                                      [ Fmt.kstr txt "%a"
                                                          Tezos_contract_metadata
                                                          .Contract_storage
                                                          .pp_arbitrary_micheline
                                                          mich_storage ] ] ] ]
                                      | `Done (Error s) ->
                                          [Fmt.kstr txt "Error: %s" s])) ] )
                      | Rest_api_query _ -> div [txt "TODO: REST API form"]
                    in
                    let view_list =
                      List.map ex.views ~f:(fun v ->
                          let open Tezos_contract_metadata.Metadata_contents
                                   .View in
                          div
                            [ code [txt v.name]; txt ": "
                            ; em
                                [ txt
                                    (Option.value ex.description
                                       ~default:"No description available …")
                                ]
                            ; div
                                (List.map v.implementations implementation_form)
                            ]) in
                    [ h4
                        [ Fmt.kstr txt "%d Views Available"
                            (List.length ex.views) ]; div view_list ] )
            | _ ->
                [ div
                    [ txt
                        "You need to fetch some metadata above for this to \
                         work!" ] ])) ] ]

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

let lwd_onload _ =
  let open Tyxml_lwd in
  let open Lwdom in
  let open Js_of_ocaml in
  let base_div = Dom_html.getElementById "attach-ui" in
  base_div##.innerHTML := Js.string "" ;
  let state =
    let gui = Gui.State.create () in
    let nodes = Query_nodes.create () in
    object
      method system = System.{dev_mode= false}

      method gui = gui

      method nodes = nodes
    end in
  Query_nodes.add_default_nodes state ;
  let doc = Gui.root_document state in
  let root = Lwd.observe doc in
  Lwd.set_on_invalidate root (fun _ ->
      dbgf "invalidate doc" ;
      ignore
        (Dom_html.window##requestAnimationFrame
           (Js.wrap_callback (fun _ -> ignore (Lwd.quick_sample root))))) ;
  List.iter ~f:(Dom.appendChild base_div)
    (Lwd_seq.to_list (Lwd.quick_sample root) : _ node list :> raw_node list) ;
  Js._false

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
  let arg s =
    List.Assoc.find Js_of_ocaml.Url.Current.arguments ~equal:String.equal s
  in
  match arg "lwd" with
  | Some "true" -> Dom_html.window##.onload := Dom_html.handler lwd_onload
  | _ -> Dom_html.window##.onload := Dom_html.handler go
