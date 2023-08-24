open Import

let tzcomet_link () =
  let open Meta_html in
  link ~target:"https://github.com/oxheadalpha/TZComet" (t "TZComet")

let navigation_menu state =
  let open State in
  let open Page in
  let open Meta_html in
  let fragment = make_fragment state in
  let fragment_self = Reactive.map ~f:Fragment.to_string fragment in
  let fragment_page p =
    Reactive.map
      ~f:(fun frg -> Fragment.(to_string (change_for_page frg p)))
      fragment
  in
  let tzcomet =
    bt "TZComet"
    % Reactive.bind fragment_self ~f:(fun f ->
          (* Invisible, but kept in order to keep pulling the fragment. *)
          span ~a:[ style "font-size: 20%" ] (link (t " ") ~target:("#" ^ f)))
    %% Reactive.bind (State.dev_mode state) ~f:(function
         | true -> it "(dev)"
         | false -> empty ())
  in
  let all_items =
    let of_page p =
      Bootstrap.Navigation_bar.item
        (bt (Page.to_string p))
        ~active:(State.current_page_is_not state p)
        ~action:(State.set_page state (`Changing_to p))
        ~fragment:(fragment_page p)
    in
    List.map ~f:of_page all_in_order
  in
  Reactive.bind (Browser_window.width state) ~f:(function
    | Some `Wide ->
        Bootstrap.Navigation_bar.(
          let brand = Bootstrap.label `Dark tzcomet in
          make ~brand all_items)
    | None | Some `Thin ->
        let nav_thing =
          Bootstrap.Navigation_bar.(make ~brand:(empty ()) all_items)
        in
        let _burger =
          div
            ~a:
              [ style "z-index: 2; position: fixed; margin: 10px 0px 0px 10px" ]
            nav_thing
        in
        let on_top =
          div
            ~a:
              [
                style
                  "z-index: 2; position: absolute; text-align: center; \
                   font-size: 200%; pointer-events: none; width: 100%; color: \
                   #004";
                classes [ "mx-auto" ];
              ]
            tzcomet
        in
        on_top % nav_thing
        (* burger
           % div
               ~a:
                 [ classes ["col-12"; "bg-dark"]
                 ; style "text-align:center; font-size: 175%; min-height: 70px" ]
               (div tzcomet) *))

let about_page state =
  let open Meta_html in
  let p = Bootstrap.p_lead in
  h2 (t "TZComet")
  % p
      (t "This is" %% tzcomet_link ()
      %% (match state#version_string with
         | None -> it "unknown version"
         | Some vs ->
             t "version "
             %% link
                  ~target:
                    (Fmt.str "https://github.com/oxheadalpha/TZComet/commit/%s"
                       vs)
                  (it vs))
      % Reactive.bind (State.dev_mode state) ~f:(function
          | true -> t " (in “dev” mode)."
          | false -> t "."))
  % p (t "An explorer/editor/validator/visualizer for Tezos contract metadata.")
  % p
      (t "The source for this webpage is available on Github:"
      %% link ~target:"https://github.com/oxheadalpha/TZComet"
           (ct "oxheadalpha/TZComet")
      % t ".")
  % p
      (t "The Contract Metadata standard, a.k.a. TZIP-16, is at: "
      % url ct
          "https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-16/tzip-16.md"
      % t ".")
  % Reactive.bind (State.dev_mode state) ~f:(function
      | false -> empty ()
      | true ->
          h2 (t "Dev-mode Junk:")
          % p (t "This is also a test/experiment in UI writing …")
          % Meta_html.Example.e1 ())

module Examples_dropdown = struct
  open Meta_html

  let make ctxt ~action l =
    let open Bootstrap.Dropdown_menu in
    let example (v, msg) =
      let cct v =
        if String.length v > 22 then
          abbreviation v (ct (String.sub v ~pos:0 ~len:21 ^ "…"))
        else ct v
      in
      item (cct v %% t "→" %% it msg) ~action:(fun () -> action v)
    in
    Reactive.bind (State.Examples.get ctxt) ~f:(fun examples ->
        button (t "Examples 💡 ")
          (List.concat_map l ~f:(fun (h, f) ->
               header h :: List.map (f examples) ~f:example)))

  let explorable ctxt =
    make ctxt
      ~action:(fun x -> State.set_explorer_input ctxt x)
      [
        (t "KT1 Contracts", fun x -> x.contracts);
        (t "TZIP-16-URIs", fun x -> x.uris);
      ]

  let editable ctxt ~action =
    make ctxt ~action
      [
        (t "TZIP-16 Metadata Content", fun x -> x.metadata_blobs);
        (t "TZIP-16-URIs", fun x -> x.uris);
        (t "Michelson Bytes Blobs", fun x -> x.michelson_bytes);
        (t "Michelson Concrete Expressions", fun x -> x.michelson_concretes);
      ]
end

module Editor = struct
  type guess = Empty | Failed | Format of State.Editor_mode.format

  let guessers : (log:(Message.t -> unit) -> string -> guess option) list =
    let of_predicate name v f ~log inp =
      let worked = try f inp with _ -> false in
      log
        Message.(
          t "Trying predicate" %% ct name %% t "→"
          % if worked then t "OK!" else t "Nope :/");
      if worked then Some v else None
    in
    let looks_like_json s =
      let open Char in
      let str = String.strip s in
      str.[0] = '{' && str.[String.length str - 1] = '}'
    in
    let oneline s =
      match String.split ~on:'\n' (String.strip s) with
      | [ "" ] -> false
      | [ _ ] -> true
      | _ -> false
    in
    let is_prefix_strip s ~prefix = String.is_prefix (String.strip s) ~prefix in
    let uri_start = Re.Posix.compile_pat "^[a-z0-9\\-]+:" in
    let looks_like_an_uri s = oneline s && Re.execp uri_start s in
    let looks_like_hexa s =
      is_prefix_strip s ~prefix:"0x"
      || String.for_all s ~f:(function
           | 'A' .. 'F' | 'a' .. 'f' | '0' .. '9' | 'x' | ' ' | '\n' -> true
           | _ -> false)
    in
    let looks_like_michelson s =
      match (String.strip s).[0] with
      | '(' | '"' | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' -> true
      | _ -> false
    in
    let format_of_predicate n v = of_predicate n (Format v) in
    [
      of_predicate "it-is-empty" Empty (fun s ->
          String.is_empty (String.strip s));
      format_of_predicate "looks-like-a-json-object" `Metadata_json
        looks_like_json;
      format_of_predicate "looks-like-an-uri" `Uri looks_like_an_uri;
      format_of_predicate "looks-like-hexadecimal" `Hex looks_like_hexa;
      format_of_predicate "looks-like-michelson" `Michelson looks_like_michelson;
    ]

  open Meta_html

  let big_answer level content =
    let kind = match level with `Ok -> `Success | `Error -> `Danger in
    h2 (Bootstrap.color kind content)

  let show_uri ctxt inpo =
    let list_of_validation_errors l =
      itemize
        (List.map l ~f:(fun (w, s, e) ->
             (match w with `Network -> t "Network" | `Address -> t "Address")
             %% ct s % t ":" %% t e))
    in
    match Contract_metadata.Uri.validate inpo with
    | Ok u, errs ->
        let header =
          match errs with
          | [] -> big_answer `Ok (t "This metadata URI is VALID 👍")
          | _ ->
              big_answer `Error
                (t "This metadata URI parses OK but is not VALID 😞")
        in
        let sec = h4 in
        let validation_errors =
          match errs with
          | [] -> empty ()
          | more ->
              sec (Bootstrap.color `Danger (t "Validation Errors:"))
              % list_of_validation_errors more
        in
        header % validation_errors
        % sec (t "Understood As:")
        % Tezos_html.metadata_uri ~open_in_editor_link:false ctxt u
    | Error el, errs -> (
        big_answer `Error (t "There were parsing/validation errors 😭")
        % Tezos_html.error_trace ctxt el
        %
        match errs with
        | [] -> empty ()
        | more -> p (t "Moreover:") % list_of_validation_errors more)

  let explode_hex bytes_code =
    let with_zero_x, bytes =
      let prefix = "0x" in
      if String.is_prefix bytes_code ~prefix then
        (true, String.chop_prefix_exn bytes_code ~prefix)
      else (false, bytes_code)
    in
    let with_zero_five, bytes =
      let prefix = "05" in
      if String.is_prefix bytes ~prefix then
        (true, String.chop_prefix_exn bytes ~prefix)
      else (false, bytes)
    in
    let bytes =
      String.filter bytes ~f:(function ' ' | '\n' | '\t' -> false | _ -> true)
    in
    (with_zero_x, with_zero_five, bytes)

  let code_block s = pre (ct s)

  let show_hex ctxt bytes_code =
    let with_zero_x, with_zero_five, bytes = explode_hex bytes_code in
    let header, result, valid_pack =
      match Michelson_bytes.parse_hex_bytes bytes with
      | Ok (json, concrete) ->
          let header =
            big_answer `Ok (t "This hexa-blob was successfully parsed 🏆")
          in
          let result =
            h4 (t "As Concrete Syntax")
            % code_block concrete
            % h4 (t "As JSON")
            % code_block (Ezjsonm.value_to_string ~minify:false json)
          in
          (header, result, true)
      | Error el ->
          ( big_answer `Error (t "There were parsing/validation errors:"),
            Bootstrap.p_lead
              (t "Failed to parse" %% ct (bytes_summary bytes) % t ":")
            %% Tezos_html.error_trace ctxt el,
            false )
    in
    let items =
      let opt_if c v = if c then Some v else None in
      List.filter_opt
        [
          opt_if with_zero_x
            (t "The prefix" %% ct "0x" %% t "just means “this is hexadecimal”.");
          opt_if with_zero_five
            (t "The first byte" %% ct "05"
            %% t "is the standard prefix/watermark for Michelson expressions.");
          opt_if valid_pack
            (t "The bytes"
            %% ct (bytes_summary bytes)
            %% t "are a valid" %% ct "PACK" % t "-ed expression.");
        ]
    in
    header % itemize items % result

  let process_micheline ctxt inp =
    try
      let o =
        Tezai_michelson.Concrete_syntax.parse_exn
          ~check_indentation:(State.check_micheline_indentation ctxt)
          ~check_primitives:false inp
      in
      let concrete = Tezai_michelson.Concrete_syntax.to_string o in
      let json = Tezai_michelson.Untyped.to_json o in
      let packed =
        try
          (* The packing can raise for missing primitives. *)
          Ok (Tezai_michelson.Pack.pack o)
          (* Michelson_bytes.pack_node_expression o) *)
        with e -> Error e
      in
      Ok (concrete, json, packed)
    with e -> Error [ Tezos_error_monad.Error_monad.Exn e ]

  let show_michelson ctxt inp =
    let header, result =
      match process_micheline ctxt inp with
      | Ok (concrete, json, packed) ->
          let header =
            big_answer `Ok (t "This Micheline was successfully parsed 🥊")
          in
          let pack_result =
            h4 (t "Serialization" %% parens (ct "PACK"))
            %
            match packed with
            | Ok packed ->
                code_block
                  (let (`Hex hx) = Hex.of_string packed in
                   "0x05" ^ hx)
            | Error e ->
                Bootstrap.color `Danger (bt "Failed:")
                %% div (Errors_html.exception_html ctxt e)
          in
          let result =
            h4 (t "Reindented")
            % code_block concrete
            % h4 (t "As JSON")
            % code_block (Ezjsonm.value_to_string ~minify:false json)
            % pack_result
          in
          (header, result)
      | Error el ->
          ( big_answer `Error (t "There were parsing/validation errors:"),
            Tezos_html.error_trace ctxt el )
    in
    header % result

  let show_binary_info ctxt (kind : guess) input_bytes =
    let packed_mich =
      if Poly.(kind = Format `Michelson) then
        match process_micheline ctxt input_bytes with
        | Error _ -> None
        | Ok (_, _, packed) -> (
            match packed with Ok o -> Some ("\x05" ^ o) | Error _ -> None)
      else None
    in
    let binary_from_hex =
      if Poly.(kind = Format `Hex) then
        try
          let _, with05, hex_bytes = explode_hex input_bytes in
          let bin = Hex.to_string (`Hex hex_bytes) in
          Some (if with05 then "\x05" ^ bin else bin)
        with _ -> None
      else None
    in
    let sizing =
      Bootstrap.Table.simple
        ~header_row:[ empty (); t "Size"; t "Delphi Burn" ]
        (let row l = H5.tr (List.map ~f:td l) in
         let sizes =
           let lif c k v = try if c then [ (k, v ()) ] else [] with _ -> [] in
           let lif_opt o k v =
             match o with Some s -> [ (k, v s) ] | None -> []
           in
           [ ("Raw Input", String.length input_bytes) ]
           @ lif
               Poly.(kind = Format `Metadata_json)
               "Minimized-JSON"
               (fun () ->
                 Ezjsonm.value_from_string input_bytes
                 |> Ezjsonm.value_to_string ~minify:true
                 |> String.length)
           @ lif_opt binary_from_hex "Binary" String.length
           @ lif_opt packed_mich "PACK-ed" (fun packed -> String.length packed)
         in
         List.fold ~init:(empty ()) sizes ~f:(fun prev (label, bytes) ->
             let ppbig ppf i =
               let open Fmt in
               pf ppf "%s" (Int.to_string_hum i ~delimiter:' ')
             in
             (* carthage: 1 militez / byte
                http://mainnet.smartpy.io/chains/main/blocks/head/context/constants
                "cost_per_byte":"1000"
                http://delphinet.smartpy.io/chains/main/blocks/head/context/constants
                "cost_per_byte":"250"
             *)
             prev
             % row
                 [
                   t label;
                   Fmt.kstr t "%a B" ppbig bytes;
                   Fmt.kstr t "%a μꜩ" ppbig (bytes * 250);
                 ]))
    in
    let hashes =
      (* let _item k bytes =
         let hash n v = Fmt.kstr it "%s:" n %% ct v in
         t k
         % itemize
             [ hash "Ledger-BLAKE2B-Base58"
                 (Base58.raw_encode (B58_hashes.blake2b bytes))
             ; hash "BLAKE2B-Hex" (hex (B58_hashes.blake2b bytes))
             ; hash "SHA256-Hex" (hex (B58_hashes.B58_crypto.sha256 bytes))
             ; hash "SHA512-Hex" (hex (B58_hashes.B58_crypto.sha512 bytes))
             ; hash "Script-ID-hash (big-map access)"
                 (B58_hashes.b58_script_id_hash bytes) ] in *)
      let hrow c =
        H5.(tr [ th ~a:[] (* a_colspan (Reactive.pure 2) *) [ c ] ])
      in
      let row k v = H5.(tr [ td [ k; br (); v ] ]) in
      let make_item k bytes hashes =
        hrow (t k)
        % list
            (List.map hashes ~f:(fun hash ->
                 let n, v = hash bytes in
                 row (t n) (ct v)))
      in
      let hash k b = (k, b) in
      let hex s =
        let (`Hex x) = Hex.of_string s in
        x
      in
      let ldgr bytes =
        hash "Ledger-BLAKE2B-Base58"
          Tezai_base58_digest.(
            Crypto_hash.String.blake2b ~size:32 bytes
            |> Raw.String.No_checksum.to_base58)
      in
      let blake2b bytes =
        hash "BLAKE2B-Hex"
          (hex (Tezai_base58_digest.Crypto_hash.String.blake2b ~size:32 bytes))
      in
      let sha256 bytes =
        hash "SHA256-Hex"
          (hex (Tezai_base58_digest.Crypto_hash.String.sha256 bytes))
      in
      let sha512 bytes =
        hash "SHA512-Hex"
          (hex (Tezai_base58_digest.Crypto_hash.String.sha512 bytes))
      in
      let expr58 bytes =
        hash "Script-ID-hash (big-map access)"
          Tezai_base58_digest.Identifier.Script_expr_hash.(
            hash_string (* B58_hashes.b58_script_id_hash *) bytes |> encode)
      in
      let items = ref [] in
      let item k b h = items := make_item k b h :: !items in
      item "Raw Input" input_bytes [ ldgr; blake2b; sha256; sha512 ];
      Option.iter binary_from_hex ~f:(fun bin ->
          item "Binary-Michelson-Expression (with watermark)" bin
            [ ldgr; blake2b; sha256; sha512; expr58 ]);
      Option.iter packed_mich ~f:(fun packed ->
          item "Binary-Michelson-Expression (with watermark)" packed
            [ ldgr; blake2b; sha256; sha512; expr58 ]);
      Bootstrap.Table.simple (list (List.rev !items))
    in
    Bootstrap.bordered ~kind:`Secondary
      (Bootstrap.container (sizing % div hashes))

  let page ctxt =
    let content = State.editor_content ctxt in
    let guess_validate input =
      let _logs = ref [] in
      let log m = _logs := m :: !_logs in
      let res =
        List.find_map guessers ~f:(fun f -> f ~log input)
        |> Option.value ~default:Failed
      in
      (input, res, List.rev !_logs)
    in
    let format_result =
      Reactive.(Bidirectional.get content ** State.editor_mode ctxt)
      |> Reactive.map ~f:(function
           | input, `Guess -> (`Guess, guess_validate input)
           | input, (#State.Editor_mode.format as fmt) ->
               (fmt, (input, Format fmt, [])))
    in
    let display_guess =
      Reactive.bind format_result ~f:(function
        | `Guess, (_, kind, _) ->
            let normal c = Bootstrap.color `Secondary c in
            div
              ~a:
                [
                  H5.a_style
                    (Reactive.pure "width: 12em; display: inline-block");
                ]
              (match kind with
              | Empty -> normal (t "The editor is Empty.")
              | Format m ->
                  normal (t "Using mode" %% ct (State.Editor_mode.to_string m))
              | Failed ->
                  Bootstrap.color `Danger (t "Failed to guess a format."))
        | _ -> empty ())
    in
    let result =
      (* We keep the [make ()]s outside the bind to that they remember their
         state: *)
      let collapse_binary = Bootstrap.Collapse.make () in
      let collapse_logs = Bootstrap.Collapse.make () in
      Reactive.bind format_result ~f:(fun (_, (inp, kind, logs)) ->
          let show_logs, logs =
            match logs with
            | [] -> (empty (), empty ())
            | _ :: _ ->
                let open Bootstrap.Collapse in
                ( make_button collapse_logs ~kind:`Secondary
                    ~style:(Reactive.pure (Fmt.str "width: 8em"))
                    (Reactive.bind (collapsed_state collapse_logs) ~f:(function
                      | true -> t "Show Logs"
                      | false -> t "Hide Logs")),
                  make_div collapse_logs (fun () ->
                      Bootstrap.terminal_logs
                        (itemize (List.map logs ~f:(Message_html.render ctxt))))
                )
          in
          let binary_info_button, binary_info =
            let open Bootstrap.Collapse in
            ( make_button collapse_binary ~kind:`Secondary
                ~style:(Reactive.pure (Fmt.str "width: 12em"))
                (Reactive.bind (collapsed_state collapse_binary) ~f:(function
                  | true -> t "Show Binary Info"
                  | false -> t "Hide Binary Info")),
              make_div collapse_binary (fun () ->
                  show_binary_info ctxt kind inp) )
          in
          let header =
            div
              (show_logs %% display_guess %% binary_info_button %% logs
             %% binary_info)
          in
          match kind with
          | Empty ->
              header
              %% div
                   ~a:
                     [
                       classes [ "mx-auto" ];
                       H5.a_style (Reactive.pure "width: 50%");
                     ]
                   (span
                      ~a:
                        [
                          H5.a_style
                            (Reactive.pure "font-size: 3000%; opacity: 0.1");
                        ]
                      (t "ꜩ"))
          | Format fmt -> (
              header
              %
              match fmt with
              | `Metadata_json ->
                  Tezos_html.show_metadata_full_validation ctxt inp
                    ~add_open_in_editor_button:false
                    ~add_explore_tokens_button:false
                    ~show_validation_big_answer:true
              | `Uri -> show_uri ctxt inp
              | `Hex -> show_hex ctxt inp
              | `Michelson -> show_michelson ctxt inp)
          | Failed ->
              header
              % h4 (t "Don't know how to validate this")
              % pre ~a:[ classes [ "pre-scrollable" ] ] (ct inp))
    in
    let local_storage_button =
      let label = t "Local-Storage" in
      let button_kind = `Light (* default for dropdowns *) in
      match Local_storage.available ctxt with
      | true ->
          Bootstrap.Dropdown_menu.(
            button label ~kind:button_kind
              [
                item
                  ~action:(fun () -> State.save_editor_content ctxt)
                  (t "Save");
                item
                  ~action:(fun () -> State.load_editor_content ctxt)
                  (t "Load");
              ])
      | false ->
          Bootstrap.button ~kind:button_kind ~disabled:true ~action:Fn.ignore
            (abbreviation
               "Local storage is not available for this browser/site combo."
               label)
    in
    let editor_function_messages = Reactive.var [] in
    let editor_message m =
      Reactive.set editor_function_messages
        (m :: Reactive.peek editor_function_messages)
    in
    let editor_function_buttons =
      let json_formatter ~minify () =
        try
          State.transform_editor_content ctxt ~f:(fun x ->
              let v = Ezjsonm.value_from_string x in
              Ezjsonm.value_to_string ~minify v)
        with _ ->
          let verb = if minify then "minify" else "re-indent" in
          editor_message
            Message.(t "Failed to" %% t verb % t ", the JSON has to be valid.")
      in
      Reactive.bind format_result ~f:(fun (_, (_, kind, _)) ->
          match kind with
          | Empty -> empty ()
          | Failed -> empty ()
          | Format `Metadata_json ->
              Bootstrap.button ~kind:`Info (t "Reindent JSON") ~outline:true
                ~action:(json_formatter ~minify:false)
              %% Bootstrap.button ~kind:`Info (t "Minify JSON") ~outline:true
                   ~action:(json_formatter ~minify:true)
          | Format _ -> empty ())
    in
    let editor =
      div
        (Examples_dropdown.editable ctxt ~action:(fun v ->
             Reactive.Bidirectional.set content v)
        % Bootstrap.Dropdown_menu.(
            button
              (t "Mode:"
              %% (State.editor_mode ctxt
                 |> Reactive.bind ~f:(fun m ->
                        ct (State.Editor_mode.to_string m))))
              (header (t "Editor Validation Modes")
              :: List.map State.Editor_mode.all ~f:(fun m ->
                     item
                       ~action:(fun () -> State.set_editor_mode ctxt m)
                       (ct (State.Editor_mode.to_string m)
                       %% t "→"
                       %% State.Editor_mode.explain m))))
        % local_storage_button % editor_function_buttons)
      % Reactive.bind_var editor_function_messages ~f:(function
          | [] -> empty ()
          | more ->
              Bootstrap.alert ~kind:`Danger
                (Bootstrap.close_button ~action:(fun () ->
                     Reactive.set editor_function_messages [])
                %% itemize (List.map more ~f:(Message_html.render ctxt))))
      % H5.(
          div
            [
              textarea
                ((* txt *) Reactive.Bidirectional.get content)
                ~a:
                  [
                    a_style (Lwd.pure "font-family: monospace");
                    classes [ "col-12" ];
                    a_rows (Lwd.pure 50);
                    a_oninput
                      (Tyxml_lwd.Lwdom.attr
                         Js_of_ocaml.(
                           fun ev ->
                             Js.Opt.iter ev##.target (fun input ->
                                 Js.Opt.iter (Dom_html.CoerceTo.textarea input)
                                   (fun input ->
                                     let v = input##.value |> Js.to_string in
                                     dbgf "TA inputs: %d bytes: %S"
                                       (String.length v) v;
                                     Reactive.Bidirectional.set content v));
                             true));
                  ];
            ])
    in
    div
      ~a:[ classes [ "row" ] ]
      (Reactive.bind (Browser_window.width ctxt) ~f:(function
        | Some `Wide ->
            div ~a:[ classes [ "col-6" ] ] editor
            % div ~a:[ classes [ "col-6" ] ] result
        | Some `Thin | None ->
            let visible = Reactive.var `Editor in
            let tabs =
              let open Bootstrap.Tab_bar in
              make
                [
                  item (t "Editor")
                    ~active:
                      (Reactive.get visible
                      |> Reactive.map ~f:(function
                           | `Editor -> false
                           | `Result -> true))
                    ~action:(fun () -> Reactive.set visible `Editor);
                  item (t "Results")
                    ~active:
                      (Reactive.get visible
                      |> Reactive.map ~f:(function
                           | `Editor -> true
                           | `Result -> false))
                    ~action:(fun () -> Reactive.set visible `Result);
                ]
            in
            div
              ~a:[ classes [ "col-12" ] ]
              (tabs
              %% Reactive.bind_var visible ~f:(function
                   | `Editor -> editor
                   | `Result -> result))))
end

module Explorer = struct
  let validate_intput input_value =
    match Tezai_base58_digest.Identifier.Kt1_address.check input_value with
    | () -> `KT1 input_value
    | exception _ when String.is_prefix input_value ~prefix:"KT" ->
        `Error
          ( input_value,
            [ Tezos_error_monad.Error_monad.failure "Invalid KT1 address" ] )
    | exception _ -> (
        match Contract_metadata.Uri.validate input_value with
        | Ok uri, _ -> `Uri (input_value, uri)
        | Error e, _ -> `Error (input_value, e))

  let input_valid state =
    Reactive.map (State.explorer_input state) ~f:validate_intput

  let input_validation_status state =
    let open Meta_html in
    let cct txt = Bootstrap.monospace (Fmt.kstr t "‘%s’" txt) in
    Reactive.bind (input_valid state) ~f:(function
      | `KT1 k ->
          cct k %% t "is a valid KT1 address" |> Bootstrap.color `Success
      | `Uri (txt, _) ->
          cct txt %% t "is a valid TZIP-16 URI" |> Bootstrap.color `Success
      | `Error ("", _) -> t "Can be a metadata URI or a contract address."
      | `Error (txt, _) ->
          cct txt %% t "is a not a valid address nor a TZIP-16 URI"
          |> Bootstrap.color `Danger)

  let full_input_quick _ctxt =
    let open Meta_html in
    function
    | `KT1 k -> t "Contract" %% Tezos_html.Block_explorer.kt1_display k
    | `Uri (u, _) -> t "URI" %% ct u
    | `Error (m, _) -> t "Erroneous input:" %% Fmt.kstr ct "%S" m

  let full_input_bloc ctxt full_input =
    let open Meta_html in
    h4 (t "Understood Input") % div (full_input_quick ctxt full_input)

  let uri_and_metadata_result ?token_metadata_big_map ctxt ~uri ~metadata
      ~full_input =
    let open Meta_html in
    full_input_bloc ctxt full_input
    % h4 (t "Metadata Location")
    % Tezos_html.metadata_uri ctxt uri
    % h4 (t "Metadata Contents")
    % Tezos_html.show_metadata_full_validation ctxt ?token_metadata_big_map
        metadata ~add_open_in_editor_button:true ~add_explore_tokens_button:true
        ~show_validation_big_answer:false

  let uri_ok_but_metadata_failure ctxt ~uri ~metadata_json ~error ~full_input =
    let open Meta_html in
    full_input_bloc ctxt full_input
    % Fmt.kstr ct "Partially failed"
    (* Tezos_contract_metadata.Metadata_uri.pp uri *)
    % h4 (t "Metadata Location")
    % Tezos_html.metadata_uri ctxt uri
    % h4 (t "Wrong Metadata Content")
    %% Tezos_html.open_in_editor ctxt metadata_json
    %% pre (ct metadata_json)
    %% Tezos_html.error_trace ctxt error

  let uri_there_but_wrong ctxt ~full_input ~uri_string:_ ~error =
    let open Meta_html in
    full_input_bloc ctxt full_input
    % h4 (t "Invalid URI")
    %% Tezos_html.error_trace ctxt error

  let uri_failed_to_fetch ctxt ~uri ~error ~full_input =
    let open Meta_html in
    full_input_bloc ctxt full_input
    % h4 (t "Metadata Location")
    % Tezos_html.metadata_uri ctxt uri
    % h4 (t "Failed To Resolve URI")
    % Errors_html.exception_html ctxt error

  let go_action ctxt =
    let open Meta_html in
    let result = State.explorer_result ctxt in
    let input_value = State.explorer_input_value ctxt in
    dbgf "Form submitted with %s" input_value;
    Async_work.reinit result;
    Async_work.wip result;
    Async_work.log result (t "Starting with: " %% ct input_value);
    Async_work.async_catch result
      ~exn_to_html:(Errors_html.exception_html ctxt)
      Lwt.Infix.(
        fun ~mkexn () ->
          let logs prefix s =
            Async_work.log result
              (it prefix %% t "→" %% Bootstrap.monospace (t s))
          in
          let full_input = validate_intput input_value in
          let on_uri ctxt ?token_metadata_big_map uri =
            Lwt.catch
              (fun () ->
                Contract_metadata.Uri.fetch ctxt uri
                  ~log:(logs "Fetching Metadata"))
              (fun e ->
                raise
                  (mkexn (uri_failed_to_fetch ctxt ~full_input ~uri ~error:e)))
            >>= fun json_code ->
            dbgf "before of-json";
            match Contract_metadata.Content.of_json json_code with
            | Ok (_, _) ->
                Async_work.ok result
                  (uri_and_metadata_result ctxt ~full_input ~uri
                     ?token_metadata_big_map ~metadata:json_code);
                Lwt.return ()
            | Error error ->
                raise
                  (mkexn
                     (uri_ok_but_metadata_failure ctxt ~uri ~full_input
                        ~metadata_json:json_code ~error))
          in
          match full_input with
          | `KT1 address -> (
              Query_nodes.metadata_value ctxt ~address ~key:""
                ~log:(logs "Getting URI")
              >>= fun metadata_uri ->
              Contract_metadata.Uri.Fetcher.set_current_contract ctxt address;
              Async_work.log result (t "Now going for: " %% ct metadata_uri);
              Lwt.catch
                (fun () ->
                  Contract_metadata.Content.token_metadata_value ctxt ~address
                    ~key:""
                    ~log:(logs "Getting Token Metadata")
                  >>= fun token_metadata -> Lwt.return_some token_metadata)
                (fun exn ->
                  Async_work.log result
                    (t "Attempt at getting a %token_metadata big-map failed:"
                    %% Errors_html.exception_html ctxt exn);
                  Lwt.return_none)
              >>= fun token_metadata_big_map ->
              match Contract_metadata.Uri.validate metadata_uri with
              | Ok uri, _ -> on_uri ctxt uri ?token_metadata_big_map
              | Error error, _ ->
                  raise
                    (mkexn
                       (uri_there_but_wrong ctxt ~uri_string:metadata_uri
                          ~full_input ~error)))
          | `Uri (_, uri) ->
              if Contract_metadata.Uri.needs_context_address uri then
                Async_work.log result
                  (bt "This URI requires a context KT1 address …");
              System.slow_step ctxt >>= fun () -> on_uri ctxt uri
          | `Error (_, el) -> raise (mkexn (Tezos_html.error_trace ctxt el)))

  let page ctxt =
    let open Meta_html in
    let result = State.explorer_result ctxt in
    Query_nodes.Update_status_loop.ensure ctxt;
    h2 (t "Contract Metadata Explorer")
    % Bootstrap.Form.(
        let enter_action () = go_action ctxt in
        State.if_explorer_should_go ctxt enter_action;
        make ~enter_action
          [
            row
              [
                cell 2 (magic (Examples_dropdown.explorable ctxt));
                cell 8
                  (input
                     ~placeholder:
                       (Reactive.pure
                          "Enter a contract address or a metadata URI")
                     (State.explorer_input_bidirectional ctxt)
                     ~help:(input_validation_status ctxt));
                cell 2
                  (submit_button (t "Go!")
                     ~active:
                       Reactive.(
                         map
                           (input_valid ctxt ** Async_work.busy result)
                           ~f:(function
                             | `Error _, _ -> false
                             | _, true -> false
                             | _ -> true))
                     enter_action);
              ];
          ])
    % Async_work.render result ~f:Fn.id
end

let root_document state =
  let open Meta_html in
  let editor = lazy (Editor.page state) in
  let settings = lazy (Settings_page.render state) in
  let about = lazy (about_page state) in
  Bootstrap.container ~suffix:"-fluid"
    (navigation_menu state
    % Reactive.bind (State.page state)
        ~f:
          State.Page.(
            function
            | `Changing_to p ->
                Lwt.async
                  Lwt.Infix.(
                    fun () ->
                      Js_of_ocaml_lwt.Lwt_js.yield () >>= fun () ->
                      State.set_page state (`Page p) ();
                      Lwt.return_unit);
                div
                  H5.(
                    img ()
                      ~a:[ a_width (Reactive.pure 100) ]
                      ~src:(Reactive.pure "loading.gif")
                      ~alt:(Reactive.pure "Loading spinner GIF"))
            | `Page Explorer -> Explorer.page state
            | `Page Editor -> Lazy.force editor
            | `Page Token_viewer -> Token_viewer.render state
            | `Page Settings -> Lazy.force settings
            | `Page About -> Lazy.force about))
