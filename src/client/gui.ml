open Import

module Message_html = struct
  open Meta_html

  let render _ m =
    let module M = Message in
    let rec msg = function
      | M.Text s -> t s
      | M.Inline_code c -> ct c
      | M.Code_block b -> pre (ct b)
      | M.List l -> List.fold ~init:(empty ()) ~f:(fun a b -> a % msg b) l in
    msg m
end

module Errors_html = struct
  open Meta_html

  type handler = exn -> (Html_types.li_content Meta_html.t * exn list) option

  let exception_html ?(handlers : handler list = []) ctxt exn =
    let rec construct = function
      | Decorate_error.E {message; trace} ->
          let trace_part =
            match trace with
            | [] -> empty ()
            | more ->
                let collapse = Bootstrap.Collapse.make () in
                Bootstrap.Collapse.fixed_width_reactive_button_with_div_below
                  collapse ~width:"12em" ~kind:`Secondary
                  ~button:(function
                    | true -> t "Show Error Trace"
                    | false -> t "Hide Error Trace")
                  (fun () -> itemize (List.map more ~f:construct)) in
          Message_html.render ctxt message % trace_part
      | Failure s -> t "Failure:" %% t s
      | e -> (
        match List.find_map handlers ~f:(fun f -> f e) with
        | Some (m, []) -> m
        | Some (m, more) -> m % itemize (List.map more ~f:construct)
        | None -> t "Exception:" % pre (Fmt.kstr ct "%a" Exn.pp e) ) in
    bt "Error:" %% construct exn
end

module Block_explorer = struct
  type vendor = Smartpy | Bcd

  let all_vendors = [Smartpy; Bcd]

  let kt1_url vendor kt1 =
    match vendor with
    | Smartpy -> Fmt.str "https://smartpy.io/dev/explorer.html?address=%s" kt1
    | Bcd -> Fmt.str "https://better-call.dev/search?text=%s" kt1

  let vendor_show_name = function Smartpy -> "SmartPy" | Bcd -> "BCD"

  let kt1_display kt1 =
    let open Meta_html in
    let sep () = t ", " in
    Bootstrap.monospace (bt kt1)
    %% small
         (parens
            (list
               (oxfordize_list ~map:Fn.id ~sep ~last_sep:sep
                  (List.map all_vendors ~f:(fun v ->
                       let target = kt1_url v kt1 in
                       link ~target (t (vendor_show_name v)))))))
end

let tzcomet_link () =
  let open Meta_html in
  link ~target:"https://github.com/tqtezos/TZComet" (t "TZComet")

let navigation_menu state =
  let open State in
  let open Page in
  let open Meta_html in
  let fragment = make_fragment state in
  let fragment_self = Reactive.map ~f:Fragment.to_string fragment in
  let fragment_page p =
    Reactive.map
      ~f:(fun frg -> Fragment.(to_string (change_for_page frg p)))
      fragment in
  Bootstrap.Navigation_bar.(
    make
      ~brand:
        (Bootstrap.label `Dark
           ( tzcomet_link ()
           %% Reactive.bind fragment_self (fun f ->
                  link (t "ʘ") ~target:("#" ^ f))
           %% Reactive.bind (State.dev_mode state) (function
                | true -> it "(dev)"
                | false -> empty ()) ))
      (let of_page p =
         item
           (bt (Page.to_string p))
           ~active:(State.current_page_is_not state p)
           ~action:(State.set_page state (`Changing_to p))
           ~fragment:(fragment_page p) in
       List.map ~f:of_page all_in_order))

let about_page state =
  let open Meta_html in
  let open State in
  let p = Bootstrap.p_lead in
  h2 (t "TZComet")
  % p
      ( t "This is" %% tzcomet_link ()
      %% ( match state#version_string with
         | None -> it "unknown version"
         | Some vs ->
             t "version "
             %% link
                  ~target:
                    (Fmt.str "https://github.com/tqtezos/TZComet/commit/%s" vs)
                  (it vs) )
      % Reactive.bind (State.dev_mode state) (function
          | true -> t " (in “dev” mode)."
          | false -> t ".") )
  % p (t "An explorer/editor/validator/visualizer for Tezos contract metadata.")
  % p
      ( t "The source for this webpage is available on Github:"
      %% link ~target:"https://github.com/tqtezos/TZComet"
           (ct "tqtezos/TZComet")
      % t "." )
  % p
      ( t "The Contract Metadata standard, a.k.a. TZIP-16, is at: "
      % url ct
          "https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-16/tzip-16.md"
      % t "." )
  % Reactive.bind (State.dev_mode state) (function
      | false -> empty ()
      | true ->
          h2 (t "Dev-mode Junk:")
          % p (t "This is also a test/experiment in UI writing …")
          % Meta_html.Example.e1 ())

module Settings_page = struct
  let nodes_form ctxt =
    let open Meta_html in
    Bootstrap.Table.simple
      ~header_row:
        [ t "Name"; t "URI-Prefix"; t "Status"
        ; t "Latest Ping"
          %% Reactive.bind
               (Query_nodes.loop_status ctxt)
               ~f:
                 (let m s = i (parens (t s)) in
                  function
                  | `Not_started -> m "ping-loop not started"
                  | `In_progress -> m "ping-loop in progress"
                  | `Sleeping -> m "ping-loop sleeping") ]
      (let row l = H5.tr (List.map ~f:td l) in
       let node_status =
         let m kind s = Bootstrap.color kind (Bootstrap.monospace (t s)) in
         Query_nodes.Node_status.(
           function
           | Uninitialized -> m `Warning "Uninitialized"
           | Non_responsive e ->
               let collapse = Bootstrap.Collapse.make () in
               m `Danger "Non-responsive"
               % Bootstrap.Collapse.fixed_width_reactive_button_with_div_below
                   collapse ~width:"12em" ~kind:`Secondary
                   ~button:(function
                     | true -> t "Show Error" | false -> t "Hide Error")
                   (fun () -> Errors_html.exception_html ctxt e)
           | Ready _ -> m `Success "Ready") in
       let ping_date date =
         if Float.(date < 10.) then (* Construction sign: *) t "🚧"
         else
           let date_string =
             (new%js Js_of_ocaml.Js.date_fromTimeValue date)##toISOString
             |> Js_of_ocaml__Js.to_string in
           Bootstrap.monospace (t date_string) in
       let row_of_node n =
         row
           Query_nodes.Node.
             [ it n.name; ct n.prefix
             ; Reactive.bind (status n) ~f:(fun (_, s) -> node_status s)
             ; Reactive.bind (status n) ~f:(fun (f, _) -> ping_date f) ] in
       let last_row =
         let name = Reactive.var "" in
         let nameb = Reactive.Bidirectional.of_var name in
         let prefix = Reactive.var "" in
         let prefixb = Reactive.Bidirectional.of_var prefix in
         row
           [ input_bidirectional nameb
               ~a:
                 [ H5.a_placeholder (Reactive.pure "Name")
                 ; classes ["form-control"] ]
           ; input_bidirectional prefixb
               ~a:
                 [ H5.a_placeholder (Reactive.pure "URL-Prefix")
                 ; classes ["form-control"] ]
           ; Bootstrap.button (t "⇐ Add/replace node (by name)")
               ~kind:`Secondary ~action:(fun () ->
                 Query_nodes.add_node ctxt
                   (Query_nodes.Node.create (Reactive.peek name)
                      (Reactive.peek prefix)) ;
                 Reactive.Bidirectional.set nameb "" ;
                 Reactive.Bidirectional.set prefixb "" ;
                 ())
           ; Bootstrap.button (t "⇑ Ping'em'all") ~kind:`Secondary
               ~action:(fun () ->
                 Query_nodes.Update_status_loop.ensure ctxt ;
                 Query_nodes.Update_status_loop.wake_up ctxt) ] in
       Reactive.bind (Query_nodes.get_nodes ctxt ~map:row_of_node)
         ~f:(fun nodes -> list nodes)
       % last_row)

  let render ctxt =
    let open Meta_html in
    let open State in
    let timeout_valid_and_changed = Reactive.var None in
    let timeout =
      Reactive.Bidirectional.make
        (System.http_timeout_peek ctxt |> Fmt.str "%f" |> Reactive.pure)
        (fun x ->
          match Float.of_string x with
          | f ->
              System.set_http_timeout ctxt f ;
              Reactive.set timeout_valid_and_changed
                (Some (t "Timeout set to " % Fmt.kstr ct "%f" f))
          | exception _ ->
              Reactive.set timeout_valid_and_changed
                (Some
                   (Bootstrap.color `Danger
                      ( t "Timeout cannot be set to"
                      %% ct x
                      % t ", it should a valid floating-point number." ))))
    in
    h2 (t "Settings")
    % Bootstrap.Form.(
        make
          [ check_box
              (State.dev_mode_bidirectional ctxt)
              ~label:(t "Dev-mode enabled")
              ~help:
                (t
                   "Shows things that regular users should not see and \
                    artificially slows down the application.")
          ; check_box
              (State.check_micheline_indentation_bidirectional ctxt)
              ~label:(t "Check Micheline Indentation")
              ~help:
                ( t
                    "Make the Micheline parser (in the Editor) also check for \
                     proper indentation like"
                %% ct "tezos-client" %% t "does." )
          ; input
              ~placeholder:(Reactive.pure "Number of seconds (with decimals).")
              ~help:
                (Reactive.bind_var timeout_valid_and_changed ~f:(function
                  | None ->
                      t
                        "How long to wait for nodes and gateways to \
                         give/accept data."
                  | Some msg -> msg))
              ~label:(t "HTTP-Call Timeout") timeout ])
    % h3 (t "Tezos Nodes")
    % nodes_form ctxt
end

module Tezos_html = struct
  let uri_parsing_error err =
    let open Meta_html in
    let open Tezos_contract_metadata.Metadata_uri.Parsing_error in
    let details =
      let sha256_host_advice =
        t "The host should look like:"
        %% ct
             "0x5891b5b522d5df086d0ff0b110fbd9d21bb4fc7163af34d08286a2e846f6be03"
        % t "." in
      let scheme_advice =
        t "The URI should start with one of:"
        %% list
             (oxfordize_list
                ["tezos-storage"; "http"; "https"; "sha256"; "ipfs"]
                ~map:(fun sch -> Fmt.kstr ct "%s:" sch)
                ~sep:(fun () -> t ", ")
                ~last_sep:(fun () -> t ", or "))
        % t "." in
      match err.error_kind with
      | Wrong_scheme None -> t "Missing URI scheme. " % scheme_advice
      | Wrong_scheme (Some scheme) ->
          t "Unknown URI scheme: " % ct scheme % t "." %% scheme_advice
      | Missing_cid_for_ipfs ->
          t "Missing content identifier in IPFS URI, it should be the host."
      | Wrong_tezos_storage_host str ->
          t "Cannot parse the “host” part of the URI: "
          %% ct str % t ", should look like " %% ct "<network>.<address>"
          % t " or just" %% ct "<address>"
      | Forbidden_slash_in_tezos_storage_path path ->
          t "For " %% ct "tezos-storage"
          % t " URIs, the “path” cannot contain any "
          % ct "/"
          % t " (“slash”) character: "
          % ct path
      | Missing_host_for_hash_uri `Sha256 ->
          t "Missing “host” in " % ct "sha256://" % t " URI. "
          %% sha256_host_advice
      | Wrong_hex_format_for_hash {hash= `Sha256; host; message} ->
          t "Failed to parse the “host” "
          %% ct host % t " in this " %% ct "sha256://" % t " URI: " % t message
          % t " → " %% sha256_host_advice in
    let exploded_uri =
      let u = Uri.of_string err.input in
      let item name opt =
        it name % t ": " % match opt with None -> t "<empty>" | Some s -> ct s
      in
      let item_some name s = item name (Some s) in
      t "The URI is understood this way: "
      % itemize
          [ item "Scheme" (Uri.scheme u); item "Host" (Uri.host u)
          ; item "User-info" (Uri.userinfo u)
          ; item "Port" (Uri.port u |> Option.map ~f:Int.to_string)
          ; item_some "Path" (Uri.path u); item "Query" (Uri.verbatim_query u)
          ; item "Fragment" (Uri.fragment u) ] in
    t "Failed to parse URI:" %% ct err.input % t ":"
    % itemize [details; exploded_uri]

  let json_encoding_error : Errors_html.handler =
    let open Meta_html in
    let open Json_encoding in
    let quote s = Fmt.kstr ct "%S" s in
    let rec go = function
      | Cannot_destruct ([], e) -> go e
      | Cannot_destruct (path, e) ->
          let p =
            Fmt.kstr ct "%a"
              (Json_query.print_path_as_json_path ~wildcards:true)
              path in
          t "At path" %% p % t ":" %% go e
      | Unexpected (u, e) ->
          let article s =
            (* 90% heuristic :) *)
            match s.[0] with
            | 'a' | 'e' | 'i' | 'o' -> t "an"
            | _ -> t "a"
            | exception _ -> t "something" in
          let with_article s = article s %% it s in
          t "Expecting" %% with_article e %% t "but got" %% with_article u
      | No_case_matched l ->
          t "No case matched expectations:" %% itemize (List.map ~f:go l)
      | Bad_array_size (u, e) ->
          Fmt.kstr t "Expecting array of size %d but got %d" e u
      | Missing_field field -> t "Missing field" %% quote field
      | Unexpected_field field -> t "Unexpected field" %% quote field
      | Bad_schema e -> t "Bad schema:" %% go e
      | e -> raise e in
    fun e -> match go e with m -> Some (m % t ".", []) | exception _ -> None

  let simple_exns_handler : Errors_html.handler =
    let open Meta_html in
    let some m = Some (m, []) in
    function
    | Ezjsonm.Parse_error (json_value, msg) ->
        some
          ( Fmt.kstr t "JSON Parsing Error: %s, JSON:" msg
          % pre (code (t (Ezjsonm.value_to_string ~minify:false json_value))) )
    | Data_encoding.Binary.Read_error e ->
        some
          (Fmt.kstr t "Binary parsing error: %a."
             Data_encoding.Binary.pp_read_error e)
    | _ -> None

  let single_error ctxt =
    let open Meta_html in
    let open Tezos_error_monad.Error_monad in
    function
    | Exn other_exn ->
        Errors_html.exception_html ctxt other_exn
          ~handlers:[json_encoding_error; simple_exns_handler]
    | Tezos_contract_metadata.Metadata_uri.Contract_metadata_uri_parsing
        parsing_error ->
        uri_parsing_error parsing_error
    | other ->
        pre
          (code
             (Fmt.kstr t "%a" Tezos_error_monad.Error_monad.pp_print_error
                [other]))

  let error_trace ctxt =
    let open Meta_html in
    function
    | [] ->
        Bootstrap.alert ~kind:`Danger (t "Empty trace from Tezos-error-monad")
    | [h] -> single_error ctxt h
    | h :: tl ->
        single_error ctxt h
        % div
            ( t "Trace:"
            %% List.fold tl ~init:(empty ()) ~f:(fun p e ->
                   p %% single_error ctxt e) )

  open Meta_html

  let field_head name =
    Fmt.kstr (fun s -> Bootstrap.color `Info (t s)) "%s:" name

  let field name content = field_head name %% content
  let monot s = Bootstrap.monospace (t s)

  let option_field name fieldopt f =
    match fieldopt with None -> [] | Some s -> [field name (f s)]

  let normal_field name x = option_field name (Some ()) (fun () -> x)

  let paragraphs blob =
    let rec go l acc =
      match List.split_while l ~f:(function "" -> false | _ -> true) with
      | ll, [] -> String.concat ~sep:" " ll :: acc
      | ll, _ :: lr -> go lr (String.concat ~sep:" " ll :: acc) in
    go (String.split blob ~on:'\n') []
    |> function
    | [] -> empty ()
    | [one] -> t one
    | more -> List.rev_map more ~f:(fun x -> div (t x)) |> H5.div

  let list_field name field f =
    option_field name (match field with [] -> None | more -> Some more) f

  let protocol s =
    let proto s = abbreviation s (ct (String.prefix s 12)) in
    let known name url =
      span (link ~target:url (it name)) %% t "(" % proto s % t ")" in
    match s with
    | "PsCARTHAGazKbHtnKfLzQg3kms52kSRpgnDY982a9oYsSXRLQEb" ->
        known "Carthage" "http://tezos.gitlab.io/protocols/006_carthage.html"
    | "PsBabyM1eUXZseaJdmXFApDSBqj8YBfwELoxZHHW77EMcAbbwAS" ->
        known "Babylon" "http://tezos.gitlab.io/protocols/005_babylon.html"
    | "PsDELPH1Kxsxt8f9eWbxQeRxkjfbxoqM52jvs5Y5fBxWWh4ifpo" ->
        known "Delphi" "https://blog.nomadic-labs.com/delphi-changelog.html"
    | s -> proto s

  let open_in_editor ctxt text =
    div (small (parens (State.link_to_editor ctxt ~text (t "Open in editor"))))

  let metadata_uri ?(open_in_editor_link = true) ctxt uri =
    let open Tezos_contract_metadata.Metadata_uri in
    let ct = monot in
    let rec go uri =
      match uri with
      | Web u -> t "Web URL:" %% url ct u
      | Ipfs {cid; path} ->
          let gatewayed = Fmt.str "https://gateway.ipfs.io/ipfs/%s%s" cid path in
          field_head "IPFS URI"
          % itemize
              [ field "CID" (ct cid); field "Path" (ct path)
              ; t "(Try " %% url ct gatewayed % t ")" ]
      | Storage {network; address; key} ->
          field_head "In-Contract-Storage"
          % itemize
              [ field "Network"
                  (Option.value_map network
                     ~default:(t "“Current network”.")
                     ~f:ct)
              ; field "Address"
                  (Option.value_map address ~default:(t "“Same contract”.")
                     ~f:Block_explorer.kt1_display)
              ; field "Key in the big-map" (Fmt.kstr ct "%S" key) ]
      | Hash {kind= `Sha256; value; target} ->
          field_head "Hash checked URI"
          % itemize
              [ field "Target" (go target)
              ; field "… should SHA256-hash to"
                  (Fmt.kstr ct "%a" Hex.pp (Hex.of_string value)) ] in
    ( if open_in_editor_link then
      open_in_editor ctxt
        (Tezos_contract_metadata.Metadata_uri.to_string_uri uri)
    else empty () )
    % div (go uri)

  let mich
      (Tezos_contract_metadata.Metadata_contents.Michelson_blob.Micheline m) =
    Michelson.micheline_canonical_to_string m

  let view_result ctxt ~result ~storage ~address ~view ~parameter =
    let open Tezos_contract_metadata.Metadata_contents in
    let open View in
    let open Implementation in
    let open Michelson_storage in
    let expanded =
      try
        let retmf =
          Michelson.Partial_type.of_type ~annotations:view.human_annotations
            view.return_type in
        Michelson.Partial_type.fill_with_value retmf result ;
        match Michelson.Partial_type.render retmf with
        | [] ->
            dbgf "view_result.expanded: empty list!" ;
            bt "This should not be shown"
        | [one] -> one
        | more -> itemize more
      with exn -> Errors_html.exception_html ctxt exn in
    let mich_node = Michelson.micheline_node_to_string in
    Bootstrap.div_lead (div (bt "Result:" %% expanded))
    % hr ()
    % Bootstrap.muted div
        (let items l =
           List.fold l ~init:(empty ()) ~f:(fun p (k, v) ->
               p % div (t k % t ":" %% Bootstrap.monospace (t v))) in
         items
           [ ( "Returned Michelson"
             , Fmt.str "%s : %s" (mich_node result) (mich view.return_type) )
           ; ("Called contract", address)
           ; ("Parameter used", mich_node parameter)
           ; ("Current storage", mich_node storage) ])

  let michelson_view ctxt ~view =
    let open Tezos_contract_metadata.Metadata_contents in
    let open View in
    let open Implementation in
    let open Michelson_storage in
    let {parameter; return_type; code; human_annotations; version} = view in
    let call_mode = Reactive.var false in
    let switch = Bootstrap.button ~kind:`Primary ~outline:true ~size:`Small in
    field_head "Michelson-storage-view"
    % Reactive.bind (Reactive.get call_mode) ~f:(function
        | false ->
            switch ~action:(fun () -> Reactive.set call_mode true) (t "Call")
            % itemize
                ( option_field "Michelson-Version" version protocol
                @ normal_field "Type"
                    (Fmt.kstr ct "%s<contract-storage> → %s"
                       ( match parameter with
                       | None -> ""
                       | Some p -> mich p ^ " × " )
                       (mich return_type))
                @ normal_field "Code"
                    (let concrete = mich code in
                     let lines =
                       1
                       + String.count concrete ~f:(function
                           | '\n' -> true
                           | _ -> false) in
                     if lines <= 1 then ct concrete
                     else
                       let collapse = Bootstrap.Collapse.make () in
                       Bootstrap.Collapse
                       .fixed_width_reactive_button_with_div_below collapse
                         ~width:"12em" ~kind:`Secondary
                         ~button:(function
                           | true -> t "Show Michelson"
                           | false -> t "Hide Michelson")
                         (fun () -> pre (ct concrete)))
                @ list_field "Annotations" human_annotations (fun anns ->
                      itemize
                        (List.map anns ~f:(fun (k, v) ->
                             ct k % t " → " % paragraphs v))) )
        | true ->
            let address =
              Reactive.var
                ( Contract_metadata.Uri.Fetcher.current_contract ctxt
                |> Reactive.peek |> Option.value ~default:"" ) in
            let parameter_input =
              Option.map parameter
                ~f:
                  (Michelson.Partial_type.of_type
                     ~annotations:view.human_annotations) in
            let wip = Async_work.empty () in
            let go_action () =
              Async_work.wip wip ;
              let log s =
                Async_work.log wip
                  (it "Calling view" %% t "→" %% Bootstrap.monospace (t s))
              in
              Async_work.async_catch wip
                ~exn_to_html:(Errors_html.exception_html ctxt)
                Lwt.Infix.(
                  fun ~mkexn () ->
                    let parameter =
                      let open Michelson in
                      match parameter_input with
                      | Some mf ->
                          Michelson.Partial_type.peek mf
                          |> parse_micheline_exn ~check_indentation:false
                               ~check_primitives:false
                      | None ->
                          parse_micheline_exn ~check_indentation:false "Unit"
                            ~check_primitives:false in
                    Query_nodes.call_off_chain_view ctxt ~log
                      ~address:(Reactive.peek address) ~view ~parameter
                    >>= function
                    | Ok (result, storage) ->
                        Async_work.ok wip
                          (view_result ctxt ~result ~storage
                             ~address:(Reactive.peek address) ~view ~parameter) ;
                        Lwt.return ()
                    | Error s ->
                        Async_work.error wip (t "Error:" %% ct s) ;
                        Lwt.return ()) ;
              dbgf "go view" in
            switch ~action:(fun () -> Reactive.set call_mode false) (t "Cancel")
            % ( Async_work.is_empty wip
              |> Reactive.bind ~f:(function
                   | false -> Async_work.render wip ~f:Fn.id
                   | true ->
                       let open Bootstrap.Form in
                       let validate_address input_value =
                         match B58_hashes.check_b58_kt1_hash input_value with
                         | _ -> true
                         | exception _ -> false in
                       let input_valid =
                         Reactive.(
                           match parameter_input with
                           | Some mf -> Michelson.Partial_type.is_valid mf
                           | None -> pure true) in
                       let active =
                         Reactive.(
                           get address ** input_valid
                           |> map ~f:(fun (a, iv) -> validate_address a && iv))
                       in
                       let addr_input =
                         let addr_help a =
                           if validate_address a then
                             Bootstrap.color `Success
                               (t "Thanks, this is valid.")
                           else
                             Bootstrap.color `Danger
                               (t "This is not a valid KT1 address") in
                         [ input
                             ~label:(t "The contract to hit the view with:")
                             ~placeholder:
                               (Reactive.pure
                                  "This requires a contract address …")
                             ~help:
                               Reactive.(
                                 bind
                                   ( get address
                                   ** get
                                        (Contract_metadata.Uri.Fetcher
                                         .current_contract ctxt) )
                                   ~f:(function
                                     | "", _ ->
                                         Bootstrap.color `Danger
                                           (t "Please, we need one.")
                                     | a, None -> addr_help a
                                     | a1, Some a2 ->
                                         if String.equal a1 a2 then
                                           t
                                             "This is the “main-address” \
                                              currently in context."
                                         else addr_help a1))
                             (Reactive.Bidirectional.of_var address) ] in
                       let param_input =
                         match parameter_input with
                         | None -> [magic (t "No parameter")]
                         | Some mf -> Michelson.Partial_type.to_form_items mf
                       in
                       make ~enter_action:go_action
                         ( addr_input @ param_input
                         @ [submit_button ~active (t "Go!") go_action] )) ))

  let michelson_instruction s =
    link (t s)
      ~target:(Fmt.str "https://michelson.nomadic-labs.com/#instr-%s" s)

  let metadata_validation_error ctxt =
    let open Tezos_contract_metadata.Metadata_contents in
    let open Validation.Error in
    let the_off_chain_view view =
      t "The off-chain-view “" % ct view % t "”" in
    function
    | Forbidden_michelson_instruction {view; instruction} ->
        the_off_chain_view view % t " uses a "
        % bt "forbidden Michelson instruction: "
        % michelson_instruction instruction
        % t " (the other forbidden instructions are: "
        % H5.span
            (oxfordize_list
               (List.filter
                  ~f:String.(( <> ) instruction)
                  Validation.Data.forbidden_michelson_instructions)
               ~sep:(fun () -> t ", ")
               ~last_sep:(fun () -> t ", and ")
               ~map:michelson_instruction)
        % t ")."
    | Michelson_version_not_a_protocol_hash {view; value} ->
        the_off_chain_view view
        % t " references a wrong version of Michelson ("
        % ct value
        % t "), it should be a valid protocol hash, like "
        % ct "PsDELPH1Kxsxt8f9eWbxQeRxkjfbxoqM52jvs5Y5fBxWWh4ifpo"
        % t "."

  let metadata_validation_warning ctxt =
    let open Tezos_contract_metadata.Metadata_contents in
    let open Validation.Warning in
    function
    | Wrong_author_format author ->
        t "The author" %% Fmt.kstr ct "%S" author
        %% t "has a wrong format, it should look like"
        %% ct "Print Name <contact-url-or-email>"
        % t "."
    | Unexpected_whitespace {field; value} ->
        t "The field" %% ct field %% t "(=" %% Fmt.kstr ct "%S" value
        % t ") uses confusing white-space characters."
    | Self_unaddressed {view; instruction} ->
        t "The off-chain-view" %% ct view %% t "uses the instruction"
        %% michelson_instruction "SELF"
        %% ( match instruction with
           | None -> t "not followed by any instruction."
           | Some i -> t "followed by" %% michelson_instruction i % t "." )
        %% t "The current recommendation is to only use the combination"
        %% Bootstrap.monospace
             ( michelson_instruction "SELF"
             % t ";"
             %% michelson_instruction "ADDRESS" )
        %% t "in off-chain-views."

  let metadata_substandards ctxt metadata =
    Contract_metadata.Content.(
      match classify metadata with
      | Tzip_16 t -> (t, [])
      | Tzip_12
          { metadata
          ; interface_claim
          ; get_balance
          ; total_supply
          ; all_tokens
          ; is_operator
          ; token_metadata
          ; permissions_descriptor } as t12 ->
          let tzip_12_block =
            let errorify c = Bootstrap.color `Danger c in
            let interface_claim =
              t "Interface claim is"
              %%
              match interface_claim with
              | None -> t "missing." |> errorify
              | Some (`Invalid s) -> t "invalid: " %% ct s |> errorify
              | Some (`Version s) -> t "valid, and defines version as" %% ct s
              | Some `Just_interface -> t "valid" in
            let view_validation ?(missing_add_on = empty) ?(mandatory = false)
                name (v : view_validation) =
              match v with
              | Missing when not mandatory ->
                  t "Optional View" %% ct name %% t "is not there."
                  %% missing_add_on ()
              | Missing ->
                  t "Mandatory View" %% ct name %% t "is missing." |> errorify
              | No_michelson_implementation _ ->
                  t "View" %% ct name
                  %% t "is invalid: it is missing a Michelson implementation."
                  |> errorify
              | Invalid {parameter_status; return_status; _} ->
                  errorify (t "View" %% ct name %% t "is invalid:")
                  %% itemize
                       [ ( t "Parameter type"
                         %%
                         match parameter_status with
                         | `Ok, _ -> t "is ok."
                         | `Wrong, None ->
                             (* This should not happen. *)
                             errorify (t "is wrong: not found.")
                         | `Wrong, Some pt ->
                             errorify
                               ( t "is wrong:"
                               %% ct
                                    (Michelson.micheline_canonical_to_string
                                       pt.original) )
                         | `Unchecked_Parameter, None ->
                             errorify (t "is expectedly not defined.")
                         | `Unchecked_Parameter, Some _ ->
                             errorify (t "is defined while it shouldn't.")
                         | `Missing_parameter, _ -> errorify (t "is missing.")
                         )
                       ; ( t "Return type"
                         %%
                         match return_status with
                         | `Ok, _ -> t "is ok."
                         | `Wrong, None ->
                             (* This should not happen. *)
                             errorify (t "is wrong: not found.")
                         | `Wrong, Some pt ->
                             errorify
                               ( t "is wrong:"
                               %% ct
                                    (Michelson.micheline_canonical_to_string
                                       pt.original) ) ) ]
              | Valid (_, _) -> t "View" %% ct name %% t "is valid" in
            let show_permissions_descriptor pd =
              match pd with
              | None ->
                  t
                    "Permissions-descriptor is not present (assuming default \
                     permissions)."
              | Some (Ok _) -> t "Permissions-descriptor is valid."
              | Some (Error e) ->
                  errorify (t "Permissions-descriptor is invalid:")
                  %% div (error_trace ctxt e) in
            let show_tokens_metadata tm =
              view_validation "token_metadata" tm ~missing_add_on:(fun () ->
                  t
                    "This means that the contract must provide token-specific \
                     metadata using a big-map annotated with"
                  %% ct "%token_metadata" % t ".") in
            let global_validity = Contract_metadata.Content.is_valid t12 in
            let show_validity_btn, validity_div =
              let validity_details () =
                itemize
                  [ interface_claim
                  ; view_validation "get_balance" get_balance ~mandatory:false
                  ; view_validation "total_supply" total_supply
                  ; view_validation "all_tokens" all_tokens
                  ; view_validation "is_operator" is_operator
                  ; show_tokens_metadata token_metadata
                  ; show_permissions_descriptor permissions_descriptor ] in
              let open Bootstrap.Collapse in
              let collapse = make () in
              let btn =
                make_button collapse
                  ~kind:(if global_validity then `Secondary else `Danger)
                  (* ~style:(Reactive.pure (Fmt.str "width: 8em")) *)
                  (Reactive.bind (collapsed_state collapse) ~f:(function
                    | true -> t "Expand Validity Info"
                    | false -> t "Collapse Validity Info")) in
              let dv = make_div collapse (fun () -> validity_details ()) in
              (btn, dv) in
            let wip_explore_tokens = Async_work.empty () in
            let can_enumerate_tokens, explore_tokens_btn =
              match (global_validity, all_tokens) with
              | true, Valid (_, view) ->
                  let action () =
                    Async_work.reinit wip_explore_tokens ;
                    Async_work.wip wip_explore_tokens ;
                    let address =
                      Reactive.var
                        ( Contract_metadata.Uri.Fetcher.current_contract ctxt
                        |> Reactive.peek
                        |> Option.value ~default:"KT1TododoTodo" ) in
                    let log s =
                      Async_work.log wip_explore_tokens
                        ( it "Exploring tokens" %% t "→"
                        %% Bootstrap.monospace (t s) ) in
                    Async_work.async_catch wip_explore_tokens
                      ~exn_to_html:(Errors_html.exception_html ctxt)
                      Lwt.Infix.(
                        fun ~mkexn () ->
                          let call_view_here view ~parameter_string =
                            let parameter =
                              let open Michelson in
                              parse_micheline_exn ~check_indentation:false
                                parameter_string ~check_primitives:false in
                            Query_nodes.call_off_chain_view ctxt ~log
                              ~address:(Reactive.peek address) ~view ~parameter
                            >>= function
                            | Ok (result, _) -> Lwt.return (Ok result)
                            | Error s -> Lwt.return (Error s) in
                          let call_view_or_fail view ~parameter_string =
                            call_view_here view ~parameter_string
                            >>= function
                            | Ok o -> Lwt.return o
                            | Error s ->
                                raise (mkexn (t "Calling view failed" %% ct s))
                          in
                          call_view_or_fail view ~parameter_string:"Unit"
                          >>= fun tokens_mich ->
                          let tokens =
                            match tokens_mich with
                            | Seq (_, nodes) ->
                                List.map nodes ~f:(function
                                  | Int (_, n) -> Z.to_int n
                                  | _ ->
                                      raise
                                        (mkexn
                                           ( t
                                               "Wrong Micheline structure for \
                                                result:"
                                           %% ct
                                                (Michelson
                                                 .micheline_node_to_string
                                                   tokens_mich) )))
                            | _ ->
                                raise
                                  (mkexn
                                     ( t "Wrong Micheline structure for result:"
                                     %% ct
                                          (Michelson.micheline_node_to_string
                                             tokens_mich) )) in
                          Fmt.kstr log "Got list of tokens %a"
                            Fmt.(Dump.list int)
                            tokens ;
                          let explore_token id =
                            let maybe_call_view view_validation
                                ~parameter_string =
                              match view_validation with
                              | Invalid _ | Missing
                               |No_michelson_implementation _ ->
                                  Lwt.return_none
                              | Valid (_, view) ->
                                  call_view_here view ~parameter_string
                                  >>= fun res -> Lwt.return_some res in
                            maybe_call_view token_metadata
                              ~parameter_string:(Int.to_string id)
                            >>= fun metadata_map ->
                            maybe_call_view total_supply
                              ~parameter_string:(Int.to_string id)
                            >>= fun total_supply ->
                            let unpaired_metadata =
                              try
                                let nope = Decorate_error.raise in
                                let ok =
                                  match metadata_map with
                                  | None -> nope Message.(t "Not available")
                                  | Some (Error s) ->
                                      nope
                                        Message.(
                                          t "Error getting view:" %% ct s)
                                  | Some
                                      (Ok
                                        (Prim (_, "Pair", [_; Seq (l, map)], _)))
                                    -> (
                                    match map with
                                    | [] -> []
                                    | Prim
                                        ( _
                                        , "Elt"
                                        , [String (_, s); Bytes (_, b)]
                                        , _ )
                                      :: more ->
                                        List.fold more
                                          ~init:[(s, Bytes.to_string b)]
                                          ~f:(fun prev -> function
                                            | Prim
                                                ( _
                                                , "Elt"
                                                , [String (_, s); Bytes (_, b)]
                                                , _ ) ->
                                                (s, Bytes.to_string b) :: prev
                                            | other ->
                                                nope
                                                  Message.(
                                                    t
                                                      "Metadata result has \
                                                       wrong structure:"
                                                    %% ct
                                                         (Michelson
                                                          .micheline_node_to_string
                                                            other)))
                                    | other ->
                                        nope
                                          Message.(
                                            t
                                              "Metadata result has wrong \
                                               structure:"
                                            %% ct
                                                 (Michelson
                                                  .micheline_node_to_string
                                                    (Seq (l, other)))) )
                                  | Some (Ok other) ->
                                      nope
                                        Message.(
                                          t
                                            "Metadata result has wrong \
                                             structure:"
                                          %% ct
                                               (Michelson
                                                .micheline_node_to_string other))
                                in
                                Ok ok
                              with Decorate_error.E {message} -> Error message
                            in
                            let piece_of_metadata k =
                              match unpaired_metadata with
                              | Ok s -> List.Assoc.find s k ~equal:String.equal
                              | Error _ -> None in
                            let symbol = piece_of_metadata "symbol" in
                            let name = piece_of_metadata "name" in
                            let decimals = piece_of_metadata "decimals" in
                            let extras =
                              match unpaired_metadata with
                              | Error m -> Some (Error m)
                              | Ok l -> (
                                match
                                  List.filter l ~f:(fun (k, _) ->
                                      not
                                        (List.mem
                                           ["symbol"; "name"; "decimals"]
                                           k ~equal:String.equal))
                                with
                                | [] -> None
                                | m -> Some (Ok m) ) in
                            let show_extras = function
                              | Ok l ->
                                  itemize
                                    (List.map l ~f:(fun (k, v) ->
                                         Fmt.kstr ct "%S" k %% t "→"
                                         %% Fmt.kstr ct "%S" v))
                              | Error m -> Message_html.render ctxt m in
                            let metarows =
                              let default_show = function
                                | Ok node ->
                                    ct (Michelson.micheline_node_to_string node)
                                | Error s -> Bootstrap.color `Danger (t s) in
                              let or_not n o ~f =
                                match o with None -> [] | Some s -> [(n, f s)]
                              in
                              [("Token Id", Fmt.kstr ct "%04d" id)]
                              @ or_not "Total Supply" total_supply
                                  ~f:default_show
                              @ or_not "Symbol" symbol ~f:it
                              @ or_not "Name" name ~f:it
                              @ or_not "Decimals" decimals ~f:it
                              @ or_not "“Extras”" extras ~f:show_extras
                              (* @ or_not "Full-Metadata" metadata_map
                                  ~f:default_show *) in
                            Lwt.return metarows in
                          Lwt_list.map_s explore_token tokens
                          >>= fun decorated_tokens ->
                          let token_list =
                            match decorated_tokens with
                            | [] -> bt "There are no tokens :("
                            | one :: more ->
                                let fields = List.map one ~f:fst in
                                let header_row = List.map fields ~f:t in
                                Bootstrap.Table.simple ~header_row
                                  (List.fold (one :: more) ~init:(empty ())
                                     ~f:(fun prev tok ->
                                       H5.tr
                                         (List.map tok ~f:(fun (_, v) -> td v))))
                          in
                          Async_work.ok wip_explore_tokens token_list ;
                          Lwt.return ()) ;
                    dbgf "go view" in
                  ( true
                  , Bootstrap.button ~kind:`Primary ~size:`Small ~outline:true
                      ~action (t "Explore Tokens") )
              | _ -> (false, empty ()) in
            let tokens_exploration x = div (bt "Tokens:" % div x) in
            div
              ( t "This looks like a TZIP-12 contract (a.k.a. FA2);"
              %%
              match global_validity with
              | true ->
                  Bootstrap.color `Success (t "it seems valid")
                  %%
                  if can_enumerate_tokens then
                    parens (t "and tokens can be enumerated/explored") % t "."
                  else t "."
              | false -> Bootstrap.color `Danger (t "it is invalid.") )
            % div
                ( show_validity_btn % explore_tokens_btn % validity_div
                % Async_work.render wip_explore_tokens ~f:tokens_exploration )
          in
          (metadata, [field "TZIP-12 Implementation Claim" tzip_12_block]))

  let metadata_contents ?(open_in_editor_link = true) ctxt =
    let open Tezos_contract_metadata.Metadata_contents in
    fun (*  as *) metadata ->
      let ct = monot in
      let license_elt l =
        let open License in
        it l.name
        % Option.value_map ~default:(empty ()) l.details ~f:(fun d ->
              Fmt.kstr t " → %s" d) in
      let url_elt u = url ct u in
      let authors_elt l =
        let author s =
          try
            match String.split (String.strip s) ~on:'<' with
            | [name; id] -> (
              match String.lsplit2 id ~on:'>' with
              | Some (u, "") when String.is_prefix u ~prefix:"http" ->
                  t name %% parens (url_elt u)
              | Some (u, "")
              (* we won't get into email address regexps here, sorry *)
                when String.mem u '@' ->
                  t name %% parens (link ~target:("mailto:" ^ u) (ct u))
              | _ -> failwith "" )
            | _ -> failwith ""
          with _ -> ct s in
        oxfordize_list l ~map:author
          ~sep:(fun () -> t ", ")
          ~last_sep:(fun () -> t ", and ")
        |> list in
      let interfaces_elt l =
        let interface s =
          let r = Re.Posix.re "TZIP-([0-9]+)" in
          let normal s = it s in
          let tok = function
            | `Text s -> normal s
            | `Delim g -> (
                let the_text = normal (Re.Group.get g 0) in
                try
                  let tzip_nb = Re.Group.get g 1 |> Int.of_string in
                  link the_text
                    ~target:
                      (Fmt.str
                         "https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-%d/tzip-%d.md"
                         tzip_nb tzip_nb)
                  (* Fmt.kstr txt "{%a --- %d}" Re.Group.pp g (Re.Group.nb_groups g)] *)
                with e ->
                  dbgf "Error in interface html: %a" Exn.pp e ;
                  the_text ) in
          Re.split_full (Re.compile r) s |> List.map ~f:tok |> list in
        List.map l ~f:interface |> List.intersperse ~sep:(t ", ") |> list in
      let _todo l = Fmt.kstr t "todo: %d items" (List.length l) in
      let view_id s = Fmt.str "view-%s" s in
      let view ?(collapsing = false) v =
        let open View in
        let purity =
          if v.is_pure then Bootstrap.color `Success (t "pure")
          else Bootstrap.color `Warning (t "impure") in
        let implementations impls =
          let open Implementation in
          itemize
            (List.map impls ~f:(function
              | Michelson_storage view -> michelson_view ctxt ~view
              | Rest_api_query raq ->
                  field "REST-API Query"
                    (itemize
                       ( normal_field "OpenAPI Spec" (ct raq.specification_uri)
                       @ option_field "Base-URI Override" raq.base_uri ct
                       @ normal_field "Path" (ct raq.path)
                       @ normal_field "Method"
                           (Fmt.kstr ct "%s"
                              (Cohttp.Code.string_of_method raq.meth)) ))))
        in
        let maybe_collapse content =
          if collapsing then
            let open Bootstrap.Collapse in
            let collapse = make () in
            let btn =
              make_button collapse ~kind:`Secondary
                ~style:(Reactive.pure (Fmt.str "width: 8em"))
                (Reactive.bind (collapsed_state collapse) ~f:(function
                  | true -> t "Expand"
                  | false -> t "Collapse")) in
            let dv = make_div collapse (fun () -> content) in
            btn %% dv
          else content in
        div
          ~a:[H5.a_id (Lwd.pure (view_id v.name))]
          ( bt v.name %% t "(" % purity % t "):"
          % maybe_collapse
              (itemize
                 ( option_field "Description" v.description paragraphs
                 @
                 match v.implementations with
                 | [] ->
                     [ Bootstrap.color `Danger
                         (t "There are no implementations.") ]
                 | l ->
                     let name =
                       Fmt.str "Implementation%s"
                         (if List.length l = 1 then "" else "s") in
                     list_field name v.implementations implementations )) )
      in
      let views_elt (views : View.t list) =
        let collapsing = List.length views >= 3 in
        itemize (List.map views ~f:(fun v -> view ~collapsing v)) in
      let source_elt source =
        let open Source in
        let tool s =
          let links =
            [ ("michelson", "https://tezos.gitlab.io")
            ; ("smartpy", "https://smartpy.io")
            ; ("flextesa", "https://tezos.gitlab.io/flextesa")
            ; ("dune", "https://dune.build")
            ; ("archetype", "https://archetype-lang.org/")
            ; ("ligo", "https://ligolang.org/")
            ; ("cameligo", "https://ligolang.org/")
            ; ("pascaligo", "https://ligolang.org/")
            ; ("reasonligo", "https://ligolang.org/")
            ; ("morley", "https://gitlab.com/morley-framework/morley")
            ; ("lorentz", "https://serokell.io/project-lorentz-indigo")
            ; ("indigo", "https://serokell.io/project-lorentz-indigo") ] in
          let splitted = String.split ~on:' ' s in
          match splitted with
          | [] | [""] -> t "Empty Tool 👎"
          | one :: more ->
              let uncap = String.lowercase one in
              List.find_map links ~f:(fun (prefix, target) ->
                  if String.equal uncap prefix then
                    Some
                      ( link ~target (bt one)
                      %
                      match more with
                      | [] -> empty ()
                      | _ -> t " " % parens (ct (String.concat ~sep:" " more))
                      )
                  else None)
              |> Option.value ~default:(bt s) in
        itemize
          ( field "Tools"
              ( ( oxfordize_list source.tools ~map:tool
                    ~sep:(fun () -> t ", ")
                    ~last_sep:(fun () -> t ", and ")
                |> list )
              % t "." )
          :: option_field "Location" source.location url_elt ) in
      let errors_elt ~views errors =
        let open Errors.Translation in
        let langs = function
          | None -> empty ()
          | Some [] -> div (t "[lang=?]")
          | Some more ->
              div
                ( t "[lang="
                % list
                    (let sep () = t "|" in
                     oxfordize_list more ~map:ct ~sep ~last_sep:sep)
                % t "]" ) in
        let expand (Michelson_blob.Micheline m as mm) =
          let open Tezos_micheline.Micheline in
          let qit s =
            abbreviation
              (Fmt.str "Michelson: %s" (mich mm))
              (Fmt.kstr it "“%s”" s) in
          match root m with
          | String (_, s) -> qit s
          | Bytes (_, b) -> qit (Bytes.to_string b)
          | _ -> ct (mich mm) in
        let view_link name =
          List.find views ~f:(fun v -> String.equal name v.View.name)
          |> function
          | None ->
              ct name %% small (Bootstrap.color `Danger (t "(Cannot find it!)"))
          | Some v ->
              let collapse = Bootstrap.Collapse.make () in
              Bootstrap.Collapse.make_button ~kind:`Secondary collapse (ct name)
              % Bootstrap.Collapse.make_div collapse (fun () -> view v) in
        itemize
          (List.map errors ~f:(function
            | Static {error; expansion; languages} ->
                field "Static-translation"
                  ( ct (mich error)
                  %% t "→" %% expand expansion %% langs languages )
            | Dynamic {view_name; languages} ->
                field "Dynamic-view" (view_link view_name %% langs languages)))
      in
      let unknown_extras kv =
        itemize
          (List.map kv ~f:(fun (k, v) ->
               ct k %% pre (ct (Ezjsonm.value_to_string ~minify:false v))))
      in
      let ( { name
            ; description
            ; version
            ; license
            ; authors
            ; homepage
            ; source
            ; interfaces
            ; errors
            ; views
            ; unknown }
          , sub_standards ) =
        metadata_substandards ctxt metadata in
      ( if open_in_editor_link then
        open_in_editor ctxt
          (Tezos_contract_metadata.Metadata_contents.to_json metadata)
      else empty () )
      % itemize
          ( option_field "Name" name ct
          @ option_field "Version" version ct
          @ option_field "Description" description paragraphs
          @ list_field "Interfaces" interfaces interfaces_elt
          @ sub_standards
          @ option_field "License" license license_elt
          @ option_field "Homepage" homepage url_elt
          @ option_field "Source" source source_elt
          @ list_field "Authors" authors authors_elt
          @ option_field "Errors" errors (errors_elt ~views)
          @ list_field "Views" views views_elt
          @ list_field "Extra/Unknown" unknown unknown_extras )
end

module Examples_dropdown = struct
  open Meta_html

  let make ctxt ~action l =
    let open Bootstrap.Dropdown_menu in
    let example (v, msg) =
      let cct v =
        if String.length v > 22 then
          abbreviation v (ct (String.sub v ~pos:0 ~len:21 ^ "…"))
        else ct v in
      item (cct v %% t "→" %% it msg) ~action:(fun () -> action v) in
    Reactive.bind (State.Examples.get ctxt) ~f:(fun examples ->
        button (t "Examples 💡 ")
          (List.concat_map l ~f:(fun (h, f) ->
               header h :: List.map (f examples) ~f:example)))

  let explorable ctxt =
    make ctxt
      ~action:(fun x -> State.set_explorer_input ctxt x)
      [ (t "KT1 Contracts", fun x -> x.contracts)
      ; (t "TZIP-16-URIs", fun x -> x.uris) ]

  let editable ctxt ~action =
    make ctxt ~action
      [ (t "TZIP-16 Metadata Content", fun x -> x.metadata_blobs)
      ; (t "TZIP-16-URIs", fun x -> x.uris)
      ; (t "Michelson Bytes Blobs", fun x -> x.michelson_bytes)
      ; (t "Michelson Concrete Expressions", fun x -> x.michelson_concretes) ]
end

module Editor = struct
  type guess = Empty | Failed | Format of State.Editor_mode.format

  let guessers : (log:(Message.t -> unit) -> string -> guess option) list =
    let of_predicate name v f ~log inp =
      let worked = try f inp with _ -> false in
      log
        Message.(
          t "Trying predicate" %% ct name %% t "→"
          % if worked then t "OK!" else t "Nope :/") ;
      if worked then Some v else None in
    let looks_like_json s =
      let open Char in
      let str = String.strip s in
      str.[0] = '{' && str.[String.length str - 1] = '}' in
    let oneline s =
      match String.split ~on:'\n' (String.strip s) with
      | [""] -> false
      | [_] -> true
      | _ -> false in
    let is_prefix_strip s ~prefix = String.is_prefix (String.strip s) ~prefix in
    let uri_start = Re.Posix.compile_pat "^[a-z0-9\\-]+:" in
    let looks_like_an_uri s = oneline s && Re.execp uri_start s in
    let looks_like_hexa s =
      is_prefix_strip s ~prefix:"0x"
      || String.for_all s ~f:(function
           | 'A' .. 'F' | 'a' .. 'f' | '0' .. '9' | 'x' | ' ' | '\n' -> true
           | _ -> false) in
    let looks_like_michelson s =
      match (String.strip s).[0] with
      | '(' | '"' | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' -> true
      | _ -> false in
    let format_of_predicate n v = of_predicate n (Format v) in
    [ of_predicate "it-is-empty" Empty (fun s ->
          String.is_empty (String.strip s))
    ; format_of_predicate "looks-like-a-json-object" `Metadata_json
        looks_like_json
    ; format_of_predicate "looks-like-an-uri" `Uri looks_like_an_uri
    ; format_of_predicate "looks-like-hexadecimal" `Hex looks_like_hexa
    ; format_of_predicate "looks-like-michelson" `Michelson looks_like_michelson
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
             %% ct s % t ":" %% t e)) in
    match Contract_metadata.Uri.validate inpo with
    | Ok u, errs ->
        let header =
          match errs with
          | [] -> big_answer `Ok (t "This metadata URI is VALID 👍")
          | _ ->
              big_answer `Error
                (t "This metadata URI parses OK but is not VALID 😞") in
        let sec = h4 in
        let validation_errors =
          match errs with
          | [] -> empty ()
          | more ->
              sec (Bootstrap.color `Danger (t "Validation Errors:"))
              % list_of_validation_errors more in
        header % validation_errors
        % sec (t "Understood As:")
        % Tezos_html.metadata_uri ~open_in_editor_link:false ctxt u
    | Error el, errs -> (
        big_answer `Error (t "There were parsing/validation errors 😭")
        % Tezos_html.error_trace ctxt el
        %
        match errs with
        | [] -> empty ()
        | more -> p (t "Moreover:") % list_of_validation_errors more )

  let show_metadata ctxt inpo =
    let open Tezos_contract_metadata.Metadata_contents in
    match Contract_metadata.Content.of_json inpo with
    | Ok m ->
        let errs, warns =
          Validation.validate m ~protocol_hash_is_valid:(fun s ->
              try
                let _ = B58_hashes.check_b58_protocol_hash s in
                true
              with e ->
                dbgf "Protocol hash problem: %a" Exn.pp e ;
                false) in
        let header =
          match (errs, warns) with
          | [], [] -> big_answer `Ok (t "This metadata JSON is VALID 👌")
          | _ :: _, _ ->
              big_answer `Error
                (t "This metadata JSON parses Okay but is not valid 👎")
          | [], _ :: _ ->
              big_answer `Ok
                (t "This metadata JSON is valid 👍, with warnings …") in
        let section title reasons to_html =
          match reasons with
          | [] -> empty ()
          | more -> h4 title %% itemize (List.map more ~f:to_html) in
        header
        % section (t "Errors") errs (Tezos_html.metadata_validation_error ctxt)
        % section (t "Warnings") warns
            (Tezos_html.metadata_validation_warning ctxt)
        % h4 (t "Contents")
        % Tezos_html.metadata_contents ~open_in_editor_link:false ctxt m
    | Error el ->
        big_answer `Error (t "This metadata JSON is not valid 🥸")
        % Tezos_html.error_trace ctxt el

  let explode_hex bytes_code =
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
            % code_block (Ezjsonm.value_to_string ~minify:false json) in
          (header, result, true)
      | Error el ->
          ( big_answer `Error (t "There were parsing/validation errors:")
          , Bootstrap.p_lead
              (t "Failed to parse" %% ct (bytes_summary bytes) % t ":")
            %% Tezos_html.error_trace ctxt el
          , false ) in
    let items =
      let opt_if c v = if c then Some v else None in
      List.filter_opt
        [ opt_if with_zero_x
            ( t "The prefix" %% ct "0x"
            %% t "just means “this is hexadecimal”." )
        ; opt_if with_zero_five
            ( t "The first byte" %% ct "05"
            %% t "is the standard prefix/watermark for Michelson expressions."
            )
        ; opt_if valid_pack
            ( t "The bytes"
            %% ct (bytes_summary bytes)
            %% t "are a valid" %% ct "PACK" % t "-ed expression." ) ] in
    header % itemize items % result

  let process_micheline ctxt inp =
    try
      match
        Michelson.parse_micheline
          ~check_indentation:(State.check_micheline_indentation ctxt)
          ~check_primitives:false inp
      with
      | Ok o ->
          let concrete = Michelson.micheline_node_to_string o in
          let json = Michelson.micheline_to_ezjsonm o in
          let packed =
            try
              (* The packing can raise for missing primitives. *)
              Ok (Michelson_bytes.pack_node_expression o)
            with e -> Error e in
          Ok (concrete, json, packed)
      | Error e -> Error e
    with e -> Error [Tezos_error_monad.Error_monad.Exn e]

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
                %% div (Errors_html.exception_html ctxt e) in
          let result =
            h4 (t "Reindented")
            % code_block concrete
            % h4 (t "As JSON")
            % code_block (Ezjsonm.value_to_string ~minify:false json)
            % pack_result in
          (header, result)
      | Error el ->
          ( big_answer `Error (t "There were parsing/validation errors:")
          , Tezos_html.error_trace ctxt el ) in
    header % result

  let show_binary_info ctxt (kind : guess) input_bytes =
    let packed_mich =
      if Poly.(kind = Format `Michelson) then
        match process_micheline ctxt input_bytes with
        | Error e -> None
        | Ok (_, _, packed) -> (
          match packed with Ok o -> Some ("\x05" ^ o) | Error e -> None )
      else None in
    let binary_from_hex =
      if Poly.(kind = Format `Hex) then
        try
          let _, with05, hex_bytes = explode_hex input_bytes in
          let bin = Hex.to_string (`Hex hex_bytes) in
          Some (if with05 then "\x05" ^ bin else bin)
        with _ -> None
      else None in
    let sizing =
      Bootstrap.Table.simple
        ~header_row:[empty (); t "Size"; t "Delphi Burn"]
        (let row l = H5.tr (List.map ~f:td l) in
         let sizes =
           let lif c k v = try if c then [(k, v ())] else [] with _ -> [] in
           let lif_opt o k v =
             match o with Some s -> [(k, v s)] | None -> [] in
           [("Raw Input", String.length input_bytes)]
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
               pf ppf "%s" (Int.to_string_hum i ~delimiter:' ') in
             (* carthage: 1 militez / byte
                http://mainnet.smartpy.io/chains/main/blocks/head/context/constants
                "cost_per_byte":"1000"
                http://delphinet.smartpy.io/chains/main/blocks/head/context/constants
                "cost_per_byte":"250"
             *)
             prev
             % row
                 [ t label; Fmt.kstr t "%a B" ppbig bytes
                 ; Fmt.kstr t "%a μꜩ" ppbig (bytes * 250) ])) in
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
      let hrow c = H5.(tr [th ~a:[ (* a_colspan (Reactive.pure 2) *) ] [c]]) in
      let row k v = H5.(tr [td [k; br (); v]]) in
      let make_item k bytes hashes =
        hrow (t k)
        % list
            (List.map hashes ~f:(fun hash ->
                 let n, v = hash bytes in
                 row (t n) (ct v))) in
      let hash k b = (k, b) in
      let hex s =
        let (`Hex x) = Hex.of_string s in
        x in
      let ldgr bytes =
        hash "Ledger-BLAKE2B-Base58"
          (Base58.raw_encode (B58_hashes.blake2b bytes)) in
      let blake2b bytes = hash "BLAKE2B-Hex" (hex (B58_hashes.blake2b bytes)) in
      let sha256 bytes =
        hash "SHA256-Hex" (hex (B58_hashes.B58_crypto.sha256 bytes)) in
      let sha512 bytes =
        hash "SHA512-Hex" (hex (B58_hashes.B58_crypto.sha512 bytes)) in
      let expr58 bytes =
        hash "Script-ID-hash (big-map access)"
          (B58_hashes.b58_script_id_hash bytes) in
      let items = ref [] in
      let item k b h = items := make_item k b h :: !items in
      item "Raw Input" input_bytes [ldgr; blake2b; sha256; sha512] ;
      Option.iter binary_from_hex ~f:(fun bin ->
          item "Binary-Michelson-Expression (with watermark)" bin
            [ldgr; blake2b; sha256; sha512; expr58]) ;
      Option.iter packed_mich ~f:(fun packed ->
          item "Binary-Michelson-Expression (with watermark)" packed
            [ldgr; blake2b; sha256; sha512; expr58]) ;
      Bootstrap.Table.simple (list (List.rev !items)) in
    Bootstrap.bordered ~kind:`Secondary
      (Bootstrap.container (sizing % div hashes))

  let page ctxt =
    let content = State.editor_content ctxt in
    let guess_validate input =
      let _logs = ref [] in
      let log m = _logs := m :: !_logs in
      let res =
        List.find_map guessers ~f:(fun f -> f ~log input)
        |> Option.value ~default:Failed in
      (input, res, List.rev !_logs) in
    let format_result =
      Reactive.(Bidirectional.get content ** State.editor_mode ctxt)
      |> Reactive.map ~f:(function
           | input, `Guess -> (`Guess, guess_validate input)
           | input, (#State.Editor_mode.format as fmt) ->
               (fmt, (input, Format fmt, []))) in
    let display_guess =
      Reactive.bind format_result ~f:(function
        | `Guess, (inp, kind, logs) ->
            let normal c = Bootstrap.color `Secondary c in
            div
              ~a:
                [H5.a_style (Reactive.pure "width: 12em; display: inline-block")]
              ( match kind with
              | Empty -> normal (t "The editor is Empty.")
              | Format m ->
                  normal (t "Using mode" %% ct (State.Editor_mode.to_string m))
              | Failed ->
                  Bootstrap.color `Danger (t "Failed to guess a format.") )
        | _ -> empty ()) in
    let result =
      (* We keep the [make ()]s outside the bind to that they remember their
         state: *)
      let collapse_binary = Bootstrap.Collapse.make () in
      let collapse_logs = Bootstrap.Collapse.make () in
      Reactive.bind format_result ~f:(fun (mode, (inp, kind, logs)) ->
          let show_logs, logs =
            match logs with
            | [] -> (empty (), empty ())
            | _ :: _ ->
                let open Bootstrap.Collapse in
                ( make_button collapse_logs ~kind:`Secondary
                    ~style:(Reactive.pure (Fmt.str "width: 8em"))
                    (Reactive.bind (collapsed_state collapse_logs) ~f:(function
                      | true -> t "Show Logs"
                      | false -> t "Hide Logs"))
                , make_div collapse_logs (fun () ->
                      Bootstrap.terminal_logs
                        (itemize (List.map logs ~f:(Message_html.render ctxt))))
                ) in
          let binary_info_button, binary_info =
            let open Bootstrap.Collapse in
            ( make_button collapse_binary ~kind:`Secondary
                ~style:(Reactive.pure (Fmt.str "width: 12em"))
                (Reactive.bind (collapsed_state collapse_binary) ~f:(function
                  | true -> t "Show Binary Info"
                  | false -> t "Hide Binary Info"))
            , make_div collapse_binary (fun () ->
                  show_binary_info ctxt kind inp) ) in
          let header =
            div
              ( show_logs %% display_guess %% binary_info_button %% logs
              %% binary_info ) in
          match kind with
          | Empty ->
              header
              %% div
                   ~a:
                     [ classes ["mx-auto"]
                     ; H5.a_style (Reactive.pure "width: 50%") ]
                   (span
                      ~a:
                        [ H5.a_style
                            (Reactive.pure "font-size: 3000%; opacity: 0.1") ]
                      (t "ꜩ"))
          | Format fmt -> (
              header
              %
              match fmt with
              | `Metadata_json -> show_metadata ctxt inp
              | `Uri -> show_uri ctxt inp
              | `Hex -> show_hex ctxt inp
              | `Michelson -> show_michelson ctxt inp )
          | Failed ->
              header
              % h4 (t "Don't know how to validate this")
              % pre ~a:[classes ["pre-scrollable"]] (ct inp)) in
    let local_storage_button =
      let label = t "Local-Storage" in
      let button_kind = `Light (* default for dropdowns *) in
      match Local_storage.available ctxt with
      | true ->
          Bootstrap.Dropdown_menu.(
            button label ~kind:button_kind
              [ item
                  ~action:(fun () -> State.save_editor_content ctxt)
                  (t "Save")
              ; item
                  ~action:(fun () -> State.load_editor_content ctxt)
                  (t "Load") ])
      | false ->
          Bootstrap.button ~kind:button_kind ~disabled:true ~action:Fn.ignore
            (abbreviation
               "Local storage is not available for this browser/site combo."
               label) in
    let editor =
      div
        ( Examples_dropdown.editable ctxt ~action:(fun v ->
              Reactive.Bidirectional.set content v)
        % Bootstrap.Dropdown_menu.(
            button
              ( t "Mode:"
              %% ( State.editor_mode ctxt
                 |> Reactive.bind ~f:(fun m ->
                        ct (State.Editor_mode.to_string m)) ) )
              ( header (t "Editor Validation Modes")
              :: List.map State.Editor_mode.all ~f:(fun m ->
                     item
                       ~action:(fun () -> State.set_editor_mode ctxt m)
                       ( ct (State.Editor_mode.to_string m)
                       %% t "→"
                       %% State.Editor_mode.explain m )) ))
        % local_storage_button )
      % H5.(
          div
            [ textarea
                ((* txt *) Reactive.Bidirectional.get content)
                ~a:
                  [ a_style (Lwd.pure "font-family: monospace")
                  ; classes ["col-12"]; a_rows (Lwd.pure 50)
                  ; a_oninput
                      (Tyxml_lwd.Lwdom.attr
                         Js_of_ocaml.(
                           fun ev ->
                             Js.Opt.iter ev##.target (fun input ->
                                 Js.Opt.iter (Dom_html.CoerceTo.textarea input)
                                   (fun input ->
                                     let v = input##.value |> Js.to_string in
                                     dbgf "TA inputs: %d bytes: %S"
                                       (String.length v) v ;
                                     Reactive.Bidirectional.set content v)) ;
                             true)) ] ]) in
    div
      ~a:[classes ["row"]]
      (Reactive.bind (Browser_window.width ctxt) ~f:(function
        | Some `Wide ->
            div ~a:[classes ["col-6"]] editor
            % div ~a:[classes ["col-6"]] result
        | Some `Thin | None ->
            let visible = Reactive.var `Editor in
            let tabs =
              let open Bootstrap.Tab_bar in
              make
                [ item (t "Editor")
                    ~active:
                      ( Reactive.get visible
                      |> Reactive.map ~f:(function
                           | `Editor -> false
                           | `Result -> true) )
                    ~action:(fun () -> Reactive.set visible `Editor)
                ; item (t "Results")
                    ~active:
                      ( Reactive.get visible
                      |> Reactive.map ~f:(function
                           | `Editor -> true
                           | `Result -> false) )
                    ~action:(fun () -> Reactive.set visible `Result) ] in
            div
              ~a:[classes ["col-12"]]
              ( tabs
              %% Reactive.bind_var visible ~f:(function
                   | `Editor -> editor
                   | `Result -> result) )))
end

module Explorer = struct
  let validate_intput input_value =
    match B58_hashes.check_b58_kt1_hash input_value with
    | _ -> `KT1 input_value
    | exception _ when String.is_prefix input_value ~prefix:"KT" ->
        `Error
          ( input_value
          , [Tezos_error_monad.Error_monad.failure "Invalid KT1 address"] )
    | exception _ -> (
      match Contract_metadata.Uri.validate input_value with
      | Ok uri, _ -> `Uri (input_value, uri)
      | Error e, _ -> `Error (input_value, e) )

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

  let full_input_quick ctxt =
    let open Meta_html in
    function
    | `KT1 k -> t "Contract" %% Block_explorer.kt1_display k
    | `Uri (u, _) -> t "URI" %% ct u
    | `Error (m, _) -> t "Erroneous input:" %% Fmt.kstr ct "%S" m

  let full_input_bloc ctxt full_input =
    let open Meta_html in
    h4 (t "Understood Input") % div (full_input_quick ctxt full_input)

  let uri_and_metadata_result ctxt ~uri ~metadata ~full_input =
    let open Meta_html in
    full_input_bloc ctxt full_input
    % h4 (t "Metadata Location")
    % Tezos_html.metadata_uri ctxt uri
    % h4 (t "Metadata Contents")
    % Tezos_html.metadata_contents ctxt metadata

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

  let uri_there_but_wrong ctxt ~full_input ~uri_string ~error =
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
    dbgf "Form submitted with %s" input_value ;
    Async_work.reinit result ;
    Async_work.wip result ;
    Async_work.log result (t "Starting with: " %% ct input_value) ;
    Async_work.async_catch result
      ~exn_to_html:(Errors_html.exception_html ctxt)
      Lwt.Infix.(
        fun ~mkexn () ->
          let logs prefix s =
            Async_work.log result
              (it prefix %% t "→" %% Bootstrap.monospace (t s)) in
          let full_input = validate_intput input_value in
          let on_uri ctxt uri =
            Lwt.catch
              (fun () ->
                Contract_metadata.Uri.fetch ctxt uri
                  ~log:(logs "Fetching Metadata"))
              (fun e ->
                raise
                  (mkexn (uri_failed_to_fetch ctxt ~full_input ~uri ~error:e)))
            >>= fun json_code ->
            let open Tezos_contract_metadata.Metadata_contents in
            dbgf "before of-json" ;
            match Contract_metadata.Content.of_json json_code with
            | Ok metadata ->
                Async_work.ok result
                  (uri_and_metadata_result ctxt ~full_input ~uri ~metadata) ;
                Lwt.return ()
            | Error error ->
                raise
                  (mkexn
                     (uri_ok_but_metadata_failure ctxt ~uri ~full_input
                        ~metadata_json:json_code ~error)) in
          match full_input with
          | `KT1 address -> (
              Query_nodes.metadata_value ctxt ~address ~key:""
                ~log:(logs "Getting URI")
              >>= fun metadata_uri ->
              Contract_metadata.Uri.Fetcher.set_current_contract ctxt address ;
              Async_work.log result (t "Now going for: " %% ct metadata_uri) ;
              match Contract_metadata.Uri.validate metadata_uri with
              | Ok uri, _ -> on_uri ctxt uri
              | Error error, _ ->
                  raise
                    (mkexn
                       (uri_there_but_wrong ctxt ~uri_string:metadata_uri
                          ~full_input ~error)) )
          | `Uri (_, uri) ->
              if Contract_metadata.Uri.needs_context_address uri then
                Async_work.log result
                  (bt "This URI requires a context KT1 address …") ;
              System.slow_step ctxt >>= fun () -> on_uri ctxt uri
          | `Error (_, el) -> raise (mkexn (Tezos_html.error_trace ctxt el)))

  let page ctxt =
    let open Meta_html in
    let result = State.explorer_result ctxt in
    Query_nodes.Update_status_loop.ensure ctxt ;
    h2 (t "Contract Metadata Explorer")
    % Bootstrap.Form.(
        let enter_action () = go_action ctxt in
        State.if_explorer_should_go ctxt enter_action ;
        make ~enter_action
          [ row
              [ cell 2 (magic (Examples_dropdown.explorable ctxt))
              ; cell 8
                  (input
                     ~placeholder:
                       (Reactive.pure
                          "Enter a contract address or a metadata URI")
                     (State.explorer_input_bidirectional ctxt)
                     ~help:(input_validation_status ctxt))
              ; cell 2
                  (submit_button (t "Go!")
                     ~active:
                       Reactive.(
                         map
                           (input_valid ctxt ** Async_work.busy result)
                           ~f:(function
                             | `Error _, _ -> false
                             | _, true -> false
                             | _ -> true))
                     enter_action) ] ])
    % Async_work.render result ~f:Fn.id
end

let root_document state =
  let open Meta_html in
  let explorer = lazy (Explorer.page state) in
  let editor = lazy (Editor.page state) in
  let settings = lazy (Settings_page.render state) in
  let about = lazy (about_page state) in
  Bootstrap.container ~suffix:"-fluid"
    ( navigation_menu state
    % Reactive.bind (State.page state)
        State.Page.(
          function
          | `Changing_to p ->
              Lwt.async
                Lwt.Infix.(
                  fun () ->
                    Js_of_ocaml_lwt.Lwt_js.yield ()
                    >>= fun () ->
                    State.set_page state (`Page p) () ;
                    Lwt.return_unit) ;
              div
                H5.(
                  img ()
                    ~a:[a_width (Reactive.pure 100)]
                    ~src:(Reactive.pure "loading.gif")
                    ~alt:(Reactive.pure "Loading spinner GIF"))
          | `Page Explorer -> Lazy.force explorer
          | `Page Editor -> Lazy.force editor
          | `Page Settings -> Lazy.force settings
          | `Page About -> Lazy.force about) )
