open Import
module Z_set = Caml.Set.Make (Z)

module Block_explorer = struct
  type vendor = Smartpy | Bcd

  let all_vendors = [ Smartpy; Bcd ]

  let kt1_url vendor kt1 =
    match vendor with
    | Smartpy -> Fmt.str "https://smartpy.io/dev/explorer.html?address=%s" kt1
    | Bcd -> Fmt.str "https://better-call.dev/search?text=%s" kt1

  let big_map_url network id =
    Option.map (Network.better_call_dev_path network) ~f:(fun pref ->
        Fmt.str "https://better-call.dev/%s/big_map/%a/keys" pref Z.pp_print id)

  let vendor_show_name = function Smartpy -> "SmartPy" | Bcd -> "BCD"

  let big_map_display ctxt id =
    let open Meta_html in
    let text = t (vendor_show_name Bcd) in
    let network = State.current_network ctxt in
    Bootstrap.monospace (Fmt.kstr bt "%a" Z.pp_print id)
    %% small
         (parens
            (match big_map_url network id with
            | None -> text
            | Some target -> link ~target text))

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

let tzip_021_url =
  "https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-21/tzip-21.md"

let uri_parsing_error err =
  let open Meta_html in
  let open Tezai_contract_metadata.Metadata_uri.Parsing_error in
  let details =
    let sha256_host_advice =
      t "The host should look like:"
      %% ct "0x5891b5b522d5df086d0ff0b110fbd9d21bb4fc7163af34d08286a2e846f6be03"
      % t "."
    in
    let scheme_advice =
      t "The URI should start with one of:"
      %% list
           (oxfordize_list
              [ "tezos-storage"; "http"; "https"; "sha256"; "ipfs" ]
              ~map:(fun sch -> Fmt.kstr ct "%s:" sch)
              ~sep:(fun () -> t ", ")
              ~last_sep:(fun () -> t ", or "))
      % t "."
    in
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
    | Wrong_hex_format_for_hash { hash = `Sha256; host; message } ->
        t "Failed to parse the “host” "
        %% ct host % t " in this " %% ct "sha256://" % t " URI: " % t message
        % t " → " %% sha256_host_advice
    | Bad_b58 (l, r) -> Fmt.kstr t "Bad Base58: %s %s" l r
    | Wrong_network (l, r) -> Fmt.kstr t "Wrong Network: %s %s" l r
  in
  let exploded_uri =
    let u = Uri.of_string err.input in
    let item name opt =
      it name % t ": " % match opt with None -> t "<empty>" | Some s -> ct s
    in
    let item_some name s = item name (Some s) in
    t "The URI is understood this way: "
    % itemize
        [
          item "Scheme" (Uri.scheme u);
          item "Host" (Uri.host u);
          item "User-info" (Uri.userinfo u);
          item "Port" (Uri.port u |> Option.map ~f:Int.to_string);
          item_some "Path" (Uri.path u);
          item "Query" (Uri.verbatim_query u);
          item "Fragment" (Uri.fragment u);
        ]
  in
  t "Failed to parse URI:" %% ct err.input % t ":"
  % itemize [ details; exploded_uri ]

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
            path
        in
        t "At path" %% p % t ":" %% go e
    | Unexpected (u, e) ->
        let article s =
          (* 90% heuristic :) *)
          match s.[0] with
          | 'a' | 'e' | 'i' | 'o' -> t "an"
          | _ -> t "a"
          | exception _ -> t "something"
        in
        let with_article s = article s %% it s in
        t "Expecting" %% with_article e %% t "but got" %% with_article u
    | No_case_matched l ->
        t "No case matched expectations:" %% itemize (List.map ~f:go l)
    | Bad_array_size (u, e) ->
        Fmt.kstr t "Expecting array of size %d but got %d" e u
    | Missing_field field -> t "Missing field" %% quote field
    | Unexpected_field field -> t "Unexpected field" %% quote field
    | Bad_schema e -> t "Bad schema:" %% go e
    | e -> raise e
  in
  fun e -> match go e with m -> Some (m % t ".", []) | exception _ -> None

let simple_exns_handler : Errors_html.handler =
  let open Meta_html in
  let some m = Some (m, []) in
  function
  | Ezjsonm.Parse_error (json_value, msg) ->
      some
        (Fmt.kstr t "JSON Parsing Error: %s, JSON:" msg
        % pre (code (t (Ezjsonm.value_to_string ~minify:false json_value))))
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
        ~handlers:[ json_encoding_error; simple_exns_handler ]
  | Tezai_contract_metadata.Metadata_uri.Contract_metadata_uri_parsing
      parsing_error ->
      uri_parsing_error parsing_error
  | other ->
      pre
        (code
           (Fmt.kstr t "%a" Tezos_error_monad.Error_monad.pp_print_error
              [ other ]))

let error_trace ctxt =
  let open Meta_html in
  function
  | [] -> Bootstrap.alert ~kind:`Danger (t "Empty trace from Tezos-error-monad")
  | [ h ] -> single_error ctxt h
  | h :: tl ->
      single_error ctxt h
      % div
          (t "Trace:"
          %% List.fold tl ~init:(empty ()) ~f:(fun p e ->
                 p %% single_error ctxt e))

open Meta_html

let field_head name = Fmt.kstr (fun s -> Bootstrap.color `Info (t s)) "%s:" name
let field name content = field_head name %% content
let monot s = Bootstrap.monospace (t s)

let option_field name fieldopt f =
  match fieldopt with None -> [] | Some s -> [ field name (f s) ]

let normal_field name x = option_field name (Some ()) (fun () -> x)

let paragraphs blob =
  let rec go l acc =
    match List.split_while l ~f:(function "" -> false | _ -> true) with
    | ll, [] -> String.concat ~sep:" " ll :: acc
    | ll, _ :: lr -> go lr (String.concat ~sep:" " ll :: acc)
  in
  go (String.split blob ~on:'\n') [] |> function
  | [] -> empty ()
  | [ one ] -> t one
  | more -> List.rev_map more ~f:(fun x -> div (t x)) |> H5.div

let list_field name field f =
  option_field name (match field with [] -> None | more -> Some more) f

let network (net : Network.t) = it (Network.to_string net)

let protocol s =
  let proto s =
    let state = Reactive.var true in
    let clickable v c =
      span
        ~a:
          [
            H5.a_onclick
              (Tyxml_lwd.Lwdom.attr (fun _ ->
                   Reactive.set state v;
                   false));
          ]
        c
    in
    Reactive.bind_var state ~f:(function
      | true -> clickable false (abbreviation s (ct (String.prefix s 12)))
      | false -> clickable true (ct s))
  in
  let known name url =
    span (link ~target:url (it name)) %% t "(" % proto s % t ")"
  in
  match s with
  | "PtNairobiyssHuh87hEhfVBGCVrK3WnS8Z2FT4ymB5tAa4r1nQf" ->
      known "Nairobi" "https://tezos.gitlab.io/protocols/017_nairobi.html"
  | "ProxfordSW2S7fvchT1Zgj2avb5UES194neRyYVXoaDGvF9egt8" ->
      known "Oxford" "https://tezos.gitlab.io/protocols/018_oxford.html"
  | s -> proto s

let open_in_editor ?(and_explorer = false) ctxt text =
  small
    (parens
       (State.link_to_editor ctxt ~text (t "Open in editor")
       %
       if and_explorer then
         t "," %% State.link_to_explorer ctxt (t "Explore") ~search:text
       else empty ()))

let open_in_token_viewer ctxt ~token_address ~token_id =
  small
    (parens
       (State.link_to_token_viewer ctxt ~token_address ~token_id
          (t "Open in token viewer")))

let tzip16_uri_short ctxt s =
  Bootstrap.color `Info (Bootstrap.monospace (t s))
  %% open_in_editor ctxt s ~and_explorer:true

let metadata_uri ?(open_in_editor_link = true) ctxt uri =
  let open Tezai_contract_metadata.Metadata_uri in
  let ct = monot in
  let rec go uri =
    match uri with
    | Web u -> t "Web URL:" %% url ct u
    | Ipfs { cid; path } ->
        let gatewayed = Contract_metadata.Uri.to_ipfs_gateway ctxt ~cid ~path in
        field_head "IPFS URI"
        % itemize
            [
              field "CID" (ct cid);
              field "Path" (ct path);
              t "(Try " %% url ct gatewayed % t ")";
            ]
    | Storage { network; address; key } ->
        field_head "In-Contract-Storage"
        % itemize
            [
              field "Network"
                (Option.value_map network
                   ~default:(t "“Current network”.")
                   ~f:ct);
              field "Address"
                (Option.value_map address ~default:(t "“Same contract”.")
                   ~f:Block_explorer.kt1_display);
              field "Key in the big-map" (Fmt.kstr ct "%S" key);
            ]
    | Hash { kind = `Sha256; value; target } ->
        field_head "Hash checked URI"
        % itemize
            [
              field "Target" (go target);
              field "… should SHA256-hash to"
                (Fmt.kstr ct "%a" Hex.pp (Hex.of_string value));
            ]
  in
  (if open_in_editor_link then
   div
     (open_in_editor ctxt
        (Tezai_contract_metadata.Metadata_uri.to_string_uri uri))
  else empty ())
  % div (go uri)

let mich
    (Tezai_contract_metadata.Metadata_contents.Michelson_blob.Michelson_blob m)
    =
  Tezai_michelson.Concrete_syntax.to_string
    (Tezai_michelson.Untyped.of_canonical_micheline m)

let view_result ctxt ~result ~storage ~address ~view ~parameter =
  let open Tezai_contract_metadata.Metadata_contents in
  let open View in
  let open Implementation in
  let open Michelson_storage in
  let expanded =
    try
      let retmf =
        Michelson.Partial_type.of_type ~annotations:view.human_annotations
          view.return_type
      in
      Michelson.Partial_type.fill_with_value retmf result;
      match
        Michelson.Partial_type.render ~tzip16_uri:(tzip16_uri_short ctxt) retmf
      with
      | [] ->
          dbgf "view_result.expanded: empty list!";
          bt "This should not be shown"
      | [ one ] -> one
      | more -> itemize more
    with exn -> Errors_html.exception_html ctxt exn
  in
  let mich_node = Michelson.micheline_node_to_string in
  Bootstrap.div_lead (div (bt "Result:" %% expanded))
  % hr ()
  % Bootstrap.muted div
      (let items l =
         List.fold l ~init:(empty ()) ~f:(fun p (k, v) ->
             p % div (t k % t ":" %% Bootstrap.monospace (t v)))
       in
       items
         [
           ( "Returned Michelson",
             Fmt.str "%s : %s" (mich_node result) (mich view.return_type) );
           ("Called contract", address);
           ("Parameter used", mich_node parameter);
           ("Current storage", mich_node storage);
         ])

let michelson_view ctxt ~view =
  let open Tezai_contract_metadata.Metadata_contents in
  let open View in
  let open Implementation in
  let open Michelson_storage in
  let { parameter; return_type; code; human_annotations; version } = view in
  let call_mode = Reactive.var false in
  let switch = Bootstrap.button ~kind:`Primary ~outline:true ~size:`Small in
  field_head "Michelson-storage-view"
  % Reactive.bind (Reactive.get call_mode) ~f:(function
      | false ->
          switch ~action:(fun () -> Reactive.set call_mode true) (t "Call")
          % itemize
              (option_field "Michelson-Version" version protocol
              @ normal_field "Type"
                  (Fmt.kstr ct "%s<contract-storage> → %s"
                     (match parameter with
                     | None -> ""
                     | Some p -> mich p ^ " × ")
                     (mich return_type))
              @ normal_field "Code"
                  (let concrete = mich code in
                   let lines =
                     1
                     + String.count concrete ~f:(function
                         | '\n' -> true
                         | _ -> false)
                   in
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
                           ct k % t " → " % paragraphs v))))
      | true ->
          let address =
            Reactive.var
              (Contract_metadata.Uri.Fetcher.current_contract ctxt
              |> Reactive.peek |> Option.value ~default:"")
          in
          let parameter_input =
            Option.map parameter
              ~f:
                (Michelson.Partial_type.of_type
                   ~annotations:view.human_annotations)
          in
          let wip = Async_work.empty () in
          let go_action () =
            Async_work.wip wip;
            let log s =
              Async_work.log wip
                (it "Calling view" %% t "→" %% Bootstrap.monospace (t s))
            in
            Async_work.async_catch wip
              ~exn_to_html:(Errors_html.exception_html ctxt)
              Lwt.Infix.(
                fun ~mkexn:_ () ->
                  let parameter =
                    match parameter_input with
                    | Some mf ->
                        Michelson.Partial_type.peek mf
                        |> Tezai_michelson.Concrete_syntax.parse_exn
                             ~check_indentation:false ~check_primitives:false
                    | None ->
                        Tezai_michelson.Concrete_syntax.parse_exn
                          ~check_indentation:false "Unit"
                          ~check_primitives:false
                  in
                  Query_nodes.call_off_chain_view ctxt ~log
                    ~address:(Reactive.peek address) ~view ~parameter
                  >>= function
                  | Ok (result, storage) ->
                      Async_work.ok wip
                        (view_result ctxt ~result ~storage
                           ~address:(Reactive.peek address) ~view ~parameter);
                      Lwt.return ()
                  | Error s ->
                      Async_work.error wip (t "Error:" %% ct s);
                      Lwt.return ());
            dbgf "go view"
          in
          switch ~action:(fun () -> Reactive.set call_mode false) (t "Cancel")
          % (Async_work.is_empty wip
            |> Reactive.bind ~f:(function
                 | false -> Async_work.render wip ~f:Fn.id
                 | true ->
                     let open Bootstrap.Form in
                     let validate_address input_value =
                       match
                         Tezai_base58_digest.Identifier.Kt1_address.check
                           input_value
                       with
                       | () -> true
                       | exception _ -> false
                     in
                     let input_valid =
                       Reactive.(
                         match parameter_input with
                         | Some mf -> Michelson.Partial_type.is_valid mf
                         | None -> pure true)
                     in
                     let active =
                       Reactive.(
                         get address ** input_valid
                         |> map ~f:(fun (a, iv) -> validate_address a && iv))
                     in
                     let addr_input =
                       let addr_help a =
                         if validate_address a then
                           Bootstrap.color `Success (t "Thanks, this is valid.")
                         else
                           Bootstrap.color `Danger
                             (t "This is not a valid KT1 address")
                       in
                       [
                         input
                           ~label:(t "The contract to hit the view with:")
                           ~placeholder:
                             (Reactive.pure "This requires a contract address …")
                           ~help:
                             Reactive.(
                               bind
                                 (get address
                                 ** get
                                      (Contract_metadata.Uri.Fetcher
                                       .current_contract ctxt))
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
                           (Reactive.Bidirectional.of_var address);
                       ]
                     in
                     let param_input =
                       match parameter_input with
                       | None -> [ magic (t "No parameter") ]
                       | Some mf -> Michelson.Partial_type.to_form_items mf
                     in
                     make ~enter_action:go_action
                       (addr_input @ param_input
                       @ [ submit_button ~active (t "Go!") go_action ]))))

let michelson_instruction s =
  link (t s) ~target:(Fmt.str "https://michelson.nomadic-labs.com/#instr-%s" s)

let metadata_validation_error _ctxt =
  let module Validation =
    Tezai_contract_metadata_manipulation.Content_validation
  in
  let open Validation.Error in
  let the_off_chain_view view = t "The off-chain-view “" % ct view % t "”" in
  function
  | Forbidden_michelson_instruction { view; instruction } ->
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
  | Michelson_version_not_a_protocol_hash { view; value } ->
      the_off_chain_view view
      % t " references a wrong version of Michelson ("
      % ct value
      % t "), it should be a valid protocol hash, like "
      % ct "PsDELPH1Kxsxt8f9eWbxQeRxkjfbxoqM52jvs5Y5fBxWWh4ifpo"
      % t "."

let metadata_validation_warning _ctxt =
  let module Validation =
    Tezai_contract_metadata_manipulation.Content_validation
  in
  let open Validation.Warning in
  function
  | Wrong_author_format author ->
      t "The author" %% Fmt.kstr ct "%S" author
      %% t "has a wrong format, it should look like"
      %% ct "Print Name <contact-url-or-email>"
      % t "."
  | Unexpected_whitespace { field; value } ->
      t "The field" %% ct field %% t "(=" %% Fmt.kstr ct "%S" value
      % t ") uses confusing white-space characters."
  | Self_unaddressed { view; instruction } ->
      t "The off-chain-view" %% ct view %% t "uses the instruction"
      %% michelson_instruction "SELF"
      %% (match instruction with
         | None -> t "not followed by any instruction."
         | Some i -> t "followed by" %% michelson_instruction i % t ".")
      %% t "The current recommendation is to only use the combination"
      %% Bootstrap.monospace
           (michelson_instruction "SELF"
           % t ";"
           %% michelson_instruction "ADDRESS")
      %% t "in off-chain-views."

let multimedia_from_tzip16_uri ?(mime_types = []) ctxt ~title ~uri =
  let show = Reactive.var false in
  let result = Async_work.empty () in
  let known_mime_type = List.Assoc.find mime_types ~equal:String.equal uri in
  let web_address =
    let open Contract_metadata.Uri in
    match validate uri with
    | Ok uri16, _ -> to_web_address ctxt uri16
    | _ -> None
  in
  let hide_show_button =
    let button = Bootstrap.button ~kind:`Info ~size:`Small ~outline:true in
    let fetch_and_show () =
      button (Fmt.kstr t "Fetch & Show Content") ~action:(fun () ->
          Async_work.wip result;
          Reactive.set show true;
          Async_work.log result (t "Getting image: " %% ct uri);
          Async_work.async_catch result
            ~exn_to_html:(Errors_html.exception_html ctxt)
            Lwt.Infix.(
              fun ~mkexn () ->
                (* TODO: MLN - is this getting validated twice and should uri be passed to fetch instead of uri16? *)
                match Contract_metadata.Uri.validate uri with
                | Ok uri16, _ ->
                    Lwt.catch
                      (fun () ->
                        Contract_metadata.Uri.fetch ctxt uri16 ~log:(fun s ->
                            Async_work.log result (it "Fetching Image:" %% t s)))
                      (fun e ->
                        raise (mkexn (Errors_html.exception_html ctxt e)))
                    >>= fun content ->
                    let format = Blob.guess_format content in
                    let content_type =
                      match format with
                      | Some ((`Image, _) as f) -> Blob.Format.to_mime f
                      | Some ((`Appx, _) as f) -> Blob.Format.to_mime f
                      | Some ((`Html, _) as f) -> Blob.Format.to_mime f
                      | _ ->
                          Async_work.log result
                            (bt "WARNING: Cannot guess content type …");
                          "image/jpeg"
                    in
                    let src =
                      Fmt.str "data:%s;base64,%s" content_type
                        (Base64.encode_exn ~pad:true
                           ~alphabet:Base64.default_alphabet content)
                    in
                    Async_work.ok result
                      (div
                         (match format with
                         | None ->
                             i
                               (t "Could not guess the format, so went with"
                               %% ct content_type % t ".")
                         | Some _ -> i (t "Guessed format" %% bt content_type))
                      %% div
                           (match content_type with
                           | image when String.is_prefix image ~prefix:"image/"
                             ->
                               H5.img
                                 ~a:
                                   [ H5.a_style (Lwd.pure "max-height: 500px") ]
                                 ~src:(Lwd.pure src)
                                 ~alt:(Fmt.kstr Lwd.pure "Image: %s" title)
                                 ()
                           | vid when String.is_prefix vid ~prefix:"video/" ->
                               H5.video
                                 ~a:
                                   [ H5.a_controls (); style "max-width: 100%" ]
                                 ~src:(Lwd.pure src) []
                           | app_x
                             when String.is_prefix app_x ~prefix:"application/"
                             ->
                               H5.iframe
                                 ~a:
                                   [
                                     H5.a_src (Lwd.pure src);
                                     H5.a_style (Lwd.pure "max-width: 100%");
                                   ]
                                 [
                                   H5.txt (Lwd.pure "This should be an iframe");
                                 ]
                           | html when String.equal html "text/html" ->
                               H5.iframe
                                 ~a:
                                   [
                                     H5.a_src (Lwd.pure src);
                                     H5.a_style (Lwd.pure "max-width: 100%");
                                   ]
                                 [
                                   H5.txt (Lwd.pure "This should be an iframe");
                                 ]
                           | _other ->
                               raise (mkexn (t "Unexpected content type"))));
                    Lwt.return ()
                | Error error, _ -> raise (mkexn (error_trace ctxt error))))
    in
    let show_button ~web ~mime () =
      button (Fmt.kstr t "Show Content") ~action:(fun () ->
          Reactive.set show true;
          let ipfs_uri uri =
            let prefix = "ipfs://" in
            let pre_len = String.length prefix in
            let suffix =
              if String.is_prefix ~prefix uri then
                String.sub uri ~pos:pre_len ~len:(String.length uri - pre_len)
              else uri
            in
            Ipfs_gateways.current_gateway ctxt ^ suffix
          in
          let content =
            match mime with
            | image when String.is_prefix image ~prefix:"image/" ->
                link ~target:web
                  (H5.img
                     ~a:[ style "max-width: 100%" ]
                     ~alt:(Fmt.kstr Lwd.pure "%s at %s" title web)
                     ~src:(Lwd.pure web) ())
            | vid when String.is_prefix vid ~prefix:"video/" ->
                H5.video
                  ~a:[ H5.a_controls (); style "max-width: 100%" ]
                  ~src:(Lwd.pure web) []
            | app_x when String.equal app_x "application/x-directory" ->
                let converted_uri = ipfs_uri uri in
                H5.iframe
                  ~a:
                    [
                      H5.a_src (Lwd.pure converted_uri);
                      H5.a_style (Lwd.pure "max-width: 100%");
                    ]
                  [ H5.txt (Lwd.pure "This should be an iframe") ]
            | html when String.equal html "text/html" ->
                H5.iframe
                  ~a:[ H5.a_src (Lwd.pure uri); style "max-width: 100%" ]
                  [ H5.txt (Lwd.pure "This should be an iframe") ]
            | _ ->
                Bootstrap.alert ~kind:`Danger
                  (bt "Unknown MIME-Type:" %% ct mime)
                %% bt "You may try the fetch-and-guess method:"
                %% fetch_and_show ()
          in
          Async_work.ok result content)
    in
    Reactive.bind_var show ~f:(function
      | true ->
          button (Fmt.kstr t "Hide Content") ~action:(fun () ->
              Reactive.set show false)
      | false -> (
          match (web_address, known_mime_type) with
          | Some web, Some mime -> show_button ~web ~mime ()
          | None, Some "video/mp4" ->
              Bootstrap.alert ~kind:`Danger
                (bt "Not implemented:" %% t "Sorry," %% ct "tezos-storage:"
                %% t "URIs that point to videos are not supported"
                %% parens
                     (t
                        "Come on! You're running all of this in your browser, \
                         what did you expect‽"))
          | _, _ -> fetch_and_show ()))
  in
  let content =
    Reactive.bind_var show ~f:(function
      | true -> Async_work.render result ~f:Fn.id
      | false -> empty ())
  in
  div
    (div
       (t "URI:"
       %% (let c = ct uri in
           match web_address with Some target -> link ~target c | None -> c)
       %% (match known_mime_type with
          | None -> empty ()
          | Some m -> parens (t "known MIME-Type:" %% ct m))
       %% hide_show_button %% t "(Potentially NSFW)")
    %% content)

module Printer_dsl = struct
  type t =
    [ `Open_right_range of Z.t | `Page of Z.t | `Range of Z.t * Z.t ] list

  let parse _ctxt iter_input =
    let to_z s =
      try Z.of_string s
      with _ -> Fmt.failwith "The string %S is not an integer." s
    in
    match
      List.concat_map (String.split ~on:',' iter_input) ~f:(fun item ->
          match String.strip item with
          | "" -> []
          | item -> (
              match String.split item ~on:'-' with
              | [ one ] -> [ `Page (to_z one) ]
              | [ one; "" ] -> [ `Open_right_range (to_z one) ]
              | [ one; two ] -> [ `Range (to_z one, to_z two) ]
              | _ -> Fmt.failwith "Cannot parse component: %S." item))
    with
    | [] -> Ok (`Printer_spec [ `Open_right_range Z.zero ])
    | more -> Ok (`Printer_spec more)
    | exception Failure s ->
        Error Message.(t "Wrong format:" %% ct iter_input % t ":" %% t s)
end

let show_micheline_result = function
  | Ok node -> ct (Tezai_michelson.Concrete_syntax.to_string node)
  | Error s -> Bootstrap.color `Danger (t s)

let show_total_supply (_ctxt : _ Context.t) ?decimals z =
  match Option.map ~f:Int.of_string decimals with
  | Some decimals ->
      let dec = Float.(Z.to_float z / (10. ** of_int decimals)) in
      Fmt.kstr t "%s (%a Units)"
        (Float.to_string_hum ~delimiter:' ' ~decimals ~strip_zero:true dec)
        Z.pp_print z
  | None | (exception _) -> Fmt.kstr t "%a Units (no decimals)" Z.pp_print z

let show_total_supply_result ctxt ?decimals ts =
  match ts with
  | Ok (Tezos_micheline.Micheline.Int (_, z)) ->
      show_total_supply ctxt ?decimals z
  | other -> ct "Error: " %% show_micheline_result other

let show_extras ctxt (extr : (String.t * String.t, Message.t) Result.t List.t) =
  let show_bytes b =
    match Michelson.Partial_type.bytes_guesses (`Raw_string b) with
    | `Dont_know -> Fmt.kstr ct "%S" b
    | `Number f -> it (Float.to_string_hum ~delimiter:' ' ~strip_zero:true f)
    | `Bool true -> it "True"
    | `Bool false -> it "False"
    | `Web_uri wuri -> url it wuri
    | `Tzip16_uri wuri -> tzip16_uri_short ctxt wuri
    | `Json json -> pre (ct (Ezjsonm.value_to_string json ~minify:false))
    | `Just_hex h -> ct h
    | `Valid_utf_8 (_, [ one_line ]) -> t one_line
    | `Valid_utf_8 (_, lines) ->
        div
          (list
             (oxfordize_list ~map:t lines
                ~sep:(fun () -> H5.br ())
                ~last_sep:(fun () -> H5.br ())))
  in
  itemize
    (List.map extr ~f:(function
      | Ok (k, v) -> Fmt.kstr ct "%S" k %% t "→" %% show_bytes v
      | Error m -> Message_html.render ctxt m))

let show_one_token ?symbol ?name ?decimals ?total_supply ?extras
    ?low_level_contents ?tzip_021 ctxt ~id ~warnings =
  let open Contract_metadata.Content in
  let open Tzip_021 in
  let or_empty o f = match o with None -> empty () | Some o -> f o in
  let basics =
    List.filter_opt
      [
        Option.map symbol ~f:(fun s -> t "symbol:" %% ct s);
        Option.map total_supply ~f:(fun s ->
            t "total-supply:" %% show_total_supply_result ctxt ?decimals s);
        Option.map decimals ~f:(fun s -> t "decimals:" %% ct s);
      ]
    |> function
    | [] -> empty ()
    | more ->
        t "["
        %% list
             (oxfordize_list more ~map:Fn.id
                ~sep:(fun () -> t " | ")
                ~last_sep:(fun () -> t " | "))
        %% t "]"
  in
  let validate_address input_value =
    match Tezai_base58_digest.Identifier.Kt1_address.check input_value with
    | () -> Some input_value
    | exception _ -> None
  in
  let token_viewer_link ctxt =
    let input_value = State.explorer_input_value ctxt in
    let validated = validate_address input_value in
    match validated with
    | Some s ->
        open_in_token_viewer ctxt ~token_address:s ~token_id:(Z.to_string id)
    | None -> empty ()
  in
  Bootstrap.bordered ~kind:`Info
    ~a:[ style "padding: 5px" ]
    (Bootstrap.div_lead
       (Fmt.kstr bt "Token %a" Z.pp_print id
       %% or_empty name (function
            | "" -> Bootstrap.color `Danger (t "<empty-name>")
            | n -> Bootstrap.color `Primary (Fmt.kstr it "“%s”" n))
       %% basics %% token_viewer_link ctxt)
    %% (match List.rev_map ~f:snd warnings with
       | [] -> empty ()
       | more ->
           let one = match more with [ _ ] -> true | _ -> false in
           div
             (Bootstrap.alert ~kind:`Warning
                (Fmt.kstr bt "TZIP-012 Warning%s:" (if one then "" else "s")
                %% if one then List.hd_exn more else itemize more)))
    %% or_empty tzip_021 (fun (tzip21 : Contract_metadata.Content.Tzip_021.t) ->
           if is_empty tzip21 then div (t "No TZIP-021 was found by TZComet.")
           else
             div
               (let intro =
                  link ~target:tzip_021_url (bt "TZIP-021")
                  %% Fmt.kstr bt
                       "→ This claims to be a %stransferable %sFungible-Token"
                       (match tzip21.transferable with
                       | Some false -> "non-"
                       | _ -> "")
                       (match tzip21.boolean_amount with
                       | Some true -> "Non-"
                       | _ -> "")
                in
                let images =
                  [
                    ("Thumbnail", tzip21.thumbnail);
                    ("Display-Image", tzip21.display);
                    ("Artifact", tzip21.artifact);
                  ]
                in
                let itembox c =
                  div
                    ~a:
                      [
                        style
                          "margin: 0px 10px 0px 10px;\n\
                           border-left: solid 1px #888;\n\
                           padding-left: 10px";
                      ]
                    (Bootstrap.bordered ~kind:`Light c)
                in
                let local_warnings = ref [] in
                let warn m = local_warnings := m :: !local_warnings in
                let symbol_stuff =
                  match (tzip21.prefers_symbol, symbol) with
                  | Some true, Some symb ->
                      itembox
                        (bt "Prefers to go by its symbol:" %% ct symb % t ".")
                  | Some true, None ->
                      warn
                        Message.(
                          t
                            "This token claims to prefer using its symbol but \
                             it does not seem to have one.");
                      empty ()
                  | _, _ -> empty ()
                in
                intro
                % or_empty tzip21.description (fun desc ->
                      itembox (bt "Description:" %% blockquote (it desc)))
                % symbol_stuff
                % or_empty tzip21.creators (function
                    | [] -> itembox (bt "Creators list is explicitly empty.")
                    | [ one ] -> itembox (bt "Creator:" %% it one)
                    | sl ->
                        itembox (bt "Creators:" %% itemize (List.map sl ~f:it)))
                % or_empty tzip21.tags (fun sl ->
                      itembox
                        (bt "Tags:"
                        %% list
                             (oxfordize_list sl
                                ~map:(fun t -> ct t)
                                ~sep:(fun () -> t ", ")
                                ~last_sep:(fun () -> t ", and "))))
                %% list
                     (List.map images ~f:(function
                       | _, None -> empty ()
                       | title, Some uri ->
                           itembox
                             (Fmt.kstr bt "%s:" title
                             %% multimedia_from_tzip16_uri ctxt ~title
                                  ~mime_types:(uri_mime_types tzip21) ~uri)))
                %% or_empty tzip21.formats (fun l ->
                       itembox
                         (bt "Formats"
                         %% itemize
                              (List.map l ~f:(fun fmt ->
                                   or_empty fmt.uri (fun u -> t "URI:" %% ct u)
                                   %% or_empty fmt.mime_type (fun u ->
                                          t "MIME-Type:" %% ct u)
                                   %%
                                   match fmt.other with
                                   | [] -> empty ()
                                   | more ->
                                       t "+"
                                       %% ct Ezjsonm.(value_to_string (`O more))))
                         ))
                %% or_empty tzip21.attributes (fun l ->
                       itembox
                         (bt "Attributes"
                         %% itemize
                              (List.map l ~f:(fun attr ->
                                   let or_missing n f = function
                                     | None ->
                                         Bootstrap.color `Danger
                                           (Fmt.kstr it "Missing %s!" n)
                                     | Some s -> f s
                                   in
                                   or_missing "name" bt attr.name
                                   % t ":"
                                   %% or_missing "value" ct attr.v
                                   %%
                                   match attr.t with
                                   | None -> empty ()
                                   | Some (`Custom s) ->
                                       parens (t "Type:" %% ct s)))))
                %%
                match tzip21.warnings with
                | [] -> empty ()
                | warns ->
                    itembox
                      (Bootstrap.alert ~kind:`Warning
                         (bt "TZIP-021 Warnings:"
                         %% itemize
                              (List.map warns ~f:(Message_html.render ctxt))))))
    %% or_empty
         (Option.bind extras ~f:(function [] -> None | s -> Some s))
         (fun e -> div (bt "Extra-info (not parsed):" %% show_extras ctxt e))
    %% or_empty low_level_contents (fun (uri, json) ->
           let open Bootstrap.Collapse in
           let collapse = make () in
           let btn =
             make_button collapse ~kind:`Secondary
               (* ~style:(Reactive.pure (Fmt.str "width: 8em")) *)
               (Reactive.bind (collapsed_state collapse) ~f:(function
                 | true -> t "Show Low-Level Values"
                 | false -> t "Hide Low-Level Values"))
           in
           let dv =
             make_div collapse (fun () ->
                 div
                   (t "Low-level token-metadata:"
                   % itemize
                       [
                         t "Uri" %% ct uri;
                         t "Contents:"
                         %% pre
                              (code
                                 (t
                                    (Ezjsonm.value_to_string ~minify:false json)));
                       ]))
           in
           btn %% dv)
       (* %% itemize
            (List.filter_map tok ~f:(function
              | _, None -> None
              | k, Some v -> Some (it k %% v))) *))

let explore_tokens_action ?token_metadata_big_map ctxt ~token_metadata_view ~how
    ~is_tzip21 ~total_supply_view wip_explore_tokens =
  let open Contract_metadata.Content in
  Async_work.reinit wip_explore_tokens;
  Async_work.wip wip_explore_tokens;
  let address =
    Reactive.var
      (Contract_metadata.Uri.Fetcher.current_contract ctxt
      |> Reactive.peek
      |> Option.value ~default:"KT1TododoTodo")
  in
  let log_prompt = ref "Exploring tokens" in
  let log s =
    Async_work.log wip_explore_tokens
      (it !log_prompt %% t "→" %% Bootstrap.monospace (t s))
  in
  Async_work.async_catch wip_explore_tokens
    ~exn_to_html:(Errors_html.exception_html ctxt)
    Lwt.Infix.(
      fun ~mkexn () ->
        let address = Reactive.peek address in
        let make_map_tokens () =
          match how with
          | `All_tokens_view all_tokens_view ->
              Contract_metadata.Content.call_view_or_fail ctxt all_tokens_view
                ~parameter_string:"Unit" ~address ~log
              >>= fun tokens_mich ->
              let tokens =
                match tokens_mich with
                | Seq (_, nodes) as seq_node ->
                    List.map nodes ~f:(function
                      | Int (_, n) -> n
                      | _ ->
                          raise
                            (mkexn
                               (t "Wrong Micheline structure for result:"
                               %% ct
                                    (Michelson.micheline_node_to_string seq_node)
                               )))
                | n ->
                    raise
                      (mkexn
                         (t "Wrong Micheline structure for result:"
                         %% ct (Michelson.micheline_node_to_string n)))
              in
              Fmt.kstr log "Got list of tokens %a"
                Fmt.(Dump.list Z.pp_print)
                tokens;
              Lwt.return (fun f -> Lwt_list.map_s f tokens)
          | `Printer_spec (spec : _ list) ->
              let open Printer_dsl in
              Lwt.return (fun f ->
                  let already_seen = ref Z_set.empty in
                  let failures = ref 0 in
                  let max_failures = 3 in
                  let rec go : t -> unit Lwt.t =
                    let run_f n next =
                      if Z_set.mem n !already_seen then Lwt.return_unit
                      else
                        Lwt.catch
                          (fun () ->
                            f n >>= fun () ->
                            already_seen := Z_set.add n !already_seen;
                            go next)
                          (fun _exn ->
                            Int.incr failures;
                            go next)
                    in
                    function
                    | [] -> Lwt.return_unit
                    | _ :: _ when !failures > max_failures -> Lwt.return_unit
                    | `Page n :: more -> run_f n more
                    | `Open_right_range n :: more ->
                        let next = `Open_right_range (Z.succ n) :: more in
                        run_f n next
                    | `Range (l, r) :: more ->
                        let next =
                          if Z.geq l r then more
                          else `Range (Z.succ l, r) :: more
                        in
                        run_f l next
                  in
                  go spec >>= fun () -> Lwt.return_nil)
        in
        let explore_token id =
          log_prompt := Fmt.str "Exploring token %a" Z.pp_print id;
          Contract_metadata.Content.maybe_call_view ctxt token_metadata_view
            ~parameter_string:(Z.to_string id) ~address ~log
          >>= fun metadata_map_opt ->
          begin
            begin
              match (metadata_map_opt, token_metadata_big_map) with
              | Some s, _ ->
                  log "Using the token_metadata off-chain-view.";
                  Lwt.return s
              | None, Some big_map_id ->
                  log "Using the %token_metadata big-map.";
                  Query_nodes.find_node_with_contract ctxt address
                  >>= fun node ->
                  Fmt.kstr log "Using %s" node.Query_nodes.Node.name;
                  Query_nodes.Node.micheline_value_of_big_map_at_nat ctxt node
                    ~log ~big_map_id ~key:id
                  >>= fun mich ->
                  Fmt.kstr log "Got value from big-map";
                  Lwt.return (Ok mich)
              | None, None -> Decorate_error.raise Message.(t "Not available")
            end
            >>= fun metadata_map ->
            Contract_metadata.Content.maybe_call_view ctxt total_supply_view
              ~parameter_string:(Z.to_string id) ~address ~log
            >>= fun total_supply ->
            let unpaired_metadata =
              try
                let nope = Decorate_error.raise in
                let ok =
                  match metadata_map with
                  | Error s -> nope Message.(t "Error getting view:" %% ct s)
                  | Ok (Prim (_, "Pair", [ _; full_map ], _)) ->
                      Michelson.Partial_type.micheline_string_bytes_map_exn
                        full_map
                  | Ok other ->
                      nope
                        Message.(
                          t "Metadata result has wrong structure:"
                          %% ct (Michelson.micheline_node_to_string other))
                in
                Ok ok
              with
              | Decorate_error.E { message; _ } -> Error message
              | e -> Error Message.(t "Exception:" %% ct (Exn.to_string e))
            in
            let piece_of_metadata_map k =
              match unpaired_metadata with
              | Ok s -> List.Assoc.find s k ~equal:String.equal
              | Error _ -> None
            in
            let uri = piece_of_metadata_map "" in
            let warnings = ref [] in
            let warn k e =
              if List.exists !warnings ~f:(fun (key, _) -> String.equal key k)
              then ()
              else warnings := (k, e) :: !warnings
            in
            begin
              begin
                match uri with
                | None -> Lwt.return_none
                | Some u -> (
                    match Contract_metadata.Uri.validate u with
                    | Ok uri, _ ->
                        Lwt.catch
                          (fun () ->
                            Contract_metadata.Uri.fetch ctxt uri ~log:(fun _ ->
                                Fmt.kstr log "Fetching %s" u)
                            >>= fun s ->
                            Lwt.return_some (u, Ezjsonm.value_from_string s))
                          (fun exn ->
                            warn "fetch-uri"
                              (bt "Fetching URI" %% ct u %% t "failed:"
                              %% Errors_html.exception_html ctxt exn);
                            Lwt.return_none)
                    | Error error, _ ->
                        warn "parsing-uri"
                          (bt "Parsing URI" %% ct u %% t "failed:"
                         %% error_trace ctxt error);
                        Lwt.return_none)
              end
              >>= fun extra_contents ->
              let piece_of_metadata ?json_type key =
                let metadata_map =
                  match unpaired_metadata with Ok o -> o | Error _ -> []
                in
                Contract_metadata.Token.piece_of_metadata ?json_type
                  ~warn:(fun k m -> warn k (Message_html.render ctxt m))
                  ~key ~metadata_map ~metadata_json:extra_contents
              in
              let symbol = piece_of_metadata "symbol" () in
              let name =
                match piece_of_metadata "name" () with
                | Some "" ->
                    warn "name-is-empty"
                      (t "The" %% ct "name" %% t "field is the empty string.");
                    None
                | o -> o
              in
              let decimals = piece_of_metadata ~json_type:`Int "decimals" () in
              let extras =
                let make_ok_list l ~f =
                  List.filter_map l ~f:(fun (k, v) ->
                      if
                        not
                          (List.mem
                             [ "symbol"; "name"; "decimals"; "" ]
                             k ~equal:String.equal)
                      then Some (Ok (k, f v))
                      else None)
                in
                let from_map =
                  match unpaired_metadata with
                  | Error m -> [ Error m ]
                  | Ok l -> make_ok_list l ~f:Fn.id
                in
                let from_json =
                  match extra_contents with
                  | None -> []
                  | Some (_, `O l) ->
                      let f = function
                        | `String s -> s
                        | `Float f -> Float.to_string f
                        | `Bool b -> Bool.to_string b
                        | other -> Ezjsonm.value_to_string other
                      in
                      make_ok_list l ~f
                  | Some (_, _other) ->
                      [ (* Error already reported above:
                           Message.(
                             t "URI" %% ct u %% t "does not point at a JSON object.") *) ]
                in
                match from_map @ from_json with [] -> None | l -> Some l
              in
              let tzip_021, extras =
                match (is_tzip21, extras) with
                | false, e -> (None, e)
                | true, None -> (None, None)
                | true, Some l ->
                    let tzip21, e =
                      Contract_metadata.Content.Tzip_021.from_extras l
                    in
                    (Some tzip21, Some e)
              in
              Async_work.wip_add_ok wip_explore_tokens
                (show_one_token ctxt ~id ?decimals ?total_supply ?symbol ?name
                   ?low_level_contents:extra_contents ?tzip_021 ?extras
                   ~warnings:!warnings);
              Lwt.return ()
            end
          end
        in
        make_map_tokens () >>= fun map_tokens ->
        map_tokens explore_token >>= fun (_ : unit list) ->
        Async_work.finish wip_explore_tokens;
        Lwt.return ());
  dbgf "go view"

let metadata_substandards ?token_metadata_big_map
    ?(add_explore_tokens_button = true) ctxt metadata =
  Contract_metadata.Content.(
    match classify ?token_metadata_big_map metadata with
    | Tzip_16 t -> (t, [])
    | Tzip_12
        {
          metadata;
          interface_claim;
          get_balance;
          total_supply;
          all_tokens;
          is_operator;
          token_metadata;
          permissions_descriptor;
          _;
        } as t12 ->
        let is_tzip21 = Option.is_some (Tzip_021.claim metadata) in
        let tzip_12_block =
          let errorify c = Bootstrap.color `Danger c in
          let interface_claim =
            t "Interface claim is"
            %%
            match interface_claim with
            | None -> t "missing." |> errorify
            | Some (`Invalid s) ->
                (t "invalid: " |> errorify)
                %% ct s % t ". It should look like" %% ct "TZIP-012[-<version>]"
                % t "."
            | Some (`Version s) -> t "valid, and defines version as" %% ct s
            | Some `Just_interface -> t "valid"
          in
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
            | Invalid { parameter_status; return_status; _ } ->
                errorify (t "View" %% ct name %% t "is invalid:")
                %% itemize
                     [
                       (t "Parameter type"
                       %%
                       match parameter_status with
                       | `Ok, _ -> t "is ok."
                       | `Wrong, None ->
                           (* This should not happen. *)
                           errorify (t "is wrong: not found.")
                       | `Wrong, Some pt ->
                           errorify
                             (t "is wrong:"
                             %% ct
                                  Tezai_michelson.(
                                    Concrete_syntax.to_string
                                      (Untyped.of_canonical_micheline
                                         pt.original)))
                       | `Unchecked_Parameter, None ->
                           errorify (t "is expectedly not defined.")
                       | `Unchecked_Parameter, Some _ ->
                           errorify (t "is defined while it shouldn't.")
                       | `Missing_parameter, _ -> errorify (t "is missing."));
                       (t "Return type"
                       %%
                       match return_status with
                       | `Ok, _ -> t "is ok."
                       | `Wrong, None ->
                           (* This should not happen. *)
                           errorify (t "is wrong: not found.")
                       | `Wrong, Some pt ->
                           errorify
                             (t "is wrong:"
                             %% ct
                                  Tezai_michelson.(
                                    Concrete_syntax.to_string
                                      (Untyped.of_canonical_micheline
                                         pt.original))));
                     ]
            | Valid (_, _) -> t "View" %% ct name %% t "is valid"
          in
          let show_permissions_descriptor pd =
            match pd with
            | None ->
                t
                  "Permissions-descriptor is not present (assuming default \
                   permissions)."
            | Some (Ok _) -> t "Permissions-descriptor is valid."
            | Some (Error e) ->
                errorify (t "Permissions-descriptor is invalid:")
                %% div (error_trace ctxt e)
          in
          let show_tokens_metadata tm =
            view_validation "token_metadata" tm ~missing_add_on:(fun () ->
                match token_metadata_big_map with
                | None when add_explore_tokens_button ->
                    errorify
                      (t
                         "This means that the contract is likely invalid: it \
                          must provide token-specific metadata using a big-map \
                          annotated with"
                      %% ct "%token_metadata"
                      % t " which was not found by TZComet.")
                | None ->
                    t
                      "This means that the contract could be invalid: it must \
                       provide token-specific metadata using a big-map \
                       annotated with"
                    %% ct "%token_metadata"
                    % t " which was not found by TZComet at this point."
                | Some id ->
                    t "This means that token-metadata must be available in the"
                    %% ct "%token_metadata" %% t "big-map:"
                    %% Block_explorer.big_map_display ctxt id
                    % t ".")
          in
          let global_validity =
            Contract_metadata.Content.is_valid
              ~ignore_token_metadata_big_map:(not add_explore_tokens_button)
              t12
          in
          let show_validity_btn, validity_div =
            let validity_details () =
              itemize
                [
                  interface_claim;
                  view_validation "get_balance" get_balance ~mandatory:false;
                  view_validation "total_supply" total_supply;
                  view_validation "all_tokens" all_tokens;
                  view_validation "is_operator" is_operator;
                  show_tokens_metadata token_metadata;
                  show_permissions_descriptor permissions_descriptor;
                ]
            in
            let open Bootstrap.Collapse in
            let collapse = make () in
            let btn =
              make_button collapse
                ~kind:(if global_validity then `Secondary else `Danger)
                (* ~style:(Reactive.pure (Fmt.str "width: 8em")) *)
                (Reactive.bind (collapsed_state collapse) ~f:(function
                  | true -> t "Expand Validity Info"
                  | false -> t "Collapse Validity Info"))
            in
            let dv = make_div collapse (fun () -> validity_details ()) in
            (btn, dv)
          in
          let wip_explore_tokens = Async_work.empty () in
          let can_enumerate_tokens = add_explore_tokens_button in
          let tokens_exploration x = div x in
          let iter_input = Reactive.var "" in
          let explore_tokens_top_button, exploration_expedition_state =
            match can_enumerate_tokens with
            | false -> (empty (), empty ())
            | true ->
                let form_status = Reactive.var `Hidden in
                let mk_button =
                  Bootstrap.button ~kind:`Primary ~size:`Small ~outline:true
                in
                let top_button =
                  Reactive.bind_var form_status ~f:(function
                    | `Hidden ->
                        mk_button (t "Explore Tokens") ~action:(fun () ->
                            Reactive.set form_status `Form)
                    | `Form ->
                        mk_button (t "Cancel Exploration ↓") ~action:(fun () ->
                            Reactive.set form_status `Hidden))
                in
                let exploration =
                  Reactive.bind_var form_status ~f:(function
                    | `Hidden -> empty ()
                    | `Form ->
                        let use_tzip_021 = Reactive.var is_tzip21 in
                        let use_all_tokens =
                          Reactive.var
                            (match all_tokens with
                            | Valid (_, _view) -> true
                            | _ -> false)
                        in
                        let compute_method ~use_all_tokens ~iter_input =
                          match use_all_tokens with
                          | true -> (
                              match all_tokens with
                              | Valid (_, view) -> Ok (`All_tokens_view view)
                              | _ ->
                                  Error
                                    Message.(
                                      t
                                        "Congratulations! You found a bug! \
                                         Please report this:"
                                      %% ct Caml.__LOC__))
                          | false -> Printer_dsl.parse ctxt iter_input
                        in
                        let method_document =
                          Reactive.(get iter_input ** get use_all_tokens)
                          |> Reactive.map
                               ~f:(fun (iter_input, use_all_tokens) ->
                                 compute_method ~iter_input ~use_all_tokens)
                        in
                        let submit_action () =
                          Reactive.set form_status `Hidden;
                          let is_tzip21 =
                            is_tzip21 || Reactive.peek use_tzip_021
                          in
                          let how =
                            match
                              compute_method
                                ~use_all_tokens:(Reactive.peek use_all_tokens)
                                ~iter_input:(Reactive.peek iter_input)
                            with
                            | Ok o -> o
                            | Error _ ->
                                Fmt.failwith
                                  "You found a bug or bypassed form \
                                   validation! %s"
                                  Caml.__LOC__
                          in
                          explore_tokens_action ctxt
                            ~token_metadata_view:token_metadata ~is_tzip21
                            ?token_metadata_big_map ~how
                            ~total_supply_view:total_supply wip_explore_tokens
                        in
                        div
                          (bt "Exploration Options:"
                          % Bootstrap.Form.(
                              make
                                [
                                  check_box
                                    ~label:
                                      (t "Assume the tokens use"
                                      %% link ~target:tzip_021_url
                                           (t "TZIP-021")
                                      %% t "rich metadata.")
                                    (Reactive.Bidirectional.of_var use_tzip_021);
                                  (match all_tokens with
                                  | Valid (_, _) ->
                                      check_box
                                        ~label:
                                          (t "Use the" %% ct "all_tokens"
                                         %% t "off-chain-view.")
                                        (Reactive.Bidirectional.of_var
                                           use_all_tokens)
                                  | _ ->
                                      magic
                                        (i
                                           (t "The" %% ct "all_tokens"
                                           %% t
                                                "off-chain-view is not \
                                                 available to list the tokens."
                                           )));
                                  (let active =
                                     Reactive.(get use_all_tokens |> map ~f:not)
                                   in
                                   input ~label:(t "Search parameters:") ~active
                                     ~placeholder:
                                       (Reactive.map active ~f:(function
                                         | true -> ""
                                         | false -> "Disabled"))
                                     ~help:
                                       (t "How to iterate over Token-Ids."
                                       %% t
                                            "It should look like the “pages” \
                                             thing of a printer job, like"
                                       %% ct "1-4,5,6-"
                                       %% t ", leave the field empty for"
                                       %% ct "0-"
                                       %% t "a.k.a. the YOLO method."
                                       %% Reactive.bind_var iter_input
                                            ~f:(fun s ->
                                              match
                                                compute_method
                                                  ~use_all_tokens:false
                                                  ~iter_input:s
                                              with
                                              | Ok _ -> empty ()
                                              | Error _err ->
                                                  Bootstrap.color `Danger
                                                    (t "WRONG !")))
                                     (Reactive.Bidirectional.of_var iter_input));
                                  magic
                                    (method_document
                                    |> Reactive.bind ~f:(function
                                         | Ok (`All_tokens_view _) ->
                                             t "Explore using the"
                                             %% ct "all_tokens" %% t "view."
                                         | Ok (`Printer_spec l) ->
                                             t "Explore using the"
                                             %% bt "Printer-style"
                                             %% t "method:"
                                             %% list
                                                  (oxfordize_list l
                                                     ~map:(function
                                                       | `Page (n : Z.t) ->
                                                           Fmt.kstr t "page %a"
                                                             Z.pp_print n
                                                       | `Open_right_range n ->
                                                           Fmt.kstr t
                                                             "pages [%a, ∞)"
                                                             Z.pp_print n
                                                       | `Range (l, r) ->
                                                           Fmt.kstr t
                                                             "pages [%a, %a]"
                                                             Z.pp_print l
                                                             Z.pp_print r)
                                                     ~sep:(fun () -> t ", ")
                                                     ~last_sep:(fun () ->
                                                       t ", and "))
                                             % t "."
                                         | Error err ->
                                             Bootstrap.alert ~kind:`Danger
                                               (Message_html.render ctxt err)));
                                  submit_button
                                    ~active:
                                      (Reactive.map method_document ~f:(function
                                        | Ok _ -> true
                                        | Error _ -> false))
                                    (t "Explore!") submit_action;
                                ])))
                in
                (top_button, exploration)
          in
          let validity_qualifier =
            if add_explore_tokens_button then
              parens (t "using storage and metadata")
            else parens (t "only according to metadata")
          in
          div
            (t "This looks like a TZIP-012 contract (a.k.a. FA2);"
            %%
            match global_validity with
            | true ->
                Bootstrap.color `Success (t "it seems valid")
                %% validity_qualifier
                %
                if can_enumerate_tokens then
                  t " and tokens can be enumerated/explored" % t "."
                else t "."
            | false ->
                Bootstrap.color `Danger (t "it is invalid")
                %% validity_qualifier % t ".")
          % div
              (show_validity_btn % explore_tokens_top_button
             (* % explore_tokens_btn % explore_tokens_yolo_btn *)
             % validity_div
              % exploration_expedition_state
              % Async_work.render
                  ~done_empty:(fun () ->
                    Bootstrap.alert ~kind:`Warning (bt "Found no tokens 😞"))
                  wip_explore_tokens ~f:tokens_exploration)
        in
        (metadata, [ field "TZIP-012 Implementation Claim" tzip_12_block ]))

let author ~namet s =
  try
    match String.split (String.strip s) ~on:'<' with
    | [ name; id ] -> (
        match String.lsplit2 id ~on:'>' with
        | Some (u, "") when String.is_prefix u ~prefix:"http" ->
            namet name %% parens (url monot u)
        | Some (u, "")
        (* we won't get into email address regexps here, sorry *)
          when String.mem u '@' ->
            namet name %% parens (link ~target:("mailto:" ^ u) (ct u))
        | _ -> failwith "")
    | _ -> failwith ""
  with _ -> ct s

let metadata_contents ?token_metadata_big_map ~add_explore_tokens_button
    ?open_in_editor_link ctxt =
  let open Tezai_contract_metadata.Metadata_contents in
  fun (*  as *) metadata ->
    let ct = monot in
    let license_elt l =
      let open License in
      it l.name
      % Option.value_map ~default:(empty ()) l.details ~f:(fun d ->
            Fmt.kstr t " → %s" d)
    in
    let url_elt u = url ct u in
    let authors_elt l =
      oxfordize_list l ~map:(author ~namet:t)
        ~sep:(fun () -> t ", ")
        ~last_sep:(fun () -> t ", and ")
      |> list
    in
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
                dbgf "Error in interface html: %a" Exn.pp e;
                the_text)
        in
        Re.split_full (Re.compile r) s |> List.map ~f:tok |> list
      in
      List.map l ~f:interface
      |> oxfordize_list ~map:Fn.id
           ~sep:(fun () -> t ", ")
           ~last_sep:(fun () -> t ", and ")
      |> list
    in
    let _todo l = Fmt.kstr t "todo: %d items" (List.length l) in
    let view_id s = Fmt.str "view-%s" s in
    let view ?(collapsing = false) v =
      let open View in
      let purity =
        if v.is_pure then Bootstrap.color `Success (t "pure")
        else Bootstrap.color `Warning (t "impure")
      in
      let implementations impls =
        let open Implementation in
        itemize
          (List.map impls ~f:(function
            | Michelson_storage view -> michelson_view ctxt ~view
            | Rest_api_query raq ->
                field "REST-API Query"
                  (itemize
                     (normal_field "OpenAPI Spec" (ct raq.specification_uri)
                     @ option_field "Base-URI Override" raq.base_uri ct
                     @ normal_field "Path" (ct raq.path)
                     @ normal_field "Method"
                         (Fmt.kstr ct "%s"
                            (Rest_api_query.string_of_cohttp_compatible_meth
                               raq.meth))))))
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
                | false -> t "Collapse"))
          in
          let dv = make_div collapse (fun () -> content) in
          btn %% dv
        else content
      in
      div
        ~a:[ H5.a_id (Lwd.pure (view_id v.name)) ]
        (bt v.name %% t "(" % purity % t "):"
        % maybe_collapse
            (itemize
               (option_field "Description" v.description paragraphs
               @
               match v.implementations with
               | [] ->
                   [
                     Bootstrap.color `Danger (t "There are no implementations.");
                   ]
               | l ->
                   let name =
                     Fmt.str "Implementation%s"
                       (if List.length l = 1 then "" else "s")
                   in
                   list_field name v.implementations implementations)))
    in
    let views_elt (views : View.t list) =
      let collapsing = List.length views >= 3 in
      itemize (List.map views ~f:(fun v -> view ~collapsing v))
    in
    let source_elt source =
      let open Source in
      let tool s =
        let links =
          [
            ("michelson", "https://tezos.gitlab.io");
            ("smartpy", "https://smartpy.io");
            ("flextesa", "https://tezos.gitlab.io/flextesa");
            ("dune", "https://dune.build");
            ("archetype", "https://archetype-lang.org/");
            ("ligo", "https://ligolang.org/");
            ("cameligo", "https://ligolang.org/");
            ("pascaligo", "https://ligolang.org/");
            ("reasonligo", "https://ligolang.org/");
            ("morley", "https://gitlab.com/morley-framework/morley");
            ("lorentz", "https://serokell.io/project-lorentz-indigo");
            ("indigo", "https://serokell.io/project-lorentz-indigo");
            ("scaml", "https://www.scamlang.dev/");
          ]
        in
        let splitted = String.split ~on:' ' s in
        match splitted with
        | [] | [ "" ] -> t "Empty Tool 👎"
        | one :: more ->
            let uncap = String.lowercase one in
            List.find_map links ~f:(fun (prefix, target) ->
                if String.equal uncap prefix then
                  Some
                    (link ~target (bt one)
                    %
                    match more with
                    | [] -> empty ()
                    | _ -> t " " % parens (ct (String.concat ~sep:" " more)))
                else None)
            |> Option.value ~default:(bt s)
      in
      itemize
        (field "Tools"
           ((oxfordize_list source.tools ~map:tool
               ~sep:(fun () -> t ", ")
               ~last_sep:(fun () -> t ", and ")
            |> list)
           % t ".")
        :: option_field "Location" source.location url_elt)
    in
    let errors_elt ~views errors =
      let open Errors.Translation in
      let langs = function
        | None -> empty ()
        | Some [] -> div (t "[lang=?]")
        | Some more ->
            div
              (t "[lang="
              % list
                  (let sep () = t "|" in
                   oxfordize_list more ~map:ct ~sep ~last_sep:sep)
              % t "]")
      in
      let expand (Michelson_blob.Michelson_blob m as mm) =
        let open Tezos_micheline.Micheline in
        let qit s =
          abbreviation
            (Fmt.str "Michelson: %s" (mich mm))
            (Fmt.kstr it "“%s”" s)
        in
        match root m with
        | String (_, s) -> qit s
        | Bytes (_, b) -> qit (Bytes.to_string b)
        | _ -> ct (mich mm)
      in
      let view_link name =
        List.find views ~f:(fun v -> String.equal name v.View.name) |> function
        | None ->
            ct name %% small (Bootstrap.color `Danger (t "(Cannot find it!)"))
        | Some v ->
            let collapse = Bootstrap.Collapse.make () in
            Bootstrap.Collapse.make_button ~kind:`Secondary collapse (ct name)
            % Bootstrap.Collapse.make_div collapse (fun () -> view v)
      in
      itemize
        (List.map errors ~f:(function
          | Static { error; expansion; languages } ->
              field "Static-translation"
                (ct (mich error) %% t "→" %% expand expansion %% langs languages)
          | Dynamic { view_name; languages } ->
              field "Dynamic-view" (view_link view_name %% langs languages)))
    in
    let unknown_extras kv =
      itemize
        (List.map kv ~f:(fun (k, v) ->
             ct k %% pre (ct (Ezjsonm.value_to_string ~minify:false v))))
    in
    let ( {
            name;
            description;
            version;
            license;
            authors;
            homepage;
            source;
            interfaces;
            errors;
            views;
            unknown;
          },
          sub_standards ) =
      metadata_substandards ?token_metadata_big_map ~add_explore_tokens_button
        ctxt metadata
    in
    (match open_in_editor_link with
    | Some content -> div (open_in_editor ctxt content)
    | None -> empty ())
    % itemize
        (option_field "Name" name ct
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
        @ list_field "Extra/Unknown" unknown unknown_extras)

let big_answer level content =
  let kind = match level with `Ok -> `Success | `Error -> `Danger in
  h2 (Bootstrap.color kind content)

let show_metadata_full_validation ?token_metadata_big_map ctxt
    ~add_explore_tokens_button ~add_open_in_editor_button
    ~show_validation_big_answer inpo =
  (* let open Tezos_contract_metadata.Contract_metadata.Content in *)
  let module Validation =
    Tezai_contract_metadata_manipulation.Content_validation
  in
  match Contract_metadata.Content.of_json inpo with
  | Ok (legacy_warnings, m) ->
      let errs, warns =
        Validation.validate m ~protocol_hash_is_valid:(fun s ->
            try
              let () = Tezai_base58_digest.Identifier.Protocol_hash.check s in
              true
            with e ->
              dbgf "Protocol hash problem: %a" Exn.pp e;
              false)
      in
      let header =
        if show_validation_big_answer then
          match (errs, warns, legacy_warnings) with
          | [], [], [] -> big_answer `Ok (t "This metadata JSON is VALID 👌")
          | _ :: _, _, _ | _, _, _ :: _ ->
              big_answer `Error
                (t "This metadata JSON parses Okay but is not valid 👎")
          | [], _ :: _, [] ->
              big_answer `Ok
                (t "This metadata JSON is valid 👍, with warnings …")
        else empty ()
      in
      let hn = h5 in
      let section title reasons to_html =
        match reasons with
        | [] -> empty ()
        | more -> hn title %% itemize (List.map more ~f:to_html)
      in
      let legacy_warning = function
        | `Fixed_legacy (old, fixed) ->
            t "The metadata uses the field"
            %% ct old %% t "instead of" %% ct fixed
            %% t "from the latest specification."
      in
      header
      % section (t "Old-Format Errors") legacy_warnings legacy_warning
      % section (t "Errors") errs (metadata_validation_error ctxt)
      % section (t "Warnings") warns (metadata_validation_warning ctxt)
      % hn (t "Contents")
      % metadata_contents ?token_metadata_big_map
          ?open_in_editor_link:
            (if add_open_in_editor_button then Some inpo else None)
          ctxt m ~add_explore_tokens_button
  | Error el ->
      big_answer `Error (t "This metadata JSON is not valid 🥸")
      % error_trace ctxt el
