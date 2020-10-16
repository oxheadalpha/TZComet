open Import

module Work_status = struct
  type log_item = Html_types.div_content_fun Meta_html.H5.elt
  type 'a status = Empty | Work_in_progress | Done of ('a, log_item) Result.t
  type 'a t = {logs: log_item Reactive.Table.t; status: 'a status Reactive.var}

  let empty () = {logs= Reactive.Table.make (); status= Reactive.var Empty}

  let reinit s =
    Reactive.Table.clear s.logs ;
    Reactive.set s.status Empty

  let log t item = Reactive.Table.append' t.logs item
  let wip t = Reactive.set t.status Work_in_progress
  let ok t o = Reactive.set t.status (Done (Ok o))
  let error t o = Reactive.set t.status (Done (Error o))

  let busy {status; _} =
    Reactive.(
      get status |> map ~f:(function Work_in_progress -> true | _ -> false))

  let async_catch wip f =
    let open Lwt in
    let exception Work_failed of log_item in
    async (fun () ->
        catch
          (fun () -> f ~fail:(fun x -> raise (Work_failed x)) ())
          Meta_html.(
            function
            | Work_failed l -> error wip l ; return ()
            | Failure s ->
                error wip (t "Failure: " %% ct s) ;
                return ()
            | exn ->
                error wip (t "Exception: " %% ct (Exn.to_string exn)) ;
                return ()))

  let render work_status ~f =
    let open Meta_html in
    let show_logs ?(wip = false) () =
      let make_logs_map _ x = H5.li [x] in
      let logs = Reactive.Table.concat_map ~map:make_logs_map work_status.logs in
      div
        ~a:[classes ["bg-dark"; "text-white"]]
        (H5.ul
           ( if wip then
             [logs; H5.li [Bootstrap.spinner ~kind:`Info (t "Working …")]]
           else [logs] )) in
    Reactive.bind_var work_status.status ~f:(function
      | Empty -> empty ()
      | Work_in_progress ->
          Bootstrap.alert ~kind:`Secondary (show_logs ~wip:true ())
      | Done (Ok x) ->
          let collapse = Bootstrap.Collapse.make ~button_kind:`Secondary () in
          Bootstrap.alert ~kind:`Success
            ( h4 (t "Success:")
            % div (f x)
            %% collapse#button (t "Show logs")
            % collapse#div (show_logs ~wip:false ()) )
      | Done (Error e) ->
          let collapse = Bootstrap.Collapse.make ~button_kind:`Secondary () in
          Bootstrap.alert ~kind:`Danger
            ( h4 (t "Errror:")
            % div ~a:[classes ["lead"]] e
            %% collapse#button (t "Show logs")
            % collapse#div (show_logs ~wip:false ()) ))
end

module State = struct
  module Page = struct
    type t = Explorer | Settings | About | Editor

    let to_string = function
      | Explorer -> "Explorer"
      | Editor -> "Editor"
      | Settings -> "Settings"
      | About -> "About"

    let all_in_order = [Explorer; Editor; Settings; About]
  end

  open Page

  type t =
    { page: Page.t Reactive.var
    ; version_string: string option
    ; dev_mode: bool Reactive.var
    ; explorer_input: string Reactive.var
    ; explorer_go: bool Reactive.var
    ; explorer_went: bool Reactive.var
    ; explorer_result: Html_types.div_content_fun Meta_html.H5.elt Work_status.t
    }

  let get (state : < gui: t ; .. > Context.t) = state#gui

  module Fragment = struct
    let to_string = Uri.to_string
    let pp = Uri.pp

    let page_to_path page =
      Fmt.str "/%s" (Page.to_string page |> String.lowercase)

    let make ~page ~dev_mode ~explorer_input ~explorer_go =
      let query =
        match explorer_input with
        | "" -> []
        | more -> [("explorer-input", [more])] in
      let query = if not dev_mode then query else ("dev", ["true"]) :: query in
      let query = if not explorer_go then query else ("go", ["true"]) :: query in
      Uri.make () ~path:(page_to_path page) ~query

    let change_for_page t page = Uri.with_path t (page_to_path page)

    let parse fragment =
      let uri = Uri.of_string (Uri.pct_decode fragment) in
      let pagename = Uri.path uri |> String.chop_prefix_if_exists ~prefix:"/" in
      let page =
        List.find all_in_order ~f:(fun page ->
            String.equal
              (String.lowercase (Page.to_string page))
              (pagename |> String.lowercase))
        |> Option.value ~default:Explorer in
      let query = Uri.query uri in
      let in_query = List.Assoc.find ~equal:String.equal query in
      let true_in_query q =
        match in_query q with Some ["true"] -> true | _ -> false in
      let dev_mode = true_in_query "dev" in
      let explorer_input =
        match in_query "explorer-input" with Some [one] -> one | _ -> "" in
      let explorer_go = true_in_query "go" in
      { page= Reactive.var page
      ; version_string= None
      ; dev_mode= Reactive.var dev_mode
      ; explorer_input= Reactive.var explorer_input
      ; explorer_go= Reactive.var explorer_go
      ; explorer_went=
          (* If page is not the explorer we will ignore the command =
             assume it aready happened. *)
          Reactive.var Poly.(page <> Page.Explorer)
      ; explorer_result= Work_status.empty () }
  end

  let create () =
    (*   let {Fragment.page; dev_mode; explorer_input} = *)
    let fragment = Js_of_ocaml.Url.Current.get_fragment () in
    Fragment.parse fragment

  (* in
     { page= Reactive.var page
     ; version_string= None
     ; dev_mode= Reactive.var dev_mode
     ; explorer_input= Reactive.var explorer_input } *)

  let version_string state = (get state).version_string
  let set_page state p () = Reactive.set (get state).page p
  let page state = (get state).page |> Reactive.get
  let explorer_result ctxt = (get ctxt).explorer_result

  let current_page_is_not state p =
    Reactive.get (get state).page |> Reactive.map ~f:Poly.(( <> ) p)

  let dev_mode state = (get state).dev_mode |> Reactive.get

  let dev_mode_bidirectional state =
    (get state).dev_mode |> Reactive.Bidirectrional.of_var

  let explorer_input state = (get state).explorer_input |> Reactive.get
  let explorer_input_value state = (get state).explorer_input |> Reactive.peek
  let set_explorer_input state = (get state).explorer_input |> Reactive.set

  let explorer_input_bidirectional state =
    (get state).explorer_input |> Reactive.Bidirectrional.of_var

  let make_fragment ctxt =
    (* WARNING: for now it is important for this to be attached "somewhere"
       in the DOM.
       WARNING-2: this function is used for side effects unrelated to the
       fragment too (system.dev_mode).
    *)
    let open Js_of_ocaml.Url in
    let state = get ctxt in
    let dev = Reactive.get state.dev_mode in
    let page = Reactive.get state.page in
    let explorer_input = Reactive.get state.explorer_input in
    let explorer_go = Reactive.get state.explorer_go in
    Reactive.(dev ** page ** explorer_input ** explorer_go)
    |> Reactive.map ~f:(fun (dev_mode, (page, (explorer_input, explorer_go))) ->
           let current = Js_of_ocaml.Url.Current.get_fragment () in
           let now =
             Fragment.(make ~page ~dev_mode ~explorer_input ~explorer_go) in
           dbgf "Updating fragment %S → %a" current Fragment.pp now ;
           Current.set_fragment (Fragment.to_string now) ;
           System.set_dev_mode ctxt dev_mode ;
           now)

  let if_explorer_should_go state f =
    if
      (get state).explorer_go |> Lwd.peek
      && not ((get state).explorer_went |> Lwd.peek)
    then (
      Lwd.set (get state).explorer_went true ;
      f () )
    else ()

  let examples state =
    let https_ok =
      "https://raw.githubusercontent.com/tqtezos/TZComet/master/data/metadata_example0.json"
    in
    let hash_of_https_ok =
      (* `sha256sum data/metadata_example0.json` → Achtung, the URL
         above takes about 5 minutes to be up to date with `master` *)
      "7d961916f05d72afc765389a695458a9b451954419b41fa3cdd5fa816128b744" in
    let sha256_https_ok =
      Fmt.str "sha256://0x%s/%s" hash_of_https_ok (Uri.pct_encode https_ok)
    in
    let sha256_https_ko =
      Fmt.str "sha256://0x%s/%s"
        (String.tr hash_of_https_ok ~target:'9' ~replacement:'1')
        (Uri.pct_encode https_ok) in
    (get state).dev_mode |> Reactive.get
    |> Reactive.map ~f:(fun dev ->
           let contracts = ref [] in
           let uris = ref [] in
           let kt1 v desc = contracts := (v, desc) :: !contracts in
           let kt1_dev v desc = if dev then kt1 v desc in
           let uri v desc = uris := (v, desc) :: !uris in
           let uri_dev v desc = if dev then uri v desc in
           kt1 "KT1XRT495WncnqNmqKn4tkuRiDJzEiR4N2C9"
             "Contract with metadata on Carthagenet." ;
           kt1_dev "KT1PcrG22mRhK6A8bTSjRhk2wV1o5Vuum2S2"
             "Should not exist any where." ;
           uri https_ok "A valid HTTPS URI." ;
           uri sha256_https_ok "A valid SHA256+HTTPS URI." ;
           uri_dev sha256_https_ko
             "A valid SHA256+HTTPS URI but the hash is not right." ;
           uri "tezos-storage://KT1XRT495WncnqNmqKn4tkuRiDJzEiR4N2C9/here"
             "An on-chain pointer to metadata." ;
           uri_dev
             "tezos-storage://KT1XRT495WncnqNmqKn4tkuRiDJzEiR4N2C9.NetXrtZMmJmZSeb/here"
             "An on-chain pointer to metadata with chain-id." ;
           uri_dev "tezos-storage:/here"
             "An on-chain pointer that requires a KT1 in context." ;
           uri "ipfs://QmWDcp3BpBjvu8uJYxVqb7JLfr1pcyXsL97Cfkt3y1758o"
             "An IPFS URI to metadata JSON." ;
           uri_dev "ipfs://ldisejdse-dlseidje" "An invalid IPFS URI." ;
           (`Contracts (List.rev !contracts), `Uris (List.rev !uris)))
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
           %% small
                (Reactive.bind fragment_self (fun f ->
                     link (t "ʘ") ~target:("#" ^ f)))
           %% Reactive.bind (State.dev_mode state) (function
                | true -> it "(dev)"
                | false -> empty ()) ))
      (let of_page p =
         item
           (bt (Page.to_string p))
           ~active:(State.current_page_is_not state p)
           ~action:(State.set_page state p) ~fragment:(fragment_page p) in
       List.map ~f:of_page all_in_order))

let about_page state =
  let open Meta_html in
  let open State in
  let p = p_lead in
  h2 (t "TZComet")
  % p
      ( t "This is" %% tzcomet_link ()
      %% ( match State.version_string state with
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

let settings_page state =
  let open Meta_html in
  let open State in
  h2 (t "Settings")
  % Bootstrap.Form.(
      make
        [ check_box
            (State.dev_mode_bidirectional state)
            ~label:(t "Dev-mode enabled")
            ~help:
              (t
                 "Shows things that regular users should not see and \
                  artificially slows down the application.") ])

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

  let one_error =
    let open Meta_html in
    let open Tezos_error_monad.Error_monad in
    function
    | Exn (Ezjsonm.Parse_error (json_value, msg)) ->
        Fmt.kstr t "JSON Parsing Error: %s, JSON:" msg
        % pre (code (t (Ezjsonm.value_to_string ~minify:false json_value)))
    | Exn (Failure text) -> Fmt.kstr t "Failure: %a" Fmt.text text
    | Exn other_exn ->
        Fmt.kstr ct "Exception: %a"
          (Json_encoding.print_error ~print_unknown:Exn.pp)
          other_exn
    | Tezos_contract_metadata.Metadata_uri.Contract_metadata_uri_parsing
        parsing_error ->
        uri_parsing_error parsing_error
    | other ->
        pre
          (code
             (Fmt.kstr t "%a" Tezos_error_monad.Error_monad.pp_print_error
                [other]))

  let error_trace =
    let open Meta_html in
    function
    | [] ->
        Bootstrap.alert ~kind:`Danger (t "Empty trace from Tezos-error-monad")
    | [h] -> one_error h
    | h :: tl ->
        one_error h
        % div
            ( t "Trace:"
            %% List.fold tl ~init:(empty ()) ~f:(fun p e -> p %% one_error e) )
end

module Editor = struct
  let page state =
    let open Meta_html in
    H5.(
      textarea
        (txt (Lwd.pure "Enter code here"))
        ~a:
          [ a_style (Lwd.pure "font-family: monospace"); a_rows (Lwd.pure 30)
          ; a_cols (Lwd.pure 80) ])
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
      | Ok uri -> `Uri (input_value, uri)
      | Error e -> `Error (input_value, e) )

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

  let uri_and_metadata_result ctxt ~uri ~metadata =
    let open Meta_html in
    Fmt.kstr ct "Done: %a %a" Tezos_contract_metadata.Metadata_uri.pp uri
      Tezos_contract_metadata.Metadata_contents.pp metadata

  let uri_ok_but_metadata_failure ctxt ~uri ~metadata_json ~error =
    let open Meta_html in
    Fmt.kstr ct "Partially failed: %a" Tezos_contract_metadata.Metadata_uri.pp
      uri
    %% pre (ct metadata_json)
    %% Tezos_html.error_trace error

  let uri_there_but_wrong ctxt ~uri_string ~error =
    let open Meta_html in
    Fmt.kstr ct "Failed to parse URI: %S" uri_string
    %% Tezos_html.error_trace error

  let go_action ctxt =
    let open Meta_html in
    let result = State.explorer_result ctxt in
    let input_value = State.explorer_input_value ctxt in
    dbgf "Form submitted with %s" input_value ;
    Work_status.reinit result ;
    Work_status.wip result ;
    Work_status.log result (t "Starting with: " %% ct input_value) ;
    Work_status.async_catch result
      Lwt.Infix.(
        fun ~fail () ->
          let logs prefix s =
            Work_status.log result
              (it prefix %% t "→" %% Bootstrap.monospace (t s)) in
          let on_uri ctxt uri =
            Contract_metadata.Uri.fetch ctxt uri ~log:(logs "Fetching Metadata")
            >>= fun json_code ->
            let open Tezos_contract_metadata.Metadata_contents in
            match of_json json_code with
            | Ok metadata ->
                Work_status.ok result
                  (uri_and_metadata_result ctxt ~uri ~metadata) ;
                Lwt.return ()
            | Error error ->
                fail
                  (uri_ok_but_metadata_failure ctxt ~uri
                     ~metadata_json:json_code ~error) in
          match validate_intput input_value with
          | `KT1 address -> (
              Query_nodes.metadata_value ctxt ~address ~key:""
                ~log:(logs "Getting URI")
              >>= fun metadata_uri ->
              Contract_metadata.Uri.Fetcher.set_current_contract ctxt address ;
              Work_status.log result (t "Now going for: " %% ct metadata_uri) ;
              match Contract_metadata.Uri.validate metadata_uri with
              | Ok uri -> on_uri ctxt uri
              | Error error ->
                  fail
                    (uri_there_but_wrong ctxt ~uri_string:metadata_uri ~error) )
          | `Uri (_, uri) ->
              if Contract_metadata.Uri.needs_context_address uri then
                Work_status.log result
                  (bt "This URI requires a context KT1 address …") ;
              System.slow_step ctxt >>= fun () -> on_uri ctxt uri
          | `Error (_, el) -> fail (Tezos_html.error_trace el))

  let page ctxt =
    let open Meta_html in
    let result = State.explorer_result ctxt in
    Query_nodes.Update_status_loop.ensure ctxt ;
    h2 (t "Contract Metadata Explorer")
    % Bootstrap.Form.(
        let enter_action () = go_action ctxt in
        State.if_explorer_should_go ctxt enter_action ;
        make
          [ row
              [ cell 2
                  (magic
                     Bootstrap.Dropdown_menu.(
                       let example (v, msg) =
                         let cct v =
                           if String.length v > 22 then
                             abbreviation v
                               (ct (String.sub v ~pos:0 ~len:21 ^ "…"))
                           else ct v in
                         item
                           (cct v %% t "→" %% it msg)
                           ~action:(fun () -> State.set_explorer_input ctxt v)
                       in
                       Reactive.bind (State.examples ctxt)
                         ~f:(fun
                              (`Contracts contract_examples, `Uris uri_examples)
                            ->
                           button (t "Examples 💡 ")
                             ( [header (t "KT1 Contracts")]
                             @ List.map contract_examples ~f:example
                             @ [header (t "TZIP-16 URIs")]
                             @ List.map uri_examples ~f:example ))))
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
                           (input_valid ctxt ** Work_status.busy result)
                           ~f:(function
                             | `Error _, _ -> false
                             | _, true -> false
                             | _ -> true))
                     enter_action) ] ])
    % Work_status.render result ~f:Fn.id
end

let root_document state =
  let open Meta_html in
  Bootstrap.container ~suffix:"-fluid"
    ( navigation_menu state
    % Reactive.bind (State.page state)
        State.Page.(
          function
          | Explorer -> Explorer.page state
          | Editor -> Editor.page state
          | Settings -> settings_page state
          | About -> about_page state) )
