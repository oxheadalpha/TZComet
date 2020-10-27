open Import

module Errors_html = struct
  open Meta_html

  let decorate_error_message ctxt m =
    let module M = Message in
    let rec msg = function
      | M.Text s -> t s
      | M.Inline_code c -> ct c
      | M.Code_block b -> pre (ct b)
      | M.List l -> List.fold ~init:(empty ()) ~f:(fun a b -> a % msg b) l in
    msg m

  let exception_html ctxt exn =
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
                    | `Hiding | `Showing -> t "~ . . . ~"
                    | `Hidden -> t "Show Error Trace"
                    | `Shown -> t "Hide Error Trace")
                  (itemize (List.map more ~f:construct)) in
          decorate_error_message ctxt message % trace_part
      | Failure s -> t "Failure:" %% t s
      | e -> t "Exception:" % pre (Fmt.kstr ct "%a" Exn.pp e) in
    bt "Error:" %% construct exn
end

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

  let is_empty {status; _} =
    Reactive.(get status |> map ~f:(function Empty -> true | _ -> false))

  let async_catch :
      type b. 'a t -> (mkexn:(log_item -> exn) -> unit -> unit Lwt.t) -> unit =
   fun wip f ->
    let open Lwt in
    let exception Work_failed of log_item in
    async (fun () ->
        catch
          (fun () -> f ~mkexn:(fun x -> Work_failed x) ())
          Meta_html.(
            function
            | Work_failed l -> error wip l ; return ()
            | exn ->
                error wip (Errors_html.exception_html () exn) ;
                return ()))

  let render work_status ~f =
    let open Meta_html in
    let show_logs ?(wip = false) () =
      let make_logs_map _ x = H5.li [x] in
      let logs = Reactive.Table.concat_map ~map:make_logs_map work_status.logs in
      Bootstrap.terminal_logs
        (H5.ul
           ( if wip then
             [logs; H5.li [Bootstrap.spinner ~kind:`Info (t "Working …")]]
           else [logs] )) in
    let collapsing_logs () =
      let collapse = Bootstrap.Collapse.make () in
      Bootstrap.Collapse.fixed_width_reactive_button_with_div_below collapse
        ~width:"12em" ~kind:`Secondary
        ~button:(function
          | `Hiding | `Showing -> t "..⻎.."
          | `Hidden -> t "Show Logs"
          | `Shown -> t "Collapse Logs")
        (show_logs ~wip:false ()) in
    Reactive.bind_var work_status.status ~f:(function
      | Empty -> empty ()
      | Work_in_progress ->
          Bootstrap.alert ~kind:`Secondary (show_logs ~wip:true ())
      | Done (Ok x) -> div (f x) % collapsing_logs ()
      | Done (Error e) ->
          Bootstrap.bordered ~kind:`Danger (div e %% collapsing_logs ()))
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
    ; explorer_input: string Reactive.var
    ; explorer_go: bool Reactive.var
    ; explorer_went: bool Reactive.var
    ; explorer_result: Html_types.div_content_fun Meta_html.H5.elt Work_status.t
    ; editor_content: string Reactive.var }

  let get (state : < gui: t ; .. > Context.t) = state#gui

  module Fragment = struct
    let to_string = Uri.to_string
    let pp = Uri.pp

    let page_to_path page =
      Fmt.str "/%s" (Page.to_string page |> String.lowercase)

    let make ~page ~dev_mode ~editor_input ~explorer_input ~explorer_go =
      let query =
        match explorer_input with
        | "" -> []
        | more -> [("explorer-input", [more])] in
      let query =
        match editor_input with
        | "" -> query
        | more -> ("editor-input", [more]) :: query in
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
      let editor_input =
        match in_query "editor-input" with Some [one] -> one | _ -> "" in
      let explorer_go = true_in_query "go" in
      ( System.create ~dev_mode ()
      , { page= Reactive.var page
        ; version_string= None
        ; explorer_input= Reactive.var explorer_input
        ; explorer_go= Reactive.var explorer_go
        ; explorer_went=
            (* If page is not the explorer we will ignore the command =
               assume it aready happened. *)
            Reactive.var Poly.(page <> Page.Explorer)
        ; explorer_result= Work_status.empty ()
        ; editor_content= Reactive.var editor_input } )
  end

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

  let dev_mode state = System.dev_mode state
  let dev_mode_bidirectional = System.dev_mode_bidirectional
  let explorer_input state = (get state).explorer_input |> Reactive.get
  let explorer_input_value state = (get state).explorer_input |> Reactive.peek
  let set_explorer_input state = (get state).explorer_input |> Reactive.set

  let explorer_input_bidirectional state =
    (get state).explorer_input |> Reactive.Bidirectrional.of_var

  let editor_content state =
    (get state).editor_content |> Reactive.Bidirectrional.of_var

  let set_editor_content state v = Reactive.set (get state).editor_content v

  let make_fragment ?(side_effects = true) ctxt =
    (* WARNING: for now it is important for this to be attached "somewhere"
       in the DOM.
       WARNING-2: this function is used for side effects unrelated to the
       fragment too (system.dev_mode).
    *)
    let open Js_of_ocaml.Url in
    let state = get ctxt in
    let dev = dev_mode ctxt in
    let page = Reactive.get state.page in
    let explorer_input = Reactive.get state.explorer_input in
    let editor_input = Reactive.get state.editor_content in
    let explorer_go = Reactive.get state.explorer_go in
    Reactive.(dev ** page ** explorer_input ** explorer_go ** editor_input)
    |> Reactive.map
         ~f:(fun
              (dev_mode, (page, (explorer_input, (explorer_go, editor_input))))
            ->
           let now =
             Fragment.(
               let editor_input =
                 if String.length editor_input < 40 then editor_input else ""
               in
               make ~page ~dev_mode ~explorer_input ~explorer_go ~editor_input)
           in
           if side_effects then (
             let current = Js_of_ocaml.Url.Current.get_fragment () in
             dbgf "Updating fragment %S → %a" current Fragment.pp now ;
             Current.set_fragment (Fragment.to_string now) ) ;
           now)

  let link_to_editor ctxt content ~text =
    let open Meta_html in
    let fragment = make_fragment ~side_effects:false ctxt in
    let href =
      Reactive.(map fragment) ~f:(fun frg ->
          "#" ^ Fragment.(to_string (change_for_page frg Page.Editor))) in
    a
      ~a:
        [ H5.a_href href
        ; H5.a_onclick
            (Tyxml_lwd.Lwdom.attr (fun _ ->
                 set_editor_content ctxt text ;
                 set_page ctxt Page.Editor () ;
                 false)) ]
      content

  let if_explorer_should_go state f =
    if
      (get state).explorer_go |> Lwd.peek
      && not ((get state).explorer_went |> Lwd.peek)
    then (
      Lwd.set (get state).explorer_went true ;
      f () )
    else ()

  module Examples = struct
    type item = string * string

    type t =
      { contracts: item list
      ; uris: item list
      ; metadata_blobs: item list
      ; michelson_bytes: item list
      ; michelson_concretes: item list }

    let get state =
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
      dev_mode state
      |> Reactive.map ~f:(fun dev ->
             let aggl () =
               let all = ref [] in
               let add v desc = all := (v, desc) :: !all in
               let add_dev v desc = if dev then add v desc else () in
               let all () = List.rev !all in
               (add, add_dev, all) in
             let kt1, kt1_dev, kt1_all = aggl () in
             let uri, uri_dev, uri_all = aggl () in
             let mtb, mtb_dev, mtb_all = aggl () in
             let mby, mby_dev, mby_all = aggl () in
             let tzc, tzc_dev, tzc_all = aggl () in
             kt1 "KT1XRT495WncnqNmqKn4tkuRiDJzEiR4N2C9"
               "Contract with metadata on Carthagenet." ;
             kt1_dev "KT1PcrG22mRhK6A8bTSjRhk2wV1o5Vuum2S2"
               "Should not exist any where." ;
             kt1_dev "KT1Su4bveK3P3PFonoCzPgefQriwBtN1KAgJ"
               "Just a version string." ;
             kt1_dev "KT1AzpTM7aM5N3hAd9RVd7FVmVN72BWkqKXh"
               "Has a URI that points nowhere." ;
             kt1 "KT1VAieU3HoaKtywG2VwuZXBB6mViguWoibH"
               "Has one off-chain-view." ;
             kt1_dev "KT1Ffua85vzkCyuHnYTr8iXAypMryh2fjaF5"
               "Event more weird off-chain-views." ;
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
             mtb "{}" "Empty, but valid, Metadata" ;
             let all_mtb_from_lib =
               let open Tezos_contract_metadata.Metadata_contents in
               let rec go n =
                 try (n, Example.build n) :: go (n + 1) with _ -> [] in
               go 0 in
             List.iter all_mtb_from_lib ~f:(fun (ith, v) ->
                 mtb_dev
                   (Tezos_contract_metadata.Metadata_contents.to_json v)
                   (Fmt.str "Meaningless example #%d" ith)) ;
             mby "0x05030b" "The Unit value, PACKed." ;
             mby
               "050707010000000c486\n\
                56c6c6f20576f726c64\n\
                2102000000260704010\n\
                0000003666f6f010000\n\
                0003626172070401000\n\
                0000474686973010000\n\
                000474686174"
               "Michelson with a (map string string)." ;
             mby_dev "0x05" "Empty but still Michelsonian bytes." ;
             (let tzself f c = Fmt.kstr (f c) "Michelson %S" c in
              List.iter ~f:(tzself tzc)
                ["Unit"; "12"; "\"hello world\""; "(Pair 42 51)"] ;
              List.iter ~f:(tzself tzc_dev)
                ["Unit 12"; "\"hœlló wörld\""; "(Pair 42 51 \"meh\")"]) ;
             { contracts= kt1_all ()
             ; uris= uri_all ()
             ; metadata_blobs= mtb_all ()
             ; michelson_bytes= mby_all ()
             ; michelson_concretes= tzc_all () })
  end
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
  let p = Bootstrap.p_lead in
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
                     | `Hiding | `Showing -> t "..🏃.."
                     | `Hidden -> t "Show Error"
                     | `Shown -> t "Hide Error")
                   (Errors_html.exception_html ctxt e)
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
         let nameb = Reactive.Bidirectrional.of_var name in
         let prefix = Reactive.var "" in
         let prefixb = Reactive.Bidirectrional.of_var prefix in
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
                 Reactive.Bidirectrional.set nameb "" ;
                 Reactive.Bidirectrional.set prefixb "" ;
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
      Reactive.Bidirectrional.make
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

  let single_error ctxt =
    let open Meta_html in
    let open Tezos_error_monad.Error_monad in
    function
    | Exn (Ezjsonm.Parse_error (json_value, msg)) ->
        Fmt.kstr t "JSON Parsing Error: %s, JSON:" msg
        % pre (code (t (Ezjsonm.value_to_string ~minify:false json_value)))
    | Exn (Failure text) -> Fmt.kstr t "Failure: %a" Fmt.text text
    | Exn other_exn -> Errors_html.exception_html ctxt other_exn
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
                     ~f:ct); field "Key in the big-map" (Fmt.kstr ct "%S" key)
              ]
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

  let parse_micheline m =
    match Tezos_micheline.Micheline_parser.tokenize m with
    | tokens, [] -> (
      match Tezos_micheline.Micheline_parser.parse_expression tokens with
      | node, [] -> node
      | _, errs -> Fmt.failwith "parsing" )
    | _, errs -> Fmt.failwith "tokeninzeing"

  let mich_node node =
    let open Tezos_micheline in
    Fmt.str "%a" Micheline_printer.print_expr
      (Micheline_printer.printable Base.Fn.id (Micheline.strip_locations node))

  module Michelson_form = struct
    type type_expression =
      | Any of string Tezos_micheline.Micheline.canonical
      | Nat
      | Mutez

    type leaf = string Reactive.var

    type t =
      | Leaf of
          {t: type_expression; v: leaf; description: (string * string) option}
      | Pair of {left: t; right: t}

    open Tezos_contract_metadata.Metadata_contents.View.Implementation
    open Tezos_contract_metadata.Metadata_contents.Michelson_blob

    let of_type ~annotations (Micheline m) =
      let view_annots = annotations in
      let open Tezos_micheline.Micheline in
      let describe annot =
        List.find view_annots ~f:(fun (k, v) ->
            List.mem annot k ~equal:String.equal) in
      let rec go = function
        | Prim (_, "nat", [], annot) ->
            Leaf {t= Nat; v= Reactive.var ""; description= describe annot}
        | Prim (_, "mutez", [], annot) ->
            Leaf {t= Mutez; v= Reactive.var ""; description= describe annot}
        | Prim (_, "pair", [l; r], _) -> Pair {left= go l; right= go r}
        | Prim (_, _, _, annot) as tp ->
            Leaf
              { t= Any (strip_locations tp)
              ; v= Reactive.var ""
              ; description= describe annot }
        | tp ->
            Leaf
              { t= Any (strip_locations tp)
              ; v= Reactive.var ""
              ; description= None } in
      go (root m)

    let rec fill_with_value mf node =
      let open Tezos_micheline.Micheline in
      match (mf, node) with
      | Leaf leaf, nn -> Reactive.set leaf.v (mich_node nn)
      | Pair {left; right}, Prim (_, "Pair", [l; r], _) ->
          fill_with_value left l ; fill_with_value right r
      | Pair _, other ->
          Decorate_error.(
            raise
              Message.(
                t "Type mismatch" %% ct (mich_node other) %% t "is not a pair."))

    let rec peek = function
      | Leaf l -> Reactive.peek l.v
      | Pair {left; right} -> Fmt.str "(Pair %s %s)" (peek left) (peek right)

    let validate_micheline m =
      match parse_micheline m with _ -> true | exception _ -> false

    let rec is_valid = function
      | Leaf {t= Nat | Mutez; v; _} ->
          Reactive.(
            get v
            |> map ~f:(function
                 | "" -> false
                 | s -> (
                   match Z.of_string s with _ -> true | exception _ -> false )))
      | Leaf lf -> Reactive.(get lf.v |> map ~f:validate_micheline)
      | Pair {left; right} ->
          Reactive.(map2 ( && ) (is_valid left) (is_valid right))

    let rec validity_error = function
      | Nat -> t "Invalid natural number."
      | Mutez -> t "Invalid μꜩ value."
      | Any _ -> t "Invalid Micheline syntax."

    let rec to_form_items mf =
      let open Meta_html in
      let open Bootstrap.Form in
      let type_expr = function
        | Nat -> b (ct "nat")
        | Mutez -> b (ct "mutez")
        | Any m ->
            let open Tezos_micheline in
            Fmt.kstr ct "%a" Micheline_printer.print_expr
              (Micheline_printer.printable Base.Fn.id m) in
      match mf with
      | Pair {left; right} -> to_form_items left @ to_form_items right
      | Leaf leaf ->
          [ input
              ~label:
                ( match leaf.description with
                | None -> t "The parameter of type" %% type_expr leaf.t % t "."
                | Some (an, s) ->
                    t "The parameter called " % ct an %% t "of type"
                    %% type_expr leaf.t % t ":" %% it s )
              ~help:
                Reactive.(
                  bind (is_valid mf) ~f:(function
                    | true -> Bootstrap.color `Success (t "OK")
                    | false -> Bootstrap.color `Danger (validity_error leaf.t)))
              ~placeholder:(Reactive.pure "Some decent Michelson right here")
              (Reactive.Bidirectrional.of_var leaf.v) ]

    let rec render mf =
      match mf with
      | Leaf leaf ->
          [ ct (Reactive.peek leaf.v)
            % Option.value_map leaf.description ~default:(empty ())
                ~f:(fun (k, v) -> t ":" %% it v %% parens (ct k)) ]
      | Pair {left; right} -> render left @ render right
  end

  let mich
      (Tezos_contract_metadata.Metadata_contents.Michelson_blob.Micheline m) =
    let open Tezos_micheline in
    Fmt.str "%a" Micheline_printer.print_expr
      (Micheline_printer.printable Base.Fn.id m)

  let view_result ctxt ~result ~storage ~address ~view ~parameter =
    let open Tezos_contract_metadata.Metadata_contents in
    let open View in
    let open Implementation in
    let open Michelson_storage in
    let expanded =
      try
        let retmf =
          Michelson_form.of_type ~annotations:view.human_annotations
            view.return_type in
        Michelson_form.fill_with_value retmf result ;
        match Michelson_form.render retmf with
        | [] ->
            dbgf "view_result.expanded: empty list!" ;
            bt "This should not be shown"
        | [one] -> one
        | more -> itemize more
      with exn -> Errors_html.exception_html ctxt exn in
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
                           | `Hiding | `Showing -> t "..🏃.."
                           | `Hidden -> t "Show Michelson"
                           | `Shown -> t "Hide Michelson")
                         (pre (ct concrete)))
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
                ~f:(Michelson_form.of_type ~annotations:view.human_annotations)
            in
            let parse_micheline m =
              match Tezos_micheline.Micheline_parser.tokenize m with
              | tokens, [] -> (
                match
                  Tezos_micheline.Micheline_parser.parse_expression tokens
                with
                | node, [] -> node
                | _, errs -> Fmt.failwith "parsing" )
              | _, errs -> Fmt.failwith "tokeninzeing" in
            let wip = Work_status.empty () in
            let go_action () =
              Work_status.wip wip ;
              let log s =
                Work_status.log wip
                  (it "Calling view" %% t "→" %% Bootstrap.monospace (t s))
              in
              Work_status.async_catch wip
                Lwt.Infix.(
                  fun ~mkexn () ->
                    let parameter =
                      match parameter_input with
                      | Some mf -> Michelson_form.peek mf |> parse_micheline
                      | None -> parse_micheline "Unit" in
                    Query_nodes.call_off_chain_view ctxt ~log
                      ~address:(Reactive.peek address) ~view ~parameter
                    >>= function
                    | Ok (result, storage) ->
                        Work_status.ok wip
                          (view_result ctxt ~result ~storage
                             ~address:(Reactive.peek address) ~view ~parameter) ;
                        Lwt.return ()
                    | Error s ->
                        Work_status.error wip (t "Error:" %% ct s) ;
                        Lwt.return ()) ;
              dbgf "go view" in
            switch ~action:(fun () -> Reactive.set call_mode false) (t "Cancel")
            % ( Work_status.is_empty wip
              |> Reactive.bind ~f:(function
                   | false -> Work_status.render wip ~f:Fn.id
                   | true ->
                       let open Bootstrap.Form in
                       let validate_address input_value =
                         match B58_hashes.check_b58_kt1_hash input_value with
                         | _ -> true
                         | exception _ -> false in
                       let input_valid =
                         Reactive.(
                           match parameter_input with
                           | Some mf -> Michelson_form.is_valid mf
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
                             (Reactive.Bidirectrional.of_var address) ] in
                       let param_input =
                         match parameter_input with
                         | None -> [magic (t "No parameter")]
                         | Some mf -> Michelson_form.to_form_items mf in
                       make ~enter_action:go_action
                         ( addr_input @ param_input
                         @ [submit_button ~active (t "Go!") go_action] )) ))

  let metadata_contents ?(open_in_editor_link = true) ctxt =
    let open Tezos_contract_metadata.Metadata_contents in
    fun ( { name
          ; description
          ; version
          ; license
          ; authors
          ; homepage
          ; source
          ; interfaces
          ; errors
          ; views
          ; unknown } as metadata ) ->
      let ct = monot in
      let license_elt l =
        let open License in
        it l.name
        % Option.value_map ~default:(empty ()) l.details ~f:(fun d ->
              Fmt.kstr t " → %s" d) in
      let authors_elt l =
        oxfordize_list l ~map:ct
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
      let view v =
        let open View in
        let purity =
          if v.is_pure then Bootstrap.color `Success (t "pure")
          else Bootstrap.color `Warning (t "inpure") in
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
        div
          ~a:[H5.a_id (Lwd.pure (view_id v.name))]
          ( bt v.name %% t "(" % purity % t "):"
          % itemize
              ( option_field "Description" v.description paragraphs
              @
              match v.implementations with
              | [] ->
                  [Bootstrap.color `Danger (t "There are no implementations.")]
              | l ->
                  let name =
                    Fmt.str "Implementation%s"
                      (if List.length l = 1 then "" else "s") in
                  list_field name v.implementations implementations ) ) in
      let views_elt (views : View.t list) =
        itemize (List.map views ~f:(fun v -> view v)) in
      let url_elt u = url t u in
      let source_elt source =
        let open Source in
        itemize
          ( field "Tools"
              ( ( oxfordize_list source.tools ~map:ct
                    ~sep:(fun () -> t ", ")
                    ~last_sep:(fun () -> t ", and ")
                |> list )
              % t "." )
          :: option_field "Location" source.location url_elt ) in
      let errors_elt errors =
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
              % Bootstrap.Collapse.make_div collapse (view v) in
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
      ( if open_in_editor_link then
        open_in_editor ctxt
          (Tezos_contract_metadata.Metadata_contents.to_json metadata)
      else empty () )
      % itemize
          ( option_field "Name" name ct
          @ option_field "Version" version ct
          @ option_field "Description" description paragraphs
          @ option_field "License" license license_elt
          @ option_field "Homepage" homepage url_elt
          @ option_field "Source" source source_elt
          @ list_field "Authors" authors authors_elt
          @ list_field "Interfaces" interfaces interfaces_elt
          @ option_field "Errors" errors errors_elt
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

  let editable ctxt =
    make ctxt
      ~action:(fun x -> State.set_editor_content ctxt x)
      [ (t "TZIP-16 Metadata Content", fun x -> x.metadata_blobs)
      ; (t "TZIP-16-URIs", fun x -> x.uris)
      ; (t "Michelson Bytes Blobs", fun x -> x.michelson_bytes)
      ; (t "Michelson Concrete Expressions", fun x -> x.michelson_concretes) ]
end

module Editor = struct
  let page ctxt =
    let open Meta_html in
    let content = State.editor_content ctxt in
    let editor =
      div (Examples_dropdown.editable ctxt)
      % H5.(
          div
            [ textarea
                (txt (Reactive.Bidirectrional.get content))
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
                                     Reactive.Bidirectrional.set content v)) ;
                             true)) ] ]) in
    let guessers =
      let of_predicate name v f ~log inp =
        let worked = f inp in
        log
          Message.(
            t "Trying predicate" %% ct name %% t "→"
            % if worked then t "OK!" else t "Nope :/") ;
        if worked then Some v else None in
      let looks_like_json s =
        match (String.strip s).[0] with '[' | '{' | '"' -> true | _ -> false
      in
      let looks_like_an_uri s =
        match String.split ~on:'\n' (String.strip s) with
        | [""] -> false
        | [_] -> true
        | _ -> false in
      [ of_predicate "is-empty" `Empty (fun s -> String.(is_empty (strip s)))
      ; of_predicate "looks-like-json" `Json looks_like_json
      ; of_predicate "looks-like-an-uri" `Uri looks_like_an_uri ] in
    let guess_validate input =
      let _logs = ref [] in
      let log m = _logs := m :: !_logs in
      let res =
        match List.find_map guessers ~f:(fun f -> f ~log input) with
        | Some s -> s
        | None -> `No_idea in
      (input, res, List.rev !_logs) in
    let big_answer level content =
      let kind = match level with `Ok -> `Success | `Error -> `Danger in
      h2 (Bootstrap.color kind content) in
    let show_metadata ctxt inpo =
      let open Tezos_contract_metadata.Metadata_contents in
      match of_json inpo with
      | Ok m -> Tezos_html.metadata_contents ~open_in_editor_link:false ctxt m
      | Error el -> Tezos_html.error_trace ctxt el in
    let list_of_validation_errors l =
      itemize
        (List.map l ~f:(fun (w, s, e) ->
             (match w with `Network -> t "Network" | `Address -> t "Address")
             %% ct s % t ":" %% t e)) in
    let show_uri ctxt inpo =
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
          | more -> p (t "Moreover:") % list_of_validation_errors more ) in
    let result =
      Reactive.Bidirectrional.get content
      |> Reactive.map ~f:guess_validate
      |> Reactive.bind ~f:(fun (inp, kind, logs) ->
             let show_logs =
               let collapse = Bootstrap.Collapse.make () in
               Bootstrap.Collapse.fixed_width_reactive_button_with_div_below
                 collapse ~width:"12em" ~kind:`Secondary
                 ~button:(function
                   | `Hiding | `Showing -> t "..🚸.."
                   | `Hidden -> t "Show Logs"
                   | `Shown -> t "Collapse Logs")
                 (Bootstrap.terminal_logs
                    (itemize
                       (List.map logs
                          ~f:(Errors_html.decorate_error_message ctxt)))) in
             match kind with
             | `Empty -> h4 (t "Editor Is Empty")
             | `Json ->
                 h4 (t "This looks like JSON Metadata …")
                 % show_metadata ctxt inp
             | `Uri -> show_uri ctxt inp
             | `No_idea ->
                 h4 (t "Don't know how to validate this")
                 % pre ~a:[classes ["pre-scrollable"]] (ct inp)
                 % div show_logs) in
    div
      ~a:[classes ["row"]]
      (div ~a:[classes ["col-6"]] editor % div ~a:[classes ["col-6"]] result)
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
    | `KT1 k -> t "Contract" %% ct k
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
    %% pre (ct metadata_json)
    %% Tezos_html.error_trace ctxt error

  let uri_there_but_wrong ctxt ~full_input ~uri_string ~error =
    let open Meta_html in
    full_input_bloc ctxt full_input
    % Fmt.kstr ct "Failed to parse URI: %S" uri_string
    %% Tezos_html.error_trace ctxt error

  let uri_failed_to_fetch ctxt ~uri ~error ~full_input =
    let open Meta_html in
    full_input_bloc ctxt full_input
    % h4 (t "Metadata Location")
    % Tezos_html.metadata_uri ctxt uri
    % Errors_html.exception_html ctxt error

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
        fun ~mkexn () ->
          let logs prefix s =
            Work_status.log result
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
            match of_json json_code with
            | Ok metadata ->
                Work_status.ok result
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
              Work_status.log result (t "Now going for: " %% ct metadata_uri) ;
              match Contract_metadata.Uri.validate metadata_uri with
              | Ok uri, _ -> on_uri ctxt uri
              | Error error, _ ->
                  raise
                    (mkexn
                       (uri_there_but_wrong ctxt ~uri_string:metadata_uri
                          ~full_input ~error)) )
          | `Uri (_, uri) ->
              if Contract_metadata.Uri.needs_context_address uri then
                Work_status.log result
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
          | Settings -> Settings_page.render state
          | About -> about_page state) )
