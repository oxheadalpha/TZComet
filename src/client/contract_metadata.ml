open Import

module Uri = struct
  let validate uri_code =
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
    Uri.of_string uri_code |> of_uri ~validate_kt1_address ~validate_network

  module Fetcher = struct
    type t = {current_contract: string option Reactive.var}

    let create () = {current_contract= Reactive.var None}
    let get (ctxt : < fetcher: t ; .. > Context.t) = ctxt#fetcher
    let current_contract ctxt = (get ctxt).current_contract

    let set_current_contract ctxt s =
      Reactive.set (get ctxt).current_contract (Some s)

    let unset_current_contract ctxt =
      Reactive.set (get ctxt).current_contract None
  end

  let rec needs_context_address =
    let open Tezos_contract_metadata.Metadata_uri in
    function
    | Storage {address= None; _} -> true
    | Web _ | Storage _ | Ipfs _ -> false
    | Hash {target; _} -> needs_context_address target

  let fetch ?(log = dbgf "Uri.fetch.log: %s") ctxt uri =
    let open Lwt.Infix in
    let logf fmt = Fmt.kstr (fun s -> dbgf "Uri.fetch: %s" s ; log s) fmt in
    let ni s = Fmt.failwith "Not Implemented: %s" s in
    dbgf "FETCCHINGG ============== " ;
    System.slow_step ctxt
    >>= fun () ->
    let rec resolve =
      let open Tezos_contract_metadata.Metadata_uri in
      function
      | Web http ->
          logf "HTTP %S (may fail because of origin policy)" http ;
          Js_of_ocaml_lwt.XmlHttpRequest.(
            get http
            >>= fun frame ->
            dbgf "%s -> code: %d" http frame.code ;
            match frame.code with
            | 200 ->
                logf "HTTP success (%d bytes)" (String.length frame.content) ;
                Lwt.return frame.content
            | other -> Fmt.failwith "Getting %S returned code: %d" http other)
          >>= fun content -> Lwt.return content
      | Ipfs {cid; path} ->
          let gateway = "https://gateway.ipfs.io/ipfs/" in
          let gatewayed = Fmt.str "%s%s%s" gateway cid path in
          logf "IPFS CID %S path %S, adding gateway %S" cid path gateway ;
          resolve (Web gatewayed)
      | Storage {network= None; address; key} ->
          let addr =
            match address with
            | Some s -> s
            | None -> (
              match Reactive.peek (Fetcher.current_contract ctxt) with
              | None -> Fmt.failwith "Missing current contract"
              | Some s -> s ) in
          logf "Using address %S (key = %S)" addr key ;
          Query_nodes.metadata_value ctxt ~address:addr ~key ~log
      | Storage {network= Some network; address; key} ->
          logf "storage %s %a %S" network Fmt.Dump.(option string) address key ;
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
          let obtained = Digestif.digest_string Digestif.sha256 content in
          logf "hash of content: %a" (Digestif.pp Digestif.sha256) obtained ;
          match Digestif.unsafe_compare Digestif.sha256 expected obtained with
          | 0 -> Lwt.return content
          | _ ->
              Fmt.failwith "Hash of content %a is different from expected %a"
                (Digestif.pp Digestif.sha256)
                obtained
                (Digestif.pp Digestif.sha256)
                expected ) in
    resolve uri

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
            [ li_field "CID" [code [txt cid]]; li_field "Path" [code [txt path]]
            ; li [txt "(Try "; a ~a:[a_href gatewayed] [txt gatewayed]; txt ")"]
            ] ]
    | Storage {network; address; key} ->
        [ txt "In Storage:"
        ; ul
            [ li_field "Network"
                [ Option.value_map network ~default:(txt "“Current”.")
                    ~f:(fun s -> code [txt s]) ]
            ; li_field "Address"
                [ Option.value_map address ~default:(txt "“Same contract”.")
                    ~f:(fun s -> code [txt s]) ]
            ; li_field "Key in the big-map" [code [txt key]] ] ]
    | Hash {kind= `Sha256; value; target} ->
        [ txt "Hash checked URI:"
        ; ul
            [ li_field "Target" (to_html target)
            ; li_field "… should SHA256-hash to"
                [code [Fmt.kstr txt "%a" Hex.pp (Hex.of_string value)]] ] ]

  let rec render ctxt uri =
    let open Meta_html in
    let open Tezos_contract_metadata.Metadata_uri in
    let field name content = Fmt.kstr it "%s:" name %% content in
    match uri with
    | Web u -> t "Web URL:" %% url ct u
    | Ipfs {cid; path} ->
        let gatewayed = Fmt.str "https://gateway.ipfs.io/ipfs/%s%s" cid path in
        t "IPFS URI:"
        % itemize
            [ field "CID" (ct cid); field "Path" (ct path)
            ; t "(Try " %% url ct gatewayed % t ")" ]
    | Storage {network; address; key} ->
        t "In-Contract-Storage:"
        % itemize
            [ field "Network"
                (Option.value_map network
                   ~default:(t "“Current network”.")
                   ~f:ct)
            ; field "Address"
                (Option.value_map address ~default:(t "“Same contract”.")
                   ~f:ct); field "Key in the big-map" (Fmt.kstr ct "%S" key) ]
    | Hash {kind= `Sha256; value; target} ->
        t "Hash checked URI:"
        % itemize
            [ field "Target" (render ctxt target)
            ; field "… should SHA256-hash to"
                (Fmt.kstr ct "%a" Hex.pp (Hex.of_string value)) ]
end

module Content = struct
  open Tezos_contract_metadata.Metadata_contents

  let to_html
      { name
      ; description
      ; version
      ; license
      ; authors
      ; homepage
      ; interfaces
      ; views
      ; unknown } =
    let open RD in
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
    let license_elt l =
      let open License in
      span
        [ i [txt l.name]
        ; span
            (Option.value_map ~default:[] l.details ~f:(fun d ->
                 [Fmt.kstr txt " → %s" d])) ] in
    let list_field name field f =
      option_field name (match field with [] -> None | more -> Some more) f
    in
    let authors_elt l =
      List.map l ~f:code_string |> List.intersperse ~sep:(txt ", ") |> span
    in
    let interfaces_elt l =
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
      List.map l ~f:interface |> List.intersperse ~sep:(txt ", ") |> span in
    let _todo l = Fmt.kstr txt "todo: %d items" (List.length l) in
    let protocol s =
      let proto s = abbr ~a:[a_title s] [code_string (String.prefix s 12)] in
      let known name url =
        span [a ~a:[a_href url] [em [txt name]]; txt " ("; proto s; txt ")"]
      in
      match s with
      | "PsCARTHAGazKbHtnKfLzQg3kms52kSRpgnDY982a9oYsSXRLQEb" ->
          known "Carthage" "http://tezos.gitlab.io/protocols/006_carthage.html"
      | "PsBabyM1eUXZseaJdmXFApDSBqj8YBfwELoxZHHW77EMcAbbwAS" ->
          known "Babylon" "http://tezos.gitlab.io/protocols/005_babylon.html"
      | "PsDELPH1Kxsxt8f9eWbxQeRxkjfbxoqM52jvs5Y5fBxWWh4ifpo" ->
          known "Delphi" "https://blog.nomadic-labs.com/delphi-changelog.html"
      | s -> proto s in
    let views_elt (views : View.t list) =
      let view v =
        let open View in
        let purity =
          if v.is_pure then span ~a:[a_class ["bg-success"]] [txt "pure"]
          else span ~a:[a_class ["bg-warning"]] [txt "inpure"] in
        let implementations impls =
          let open Implementation in
          ul
            (List.map impls ~f:(function
              | Michelson_storage
                  { parameter
                  ; return_type
                  ; code= michcode
                  ; human_annotations
                  ; version } ->
                  let open Michelson_storage in
                  let mich (Micheline m) =
                    let open Tezos_micheline in
                    Fmt.str "%a" Micheline_printer.print_expr
                      (Micheline_printer.printable Base.Fn.id m) in
                  li
                    [ b [txt "Michelson-storage-view:"]
                    ; ul
                        ( option_field "Michelson-Version" version protocol
                        @ normal_field "Type"
                            (code
                               [ Fmt.kstr txt "%s<contract-storage> → %s"
                                   ( match parameter with
                                   | None -> ""
                                   | Some p -> mich p ^ " × " )
                                   (mich return_type) ])
                        @ normal_field "Code"
                            (details
                               (summary [txt "Expand"])
                               [pre [code [txt (mich michcode)]]])
                        @ list_field "Annotations" human_annotations
                            (fun anns ->
                              ul
                                (List.map anns ~f:(fun (k, v) ->
                                     li [code_string k; txt " → "; em [txt v]])))
                        ) ]
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
              @ list_field "Implementation(s)" v.implementations implementations
              ) ] in
      ul (List.map views ~f:(fun v -> li [view v])) in
    let unknown_extras kv =
      ul
        (List.map kv ~f:(fun (k, v) ->
             li
               [ code_string k; txt " "
               ; pre [code [txt (Ezjsonm.value_to_string ~minify:false v)]] ]))
    in
    let url_elt u = span [a ~a:[a_href u] [txt u]] in
    ul
      ( option_field "Name" name code_string
      @ option_field "Version" version code_string
      @ option_field "Description" description paragraphs
      @ option_field "License" license license_elt
      @ option_field "Homepage" homepage url_elt
      @ list_field "Authors" authors authors_elt
      @ list_field "Interfaces" interfaces interfaces_elt
      @ list_field "Views" views views_elt
      @ list_field "Extra/Unknown" unknown unknown_extras )

  let render ctxt
      { name
      ; description
      ; version
      ; license
      ; authors
      ; homepage
      ; interfaces
      ; views
      ; unknown } =
    let open Meta_html in
    let option_field name field f =
      match field with None -> [] | Some s -> [Fmt.kstr it "%s: " name % f s]
    in
    let normal_field name x = option_field name (Some ()) (fun () -> x) in
    let paragraphs blob =
      let rec go l acc =
        match List.split_while l ~f:(function "" -> false | _ -> true) with
        | ll, [] -> String.concat ~sep:" " ll :: acc
        | ll, _ :: lr -> go lr (String.concat ~sep:" " ll :: acc) in
      go (String.split blob ~on:'\n') []
      |> List.rev_map ~f:(fun x -> p (t x))
      |> H5.div in
    let license_elt l =
      let open License in
      it l.name
      % Option.value_map ~default:(empty ()) l.details ~f:(fun d ->
            Fmt.kstr t " → %s" d) in
    let list_field name field f =
      option_field name (match field with [] -> None | more -> Some more) f
    in
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
      | s -> proto s in
    let views_elt (views : View.t list) =
      let view v =
        let open View in
        let purity =
          if v.is_pure then Bootstrap.color `Success (t "pure")
          else Bootstrap.color `Warning (t "inpure") in
        let implementations impls =
          let open Implementation in
          itemize
            (List.map impls ~f:(function
              | Michelson_storage
                  { parameter
                  ; return_type
                  ; code= michcode
                  ; human_annotations
                  ; version } ->
                  let open Michelson_storage in
                  let mich (Micheline m) =
                    let open Tezos_micheline in
                    Fmt.str "%a" Micheline_printer.print_expr
                      (Micheline_printer.printable Base.Fn.id m) in
                  it "Michelson-storage-view:"
                  % itemize
                      ( option_field "Michelson-Version" version protocol
                      @ normal_field "Type"
                          (Fmt.kstr ct "%s<contract-storage> → %s"
                             ( match parameter with
                             | None -> ""
                             | Some p -> mich p ^ " × " )
                             (mich return_type))
                      @ normal_field "Code"
                          H5.(
                            details
                              (summary [txt (Lwd.pure "Expand")])
                              [pre [code [txt (Lwd.pure (mich michcode))]]])
                      @ list_field "Annotations" human_annotations (fun anns ->
                            itemize
                              (List.map anns ~f:(fun (k, v) ->
                                   ct k % t " → " % it v))) )
              | Rest_api_query raq ->
                  it "REST-API Query:"
                  % itemize
                      ( normal_field "OpenAPI Spec" (ct raq.specification_uri)
                      @ option_field "Base-URI Override" raq.base_uri ct
                      @ normal_field "Path" (ct raq.path)
                      @ normal_field "Method"
                          (Fmt.kstr ct "%s"
                             (Cohttp.Code.string_of_method raq.meth)) ))) in
        div
          ( bt v.name %% t "(" % purity % t "):"
          % itemize
              ( option_field "Description" v.description paragraphs
              @ list_field "Implementation(s)" v.implementations implementations
              ) ) in
      itemize (List.map views ~f:(fun v -> view v)) in
    let unknown_extras kv =
      itemize
        (List.map kv ~f:(fun (k, v) ->
             ct k %% pre (ct (Ezjsonm.value_to_string ~minify:false v)))) in
    let url_elt u = url t u in
    itemize
      ( option_field "Name" name ct
      @ option_field "Version" version ct
      @ option_field "Description" description paragraphs
      @ option_field "License" license license_elt
      @ option_field "Homepage" homepage url_elt
      @ list_field "Authors" authors authors_elt
      @ list_field "Interfaces" interfaces interfaces_elt
      @ list_field "Views" views views_elt
      @ list_field "Extra/Unknown" unknown unknown_extras )
end
