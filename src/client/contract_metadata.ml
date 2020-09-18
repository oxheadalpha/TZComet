open Import

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
              | Michelson_storage ms ->
                  let open Michelson_storage in
                  let mich (Micheline m) =
                    let open Tezos_micheline in
                    Fmt.str "%a" Micheline_printer.print_expr
                      (Micheline_printer.printable Base.Fn.id m) in
                  li
                    [ b [txt "Michelson-storage-view:"]
                    ; ul
                        ( option_field "Michelson-Version" ms.version (fun s ->
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
end
