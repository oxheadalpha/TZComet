open Base

let dbg fmt = Fmt.pf Fmt.stdout "@[comevitz-debug: %a@]%!" fmt ()
let dbgf fmt = Fmt.(kstr (fun s -> dbg (const string s))) fmt

let gui () =
  let ex =
    let open Tezos_contract_metadata.Metadata_contents in
    Fmt.str "Example 5:\n%a" pp (Example.build 5) in
  Js_of_ocaml_tyxml.Tyxml_js.Html.(
    div [h3 [txt "Examples from the library:"]; pre [code [txt ex]]])

let attach_to_page gui =
  let open Js_of_ocaml in
  let base_div = Dom_html.getElementById "attach-ui" in
  base_div##.innerHTML := Js.string "" ;
  base_div##appendChild (Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_node gui)
  |> ignore ;
  Lwt.return ()

let go _ =
  dbg Fmt.(const string "Hello Go!") ;
  ignore
    Lwt.(
      catch
        (fun () -> attach_to_page (gui ()) >>= fun () -> return ())
        (fun exn ->
          Printf.ksprintf
            (fun s -> Fmt.epr "ERROR: %s" s ; failwith s)
            "Uncaught Exception: %s" (Exn.to_string exn))) ;
  Js_of_ocaml.Js._true

let _ =
  dbgf "Hello Main!" ;
  let open Js_of_ocaml in
  (Lwt.async_exception_hook := fun e -> dbgf "Async Exn: %s" (Exn.to_string e)) ;
  Dom_html.window##.onload := Dom_html.handler go
