open Import

let get_version () =
  let open Lwt.Infix in
  Js_of_ocaml_lwt.XmlHttpRequest.(
    get "./VERSION" >>= fun frame ->
    dbgf "version: %d" frame.code;
    if frame.code = 200 then Lwt.return (Some frame.content)
    else Lwt.return None)

let lwd_onload _ =
  let open Tyxml_lwd in
  let open Js_of_ocaml in
  let base_div = Dom_html.getElementById "attach-ui" in
  base_div##.innerHTML := Js.string "";
  Lwt.ignore_result
    Lwt.Infix.(
      get_version () >>= fun version_string ->
      let fragment = Js_of_ocaml.Url.Current.get_fragment () in
      let sys, `Extra_node_prefixes more_nodes, gui =
        State.Fragment.parse fragment
      in
      let nodes = Query_nodes.create () in
      let ipfs_gateways = Ipfs_gateways.create () in
      let fetcher = Contract_metadata.Uri.Fetcher.create () in
      let storage = Local_storage.create () in
      let window = Browser_window.create () in
      let state =
        object
          method system = sys
          method gui = gui
          method nodes = nodes
          method ipfs_gateways = ipfs_gateways
          method fetcher = fetcher
          method storage = storage
          method window = window
          method version_string = version_string
        end
      in
      Query_nodes.add_default_nodes state;
      List.iter more_nodes ~f:(fun prefix ->
          Query_nodes.add_node state
            (Query_nodes.Node.create ~network:`Sandbox prefix prefix));
      let doc = Gui.root_document state in
      let root = Lwd.observe doc in
      Lwd.set_on_invalidate root (fun _ ->
          ignore
            (Dom_html.window##requestAnimationFrame
               (Js.wrap_callback (fun _ ->
                    while Lwd.is_damaged root do
                      ignore (Lwd.quick_sample root)
                    done))));
      List.iter ~f:(Dom.appendChild base_div)
        (Lwd_seq.to_list (Lwd.quick_sample root) : _ node list :> raw_node list);
      Lwt.return_unit);
  Js._false

let gen_eight_byte_mults n =
  let eight = "AF00DFED" in
  let rec loop acc n = if n > 0 then loop (eight ^ acc) (n - 1) else acc in
  "0x" ^ loop "" n

(* TODO: run this as a test *)
let _parse_test () =
  let the_bytes = gen_eight_byte_mults 1000 in
  let test_json : string =
    let before = " {\"prim\": \"Pair\", \"args\": [{\"bytes\": \"" in
    let after = "\"}, {\"int\": \"40462\"}]}" in
    before ^ the_bytes ^ after
  in
  Stdlib.output_string Stdlib.stdout (test_json ^ "\n");
  let _z_value = Ezjsonm.value_from_string test_json in
  Stdlib.output_string Stdlib.stdout ("Done." ^ "\n")

let _ =
  dbgf "Hello Main!";
  let open Js_of_ocaml in
  (Lwt.async_exception_hook := fun e -> dbgf "Async Exn: %s" (Exn.to_string e));
  Dom_html.window##.onload := Dom_html.handler lwd_onload
