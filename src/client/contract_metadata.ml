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
    let uri =
      Uri.of_string uri_code |> of_uri ~validate_kt1_address ~validate_network
    in
    (uri, List.rev !errors)

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

end

