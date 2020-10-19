open! Base

let dbgf fmt = Fmt.kstr (fun s -> Fmt.epr "deploy-examples: %s\n%!" s) fmt

module Env = struct
  let with_default v default =
    match Caml.Sys.getenv_opt v with None -> default | Some v -> v

  let or_fail v =
    try Caml.Sys.getenv v
    with _ -> Fmt.failwith "Missing required environment variable %S" v

  let tezos_client () = with_default "tezos_client_bin" "tezos-client"
  let funder () = or_fail "funder_account"
end

module System = struct
  let cmd s =
    match Caml.Sys.command s with
    | 0 -> ()
    | n -> Fmt.failwith "Command %S returned %d" s n

  let escape = Caml.Filename.quote
  let exec l = cmd (String.concat ~sep:" " (List.map l ~f:escape))

  let exec_and_redirect_stdout ~stdout l =
    Fmt.kstr cmd "%s >> %s"
      (String.concat ~sep:" " (List.map l ~f:escape))
      (escape stdout)

  let append_to_file ~file s =
    Fmt.kstr cmd "printf '%%s' %s >> %s" (escape s) (escape file)
end

let tezos_client args = Env.tezos_client () :: args

let originate ?(balance = 0) ?(description = "") ~source ~init ~name ~logfile ()
    =
  let cmd =
    tezos_client
      [ "--wait"; "0"; "originate"; "contract"; name; "transferring"
      ; Int.to_string balance; "from"; Env.funder (); "running"; source
      ; "--burn-cap"; "10"; "--init"; init; "--force" ] in
  System.exec cmd ;
  System.append_to_file ~file:logfile (Fmt.str "## Contract `%s`\n\n" name) ;
  System.append_to_file ~file:logfile (Fmt.str "- Init: `%s`\n- Address: " init) ;
  System.exec_and_redirect_stdout ~stdout:logfile
    (tezos_client ["show"; "known"; "contract"; name]) ;
  System.append_to_file ~file:logfile (Fmt.str "\n%s\n\n" description) ;
  ()

let contract () =
  {tz|
parameter unit;
storage (pair nat (big_map %metadata string bytes));
code { FAILWITH; };
|tz}

module Micheline_views = struct
  open Ezjsonm

  let prim ?(annotations = []) p l =
    dict [("prim", string p); ("args", `A l); ("annots", strings annotations)]

  let nat = prim "nat" []
  let mutez = prim "mutez" []
  let timestamp = prim "timestamp" []
  let prims = List.map ~f:(fun p -> prim p [])

  let view_with_code ?pure ?version ?parameter ?(return_type = nat)
      ?(annotations = []) name code =
    let open Ezjsonm in
    let or_empty opt f = Option.value_map opt ~default:[] ~f in
    dict
      ( or_empty pure (fun b -> [("pure", bool b)])
      @ [ ("name", string name)
        ; ( "implementations"
          , `A
              [ dict
                  [ ( "michelson-storage-view"
                    , dict
                        ( Option.value_map parameter ~default:[] ~f:(fun p ->
                              [("parameter", p)])
                        @ [("return-type", return_type); ("code", `A code)]
                        @ ( match annotations with
                          | [] -> []
                          | more ->
                              [ ( "annotations"
                                , list
                                    (fun (k, v) ->
                                      dict
                                        [ ("name", string k)
                                        ; ("description", string v) ])
                                    more ) ] )
                        @
                        match version with
                        | None -> []
                        | Some s -> [("version", string s)] ) ) ] ] ) ] )
end

let all ?(just_one_actually = false) ~logfile () =
  let id = ref 0 in
  let simple description ?(the_nat = 7) bm =
    let name = Caml.incr id ; Fmt.str "de%d" !id in
    let source = contract () in
    let to_hex s =
      Fmt.str "0x%s"
        (let (`Hex x) = Hex.of_string s in
         x) in
    let init =
      Fmt.str "(Pair %d {%s})" the_nat
        (String.concat ~sep:" ; "
           (List.map bm ~f:(fun (k, v) -> Fmt.str "Elt %S %s" k (to_hex v))))
    in
    originate ~description ~logfile ~name ~source ~init () in
  let root uri = ("", uri) in
  let self_host description json =
    simple
      (Fmt.str "Self-hosted JSON.\n\n%s\n\n```json\n%s\n```\n\n" description
         (Ezjsonm.value_to_string ~minify:false json))
      [root "tezos-storage:contents"; ("contents", Ezjsonm.value_to_string json)]
  in
  let self_describe description more_fields =
    self_host description
      Ezjsonm.(
        dict
          ( [ ("description", string description)
            ; ("version", string "tzcomet-example v0.0.42") ]
          @ more_fields )) in
  let open Micheline_views in
  let basics =
    Ezjsonm.
      [ ( "license"
        , dict [("name", string "MIT"); ("details", string "The MIT License")]
        ); ("homepage", string "https://gitlab.com/tqtezos/TZComet") ] in
  let failwith_01 =
    view_with_code
      ~parameter:(prim "pair" ~annotations:["%arg_zero"] [nat; mutez])
      "just-call-failwith"
      ~annotations:[("%arg_zero", "This is obvioulsy ignored.")]
      [prim "FAILWITH" []] in
  let multiply_the_nat =
    (* let code = prims ["CAR"; "SELF"; "CAR"; "MUL"] in *)
    let code = prims ["CAR"; "DUP"; "CDAR"; "SWAP"; "CAR"; "MUL"] in
    view_with_code ~pure:true ~parameter:nat "multiply-the-nat-in-storage" code
  in
  let call_balance =
    let code = prims ["DROP"; "BALANCE"] in
    view_with_code "just-call-balance" code in
  let call_self_address =
    let code = prims ["DROP"; "SELF"; "ADDRESS"] in
    view_with_code "get-contract-address" code ~return_type:(prim "address" [])
  in
  let basics_and_views l = Ezjsonm.(basics @ [("views", list Fn.id l)]) in
  let one () =
    self_describe "This contract has a couple of off-chain-views."
      (basics_and_views
         [failwith_01; multiply_the_nat; call_balance; call_self_address]) in
  let many () =
    originate ~logfile ~name:"de0" ~source:(contract ()) ~init:"(Pair 2 {})" () ;
    simple "The *empty* one." [] ;
    simple "Has a URI that points nowhere." [root "tezos-storage:onekey"] ;
    self_host "" Ezjsonm.(dict []) ;
    self_host "Just a version."
      Ezjsonm.(dict [("version", string "tzcomet-example v0.0.42")]) ;
    self_describe "This contract has few more fields." basics ;
    self_describe "This contract has a couple of off-chain-views."
      (basics_and_views
         [failwith_01; multiply_the_nat; call_balance; call_self_address]) ;
    () in
  if just_one_actually then one () else many () ;
  ()

let () =
  let usage () = Fmt.epr "usage: %s <TODO>\n%!" Caml.Sys.argv.(0) in
  let logfile = "/tmp/originations.md" in
  match Caml.Sys.argv.(1) with
  | "all" -> all () ~logfile
  | "one" -> all () ~logfile ~just_one_actually:true
  | other ->
      Fmt.epr "Unknown command: %S!\n%!" other ;
      usage () ;
      Caml.exit 2
  | exception _ ->
      Fmt.epr "Missing command\n%!" ;
      usage () ;
      Caml.exit 2
