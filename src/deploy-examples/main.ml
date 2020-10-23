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
      ; "--burn-cap"; "100"; "--init"; init; "--force" ] in
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

  let int i = dict [("int", string (Int.to_string i))]
  let seq l = list Fn.id l
  let nat = prim "nat" []
  let mutez = prim "mutez" []
  let timestamp = prim "timestamp" []
  let prims = List.map ~f:(fun p -> prim p [])
  let or_empty opt f = Option.value_map opt ~default:[] ~f

  let view ?description ?pure name implementations =
    dict
      ( or_empty pure (fun b -> [("pure", bool b)])
      @ [("name", string name)]
      @ or_empty description (fun d -> [("description", string d)])
      @ [("implementations", `A implementations)] )

  let storage_view_implementation ?version ?parameter ?(return_type = nat)
      ?(annotations = []) code =
    dict
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
                          dict [("name", string k); ("description", string v)])
                        more ) ] )
            @ match version with None -> [] | Some s -> [("version", string s)]
            ) ) ]

  let view_with_code ?description ?pure ?version ?parameter ?return_type
      ?(annotations = []) name code =
    view ?description ?pure name
      [ storage_view_implementation ?version ?parameter ?return_type
          ~annotations code ]
end

let all ?only ~logfile () =
  let id = ref 0 in
  let originate ~description ~logfile ~name ~source ~init () =
    match only with
    | Some l when not (List.mem l name ~equal:String.equal) -> ()
    | None | Some _ -> originate ~description ~logfile ~name ~source ~init ()
  in
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
        ); ("homepage", string "https://github.com/tqtezos/TZComet") ] in
  let empty_view_01 =
    view
      ~description:
        "This view has no implementations …\n\nWhich is indeed useless."
      "an-empty-useless-view" [] in
  let view_with_too_much_code =
    view
      ~description:
        "This view has a bunch of implementations …\n\n\
         They are all meaningless."
      "an-empty-useless-view"
      [ storage_view_implementation ~return_type:nat
          ( prims ["DUP"; "DUP"; "DUP"; "DUP"; "DUP"; "DUP"; "PAIR"]
          @ [prim "DIP" (prims (List.init 50 ~f:(Fn.const "PAIR")))] )
      ; storage_view_implementation ~return_type:nat
          ( prims ["DUP"; "DUP"; "DUP"; "DUP"; "DUP"; "DUP"; "PAIR"]
          @ [ prim "DIP"
                ( prims (List.init 50 ~f:(Fn.const "PAIR"))
                @ [prim "DIP" (prims (List.init 50 ~f:(Fn.const "PAIR")))] ) ]
          ) ] in
  let failwith_01 =
    view_with_code
      ~return_type:(prim "int" ~annotations:["%negative_even_number"] [])
      ~parameter:(prim "int" ~annotations:["%the_decisive_argument"] [])
      "multiply-negative-number-or-call-failwith"
      ~annotations:
        [ ( "%the_decisive_argument"
          , "The integer argument if >0 this will fail." )
        ; ( "%negative_even_number"
          , "The result, if any, is twice the argument \
             (%the_decisive_argument)." ) ]
      [ prim "CAR" []; prim "DUP" []; prim "PUSH" [prim "int" []; int 0]
      ; prim "COMPARE" []; prim "LT" []
      ; prim "IF"
          [ seq [prim "FAILWITH" []]
          ; seq [prim "PUSH" [prim "int" []; int 2]; prim "MUL" []] ] ] in
  let identity_01 =
    let big_type ann2 =
      prim "pair"
        [ prim "nat" ~annotations:["%arg_zero"] []
        ; prim "pair"
            [ prim "string" ~annotations:[ann2] []
            ; prim "mutez" ~annotations:["%arg_two"] [] ] ] in
    let parameter = big_type "%arg_one" in
    let return_type = big_type "%arg_one_result" in
    view_with_code ~parameter "the-identity" ~return_type
      ~annotations:
        [ ("%arg_zero", "This is obvioulsy ignored.")
        ; ("%arg_one", "This is also ignored, but different.")
        ; ("%arg_one_result", "This is %arg_one on the resulting side.")
        ; ( "%arg_two"
          , "This is also ignored, but with a lot of data\n\n\
             Lorem ipsuming and all." ) ]
      [prim "CAR" []] in
  let multiply_the_nat =
    (* let code = prims ["CAR"; "SELF"; "CAR"; "MUL"] in *)
    let code = prims ["DUP"; "CDR"; "CAR"; "SWAP"; "CAR"; "MUL"] in
    view_with_code ~pure:true ~parameter:nat "multiply-the-nat-in-storage" code
      ~description:
        "This one is pure, it multiplies the natural number given as argument \
         with the one in storage." in
  let call_balance =
    let code = prims ["DROP"; "BALANCE"] in
    view_with_code ~return_type:mutez "just-call-balance" code in
  let call_self_address =
    let code = prims ["DROP"; "SELF"; "ADDRESS"] in
    view_with_code "get-contract-address" code
      ~return_type:(prim "address" [] ~annotations:["%ret"])
      ~annotations:
        [ ( "%ret"
          , "The address of the (any) contract, re-obtained in Michelson." ) ]
  in
  let basics_and_views l = Ezjsonm.(basics @ [("views", list Fn.id l)]) in
  let many () =
    originate ~logfile ~description:"Empty contract" ~name:"de0"
      ~source:(contract ()) ~init:"(Pair 2 {})" () ;
    simple "The *empty* one." [] ;
    simple "Has a URI that points nowhere." [root "tezos-storage:onekey"] ;
    self_host "" Ezjsonm.(dict []) ;
    self_host "Just a version."
      Ezjsonm.(dict [("version", string "tzcomet-example v0.0.42")]) ;
    self_describe "This contract has few more fields." basics ;
    self_describe "This contract has a one off-chain-view."
      (basics_and_views [multiply_the_nat]) ;
    self_describe "This contract has a couple of off-chain-views."
      (basics_and_views
         [ empty_view_01; failwith_01; multiply_the_nat; call_balance
         ; identity_01; view_with_too_much_code; call_self_address ]) ;
    () in
  many () ; ()

let () =
  let usage () = Fmt.epr "usage: %s <TODO>\n%!" Caml.Sys.argv.(0) in
  let logfile = "/tmp/originations.md" in
  match Array.to_list Caml.Sys.argv |> List.tl_exn with
  | ["all"] -> all () ~logfile
  | "only" :: these -> all () ~logfile ~only:these
  | other :: _ ->
      Fmt.epr "Unknown command: %S!\n%!" other ;
      usage () ;
      Caml.exit 2
  | [] ->
      Fmt.epr "Missing command\n%!" ;
      usage () ;
      Caml.exit 2
