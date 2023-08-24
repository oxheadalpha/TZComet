(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 TQ Tezos <contact@tqtezos.com>                         *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(*
   let get_script_field_exn string_micheline field =
     let open Micheline in
     let type_opt =
       match root string_micheline with
       | Seq (_, l) ->
           Base.List.find_map l ~f:(function
             | Prim (_, f, [t], _) when f = field -> Some t
             | _ -> None )
       | _ -> None in
     match type_opt with
     | None -> Fmt.failwith "Cannot find the %S field for the contract" field
     | Some s -> s

   let get_storage_type_exn string_micheline =
     get_script_field_exn string_micheline "storage"

   let get_parameter_type_exn string_micheline =
     get_script_field_exn string_micheline "parameter"

   let pp_arbitrary_micheline ppf e =
     let module P = Micheline_printer in
     P.print_expr ppf
       (Micheline.map_node (fun _ -> {P.comment= None}) (fun x -> x) e)
*)

let rec normalize_combs ~primitive m =
  let open Tezai_michelson.Untyped.M in
  let continue = normalize_combs ~primitive in
  let is_prim p = String.equal p primitive in
  match m with
  | Prim (loc, p, [ l; r ], ann) when is_prim p ->
      Prim (loc, p, [ continue l; continue r ], ann)
  | Prim (loc, p, one :: more, ann) when is_prim p ->
      let right = Prim (loc, p, List.map continue more, []) |> continue in
      Prim (loc, p, [ continue one; right ], ann)
  | other -> other

let%expect_test _ =
  let test c =
    let m = Tezai_michelson.Untyped.C.concrete c in
    Fmt.pr "%a\n%!" Tezai_michelson.Untyped.pp
      (normalize_combs ~primitive:"Pair" m)
  in
  test "(Pair 2 3)";
  [%expect {| (Pair 2 3) |}];
  test "(Pair 2 3 4)";
  [%expect {| (Pair 2 (Pair 3 4)) |}];
  test "(Pair 2 (Pair 3 4))";
  [%expect {| (Pair 2 (Pair 3 4)) |}];
  test "(Pair 2 (Pair 1 2 3 4))";
  [%expect {| (Pair 2 (Pair 1 (Pair 2 (Pair 3 4)))) |}];
  test "(Pair 2 (Junk 1 2 3 4))";
  [%expect {| (Pair 2 (Junk 1 2 3 4)) |}];
  test "(Pair (Pair %hello 2 3) (Pair 1 2 3) 4)";
  [%expect {| (Pair (Pair %hello 2 3) (Pair (Pair 1 (Pair 2 3)) 4)) |}];
  ()

let rec find_metadata_big_maps ~storage_node ~type_node =
  let open Tezai_michelson.Untyped.M in
  let go (storage_node, type_node) =
    find_metadata_big_maps ~storage_node ~type_node
  in
  match (storage_node, type_node) with
  | Prim (_, "Pair", [ l; r ], _), Prim (_, "pair", [ lt; rt ], _) ->
      go (l, lt) @ go (r, rt)
  | ( Int (_, z),
      Prim
        ( _,
          "big_map",
          [ Prim (_, "string", [], _); Prim (_, "bytes", [], _) ],
          [ "%metadata" ] ) ) ->
      [ z ]
  | Int (_, _z), _ -> []
  | String (_, _s), _ -> []
  | Bytes (_, _b), _ -> []
  | Prim (_, _prim, _args, _annot), _t -> []
  | Seq (_, _l), _t -> []

let build_off_chain_view_contract view ~contract_balance ~contract_address
    ~contract_storage_type ~contract_parameter_type ~view_parameters
    ~contract_storage =
  let open Tezai_contract_metadata in
  let open Metadata_contents.View in
  let open Metadata_contents.Michelson_blob in
  let open Implementation.Michelson_storage in
  let open Tezai_michelson.Untyped.M in
  let getm = function
    | Michelson_blob m -> Tezai_michelson.Untyped.of_canonical_micheline m
  in
  let open Tezai_michelson.Untyped.C in
  let parameter, input =
    match Option.map getm view.parameter with
    | Some m ->
        ( prim "pair" [ m; contract_storage_type ],
          prim "Pair" [ view_parameters; contract_storage ] )
    | None -> (contract_storage_type, contract_storage)
  in
  let storage = getm view.return_type in
  let code = getm view.code in
  let rec fix_code c =
    let continue = List.map (fun c -> fix_code c) in
    match c with
    | (Int _ | String _ | Bytes _) as lit ->
        Tezai_michelson.Untyped.of_micheline_node lit
    | Prim (_loc, "SELF", [], annotations) ->
        seq
          [
            prim "PUSH" [ prim "address" []; string contract_address ];
            prim ~annotations "CONTRACT" [ contract_parameter_type ];
            prim "IF_NONE"
              [ seq [ prim "UNIT" []; prim "FAILWITH" [] ]; seq [] ];
          ]
    | Prim (_loc, "BALANCE", [], annotations) ->
        prim "PUSH" [ prim "mutez" []; int contract_balance ] ~annotations
    | Prim (_, name, args, annotations) ->
        prim name (continue args) ~annotations
    | Seq (_, l) -> seq (continue l)
  in
  ( `Contract
      (seq
         [
           prim "parameter" [ parameter ];
           prim "storage" [ prim "option" [ storage ] ];
           prim "code"
             [
               seq
                 [
                   prim "CAR" [] (* We drop the storage (= None). *);
                   fix_code code;
                   prim "SOME" [];
                   prim "NIL" [ prim "operation" [] ];
                   prim "PAIR" [];
                 ];
             ];
         ]),
    `Input input,
    `Storage (prim "None" []) )
