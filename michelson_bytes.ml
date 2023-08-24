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

open! Import

module Hex_reimplementation = struct
  open Caml
  (** We rewrite some of `hex.ml` to improve error messages. *)

  let to_char ~position x y =
    let code pos c =
      match c with
      | '0' .. '9' -> Char.code c - 48 (* Char.code '0' *)
      | 'A' .. 'F' -> Char.code c - 55 (* Char.code 'A' + 10 *)
      | 'a' .. 'f' -> Char.code c - 87 (* Char.code 'a' + 10 *)
      | _ ->
          Decorate_error.raise
            Message.(
              text "Character “"
              % inline_code (Char.escaped c)
              % text "”"
              %% parens
                   (int inline_code (Char.code c)
                   % text ", "
                   %% Fmt.kstr inline_code "0x%02x" (Char.code c))
              %% text "at position" %% int inline_code pos
              %% text "is not valid Hexadecimal encoding.")
    in
    Char.chr ((code position x lsl 4) + code (position + 1) y)

  let to_helper ~empty_return ~create ~set (`Hex s) =
    if s = "" then empty_return
    else
      let n = String.length s in
      let buf = create (n / 2) in
      let rec aux i j =
        if i >= n then ()
        else if j >= n then
          Decorate_error.raise
            Message.(
              text "Invalid hexadecimal string: length should be even, not"
              %% int inline_code n % text ".")
        else (
          set buf (i / 2) (to_char ~position:j s.[i] s.[j]);
          aux (j + 1) (j + 2))
      in
      aux 0 1;
      buf

  let to_bytes hex =
    to_helper ~empty_return:Bytes.empty ~create:Bytes.create ~set:Bytes.set hex
end

let parse_hex_bytes bytes =
  try
    let mich =
      Data_encoding.Binary.of_bytes_exn
        (* Tezos_micheline.Micheline.canonical_location_encoding *)
        Tezai_michelson.Untyped.expr_encoding
        (Hex_reimplementation.to_bytes (`Hex bytes))
    in
    let json =
      Data_encoding.Json.construct Tezai_michelson.Untyped.expr_encoding
        (* Tezos_micheline.Micheline.canonical_location_encoding *)
        mich
    in
    Ok
      ( json,
        let open Tezos_micheline in
        Fmt.str "%a" Micheline_printer.print_expr
          (Micheline_printer.printable Base.Fn.id mich) )
  with e ->
    let open Tezos_error_monad.Error_monad in
    Error [ Exn e ]

let pack_node_expression e =
  Data_encoding.Binary.to_bytes_exn Tezai_michelson.Untyped.expr_encoding
    (Tezos_micheline.Micheline.strip_locations e)
  |> Bytes.to_string

let encode_michelson_string s =
  Data_encoding.Binary.to_bytes_exn Tezai_michelson.Untyped.expr_encoding
    Tezos_micheline.Micheline.(String (0, s) |> strip_locations)
  |> Bytes.to_string

let encode_michelson_int i =
  Data_encoding.Binary.to_bytes_exn Tezai_michelson.Untyped.expr_encoding
    Tezos_micheline.Micheline.(Int (0, i) |> strip_locations)
  |> Bytes.to_string

let b58_script_id_hash_of_michelson_string s =
  Tezai_base58_digest.Identifier.Script_expr_hash.(
    hash_string ("\x05" ^ encode_michelson_string s) |> encode)

let b58_script_id_hash_of_michelson_int s =
  Tezai_base58_digest.Identifier.Script_expr_hash.(
    hash_string ("\x05" ^ encode_michelson_int s) |> encode)

let%expect_test _ =
  let p f v = Caml.Printf.printf "%S\n%!" (f v) in
  let ps = p b58_script_id_hash_of_michelson_string in
  let pi i = p b58_script_id_hash_of_michelson_int (Z.of_int i) in
  ps "" (* Check against `tezos-client  hash data '""' of type string` *);
  [%expect {| "expru5X1yxJG6ezR2uHMotwMLNmSzQyh5t1vUnhjx4cS6Pv9qE1Sdo" |}];
  ps "hello";
  [%expect {| "exprtsjEVVZk3Gm82U9wEs8kvwRiQwUT7zipJwvCeFMNsApe2tQ15s" |}];
  pi 0;
  [%expect {| "exprtZBwZUeYYYfUs9B9Rg2ywHezVHnCCnmF9WsDQVrs582dSK63dC" |}];
  pi (-1);
  [%expect {| "expru57wdzZCHCeGnKwUzxCJjG1HjveGXp1CCusScXEMq9kbidSvDG" |}];
  pi (-10_000);
  [%expect {| "expruboESrygwvfT6TdLDL6JWZ1RSyGxKV3szmVs6bgMWXbGnrToHi" |}];
  pi 10_000;
  [%expect {| "exprvLmTaiHBSiSgMnh1prUQA6wK2pGcmxHzTAkzX6Ym8b2Kjj1QHL" |}];
  pi 1;
  [%expect {| "expru2dKqDfZG8hu4wNGkiyunvq2hdSKuVYtcKta7BWP6Q18oNxKjS" |}];
  ()

let example () =
  let bytes = "0707002a002a" in
  let to_display =
    try
      let mich =
        Data_encoding.Binary.of_bytes_exn
          (* Tezos_micheline.Micheline.canonical_location_encoding *)
          Tezai_michelson.Untyped.expr_encoding
          (Hex.to_bytes (`Hex bytes))
      in
      let json =
        Data_encoding.Json.construct Tezai_michelson.Untyped.expr_encoding
          (* Tezos_micheline.Micheline.canonical_location_encoding *)
          mich
      in
      Ezjsonm.value_to_string ~minify:false json
    with
    | Data_encoding.Binary.Read_error e ->
        Fmt.str "readerror: %a" Data_encoding.Binary.pp_read_error e
    | e -> Fmt.str "exn: %a" Base.Exn.pp e
  in
  to_display
