open Import

let primitives = Tezai_michelson.Untyped.primitives

let expr_encoding =
  Tezos_micheline.Micheline.canonical_encoding_v1 ~variant:"michelson_v1"
    (* Data_encoding.Encoding.string *)
    (let open Data_encoding in
    def "michelson.v1.primitives" @@ string_enum primitives)

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
              t "Character “"
              % ct (Char.escaped c)
              % t "”"
              %% parens
                   (int ct (Char.code c)
                   % t ", "
                   %% Fmt.kstr ct "0x%02x" (Char.code c))
              %% t "at position" %% int ct pos
              %% t "is not valid Hexadecimal encoding.")
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
              t "Invalid hexadecimal string: length should be even, not"
              %% int ct n % t ".")
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
        expr_encoding
        (Hex_reimplementation.to_bytes (`Hex bytes))
    in
    let json =
      Data_encoding.Json.construct expr_encoding
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

let encode_michelson_string s =
  Data_encoding.Binary.to_bytes_exn expr_encoding
    Tezos_micheline.Micheline.(String (0, s) |> strip_locations)
  |> Bytes.to_string

let encode_michelson_int z =
  Data_encoding.Binary.to_bytes_exn expr_encoding
    Tezos_micheline.Micheline.(Int (0, z) |> strip_locations)
  |> Bytes.to_string

let example () =
  let bytes = "0707002a002a" in
  let to_display =
    try
      let mich =
        Data_encoding.Binary.of_bytes_exn
          (* Tezos_micheline.Micheline.canonical_location_encoding *)
          expr_encoding
          (Hex.to_bytes (`Hex bytes))
      in
      let json =
        Data_encoding.Json.construct expr_encoding
          (* Tezos_micheline.Micheline.canonical_location_encoding *)
          mich
      in
      Ezjsonm.value_to_string ~minify:false json
    with
    | Data_encoding.Binary.Read_error e ->
        Fmt.str "readerror: %a" Data_encoding.Binary.pp_read_error e
    | e -> Fmt.str "exn: %a" Exn.pp e
  in
  to_display
