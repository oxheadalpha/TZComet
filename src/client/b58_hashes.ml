open Import

module B58_crypto = struct
  let sha256 s = Digestif.SHA256.(to_raw_string (digest_string s))
  let sha512 s = Digestif.SHA512.(to_raw_string (digest_string s))
end

let script_expr_hash =
  (* Taken from src/proto_006_PsCARTHA/lib_protocol/script_expr_hash.ml *)
  (* expr(54) *)
  "\013\044\064\027"

let contract_hash =
  (* src/proto_006_PsCARTHA/lib_protocol/contract_hash.ml KT1(36) *)
  "\002\090\121"

let chain_id = (* src/lib_crypto/base58.ml *) "\087\082\000"
let protocol_hash = "\002\170" (* P(51) *)

let blake2b32 = Digestif.blake2b 32

let blake2b x =
  Digestif.digest_string blake2b32 x |> Digestif.to_raw_string blake2b32

let b58 s = Base58.of_bytes (module B58_crypto) s |> Base58.to_string

let unb58 s =
  Base58.of_string_exn (module B58_crypto) s
  |> Base58.to_bytes (module B58_crypto)

let b58_script_id_hash s = b58 (script_expr_hash ^ blake2b s)

let check_b58_hash ~prefix ~size s =
  let ppstring ppf s =
    let open Fmt in
    pf ppf "[%d→0x%a]" (String.length s) Hex.pp (Hex.of_string s) in
  let optry o k =
    Fmt.kstr
      (fun message ->
        match o () with
        | Some s -> s
        | None -> Fmt.failwith "%s" message
        | exception e -> Fmt.failwith "%s (%a)" message Exn.pp e)
      k in
  String.mapi s ~f:(fun idx c ->
      let bitcoin =
        "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" in
      if String.mem bitcoin c then '\x00'
      else
        Fmt.failwith
          "Invalid character '%c' at position %d in supposedly base-58 %S" c idx
          s)
  |> ignore ;
  let b58 =
    optry
      (fun () -> Base58.of_string (module B58_crypto) s)
      "Cannot decode base58 from %S" s in
  dbgf "base58: %a" Base58.pp b58 ;
  let data =
    optry
      (fun () -> Base58.to_bytes (module B58_crypto) b58)
      "Cannot get data from base58 %a" Base58.pp b58 in
  dbgf "data: %a" ppstring data ;
  dbgf "prefix: %a" ppstring prefix ;
  if not (String.is_prefix data ~prefix) then
    Fmt.failwith "Wrong prefix for data 0x%a, expecting 0x%a" Hex.pp
      (Hex.of_string data) Hex.pp (Hex.of_string prefix) ;
  let hashpart =
    optry
      (fun () -> String.chop_prefix data ~prefix)
      "Wrong refix AGAIN??? %S %S" data prefix in
  dbgf "hash: %a" ppstring hashpart ;
  optry
    (fun () -> Digestif.of_raw_string_opt (Digestif.blake2b size) hashpart)
    "This is not a blake2b hash: %a" ppstring hashpart

let check_b58_kt1_hash s = check_b58_hash ~prefix:contract_hash ~size:20 s
let check_b58_chain_id_hash s = check_b58_hash ~prefix:chain_id ~size:4 s
let check_b58_protocol_hash s = check_b58_hash ~prefix:protocol_hash ~size:32 s

let b58_script_id_hash_of_michelson_string s =
  b58_script_id_hash ("\x05" ^ Michelson_bytes.encode_michelson_string s)

let b58_script_id_hash_of_michelson_int s =
  b58_script_id_hash ("\x05" ^ Michelson_bytes.encode_michelson_int s)

let crypto_test () =
  dbgf "TRYING BLAKE2B: %s"
    (let dgst = Digestif.digest_string (Digestif.blake2b 32) "" in
     Digestif.to_hex (Digestif.blake2b 32) dgst) ;
  dbgf "TRYING base58: %a %S"
    Fmt.(Dump.option Base58.pp)
    (Base58.of_string
       (module B58_crypto)
       "expru5X1yxJG6ezR2uHMotwMLNmSzQyh5t1vUnhjx4cS6Pv9qE1Sdo")
    ( Base58.of_string_exn
        (module B58_crypto)
        "expru5X1yxJG6ezR2uHMotwMLNmSzQyh5t1vUnhjx4cS6Pv9qE1Sdo"
    |> Base58.to_bytes (module B58_crypto)
    |> Option.value_map ~default:"EEEERRRROR" ~f:(fun x ->
           let (`Hex h) = Hex.of_string x in
           h) ) ;
  let michelson_string_expr_hash s =
    dbgf "mseh: %S" s ;
    let bytes = Michelson_bytes.encode_michelson_string s in
    let ppb ppf b =
      let (`Hex hx) = Hex.of_string b in
      Fmt.pf ppf "0x%s" hx in
    dbgf "bytes: %a" ppb bytes ;
    let dgst x =
      Digestif.digest_string (Digestif.blake2b 32) x
      |> Digestif.to_raw_string (Digestif.blake2b 32) in
    let b58 s = Base58.of_bytes (module B58_crypto) s |> Base58.to_string in
    dbgf "digest raw: %a -> %s (%s)" ppb (dgst bytes)
      (b58 (dgst bytes))
      (Base58.raw_encode (dgst bytes)) ;
    let with05 = "\x05" ^ bytes in
    dbgf "digest-05: %a → %s [%s]" ppb (dgst with05)
      (b58 (dgst with05))
      (Base58.raw_encode (dgst with05)) ;
    dbgf "digest-pfx: %a → %s" ppb (dgst with05)
      (b58 (script_expr_hash ^ dgst with05)) in
  michelson_string_expr_hash "" ;
  michelson_string_expr_hash "foo" ;
  ()
