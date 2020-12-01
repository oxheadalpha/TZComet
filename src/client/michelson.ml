open! Import
open Tezos_micheline

let micheline_of_ezjsonm json =
  let enc =
    Micheline.canonical_encoding ~variant:"custom" Data_encoding.string in
  let mich = Data_encoding.Json.destruct enc json in
  Micheline.root mich

let micheline_of_json s =
  dbgf "micheline_of_json : %d bytes" (String.length s) ;
  let json =
    match Ezjsonm.value_from_string s with
    | `O (("code", code) :: _) -> code
    | other -> other in
  dbgf "micheline_of_json: done parsing" ;
  micheline_of_ezjsonm json

let micheline_to_ezjsonm mich =
  let enc =
    Micheline.canonical_encoding ~variant:"custom" Data_encoding.string in
  let json = Data_encoding.Json.construct enc (Micheline.strip_locations mich) in
  json

let parse_micheline ~check_indentation ~check_primitives m =
  let rec primitive_check =
    let open Tezos_micheline.Micheline in
    function
    | Prim (_, s, args, _) ->
        ( match
            List.find Michelson_bytes.primitives ~f:(fun (p, _) ->
                String.equal p s)
          with
        | Some _ -> ()
        | None -> Fmt.failwith "Unknown primitive: %S" s ) ;
        List.iter args ~f:primitive_check
    | _ -> () in
  match Micheline_parser.tokenize m with
  | tokens, [] -> (
    match Micheline_parser.parse_expression ~check:check_indentation tokens with
    | node, [] -> (
      try
        if check_primitives then primitive_check node ;
        Ok node
      with e -> Error [Tezos_error_monad.Error_monad.Exn e] )
    | _, errs -> Error errs )
  | _, errs -> Error errs

let parse_micheline_exn ~check_indentation ~check_primitives m =
  match parse_micheline ~check_indentation ~check_primitives m with
  | Ok o -> o
  | Error e ->
      Fmt.failwith "parse_micheline: %a"
        Tezos_error_monad.Error_monad.pp_print_error e

let micheline_canonical_to_string c =
  Fmt.str "%a" Micheline_printer.print_expr
    (Micheline_printer.printable Base.Fn.id c)

let micheline_node_to_string node =
  micheline_canonical_to_string (Micheline.strip_locations node)
