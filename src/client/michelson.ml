open! Import

let micheline_of_json s =
  dbgf "micheline_of_json : %d bytes" (String.length s);
  let json =
    match Ezjsonm.value_from_string s with
    | `O (("code", code) :: _) -> code
    | other -> other
  in
  dbgf "micheline_of_json: done parsing";
  Tezai_michelson.Untyped.of_json json

let micheline_node_to_string node =
  Tezai_michelson.Untyped.of_micheline_node node
  |> Tezai_michelson.Concrete_syntax.to_string

module Partial_type = struct
  module Structure = struct
    type type_kind =
      | Any
      | Nat
      | Mutez
      | Bytes
      | Address
      | Bool
      | String
      | List of type_kind
      | Map of type_kind * type_kind

    type leaf = string Reactive.var

    type t =
      | Leaf of {
          raw : string Tezos_micheline.Micheline.canonical;
          kind : type_kind;
          v : leaf;
          description : (string * string) option;
        }
      | Pair of { left : t; right : t }
  end

  type t = {
    original : string Tezos_micheline.Micheline.canonical;
    structure : Structure.t;
  }

  open Structure
  open! Tezai_contract_metadata.Metadata_contents.View.Implementation
  open! Tezai_contract_metadata.Metadata_contents.Michelson_blob

  let of_type ?(annotations = []) (Michelson_blob m) =
    let view_annots = annotations in
    let open Tezos_micheline.Micheline in
    let describe annot =
      List.find view_annots ~f:(fun (k, _) ->
          List.mem annot k ~equal:String.equal)
    in
    let rec go tp =
      let raw = strip_locations tp in
      let leaf ?annot kind =
        let description = Option.bind ~f:describe annot in
        Leaf { raw; kind; v = Reactive.var ""; description }
      in
      match tp with
      | Prim (_, "nat", [], annot) -> leaf Nat ~annot
      | Prim (_, "mutez", [], annot) -> leaf Mutez ~annot
      | Prim (_, "bytes", [], annot) -> leaf Bytes ~annot
      | Prim (_, "string", [], annot) -> leaf String ~annot
      | Prim (_, "address", [], annot) -> leaf Address ~annot
      | Prim (_, "bool", [], annot) -> leaf Bool ~annot
      | Prim (_, "pair", [ l; r ], _) -> Pair { left = go l; right = go r }
      | Prim (_, "list", [ Prim (_, "nat", [], _) ], annot) ->
          leaf (List Nat) ~annot
      | Prim
          ( _,
            "map",
            [ Prim (_, "string", [], _); Prim (_, "bytes", [], _) ],
            annot ) ->
          leaf (Map (String, Bytes)) ~annot
      | Prim (_, _, _, annot) -> leaf Any ~annot
      | _ -> leaf Any
    in
    { original = m; structure = go (root m) }

  let rec fill_structure_with_value mf node =
    let open Tezos_micheline.Micheline in
    let mich_node nod =
      Tezai_michelson.(
        Concrete_syntax.to_string (Untyped.of_micheline_node nod))
    in
    match (mf, node) with
    | Leaf leaf, nn -> Reactive.set leaf.v (mich_node nn)
    | Pair { left; right }, Prim (_, "Pair", [ l; r ], _) ->
        fill_structure_with_value left l;
        fill_structure_with_value right r
    | Pair _, other ->
        Decorate_error.(
          raise
            Message.(
              t "Type mismatch" %% ct (mich_node other) %% t "is not a pair."))

  let fill_with_value mf node = fill_structure_with_value mf.structure node

  let peek m =
    let rec pk = function
      | Leaf l -> Reactive.peek l.v
      | Pair { left; right } -> Fmt.str "(Pair %s %s)" (pk left) (pk right)
    in
    pk m.structure

  let validate_micheline m =
    match
      Tezai_michelson.Concrete_syntax.parse_exn ~check_indentation:false
        ~check_primitives:true m
    with
    | (_ : Tezai_michelson.Untyped.t) -> true
    | exception _ -> false

  let rec validate_structure = function
    | Leaf { kind = Nat | Mutez; v; _ } ->
        Reactive.(
          get v
          |> map ~f:(function
               | "" -> false
               | s -> (
                   match Z.of_string s with _ -> true | exception _ -> false)))
    | Leaf { kind = Bytes; v; _ } ->
        Reactive.(
          get v
          |> map ~f:(function
               | "" -> false
               | s -> (
                   match String.chop_prefix (String.strip s) ~prefix:"0x" with
                   | None -> false
                   | Some s -> (
                       match Hex.to_string (`Hex s) with
                       | _ -> true
                       | exception _ -> false))))
    | Leaf lf -> Reactive.(get lf.v |> map ~f:validate_micheline)
    | Pair { left; right } ->
        Reactive.(
          map2 ~f:( && ) (validate_structure left) (validate_structure right))

  let is_valid pt = validate_structure pt.structure

  open Meta_html

  let validity_error = function
    | Nat -> t "Invalid natural number."
    | Mutez -> t "Invalid μꜩ value."
    | Bytes -> t "Invalid bytes value."
    | String -> t "Invalid string value."
    | Address -> t "Invalid address."
    | Bool -> t "Invalid boolean."
    | Any | List _ | Map _ -> t "Invalid Micheline syntax."

  let to_form_items mf =
    let open Meta_html in
    let open Bootstrap.Form in
    let type_expr m =
      Fmt.kstr ct "%s"
        Tezai_michelson.(
          Concrete_syntax.to_string (Untyped.of_canonical_micheline m))
      (*         micheline_canonical_to_string m)  *)
    in
    let rec go = function
      | Pair { left; right } -> go left @ go right
      | Leaf leaf as leaf_structure ->
          [
            input
              ~label:
                (match leaf.description with
                | None ->
                    t "The parameter of type" %% type_expr leaf.raw % t "."
                | Some (an, s) ->
                    t "The parameter called " % ct an %% t "of type"
                    %% type_expr leaf.raw % t ":" %% it s)
              ~help:
                Reactive.(
                  bind (validate_structure leaf_structure) ~f:(function
                    | true -> Bootstrap.color `Success (t "OK")
                    | false ->
                        Bootstrap.color `Danger (validity_error leaf.kind)))
              ~placeholder:(Reactive.pure "Some decent Michelson right here")
              (Reactive.Bidirectional.of_var leaf.v);
          ]
    in
    go mf.structure

  let bytes_guesses input =
    try
      let raw, default_value =
        match input with
        | `Zero_x bytes ->
            let hex = String.chop_prefix_exn bytes ~prefix:"0x" in
            (Hex.to_string (`Hex hex), `Just_hex hex)
        | `Raw_string s ->
            let (`Hex hex) = Hex.of_string s in
            (s, `Just_hex hex)
      in
      let json () = `Json (Ezjsonm.value_from_string raw) in
      let utf8 () =
        let maxperline =
          let nl = Uchar.of_char '\n' in
          let folder (count, max_per_line) _ = function
            | `Uchar n when Uchar.equal n nl -> (0, max count max_per_line)
            | `Uchar _ -> (count + 1, max_per_line)
            | `Malformed _ -> Fmt.failwith "nop"
          in
          let c, m = Uutf.String.fold_utf_8 folder (0, 0) raw in
          max c m
        in
        let lines =
          match raw with "" -> [] | _ -> String.split ~on:'\n' raw
        in
        `Valid_utf_8 (maxperline, lines)
      in
      let bool () = `Bool (Bool.of_string raw) in
      let number () = `Number (Float.of_string raw) in
      let one_line_not_weird () =
        String.for_all raw ~f:(function '\n' | '\t' -> false | _ -> true)
      in
      let any_prefix l =
        List.exists l ~f:(fun prefix -> String.is_prefix raw ~prefix)
      in
      let web_uri () =
        if
          any_prefix [ "https://"; "http://"; "ftp://" ]
          && one_line_not_weird ()
        then `Web_uri raw
        else failwith "not web uri :)"
      in
      let tzip16_uri () =
        if
          any_prefix [ "tezos-storage://"; "ipfs://"; "sha256://" ]
          && one_line_not_weird ()
        then `Tzip16_uri raw
        else failwith "not tzip16 uri :)"
      in
      match
        List.find_map [ bool; number; web_uri; tzip16_uri; json; utf8 ]
          ~f:(fun f -> try Some (f ()) with _ -> None)
      with
      | Some s -> s
      | None -> default_value
    with _ -> `Dont_know

  let micheline_string_bytes_map_exn node =
    let open Tezos_micheline.Micheline in
    let nope = Decorate_error.raise in
    match node with
    | Seq (l, map) -> (
        match map with
        | [] -> []
        | Prim (_, "Elt", [ String (_, s); Bytes (_, b) ], _) :: more ->
            List.fold more
              ~init:[ (s, Bytes.to_string b) ]
              ~f:
                (fun prev -> function
                  | Prim (_, "Elt", [ String (_, s); Bytes (_, b) ], _) ->
                      (s, Bytes.to_string b) :: prev
                  | other ->
                      nope
                        Message.(
                          t "Michelson-map element has wrong structure:"
                          %% ct (micheline_node_to_string other)))
        | other ->
            nope
              Message.(
                t "Metadata result has wrong structure:"
                %% ct (micheline_node_to_string (Seq (l, other)))))
    | other ->
        nope
          Message.(
            t "Expecting Michelson-map but got"
            %% ct (micheline_node_to_string other))

  let desc ?(default = empty ()) description =
    Option.value_map description ~default ~f:(fun (k, v) ->
        t ":" %% it v %% parens (ct k))

  let show_bytes_result ~tzip16_uri ?description content =
    let show_content name f =
      let collapse = Bootstrap.Collapse.make () in
      Bootstrap.Collapse.fixed_width_reactive_button_with_div_below collapse
        ~width:"12em" ~kind:`Secondary
        ~button:(function
          | true -> t "Show" %% t name | false -> t "Hide" %% t name)
        f
    in
    let utf8_line_threshold = 78 in
    let show_summary = function
      | `Zero_x content ->
          ct (content |> bytes_summary ~threshold:30 ~left:15 ~right:15)
      | `Raw_string content ->
          let (`Hex hex) = Hex.of_string content in
          ct ("0x" ^ hex |> bytes_summary ~threshold:24 ~left:10 ~right:10)
    in
    [
      (show_summary content % desc description
      %%
      match bytes_guesses content with
      | `Just_hex hex ->
          show_content "Hex Dump" (fun () ->
              pre (ct (Hex.hexdump_s (`Hex hex))))
      | `Number f ->
          t "→ The number"
          %% it (Float.to_string_hum ~delimiter:' ' ~strip_zero:true f)
      | `Bool b -> t "→ The boolean" %% it (Bool.to_string b)
      | `Web_uri wuri -> t "→" %% url it wuri
      | `Tzip16_uri wuri -> t "→" %% tzip16_uri wuri
      | `Json v ->
          t "→"
          %% Bootstrap.color `Success (t "It is valid JSON!")
          %% show_content "Indented JSON" (fun () ->
                 pre (ct (Ezjsonm.value_to_string ~minify:false v)))
      | `Valid_utf_8 (maxperline, [ one ])
        when maxperline <= utf8_line_threshold ->
          t "→" %% t one %% parens (Bootstrap.color `Success (t "Valid UTF-8"))
      | `Valid_utf_8 (maxperline, lines) ->
          t "→"
          %% Bootstrap.color `Success
               (let lnnb = List.length lines in
                match lnnb with
                | 0 -> t "It's just empty."
                | _ ->
                    Fmt.kstr t
                      "It is valid UTF-8 text, %d line%s %d characters!" lnnb
                      (if lnnb <> 1 then "s, each ≤" else ",")
                      maxperline)
          %%
          if maxperline = 0 then empty ()
          else
            show_content "Text" (fun () ->
                div
                  (let sep () = H5.br () in
                   List.fold lines ~init:(empty ()) ~f:(fun p l ->
                       p % sep () % t l)))
      | `Dont_know -> parens (t "Can't identify"));
    ]

  let render ~tzip16_uri mf =
    let default content description_opt =
      [ ct content % desc description_opt ]
    in
    let rec structure = function
      | Leaf ({ kind = Bytes; _ } as leaf) ->
          let content = Reactive.peek leaf.v in
          show_bytes_result ~tzip16_uri (`Zero_x content)
            ?description:leaf.description
      | Leaf { kind = Map (String, Bytes); v; description; _ } -> (
          let content = Reactive.peek v in
          match
            Tezai_michelson.Concrete_syntax.parse_exn ~check_primitives:false
              ~check_indentation:false content
          with
          | node -> (
              try
                let map = micheline_string_bytes_map_exn node in
                [
                  t "Map"
                  %% parens (ct "string → bytes")
                  % desc description % t ":"
                  % itemize
                      (List.map map ~f:(fun (k, v) ->
                           Fmt.kstr ct "%S" k %% t "→"
                           % list
                               (show_bytes_result ~tzip16_uri (`Raw_string v))));
                ]
              with _ -> default content description)
          | exception _ -> default content description)
      | Leaf leaf -> default (Reactive.peek leaf.v) leaf.description
      | Pair { left; right } -> structure left @ structure right
    in
    structure mf.structure
end
