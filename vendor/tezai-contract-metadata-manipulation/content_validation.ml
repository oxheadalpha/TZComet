open! Import

module Error = struct
  type t =
    | Forbidden_michelson_instruction of { view : string; instruction : string }
    | Michelson_version_not_a_protocol_hash of { view : string; value : string }

  let pp ppf =
    let open Fmt in
    let textf f = kstr (fun s -> (box text) ppf s) f in
    function
    | Forbidden_michelson_instruction { view; instruction } ->
        textf "Forbidden Michelson instruction %S in view %S" instruction view
    | Michelson_version_not_a_protocol_hash { view; value } ->
        textf "Michelson version %S in view %S is not a protocol hash" value
          view
end

module Warning = struct
  type t =
    | Wrong_author_format of string
    | Unexpected_whitespace of { field : string; value : string }
    | Self_unaddressed of { view : string; instruction : string option }

  let pp ppf =
    let open Fmt in
    let textf f = kstr (fun s -> (box text) ppf s) f in
    function
    | Wrong_author_format auth -> textf "Wrong format for author field: %S" auth
    | Unexpected_whitespace { field; value } ->
        textf "Unexpected whitespace character(s) in field %S = %S" field value
    | Self_unaddressed { view; instruction } ->
        textf "SELF instruction not followed by ADDRESS (%s) in view %S"
          (Option.value instruction ~default:"by nothing")
          view
end

module Data = struct
  let author_re = lazy Re.Posix.(re "^[^\\<\\>]*<[^ ]+>$" |> compile)

  let forbidden_michelson_instructions =
    [
      "AMOUNT";
      "CREATE_CONTRACT";
      "SENDER";
      "SET_DELEGATE";
      "SOURCE";
      "TRANSFER_TOKENS";
    ]
end

open Tezai_contract_metadata.Metadata_contents
open Data

let validate ?(protocol_hash_is_valid = fun _ -> true) (metadata : t) =
  let errors = ref [] in
  let warnings = ref [] in
  let error e = errors := e :: !errors in
  let warning e = warnings := e :: !warnings in
  let nl_or_tab = function '\n' | '\t' -> true | _ -> false in
  let nl_or_tab_or_sp = function '\n' | '\t' | ' ' -> true | _ -> false in
  let check_for_whitespace ?(whitespace = nl_or_tab) field value =
    if Base.String.exists value ~f:whitespace then
      warning Warning.(Unexpected_whitespace { field; value })
  in
  let check_author = function
    | s when not (Re.execp (Lazy.force author_re) s) ->
        warning Warning.(Wrong_author_format s)
    | _ -> ()
  in
  List.iter
    ~f:(fun a ->
      check_author a;
      check_for_whitespace "author" a)
    metadata.authors;
  Option.iter ~f:(check_for_whitespace "name") metadata.name;
  Option.iter ~f:(check_for_whitespace "version") metadata.version;
  let check_view (v : View.t) =
    let implementation (i : View.Implementation.t) =
      let open View.Implementation in
      match i with
      | Michelson_storage { code = Michelson_blob mich_code; version; _ } -> (
          Option.iter
            ~f:(fun value ->
              if protocol_hash_is_valid value then ()
              else
                error
                  (Error.Michelson_version_not_a_protocol_hash
                     { view = v.name; value }))
            version;
          let open Tezos_micheline.Micheline in
          let node = root mich_code in
          let rec iter = function
            | Int _ | String _ | Bytes _ -> `Other "literal"
            | Prim (_loc, p, args, _annots) -> (
                if
                  List.mem forbidden_michelson_instructions p
                    ~equal:String.equal
                then
                  error
                    (Error.Forbidden_michelson_instruction
                       { view = v.name; instruction = p });
                let _ = List.map ~f:iter args in
                match p with
                | "SELF" -> `Self
                | "ADDRESS" -> `Address
                | _ -> `Other p)
            | Seq (_loc, l) ->
                let selves = List.map ~f:iter l in
                List.fold
                  (selves : [ `Address | `Other of string | `Self ] list)
                  ~init:
                    (`Other "none" : [ `Address | `Other of string | `Self ])
                  ~f:(fun prev cur ->
                    match (prev, cur) with
                    | `Other _, _ -> cur
                    | `Self, `Address -> cur
                    | `Self, _ ->
                        warning
                          Warning.(
                            Self_unaddressed
                              {
                                view = v.name;
                                instruction =
                                  (match cur with
                                  | `Self -> Some "SELF"
                                  | `Other p -> Some p
                                  | `Address -> assert false);
                              });
                        cur
                    | `Address, _ -> cur)
          in
          match iter node with
          | `Self ->
              warning
                Warning.(Self_unaddressed { view = v.name; instruction = None })
          | _ -> ())
      | Rest_api_query _ -> ()
    in
    check_for_whitespace "view.name" v.name ~whitespace:nl_or_tab_or_sp;
    List.iter ~f:implementation v.implementations
  in
  List.iter ~f:check_view metadata.views;
  (List.rev !errors, List.rev !warnings)

let pp ppf =
  let open Fmt in
  function
  | [], [] -> pf ppf "No errors nor warnings."
  | errs, warns ->
      let pp_events prompt pp =
        let itemize ppf = function
          | [] -> const string "None" ppf ()
          | more -> (cut ++ list ~sep:cut (const string "* " ++ pp)) ppf more
        in
        vbox ~indent:2 (const string prompt ++ itemize)
      in
      vbox
        (const (pp_events "Errors: " Error.pp) errs
        ++ cut
        ++ const (pp_events "Warnings: " Warning.pp) warns)
        ppf ()
