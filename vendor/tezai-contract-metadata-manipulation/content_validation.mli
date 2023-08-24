module Error : sig
  type t =
    | Forbidden_michelson_instruction of { view : string; instruction : string }
    | Michelson_version_not_a_protocol_hash of { view : string; value : string }

  val pp : Caml.Format.formatter -> t -> unit
end

module Warning : sig
  type t =
    | Wrong_author_format of string
    | Unexpected_whitespace of { field : string; value : string }
    | Self_unaddressed of { view : string; instruction : string option }

  val pp : Caml.Format.formatter -> t -> unit
end

module Data : sig
  val author_re : Re.re lazy_t
  val forbidden_michelson_instructions : string list
end

val validate :
  ?protocol_hash_is_valid:(string -> bool) ->
  Tezai_contract_metadata.Metadata_contents.t ->
  Error.t list * Warning.t list
(** Run the validation on a metadata instance. The default
    [protocol_hash_is_valid] is [(fun _ -> true)], so by default the error
    [Michelson_version_not_a_protocol_hash _] is not reported (for library
    dependency reasons). *)

val pp : Caml.Format.formatter -> Error.t list * Warning.t list -> unit
