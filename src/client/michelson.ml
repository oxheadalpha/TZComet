open! Import

let micheline_of_ezjsonm json =
  let enc =
    Tezos_micheline.Micheline.canonical_encoding ~variant:"custom"
      Data_encoding.string in
  let mich = Data_encoding.Json.destruct enc json in
  Tezos_micheline.Micheline.root mich

let micheline_of_json s =
  let json =
    match Ezjsonm.value_from_string s with
    | `O (("code", code) :: _) -> code
    | other -> other in
  micheline_of_ezjsonm json

let micheline_to_ezjsonm mich =
  let enc =
    Tezos_micheline.Micheline.canonical_encoding ~variant:"custom"
      Data_encoding.string in
  let json =
    Data_encoding.Json.construct enc
      (Tezos_micheline.Micheline.strip_locations mich) in
  json