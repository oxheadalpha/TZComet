open! Import

type t =
  [`Mainnet | `Hangzhounet | `Granadanet | `Idiazabalnet | `Ithacanet | `Sandbox]

let to_string : t -> string = function
  | `Mainnet -> "Mainnet"
  | `Granadanet -> "Granadanet"
  | `Hangzhounet -> "Hangzhounet"
  | `Idiazabalnet -> "Idiazabalnet"
  | `Ithacanet -> "Ithacanet"
  | `Sandbox -> "Sandbox"

let better_call_dev_path : t -> string option = function
  | `Mainnet -> Some "mainnet"
  | `Granadanet -> Some "granadanet"
  | `Hangzhounet -> Some "hangzhounet"
  | `Idiazabalnet -> Some "idiazabalnet"
  | `Ithacanet -> Some "ithacanet"
  | `Sandbox -> None

let all : t list =
  [`Mainnet; `Granadanet; `Hangzhounet; `Idiazabalnet; `Ithacanet; `Sandbox]
