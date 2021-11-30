open! Import

type t = [`Mainnet | `Hangzhounet | `Granadanet | `Sandbox]

let to_string : t -> string = function
  | `Mainnet -> "Mainnet"
  | `Granadanet -> "Granadanet"
  | `Hangzhounet -> "Hangzhounet"
  | `Sandbox -> "Sandbox"

let better_call_dev_path : t -> string option = function
  | `Mainnet -> Some "mainnet"
  | `Granadanet -> Some "granadanet"
  | `Hangzhounet -> Some "hangzhounet"
  | `Sandbox -> None

let all : t list = [`Mainnet; `Granadanet; `Hangzhounet; `Sandbox]
