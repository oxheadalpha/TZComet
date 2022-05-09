open! Import

type t = [`Mainnet | `Hangzhounet | `Jakartanet | `Ithacanet | `Sandbox]

let to_string : t -> string = function
  | `Mainnet -> "Mainnet"
  | `Jakartanet -> "Jakartanet"
  | `Hangzhounet -> "Hangzhounet"
  | `Ithacanet -> "Ithacanet"
  | `Sandbox -> "Sandbox"

let better_call_dev_path : t -> string option = function
  | `Mainnet -> Some "mainnet"
  | `Jakartanet -> Some "jakartanet"
  | `Hangzhounet -> Some "hangzhounet"
  | `Ithacanet -> Some "ithacanet"
  | `Sandbox -> None

let all : t list = [`Mainnet; `Jakartanet; `Hangzhounet; `Ithacanet; `Sandbox]
