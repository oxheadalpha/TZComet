open! Import

type t = [`Mainnet | `Kathmandunet | `Jakartanet | `Ithacanet | `Sandbox]

let to_string : t -> string = function
  | `Mainnet -> "Mainnet"
  | `Jakartanet -> "Jakartanet"
  | `Kathmandunet -> "Kathmandunet"
  | `Ithacanet -> "Ithacanet"
  | `Sandbox -> "Sandbox"

let better_call_dev_path : t -> string option = function
  | `Mainnet -> Some "mainnet"
  | `Jakartanet -> Some "jakartanet"
  | `Kathmandunet -> Some "kathmandunet"
  | `Ithacanet -> Some "ithacanet"
  | `Sandbox -> None

let all : t list = [`Mainnet; `Jakartanet; `Kathmandunet; `Ithacanet; `Sandbox]
