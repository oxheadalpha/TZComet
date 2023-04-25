open! Import

type t = [`Mainnet | `Mumbainet | `Nairobinet | `Ghostnet | `Sandbox]

let to_string : t -> string = function
  | `Mainnet -> "Mainnet"
  | `Nairobinet -> "Nairobinet"
  | `Mumbainet -> "Mumbainet"
  | `Ghostnet -> "Ghostnet"
  | `Sandbox -> "Sandbox"

let better_call_dev_path : t -> string option = function
  | `Mainnet -> Some "mainnet"
  | `Nairobinet -> Some "nairobinet"
  | `Mumbainet -> Some "mumbainet"
  | `Ghostnet -> Some "ghostnet"
  | `Sandbox -> None

let all : t list = [`Mainnet; `Nairobinet; `Mumbainet; `Ghostnet; `Sandbox]
