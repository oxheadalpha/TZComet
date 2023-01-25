open! Import

type t = [`Mainnet | `Mumbainet | `Limanet | `Ghostnet | `Sandbox]

let to_string : t -> string = function
  | `Mainnet -> "Mainnet"
  | `Limanet -> "Limanet"
  | `Mumbainet -> "Mumbainet"
  | `Ghostnet -> "Ghostnet"
  | `Sandbox -> "Sandbox"

let better_call_dev_path : t -> string option = function
  | `Mainnet -> Some "mainnet"
  | `Limanet -> Some "limanet"
  | `Mumbainet -> Some "mumbainet"
  | `Ghostnet -> Some "ghostnet"
  | `Sandbox -> None

let all : t list = [`Mainnet; `Limanet; `Mumbainet; `Ghostnet; `Sandbox]
