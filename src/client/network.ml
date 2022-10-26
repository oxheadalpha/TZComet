open! Import

type t = [`Mainnet | `Kathmandunet | `Limanet | `Ghostnet | `Sandbox]

let to_string : t -> string = function
  | `Mainnet -> "Mainnet"
  | `Limanet -> "Limanet"
  | `Kathmandunet -> "Kathmandunet"
  | `Ghostnet -> "Ghostnet"
  | `Sandbox -> "Sandbox"

let better_call_dev_path : t -> string option = function
  | `Mainnet -> Some "mainnet"
  | `Limanet -> Some "limanet"
  | `Kathmandunet -> Some "kathmandunet"
  | `Ghostnet -> Some "ghostnet"
  | `Sandbox -> None

let all : t list = [`Mainnet; `Limanet; `Kathmandunet; `Ghostnet; `Sandbox]
