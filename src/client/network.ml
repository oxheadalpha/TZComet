open! Import

type t = [ `Mainnet | `Oxfordnet | `Nairobinet | `Ghostnet | `Sandbox ]

let to_string : t -> string = function
  | `Mainnet -> "Mainnet"
  | `Nairobinet -> "Nairobinet"
  | `Oxfordnet -> "Oxfordnet"
  | `Ghostnet -> "Ghostnet"
  | `Sandbox -> "Sandbox"

let better_call_dev_path : t -> string option = function
  | `Mainnet -> Some "mainnet"
  | `Nairobinet -> Some "nairobinet"
  | `Oxfordnet -> Some "oxfordnet"
  | `Ghostnet -> Some "ghostnet"
  | `Sandbox -> None

let all : t list = [ `Mainnet; `Nairobinet; `Oxfordnet; `Ghostnet; `Sandbox ]
