open! Import

type t = [`Mainnet | `Edonet | `Florencenet | `Granadanet | `Sandbox]

let to_string : t -> string = function
  | `Mainnet -> "Mainnet"
  | `Granadanet -> "Granadanet"
  | `Edonet -> "Edonet"
  | `Florencenet -> "Florencenet"
  | `Sandbox -> "Sandbox"

let better_call_dev_path : t -> string option = function
  | `Mainnet -> Some "mainnet"
  | `Granadanet -> Some "granadanet"
  | `Edonet -> Some "edo2net"
  | `Florencenet -> Some "florencenet"
  | `Sandbox -> None

let all : t list = [`Mainnet; `Granadanet; `Edonet; `Florencenet; `Sandbox]
