open! Import

type t = [`Mainnet | `Delphinet | `Edonet | `Florencenet | `Sandbox]

let to_string : t -> string = function
  | `Mainnet -> "Mainnet"
  | `Delphinet -> "Delphinet"
  | `Edonet -> "Edonet"
  | `Florencenet -> "Florencenet"
  | `Sandbox -> "Sandbox"

let better_call_dev_path : t -> string option = function
  | `Mainnet -> Some "mainnet"
  | `Delphinet -> Some "delphinet"
  | `Edonet -> Some "edo2net"
  | `Florencenet -> Some "florencenet"
  | `Sandbox -> None

let all : t list = [`Mainnet; `Delphinet; `Edonet; `Florencenet; `Sandbox]
