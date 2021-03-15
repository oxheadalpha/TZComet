open! Import

type t =
  [`Mainnet | `Delphinet | `Edonet | `Florence_NoBA | `Florence_BA | `Sandbox]

let to_string : t -> string = function
  | `Mainnet -> "Mainnet"
  | `Delphinet -> "Delphinet"
  | `Edonet -> "Edonet"
  | `Florence_NoBA -> "Florence_NoBA"
  | `Florence_BA -> "Florence_BA"
  | `Sandbox -> "Sandbox"

let better_call_dev_path : t -> string option = function
  | `Mainnet -> Some "mainnet"
  | `Delphinet -> Some "delphinet"
  | `Edonet -> Some "edo2net"
  | `Florence_NoBA -> None
  | `Florence_BA -> None
  | `Sandbox -> None

let all = [`Mainnet; `Delphinet; `Edonet; `Florence_NoBA; `Florence_BA; `Sandbox]
