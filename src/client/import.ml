include Base

let dbg fmt = Fmt.pf Fmt.stdout "@[comevitz-debug: %a@]%!" fmt ()
let dbgf fmt = Fmt.(kstr (fun s -> dbg (const string s))) fmt

let rec oxfordize_list l ~map ~sep ~last_sep =
  match l with
  | [] -> []
  | [one] -> [map one]
  | [one; two] -> [map one; last_sep (); map two]
  | one :: more -> map one :: sep () :: oxfordize_list more ~map ~sep ~last_sep

module Var = struct
  type 'a t =
    {name: string; signal: 'a React.S.t; set: ?step:React.step -> 'a -> unit}

  let create ?eq name v =
    let signal, set = React.S.create ?eq v in
    {name; signal; set}

  let set var v = var.set v
  let signal v = v.signal
  let value v = React.S.value v.signal

  let map ?name v ~f =
    let sgn = signal v in
    let new_signal = React.S.map f sgn in
    let name = Option.value name ~default:(v.name ^ "-m") in
    {name; signal= new_signal; set= (fun ?step:_ _ -> failwith "not setable")}

  let map_to_list v ~f =
    let sgn = signal v in
    React.S.map f sgn |> ReactiveData.RList.from_signal
end

module RD = struct
  include Js_of_ocaml_tyxml.Tyxml_js.Html

  module Reactive = struct
    include Js_of_ocaml_tyxml.Tyxml_js.R.Html

    let div_of_var v ~f = Var.map_to_list v ~f |> div
  end
end
