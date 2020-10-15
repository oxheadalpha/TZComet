include Base

let dbg fmt = Fmt.pf Fmt.stdout "@[tzcomet-debug: %a@]%!" fmt ()
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

module Context = struct type 'a t = 'a constraint 'a = < .. > end

module Reactive = struct
  include Lwd

  let map x ~f = map f x
  let bind x ~f = bind x f
  let ( ** ) = pair
  let split_var v = (get v, set v)
  let bind_var : 'a var -> f:('a -> 'b t) -> 'b t = fun v ~f -> bind ~f (get v)

  module Bidirectrional = struct
    type 'a t = {lwd: 'a Lwd.t; set: 'a -> unit}

    let make lwd set = {lwd; set}
    let of_var v = make (Lwd.get v) (Lwd.set v)
    let get v = v.lwd
    let set v x = v.set x
  end

  module Table = struct
    include Lwd_table

    let append = append'

    let concat_map ~map table =
      Lwd.join (Lwd_table.map_reduce map Lwd_seq.lwd_monoid table)
  end
  module Sequence = Lwd_seq
end
