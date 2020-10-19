include Base

let dbg fmt = Fmt.pf Fmt.stdout "@[tzcomet-debug: %a@]%!" fmt ()
let dbgf fmt = Fmt.(kstr (fun s -> dbg (const string s))) fmt

let rec oxfordize_list l ~map ~sep ~last_sep =
  match l with
  | [] -> []
  | [one] -> [map one]
  | [one; two] -> [map one; last_sep (); map two]
  | one :: more -> map one :: sep () :: oxfordize_list more ~map ~sep ~last_sep

let ellipsize_string ?(ellipsis = " …") s ~max_length =
  if String.length s <= max_length then s
  else String.prefix s max_length ^ ellipsis

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

module System = struct
  type t = {mutable dev_mode: bool}

  let get (state : < system: t ; .. > Context.t) = state#system

  let set_dev_mode c v =
    dbgf "system: setting dev_mode to %b" v ;
    (get c).dev_mode <- v

  let slow_step ctxt =
    if (get ctxt).dev_mode then Js_of_ocaml_lwt.Lwt_js.sleep 0.5
    else Lwt.return ()
end

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

    let fold t ~init ~f =
      let rec go acc = function
        | None -> acc
        | Some row -> (
          match get row with None -> acc | Some x -> go (f acc x) (next row) )
      in
      go init (first t)

    module Lwt = struct
      open Lwt.Infix

      let find_map (type a) t ~f =
        let exception Found of a in
        Lwt.catch
          (fun () ->
            fold t ~init:Lwt.return_none ~f:(fun pnone x ->
                pnone
                >>= fun none ->
                f x
                >>= function
                | Some x -> Lwt.fail (Found x) | None -> Lwt.return none))
          (function Found x -> Lwt.return_some x | e -> Lwt.fail e)

      let find x ~f =
        find_map x ~f:(fun x ->
            f x
            >>= function true -> Lwt.return_some x | false -> Lwt.return_none)
    end
  end

  module Sequence = Lwd_seq
end

module Decorate_error = struct
  module Message = struct
    type t =
      | Text of string
      | Inline_code of string
      | Code_block of string
      | List of t list

    let t s = Text s
    let ct s = Inline_code s
    let code_block s = Code_block s
    let list l = List l
    let ( % ) a b = List [a; b]
    let ( %% ) a b = List [a; t " "; b]
  end

  exception E of {message: Message.t; trace: exn list}

  let raise ?(trace = []) message = raise (E {message; trace})
  let reraise message ~f = Lwt.catch f (fun e -> raise message ~trace:[e])
end
