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
    let convert {lwd; set} f t = {lwd= map lwd ~f; set= (fun x -> set (t x))}
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

    let iter_rows t ~f =
      let rec go = function
        | None -> ()
        | Some s ->
            let prepare_next = next s in
            f s ; go prepare_next in
      go (first t)

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

module Decorate_error = struct
  exception E of {message: Message.t; trace: exn list}

  let raise ?(trace = []) message = raise (E {message; trace})
  let reraise message ~f = Lwt.catch f (fun e -> raise message ~trace:[e])
end

module System = struct
  type t = {dev_mode: bool Reactive.var; http_timeout: float Reactive.var}

  let create ?(dev_mode = false) () =
    {dev_mode= Reactive.var dev_mode; http_timeout= Reactive.var 3.}

  let get (state : < system: t ; .. > Context.t) = state#system

  let set_dev_mode c v =
    dbgf "system: setting dev_mode to %b" v ;
    Reactive.set (get c).dev_mode v

  let dev_mode c = Reactive.get (get c).dev_mode

  let dev_mode_bidirectional state =
    (get state).dev_mode |> Reactive.Bidirectrional.of_var

  let if_dev c f = if Reactive.peek (get c).dev_mode then f () else ()
  let set_http_timeout c v = Reactive.set (get c).http_timeout v
  let http_timeout c = Reactive.get (get c).http_timeout
  let http_timeout_peek c = Reactive.peek (get c).http_timeout

  let http_timeout_bidirectional c =
    Reactive.Bidirectrional.of_var (get c).http_timeout

  let slow_step ctxt =
    if Reactive.peek (get ctxt).dev_mode then Js_of_ocaml_lwt.Lwt_js.sleep 0.5
    else Lwt.return ()

  let with_timeout ctxt ~f ~raise =
    let open Lwt.Infix in
    let timeout = http_timeout_peek ctxt in
    Lwt.pick
      [f (); (Js_of_ocaml_lwt.Lwt_js.sleep timeout >>= fun () -> raise timeout)]
end
