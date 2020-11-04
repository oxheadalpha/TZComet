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

  let ( ** ) = pair
  let split_var v = (get v, set v)
  let bind_var : 'a var -> f:('a -> 'b t) -> 'b t = fun v ~f -> bind ~f (get v)

  module Bidirectional = struct
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
    (get state).dev_mode |> Reactive.Bidirectional.of_var

  let if_dev c f = if Reactive.peek (get c).dev_mode then f () else ()
  let set_http_timeout c v = Reactive.set (get c).http_timeout v
  let http_timeout c = Reactive.get (get c).http_timeout
  let http_timeout_peek c = Reactive.peek (get c).http_timeout

  let http_timeout_bidirectional c =
    Reactive.Bidirectional.of_var (get c).http_timeout

  let slow_step ctxt =
    if Reactive.peek (get ctxt).dev_mode then Js_of_ocaml_lwt.Lwt_js.sleep 0.5
    else Lwt.return ()

  let with_timeout ctxt ~f ~raise =
    let open Lwt.Infix in
    let timeout = http_timeout_peek ctxt in
    Lwt.pick
      [f (); (Js_of_ocaml_lwt.Lwt_js.sleep timeout >>= fun () -> raise timeout)]
end

module Browser_window = struct
  type width_state = [`Thin | `Wide]
  type t = {width: width_state option Reactive.var}

  open Js_of_ocaml

  let create ?(threshold = 992) () =
    let find_out () =
      match Js.Optdef.to_option Dom_html.window##.innerWidth with
      | Some s when s >= threshold -> Some `Wide
      | Some _ -> Some `Thin
      | None -> None in
    let width = Reactive.var (find_out ()) in
    Dom_html.window##.onresize :=
      Dom_html.handler (fun _ ->
          let current = Reactive.peek width in
          let new_one = find_out () in
          if Poly.(current <> new_one) then Reactive.set width new_one ;
          Js._true) ;
    {width}

  let get (c : < window: t ; .. > Context.t) = c#window
  let width c = (get c).width |> Reactive.get
end

module Local_storage : sig
  type t

  val create : unit -> t
  val get : < storage: t ; .. > Context.t -> t
  val available : < storage: t ; .. > Context.t -> bool
  val read_file : < storage: t ; .. > Context.t -> string -> string option

  val write_file :
    < storage: t ; .. > Context.t -> string -> content:string -> unit

  val remove_file : < storage: t ; .. > Context.t -> string -> unit
end = struct
  open Js_of_ocaml

  type t = Js_of_ocaml.Dom_html.storage Js_of_ocaml.Js.t option

  let create () : t =
    Js.Optdef.case
      Dom_html.window##.localStorage
      (fun () -> dbgf "Local_storage: nope" ; None)
      (fun x ->
        dbgf "Local_storage: YES length: %d" x##.length ;
        Some x)

  let get (c : < storage: t ; .. > Context.t) = c#storage
  let available c = get c |> Option.is_some

  let read_file ctxt path =
    get ctxt
    |> Option.bind ~f:(fun sto ->
           Js.Opt.to_option (sto##getItem (Js.string path))
           |> Option.map ~f:Js.to_string)

  let write_file ctxt path ~content =
    get ctxt
    |> Option.iter ~f:(fun sto ->
           sto##setItem (Js.string path) (Js.string content))

  let remove_file ctxt path =
    get ctxt |> Option.iter ~f:(fun sto -> sto##removeItem (Js.string path))
end

module Ezjsonm = struct
  include Ezjsonm

  module Stack_reimplementation = struct
    exception Escape of ((int * int) * (int * int)) * Jsonm.error

    module List = struct
      include List

      (* Tail-recursive List.map *)
      let map f l = rev (rev_map f l)
    end

    let json_of_src src =
      let d = Jsonm.decoder src in
      let dec () =
        match Jsonm.decode d with
        | `Lexeme l -> l
        | `Error e -> raise (Escape (Jsonm.decoded_range d, e))
        | `End | `Await -> assert false in
      let pp_value ppf v = Fmt.pf ppf "%s" (Ezjsonm.value_to_string v) in
      let module Stack_type = struct
        type t =
          [ `A of Ezjsonm.value List.t
          | `Bool of bool
          | `Float of float
          | `In_array of Ezjsonm.value list
          | `In_object of string option * (string * Ezjsonm.value) list
          | `Null
          | `O of (string * Ezjsonm.value) list
          | `String of string ]
      end in
      let pp_stack =
        let open Fmt in
        list ~sep:(any " :: ") (fun ppf -> function
          | `In_object (m, l) ->
              pf ppf "(in-obj %a %a)" (Dump.option string) m
                (list (pair ~sep:(any ":") string pp_value))
                l
          | `In_array l -> pf ppf "(in-array %a)" (list pp_value) l
          | #Ezjsonm.value as v -> pp_value ppf v) in
      let stack = ref [] in
      let fail_stack fmt =
        Fmt.kstr (fun m -> Fmt.failwith "%s [stack: %a]" m pp_stack !stack) fmt
      in
      let rec go () =
        let stack_value (v : [< Ezjsonm.value]) =
          match !stack with
          | `In_array l :: more -> stack := `In_array (v :: l) :: more
          | `In_object (Some n, l) :: more ->
              stack := `In_object (None, (n, v) :: l) :: more
          | [] -> stack := [(v :> Stack_type.t)]
          | other -> fail_stack "wrong stack" in
        let pop () =
          match !stack with
          | _ :: more -> stack := more
          | [] -> fail_stack "cannot remove element from stack" in
        ( match dec () with
        | `Os -> stack := `In_object (None, []) :: !stack
        | `Oe -> (
          match !stack with
          | `In_object (Some n, l) :: more -> fail_stack "name not none"
          | `In_object (None, l) :: more ->
              pop () ;
              stack_value (`O (List.rev l))
          | other ->
              fail_stack "wrong stack, expecting in-object to close object" )
        | `As -> stack := `In_array [] :: !stack
        | `Ae -> (
          match !stack with
          | `In_array l :: more ->
              pop () ;
              stack_value (`A (List.rev l))
          | _ -> fail_stack "array end not in array" )
        | `Name n -> (
          match !stack with
          | `In_object (None, l) :: more ->
              stack := `In_object (Some n, l) :: more
          | other ->
              fail_stack "wrong stack, expecting in-object for field-name" )
        | (`Bool _ | `Null | `Float _ | `String _) as v -> stack_value v ) ;
        match !stack with
        | `In_array _ :: _ | `In_object _ :: _ -> go ()
        | [(#Ezjsonm.value as one)] -> one
        | [] -> fail_stack "stack is empty"
        | _ :: _ :: _ -> go () in
      try `JSON (go ()) with Escape (r, e) -> `Error (r, e)
  end

  let value_from_string s =
    match Stack_reimplementation.json_of_src (`String s) with
    | `JSON j -> j
    | `Error (((a, b), (c, d)), err) ->
        Fmt.failwith "JSON Parising error: (%d, %d):(%d, %d): %a" a b c d
          Jsonm.pp_error err
    | exception e -> Fmt.failwith "JSON Parising error: exception %a" Exn.pp e
end
