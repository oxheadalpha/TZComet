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

let bytes_summary ?(threshold = 25) ?(left = 10) ?(right = 10) bytes =
  match String.length bytes with
  | m when m < threshold -> bytes
  | m ->
      Fmt.str "%s…%s"
        (String.sub bytes ~pos:0 ~len:left)
        (String.sub bytes ~pos:(m - right) ~len:right)

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
                | Some x -> Lwt.fail (Found x) | None -> Lwt.return none ) )
          (function Found x -> Lwt.return_some x | e -> Lwt.fail e)

      let find x ~f =
        find_map x ~f:(fun x ->
            f x
            >>= function true -> Lwt.return_some x | false -> Lwt.return_none )
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
  let int f i : t = f (Int.to_string_hum ~delimiter:'_' i)
  let kpp f pp x : t = Fmt.kstr f "%a" pp x
  let ct s = Inline_code s
  let code_block s = Code_block s
  let list l = List l
  let ( % ) a b = List [a; b]
  let ( %% ) a b = List [a; t " "; b]
  let parens tt = list [t "("; tt; t ")"]

  let rec pp ppf =
    let open Fmt in
    function
    | Text s -> pf ppf "%s" s
    | Inline_code s -> pf ppf "`%s`" s
    | Code_block s -> pf ppf "@.```@.%s@.```@." s
    | List l -> List.iter l ~f:(pp ppf)
end

module Decorate_error = struct
  exception E of {message: Message.t; trace: exn list}

  let raise ?(trace = []) message = raise (E {message; trace})
  let reraise message ~f = Lwt.catch f (fun e -> raise message ~trace:[e])

  let () =
    Caml.Printexc.register_printer (function
      | E {message; _} -> Some (Fmt.str "Decorated-Error %a" Message.pp message)
      | _ -> None )
end

module System = struct
  type t = {dev_mode: bool Reactive.var; http_timeout: float Reactive.var}

  let create ?(dev_mode = false) () =
    {dev_mode= Reactive.var dev_mode; http_timeout= Reactive.var 5.}

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

  let now () = (new%js Js_of_ocaml.Js.date_now)##valueOf /. 1000.
  let time_zero = now ()
  let program_time () = now () -. time_zero
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
          Js._true ) ;
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
        Some x )

  let get (c : < storage: t ; .. > Context.t) = c#storage
  let available c = get c |> Option.is_some

  let read_file ctxt path =
    get ctxt
    |> Option.bind ~f:(fun sto ->
           Js.Opt.to_option (sto##getItem (Js.string path))
           |> Option.map ~f:Js.to_string )

  let write_file ctxt path ~content =
    get ctxt
    |> Option.iter ~f:(fun sto ->
           sto##setItem (Js.string path) (Js.string content) )

  let remove_file ctxt path =
    get ctxt |> Option.iter ~f:(fun sto -> sto##removeItem (Js.string path))
end

module Ezjsonm = struct
  include Ezjsonm

  module Stack_reimplementation = struct
    exception Escape of ((int * int) * (int * int)) * Tzcomet_jsonm.error

    let json_of_src src =
      let d = Tzcomet_jsonm.decoder src in
      let dec () =
        match Tzcomet_jsonm.decode d with
        | `Lexeme l -> l
        | `Error e -> raise (Escape (Tzcomet_jsonm.decoded_range d, e))
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
          | #Ezjsonm.value as v -> pp_value ppf v ) in
      let stack = ref [] in
      let fail_stack fmt =
        Fmt.kstr
          (fun m ->
            let (a, b), (c, d) = Tzcomet_jsonm.decoded_range d in
            Fmt.failwith "%s [%d,%d - %d,%d stack: %a]" m a b c d pp_stack
              !stack )
          fmt in
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

    let value_to_dst ?(minify = true) dst json =
      let encoder = Tzcomet_jsonm.encoder ~minify dst in
      let encode l = ignore (Tzcomet_jsonm.encode encoder (`Lexeme l)) in
      let rec go = function
        | [] -> ()
        | `Value ((`Bool _ | `Null | `Float _ | `String _) as v) :: more ->
            encode v ; go more
        | `Value (`O l) :: more ->
            encode `Os ;
            go (`Object l :: more)
        | `Value (`A l) :: more ->
            encode `As ;
            go (`Array l :: more)
        | `Object [] :: more ->
            encode `Oe ;
            go more
        | `Object ((k, v) :: o) :: more ->
            encode (`Name k) ;
            go (`Value v :: `Object o :: more)
        | `Array [] :: more ->
            encode `Ae ;
            go more
        | `Array (v :: aa) :: more -> go (`Value v :: `Array aa :: more) in
      go [`Value json] ;
      ignore (Tzcomet_jsonm.encode encoder `End)
  end

  open Stack_reimplementation

  let value_to_buffer ?minify buf json = value_to_dst ?minify (`Buffer buf) json

  let value_to_string ?minify json =
    let buf = Buffer.create 1024 in
    value_to_buffer ?minify buf json ;
    Buffer.contents buf

  let value_from_string s =
    match json_of_src (`String s) with
    | `JSON j -> j
    | `Error (((line, col), (eline, ecol)), err) ->
        dbgf "Error l-%d c-%d -- l-%d c-%d" line col eline ecol ;
        Decorate_error.raise
          Message.(
            (* Adapted from
               https://github.com/dbuenzli/jsonm/blob/master/src/jsonm.ml *)
            let control_char u = Fmt.kstr ct "U+%04X" u in
            let uchar u =
              let module Uchar = Caml.Uchar in
              if Uchar.to_int u <= 0x1F (* most control chars *) then
                control_char (Uchar.to_int u)
              else
                let b = Buffer.create 4 in
                Uutf.Buffer.add_utf_8 b u ;
                Fmt.kstr t "“%s” (=" (Buffer.contents b)
                %% control_char (Uchar.to_int u)
                % t ")" in
            let err_message =
              let pp = Fmt.kstr in
              let ppf = t in
              match err with
              | `Illegal_BOM ->
                  pp ppf
                    "Illegal initial Byte-Order-Mark (BOM) in character stream."
              | `Illegal_escape r -> (
                  pp ppf "Illegal escape:"
                  %%
                  match r with
                  | `Not_hex_uchar u -> uchar u %% t "is not a hex-digit"
                  | `Not_esc_uchar u ->
                      uchar u %% t "is not an escape character"
                  | `Lone_lo_surrogate p ->
                      control_char p %% t "lone low surrogate"
                  | `Lone_hi_surrogate p ->
                      control_char p %% t "lone high surrogate"
                  | `Not_lo_surrogate p ->
                      control_char p %% t "not a low surrogate" )
              | `Illegal_string_uchar u ->
                  t "Illegal character in JSON string:" %% uchar u
              | `Illegal_bytes bs ->
                  let l = String.length bs in
                  let (`Hex hx) = Hex.of_string bs in
                  t "Illegal bytes in character stream ("
                  % Fmt.kstr ct "0x%s" hx % t ", length:" %% int ct l % t ")"
              | `Illegal_number n -> t "Illegal number:" %% ct n
              | `Illegal_literal l -> t "Illegal literal:" %% ct l
              | `Unclosed r -> (
                  t "Unclosed"
                  %%
                  match r with
                  | `As -> t "array"
                  | `Os -> t "object"
                  | `String -> t "string"
                  | `Comment -> t "comment" )
              | `Expected r -> (
                  let value_sep = t "value separator" %% parens (ct ",") in
                  let tor = t "or" in
                  let array_end = t "end of array" %% parens (ct "]") in
                  let object_end = t "end of object" %% parens (ct "}") in
                  let field_name = t "field name" %% parens (ct "\"…\"") in
                  t "Expected "
                  %%
                  match r with
                  | `Comment -> t "JavaScript comment"
                  | `Value -> t "JSON value"
                  | `Name -> field_name
                  | `Name_sep -> t "field-name separator" %% parens (ct ":")
                  | `Aval true -> t "JSON-value" %% tor %% array_end
                  | `Aval false -> value_sep %% tor %% array_end
                  | `Omem true -> field_name %% tor %% object_end
                  | `Omem false -> value_sep %% tor %% object_end
                  | `Json -> t "JSON value"
                  | `Eoi -> t "end of input" ) in
            t "JSON Parsing: at line" %% int ct line %% t ", column"
            %% int ct col % t ":" %% err_message % t ".")
    | exception e -> Fmt.failwith "JSON Parising error: exception %a" Exn.pp e
end

module Blob = struct
  module Format = struct
    type t = [`Image | `Video] * string

    let gif = (`Image, "gif")
    let jpeg = (`Image, "jpeg")
    let png = (`Image, "png")
    let mp4 = (`Video, "mp4")

    let of_mime_exn = function
      | image when String.is_prefix image ~prefix:"image/" ->
          (`Image, String.chop_prefix_exn image ~prefix:"image/")
      | vid when String.is_prefix vid ~prefix:"video/" ->
          (`Video, String.chop_prefix_exn vid ~prefix:"video/")
      | other -> Fmt.failwith "Unknown MIME type: %S" other

    let to_mime = function
      | `Image, f -> "image/" ^ f
      | `Video, f -> "video/" ^ f
  end

  let guess_format s : Format.t option =
    (* https://stackoverflow.com/questions/55869/determine-file-type-of-an-image
       https://en.wikipedia.org/wiki/JPEG *)
    let open Format in
    let prefixes =
      [ ("\255\216\255", jpeg)
      ; ("\137\080\078\071", png)
      ; ("GIF", gif)
      ; ("\x00\x00\x00\x20ftypmp42", mp4) ] in
    List.find_map prefixes ~f:(fun (prefix, fmt) ->
        if String.is_prefix s ~prefix then Some fmt else None )
end
