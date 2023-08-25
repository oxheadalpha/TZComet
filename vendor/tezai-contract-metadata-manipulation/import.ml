include Base

module Message = struct
  type t =
    | Text of string
    | Inline_code of string
    | Code_block of string
    | List of t list

  let text s = Text s
  let int f i : t = f (Int.to_string_hum ~delimiter:'_' i)
  let kpp f pp x : t = Fmt.kstr f "%a" pp x
  let inline_code s = Inline_code s
  let code_block s = Code_block s
  let list l = List l
  let ( % ) a b = List [ a; b ]
  let ( %% ) a b = List [ a; text " "; b ]
  let parens tt = list [ text "("; tt; text ")" ]

  let rec pp ppf =
    let open Fmt in
    function
    | Text s -> pf ppf "%s" s
    | Inline_code s -> pf ppf "`%s`" s
    | Code_block s -> pf ppf "@.```@.%s@.```@." s
    | List l -> List.iter l ~f:(pp ppf)
end

module Decorate_error = struct
  exception E of { message : Message.t; trace : exn list }

  let raise ?(trace = []) message = raise (E { message; trace })
  let reraise message ~f = Lwt.catch f (fun e -> raise message ~trace:[ e ])

  let () =
    Caml.Printexc.register_printer (function
      | E { message; _ } ->
          Some (Fmt.str "Decorated-Error %a" Message.pp message)
      | _ -> None)
end
