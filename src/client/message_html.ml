open Import
open Meta_html

let render _ m =
  let module M = Message in
  let rec msg = function
    | M.Text s -> t s
    | M.Inline_code c -> ct c
    | M.Code_block b -> pre (ct b)
    | M.List l -> List.fold ~init:(empty ()) ~f:(fun a b -> a % msg b) l
  in
  msg m
