open Import
module H5 = Tyxml_lwd.Html

(*
  type 'a live = 'a Lwd_seq.t Lwd.t
  type 'a elt = 'a Tyxml_lwd.node Tyxml_lwd.live
*)
(*
module Content = struct
  type 'a t =
    | Html : 'a H5.elt -> 'a t
    | Html_post_process : (('a H5.elt list -> 'b H5.elt) * 'a t) -> 'b t
    | List : 'a t list -> 'a t
end

open Content
 *)

type 'a t = 'a H5.elt (* list Lwd.t *)

(*
let txt (s : string) : _ t = Lwd.pure (Html H5.(txt (Lwd.pure s)))
 *)
let txt (s : string) : _ t = H5.(txt (Lwd.pure s))
let ( % ) a b : _ t = Lwd.map2 Lwd_seq.concat a b
let ( %% ) a b : _ t = a % txt " " % b

(*
let singletize  : 'a . (?a: 'a list -> 'b) -> 'a -> 'b = fun f ?a x -> f ?a [x]
 *)
let singletize f ?a x = f ?a [x]
let p ?a l = singletize H5.p ?a l

module H = struct let button ?a l = singletize H5.button ?a l end

let button ~action k =
  H.button
    ~a:[H5.a_onclick (Tyxml_lwd.Lwdom.attr (fun _ -> action () ; false))]
    k

let var_map v ~f : _ t = Lwd.bind (Lwd.get v) f

module Example = struct
  let e0 () = txt "Hello" %% txt "World"

  let e1 () =
    let button_calls = Lwd.var 0 in
    p (e0 ())
    % p (txt "This is greater than great.")
    % p
        (button
           ~action:(fun () -> Lwd.set button_calls (Lwd.peek button_calls + 1))
           (var_map button_calls ~f:(Fmt.kstr txt "Click %d")))
end
