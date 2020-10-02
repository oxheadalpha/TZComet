open Import
module H5 = Tyxml_lwd.Html

type 'a t = 'a H5.elt (* list Lwd.t *)

let t (s : string) : _ t = H5.(txt (Lwd.pure s))
let ( % ) a b : _ t = Lwd.map2 Lwd_seq.concat a b
let ( %% ) a b : _ t = a % t " " % b

(*
let singletize  : 'a . (?a: 'a list -> 'b) -> 'a -> 'b = fun f ?a x -> f ?a [x]
 *)
let singletize f ?a x = f ?a [x]
let p ?a l = singletize H5.p ?a l
let i ?a l = singletize H5.i ?a l
let b ?a l = singletize H5.b ?a l
let it s = i (t s)
let bt s = b (t s)

module H = struct let button ?a l = singletize H5.button ?a l end

let button ~action k =
  H.button
    ~a:[H5.a_onclick (Tyxml_lwd.Lwdom.attr (fun _ -> action () ; false))]
    k

let bind_var : 'a Lwd.var -> f:('a -> 'b t) -> 'b t =
 fun v ~f -> Lwd.bind (Lwd.get v) f

module Example = struct
  let e0 () = t "Hello" %% it "World"

  let e1 () =
    let button_calls = Lwd.var 0 in
    p (e0 ())
    % p (t "This is greater than great.")
    % p
        (button
           ~action:(fun () -> Lwd.set button_calls (Lwd.peek button_calls + 1))
           (bind_var button_calls ~f:(fun count ->
                H5.span
                  [ Fmt.kstr
                      (if Stdlib.( mod ) count 2 = 0 then it else bt)
                      "Click %d" count ])))
    % p
        (H5.span
           ~a:[H5.a_class (Lwd.pure ["alert"; "alert-primary"])]
           [ Lwd.map
               (fun x ->
                 Fmt.kstr (fun s -> H5.txt (Lwd.pure s)) "[%d cLicks]" x)
               (Lwd.get button_calls)
             |> Lwd.join ])
    % H5.(
        let open Tyxml_lwd.Lwdom in
        div
          ~a:[a_class (Lwd.pure ["dropdown"])]
          [ button
              ~a:
                [ a_class (Lwd.pure ["btn"; "btn-secondary"; "dropdown-toggle"])
                  (* ; a_type (Lwd.pure `Button) *)
                ; a_id (Lwd.pure "theIdOfTheDropDown")
                ; a_user_data "toggle" (Lwd.pure "dropdown")
                ; a_aria "haspopup" (Lwd.pure ["true"])
                ; a_aria "expanded" (Lwd.pure ["false"]) ]
              [txt (Lwd.pure "Example Dropdown")]
          ; div
              ~a:
                [ a_class (Lwd.pure ["dropdown-menu"])
                ; a_aria "labelledby" (Lwd.pure ["theIdOfTheDropDown"]) ]
              [ a
                  ~a:[a_class (Lwd.pure ["dropdown-item"])]
                  [txt (Lwd.pure "one")] ] ])
end
