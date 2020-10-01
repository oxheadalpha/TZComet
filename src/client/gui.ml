open Import

module State = struct
  module Page = struct
    type t = Welcome | Settings

    let to_string = function Welcome -> "Welcome" | Settings -> "Settings"
  end

  open Page

  type t = {page: Page.t Lwd.var}

  let create () = {page= Lwd.var Welcome}
end

let root_document () =
  let open Tyxml_lwd in
  let state = State.create () in
  Html.div
    [ Meta_html.Example.e1 ()
    ; Html.p
        [ Meta_html.Example.e0 (); Html.txt (Lwd.pure "; ")
        ; Html.span
            ~a:
              [ Html.a_onclick
                  (Lwdom.attr (fun _ ->
                       let open State.Page in
                       ( match Lwd.peek state.State.page with
                       | Welcome -> Lwd.set state.State.page Settings
                       | Settings -> Lwd.set state.State.page Welcome ) ;
                       true)) ]
            [Html.txt (Lwd.get state.State.page |> Lwd.map State.Page.to_string)]
        ] ]
