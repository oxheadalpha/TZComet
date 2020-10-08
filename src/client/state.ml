open Import

module View = struct
  type t =
    | Welcome
    | Metadata_json_editor
    | Metadata_uri_editor
    | Michelson_bytes_parser
    | Metadata_explorer

  let to_string = function
    | Welcome -> "Welcome"
    | Metadata_json_editor -> "Metadata_json_editor"
    | Metadata_uri_editor -> "Metadata_uri_editor"
    | Michelson_bytes_parser -> "Michelson_bytes_parser"
    | Metadata_explorer -> "Metadata_explorer"

  let of_string = function
    | "Welcome" -> Welcome
    | "Metadata_json_editor" -> Metadata_json_editor
    | "Metadata_uri_editor" -> Metadata_uri_editor
    | "Michelson_bytes_parser" -> Michelson_bytes_parser
    | "Metadata_explorer" -> Metadata_explorer
    | other -> Fmt.failwith "View.of_string: %S" other
end

type t =
  { dev_mode: bool
  ; current_view: View.t Var.t
  ; explorer_address_input: string Var.t
  ; start_fetching_address: bool Var.t
  ; explorer_uri_input: string Var.t
  ; start_fetching_uri: bool Var.t }

let kt1_with_metadata = "KT1XRT495WncnqNmqKn4tkuRiDJzEiR4N2C9"

let init ~arguments () =
  let arg s = List.Assoc.find arguments ~equal:String.equal s in
  let is_true s =
    let ( = ) = Option.equal String.equal in
    arg s = Some "true" in
  let dev_mode = is_true "dev" in
  let fetch_uri = is_true "fetch_uri" in
  let fetch_address = is_true "fetch_address" in
  let initial_view =
    match arg "tab" with
    | Some s -> (
      try View.of_string s
      with e ->
        dbgf "Wrong view name: %a" Exn.pp e ;
        View.Welcome )
    | None -> View.Welcome in
  let initial_explorer_address =
    arg "explorer_address" |> Option.value ~default:kt1_with_metadata in
  let initial_explorer_uri =
    arg "explorer_uri"
    |> Option.value ~default:"https://example.com/my_contract/metadata.json"
  in
  let explorer_address_input =
    Var.create "explorer_address_input" initial_explorer_address in
  let explorer_uri_input =
    Var.create "explorer_uri_input" initial_explorer_uri in
  { current_view= Var.create "current-view" initial_view
  ; dev_mode
  ; explorer_address_input
  ; explorer_uri_input
  ; start_fetching_uri= Var.create "start_fetching_uri" fetch_uri
  ; start_fetching_address= Var.create "start_fetching_address" fetch_address }

let go_to_explorer state ?uri ?address () =
  Option.iter address ~f:(fun a ->
      Var.set state.explorer_address_input a ;
      Var.set state.start_fetching_address true) ;
  Option.iter uri ~f:(fun u ->
      Var.set state.explorer_uri_input u ;
      Var.set state.start_fetching_uri true) ;
  Var.set state.current_view View.Metadata_explorer

let should_start_fetching_address state =
  let should_i = Var.value state.start_fetching_address in
  Var.set state.start_fetching_address false ;
  should_i

let should_start_fetching_uri state =
  let should_i = Var.value state.start_fetching_uri in
  Var.set state.start_fetching_uri false ;
  should_i

let slow_step s =
  if s.dev_mode then Js_of_ocaml_lwt.Lwt_js.sleep 0.5 else Lwt.return ()
