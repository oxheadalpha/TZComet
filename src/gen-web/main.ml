open! Base

let base_bootstrap page_title =
  (* From there: https://getbootstrap.com/docs/4.5/getting-started/introduction/ *)
  String.concat ~sep:""
    [
      {html|
<!doctype html>
<html lang="en">
  <head>
    <!-- Required meta tags -->
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">

    <!-- Bootstrap CSS -->
    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css" integrity="sha384-JcKb8q3iqJ61gNV9KGb8thSsNjpSL0n8PARn9HuZOnIxN0hoP+VmmDGMN5t9UJ0Z" crossorigin="anonymous">

    <title>|html};
      page_title;
      {html|</title>
  </head>
  <body>
    <div id="attach-ui">
     <h2>Loading TZComet …</h2>
     <img src="loading.gif"/><br/><br/>
     See also <a href="https://github.com/tqtezos/TZComet">https://github.com/tqtezos/TZComet</a>
    </div>
    <!-- Optional JavaScript -->
    <!-- jQuery first, then Popper.js, then Bootstrap JS -->
    <script src="https://code.jquery.com/jquery-3.5.1.slim.min.js" integrity="sha384-DfXdz2htPH0lsSSs5nCTpuj/zy4C+OGpamoFVy38MVBnE+IbbVYUew+OrCXaRkfj" crossorigin="anonymous"></script>
    <script src="https://cdn.jsdelivr.net/npm/popper.js@1.16.1/dist/umd/popper.min.js" integrity="sha384-9/reFTGAW83EW2RDu2S0VKaIzap3H66lZH81PoYlFhbGU+6BZp6G7niu735Sk7lN" crossorigin="anonymous"></script>
    <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/js/bootstrap.min.js" integrity="sha384-B4gt1jrGC7Jh4AgTPSdUtOBvfO8shuf57BaghqFfPlYxofvL8/KUEfYiJOMMV+rV" crossorigin="anonymous"></script>
    <script src="main-client.js"></script>
    <script>
|html};
      List.map [ "shown"; "show"; "hide"; "hidden" ] ~f:(fun evname ->
          Fmt.str
            {html|
$(document).on('%s.bs.collapse', function (e) {
   console.log('##%s ' + e.type + ' TRGT ' + e.target.id);
   var ev = new CustomEvent('collapse-%s', { detail: e.target.id });
   document.body.dispatchEvent(ev);
})|html}
            evname evname evname)
      |> String.concat ~sep:"\n";
      (*
$(document).on('hidden.bs.collapse', function (e) {
   console.log('##shown ' + e.type + ' TRGT ' + e.target.id);
   var ev = new CustomEvent('collapse-hidden', { detail: e.target.id } );
   document.body.dispatchEvent(ev);
})
 *)
      {html|
</script>
  </body>
</html>
|html};
    ]

let () =
  let usage () =
    Fmt.epr "usage: %s index '<page-title>'\n%!" Caml.Sys.argv.(0)
  in
  match Caml.Sys.argv.(1) with
  | "index" -> Fmt.pr "%s\n%!" (base_bootstrap Caml.Sys.argv.(2))
  | other ->
      Fmt.epr "Unknown command: %S!\n%!" other;
      usage ();
      Caml.exit 2
  | exception _ ->
      Fmt.epr "Missing command\n%!";
      usage ();
      Caml.exit 2
