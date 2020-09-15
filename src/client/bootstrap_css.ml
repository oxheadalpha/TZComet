let uri =
  "https://stackpath.bootstrapcdn.com/bootstrap/3.4.1/css/bootstrap.min.css"

let ensure () =
  let (_ : unit) =
    Fmt.kstr Js_of_ocaml.Js.Unsafe.eval_string
      {js|
// Create new link Element 
var link = document.createElement('link');  
link.rel = 'stylesheet';  
link.type = 'text/css'; 
link.href = '%s';  
// Get HTML head element to append  
// link element to it  
document.getElementsByTagName('HEAD')[0].appendChild(link);  
|js}
      uri in
  ()
