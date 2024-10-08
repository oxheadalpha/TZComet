open! Import

type t = {
  gateways : string list Reactive.var;
  current_index : int Reactive.var;
}

let default_gateways : string list =
  [
    "https://ipfs.io/ipfs/";
    "https://dweb.link/ipfs/" (*   ; "https://cloudflare-ipfs.com/ipfs/" *);
  ]

let create () =
  { gateways = Reactive.var default_gateways; current_index = Reactive.var 0 }

let get (ctxt : < ipfs_gateways : t ; .. > Context.t) = ctxt#ipfs_gateways
let gateways t = (get t).gateways
let current_index t = (get t).current_index

exception Bad_gateway_index of string

let get_nth the_list idx =
  let count = List.length the_list in
  let error_str =
    Printf.sprintf "Trying to use index %d on a list of gateways of size %d" idx
      count
  in
  let new_gw =
    try List.nth_exn the_list idx
    with Invalid_argument _ -> raise (Bad_gateway_index error_str)
  in
  new_gw

let current_gateway t =
  let the_list = Reactive.peek (get t).gateways in
  let idx = Reactive.peek (get t).current_index in
  get_nth the_list idx

let try_next ctxt =
  let ipfs = get ctxt in
  let old_gw = current_gateway ctxt in
  let the_list = Reactive.peek ipfs.gateways in
  let idx = Reactive.peek ipfs.current_index in
  let count = List.length the_list in
  let new_idx = if phys_equal idx (count - 1) then 0 else idx + 1 in
  Reactive.set (current_index ctxt) new_idx;
  let new_gw = get_nth the_list new_idx in
  dbgf "Ipfs_gateways.try_next - rotating IPFS gateways: %S ----> %S" old_gw
    new_gw;
  new_gw

let add ctxt gateway =
  Reactive.set (gateways ctxt) (gateway :: Reactive.peek (gateways ctxt))

let remove_gateway ctxt ~uri =
  let ipfs = get ctxt in
  let gws = Reactive.peek ipfs.gateways in
  let new_list = List.filter gws ~f:(fun u -> not (String.equal u uri)) in
  let new_len = List.length new_list in
  if phys_equal new_len 0 then false (* tried to remove them all *)
  else
    let prev_idx = Reactive.peek ipfs.current_index in
    if prev_idx >= new_len then Reactive.set (current_index ctxt) 0;
    Reactive.set (gateways ctxt) new_list;
    true
