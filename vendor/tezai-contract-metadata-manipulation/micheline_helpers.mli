(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 TQ Tezos <contact@tqtezos.com>                         *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Contract-storage-parsing helper functions for the implementation of TZIP-16. *)

val normalize_combs :
  primitive:string -> Tezai_michelson.Untyped.t -> Tezai_michelson.Untyped.t
(** Attempt to do what normalization RPC should do for right combs. [~primitive]
    should be ["Pair"] for values, and ["pair"] for types (sequences are not
    implemented). *)

val find_metadata_big_maps :
  storage_node:Tezai_michelson.Untyped.t ->
  type_node:Tezai_michelson.Untyped.t ->
  Z.t list
(** Assuming that [storage_node] is the storage expression of a contract has
    type [type_node], find the identifier of metadata-big-map according to the
    TZIP-16 specification. *)

val build_off_chain_view_contract :
  Tezai_contract_metadata.Metadata_contents.View.Implementation
  .Michelson_storage
  .t ->
  contract_balance:Z.t ->
  contract_address:string ->
  contract_storage_type:Tezai_michelson.Untyped.t ->
  contract_parameter_type:Tezai_michelson.Untyped.t ->
  view_parameters:Tezai_michelson.Untyped.t ->
  contract_storage:Tezai_michelson.Untyped.t ->
  [ `Contract of Tezai_michelson.Untyped.t ]
  * [ `Input of Tezai_michelson.Untyped.t ]
  * [ `Storage of Tezai_michelson.Untyped.t ]
(** Build a contract for the [".../run_script"] RPC of the node. *)
