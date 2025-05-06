open BinInt
open BinNums
open PrimInt63
open Uint0

(** val min_int : Uint63.t **)

let min_int =
  (Uint63.of_int (-4611686018427387904))

(** val to_Z : Uint63.t -> coq_Z **)

let to_Z i =
  if ltb i min_int then to_Z i else Z.opp (to_Z (opp i))
