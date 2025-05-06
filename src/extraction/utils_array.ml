open BinInt
open Datatypes
open Uint0

(** val int_of_nat : nat -> Uint63.t **)

let int_of_nat n =
  of_Z (Z.of_nat n)
