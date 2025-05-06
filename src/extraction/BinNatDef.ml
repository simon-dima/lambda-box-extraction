open BinNums
open BinPos
open Datatypes

module N =
 struct
  (** val of_nat : nat -> coq_N **)

  let of_nat = function
  | O -> N0
  | S n' -> Npos (Pos.of_succ_nat n')
 end
