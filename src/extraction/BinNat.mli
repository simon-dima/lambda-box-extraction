open BinNums
open BinPos
open Datatypes

module N :
 sig
  val succ_double : coq_N -> coq_N

  val double : coq_N -> coq_N

  val succ : coq_N -> coq_N

  val succ_pos : coq_N -> positive

  val add : coq_N -> coq_N -> coq_N

  val sub : coq_N -> coq_N -> coq_N

  val mul : coq_N -> coq_N -> coq_N

  val compare : coq_N -> coq_N -> comparison

  val eqb : coq_N -> coq_N -> bool

  val leb : coq_N -> coq_N -> bool

  val max : coq_N -> coq_N -> coq_N

  val pos_div_eucl : positive -> coq_N -> coq_N * coq_N

  val div_eucl : coq_N -> coq_N -> coq_N * coq_N

  val coq_lor : coq_N -> coq_N -> coq_N

  val coq_land : coq_N -> coq_N -> coq_N

  val ldiff : coq_N -> coq_N -> coq_N

  val coq_lxor : coq_N -> coq_N -> coq_N

  val testbit : coq_N -> coq_N -> bool

  val to_nat : coq_N -> nat

  val of_nat : nat -> coq_N

  val eq_dec : coq_N -> coq_N -> bool
 end
