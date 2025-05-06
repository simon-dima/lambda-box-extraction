open BinNums
open Datatypes
open Nat0

val addn_rec : nat -> nat -> nat

val addn : nat -> nat -> nat

module NatTrec :
 sig
  val add : nat -> nat -> nat

  val double : nat -> nat
 end

val nat_of_pos : positive -> nat

val nat_of_bin : coq_N -> nat

val pos_of_nat : nat -> nat -> positive

val bin_of_nat : nat -> coq_N
