open BinInt
open BinNums
open Bool
open Datatypes
open Nat0
open PrimInt63

type __ = Obj.t

val size : nat

module Uint63NotationsInternalB :
 sig
 end

val digits : Uint63.t

val max_int : Uint63.t

val get_digit : Uint63.t -> Uint63.t -> bool

val set_digit : Uint63.t -> Uint63.t -> bool -> Uint63.t

val is_zero : Uint63.t -> bool

val is_even : Uint63.t -> bool

val bit : Uint63.t -> Uint63.t -> bool

val opp : Uint63.t -> Uint63.t

val oppcarry : Uint63.t -> Uint63.t

val succ : Uint63.t -> Uint63.t

val pred : Uint63.t -> Uint63.t

val addcarry : Uint63.t -> Uint63.t -> Uint63.t

val subcarry : Uint63.t -> Uint63.t -> Uint63.t

val addc_def : Uint63.t -> Uint63.t -> Uint63.t Uint63.carry

val addcarryc_def : Uint63.t -> Uint63.t -> Uint63.t Uint63.carry

val subc_def : Uint63.t -> Uint63.t -> Uint63.t Uint63.carry

val subcarryc_def : Uint63.t -> Uint63.t -> Uint63.t Uint63.carry

val diveucl_def : Uint63.t -> Uint63.t -> Uint63.t * Uint63.t

val addmuldiv_def : Uint63.t -> Uint63.t -> Uint63.t -> Uint63.t

module Uint63NotationsInternalC :
 sig
 end

val oppc : Uint63.t -> Uint63.t Uint63.carry

val succc : Uint63.t -> Uint63.t Uint63.carry

val predc : Uint63.t -> Uint63.t Uint63.carry

val compare_def : Uint63.t -> Uint63.t -> comparison

val to_Z_rec : nat -> Uint63.t -> coq_Z

val to_Z : Uint63.t -> coq_Z

val of_pos_rec : nat -> positive -> Uint63.t

val of_pos : positive -> Uint63.t

val of_Z : coq_Z -> Uint63.t

val wB : coq_Z

module Uint63NotationsInternalD :
 sig
 end

val sqrt_step :
  (Uint63.t -> Uint63.t -> Uint63.t) -> Uint63.t -> Uint63.t -> Uint63.t

val iter_sqrt :
  nat -> (Uint63.t -> Uint63.t -> Uint63.t) -> Uint63.t -> Uint63.t ->
  Uint63.t

val sqrt : Uint63.t -> Uint63.t

val high_bit : Uint63.t

val sqrt2_step :
  (Uint63.t -> Uint63.t -> Uint63.t -> Uint63.t) -> Uint63.t -> Uint63.t ->
  Uint63.t -> Uint63.t

val iter2_sqrt :
  nat -> (Uint63.t -> Uint63.t -> Uint63.t -> Uint63.t) -> Uint63.t ->
  Uint63.t -> Uint63.t -> Uint63.t

val sqrt2 : Uint63.t -> Uint63.t -> Uint63.t * Uint63.t Uint63.carry

val gcd_rec : nat -> Uint63.t -> Uint63.t -> Uint63.t

val gcd : Uint63.t -> Uint63.t -> Uint63.t

val eqs : Uint63.t -> Uint63.t -> bool

val cast : Uint63.t -> Uint63.t -> (__ -> __ -> __) option

val eqo : Uint63.t -> Uint63.t -> __ option

val eqbP : Uint63.t -> Uint63.t -> reflect

val ltbP : Uint63.t -> Uint63.t -> reflect

val lebP : Uint63.t -> Uint63.t -> reflect

val b2i : bool -> Uint63.t

module Uint63Notations :
 sig
 end
