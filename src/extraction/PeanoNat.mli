open Datatypes
open Decimal

module Nat :
 sig
  val pred : nat -> nat

  val add : nat -> nat -> nat

  val mul : nat -> nat -> nat

  val sub : nat -> nat -> nat

  val eqb : nat -> nat -> bool

  val leb : nat -> nat -> bool

  val ltb : nat -> nat -> bool

  val compare : nat -> nat -> comparison

  val max : nat -> nat -> nat

  val even : nat -> bool

  val odd : nat -> bool

  val pow : nat -> nat -> nat

  val to_little_uint : nat -> uint -> uint

  val to_uint : nat -> uint

  val divmod : nat -> nat -> nat -> nat -> nat * nat

  val div : nat -> nat -> nat

  val modulo : nat -> nat -> nat

  val div2 : nat -> nat

  val eq_dec : nat -> nat -> bool

  val b2n : bool -> nat
 end
