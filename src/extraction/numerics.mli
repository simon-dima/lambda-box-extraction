open BinInt
open BinNums
open Bits
open Coqlib0
open Datatypes
open Floats
open Integers
open Zbits
open Zpower

module Wasm_int :
 sig
  module Int32 :
   sig
    val wordsize : nat

    val modulus : coq_Z

    val half_modulus : coq_Z

    type int = coq_Z
      (* singleton inductive, whose constructor was mkint *)

    val intval : int -> coq_Z

    val coq_Z_mod_modulus : coq_Z -> coq_Z

    val unsigned : int -> coq_Z

    val signed : int -> coq_Z

    val repr : coq_Z -> int
   end

  module Int64 :
   sig
    val wordsize : nat

    val modulus : coq_Z

    val half_modulus : coq_Z

    type int = coq_Z
      (* singleton inductive, whose constructor was mkint *)

    val intval : int -> coq_Z

    val coq_Z_mod_modulus : coq_Z -> coq_Z

    val unsigned : int -> coq_Z

    val signed : int -> coq_Z

    val repr : coq_Z -> int
   end
 end

module Wasm_float :
 sig
  module FloatSize32 :
   sig
    val to_bits : float32 -> Int.int
   end

  module FloatSize64 :
   sig
    val to_bits : float -> Int64.int
   end
 end
