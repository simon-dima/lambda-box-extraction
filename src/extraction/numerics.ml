open BinInt
open BinNums
open Bits
open Coqlib0
open Datatypes
open Floats
open Integers
open Zbits
open Zpower

module Wasm_int =
 struct
  module Int32 =
   struct
    (** val wordsize : nat **)

    let wordsize =
      Wordsize_32.wordsize

    (** val modulus : coq_Z **)

    let modulus =
      two_power_nat wordsize

    (** val half_modulus : coq_Z **)

    let half_modulus =
      Z.div modulus (Zpos (Coq_xO Coq_xH))

    type int = coq_Z
      (* singleton inductive, whose constructor was mkint *)

    (** val intval : int -> coq_Z **)

    let intval i =
      i

    (** val coq_Z_mod_modulus : coq_Z -> coq_Z **)

    let coq_Z_mod_modulus = function
    | Z0 -> Z0
    | Zpos p -> coq_P_mod_two_p p wordsize
    | Zneg p ->
      let r = coq_P_mod_two_p p wordsize in
      if zeq r Z0 then Z0 else Z.sub modulus r

    (** val unsigned : int -> coq_Z **)

    let unsigned n =
      n

    (** val signed : int -> coq_Z **)

    let signed n =
      let x = unsigned n in if zlt x half_modulus then x else Z.sub x modulus

    (** val repr : coq_Z -> int **)

    let repr =
      coq_Z_mod_modulus
   end

  module Int64 =
   struct
    (** val wordsize : nat **)

    let wordsize =
      Wordsize_64.wordsize

    (** val modulus : coq_Z **)

    let modulus =
      two_power_nat wordsize

    (** val half_modulus : coq_Z **)

    let half_modulus =
      Z.div modulus (Zpos (Coq_xO Coq_xH))

    type int = coq_Z
      (* singleton inductive, whose constructor was mkint *)

    (** val intval : int -> coq_Z **)

    let intval i =
      i

    (** val coq_Z_mod_modulus : coq_Z -> coq_Z **)

    let coq_Z_mod_modulus = function
    | Z0 -> Z0
    | Zpos p -> coq_P_mod_two_p p wordsize
    | Zneg p ->
      let r = coq_P_mod_two_p p wordsize in
      if zeq r Z0 then Z0 else Z.sub modulus r

    (** val unsigned : int -> coq_Z **)

    let unsigned n =
      n

    (** val signed : int -> coq_Z **)

    let signed n =
      let x = unsigned n in if zlt x half_modulus then x else Z.sub x modulus

    (** val repr : coq_Z -> int **)

    let repr =
      coq_Z_mod_modulus
   end
 end

module Wasm_float =
 struct
  module FloatSize32 =
   struct
    (** val to_bits : float32 -> Int.int **)

    let to_bits f =
      Int.repr (bits_of_b32 f)
   end

  module FloatSize64 =
   struct
    (** val to_bits : float -> Int64.int **)

    let to_bits f =
      Int64.repr (bits_of_b64 f)
   end
 end
