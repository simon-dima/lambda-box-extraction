open BinNums
open Bool
open Datatypes
open Uint0
open Bytestring

type __ = Obj.t

module Int63 :
 sig
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
 end

type inttype =
| Int
| Int32
| Int64
| Bigint

type numtype =
| Coq_embed_inttype of inttype
| Float64

type numconst =
| Coq_numconst_Int of Uint63.t
| Coq_numconst_Bigint of coq_Z
| Coq_numconst_Float64 of Float64.t

type unary_num_op =
| Neg
| Not

type binary_arith_op =
| Add
| Sub
| Mul
| Div
| Mod

type binary_bitwise_op =
| And
| Or
| Xor
| Lsl
| Lsr
| Asr

type binary_comparison =
| Lt
| Gt
| Lte
| Gte
| Eq

type binary_num_op =
| Coq_embed_binary_arith_op of binary_arith_op
| Coq_embed_binary_bitwise_op of binary_bitwise_op
| Coq_embed_binary_comparison of binary_comparison

type vector_type =
| Array
| Bytevec

type case =
| Tag of Uint63.t
| Deftag
| Intrange of (Uint63.t * Uint63.t)

module Ident :
 sig
  module Coq__1 : sig
   type t = String.t
  end
  include module type of struct include Coq__1 end

  module Map :
   sig
    type 'a t = Coq__1.t -> 'a
   end
 end

module Longident :
 sig
  type t = String.t
 end

type t =
| Mvar of Ident.t
| Mlambda of (Ident.t list * t)
| Mapply of (t * t list)
| Mlet of (binding list * t)
| Mnum of numconst
| Mstring of String.t
| Mglobal of Longident.t
| Mswitch of (t * (case list * t) list)
| Mnumop1 of ((unary_num_op * numtype) * t)
| Mnumop2 of (((binary_num_op * numtype) * t) * t)
| Mconvert of ((numtype * numtype) * t)
| Mvecnew of ((vector_type * t) * t)
| Mvecget of ((vector_type * t) * t)
| Mvecset of (((vector_type * t) * t) * t)
| Mveclen of (vector_type * t)
| Mlazy of t
| Mforce of t
| Mblock of (Uint63.t * t list)
| Mfield of (Uint63.t * t)
and binding =
| Unnamed of t
| Named of (Ident.t * t)
| Recursive of (Ident.t * t) list

type program = (Ident.t * t option) list * t

type 'id prim_def =
| Global of 'id * 'id
| Primitive of String.t * nat
| Erased

type primitives = (String.t * String.t prim_def) list
