open BinNums
open Bool
open Datatypes
open Uint0
open Bytestring

type __ = Obj.t

module Int63 = Uint0

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

module Ident =
 struct
  module Coq__1 = struct
   type t = String.t
  end
  include Coq__1

  module Map =
   struct
    type 'a t = Coq__1.t -> 'a
   end
 end

module Longident =
 struct
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
