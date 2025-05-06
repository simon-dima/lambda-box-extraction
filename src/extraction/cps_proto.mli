open AstCommon
open BinNums
open Datatypes
open Frame
open List0
open Nat0
open Cps
open Cps_proto_univ

type 'a coq_Sized = 'a -> nat

val size : 'a1 coq_Sized -> 'a1 -> nat

val coq_Sized_pos : positive coq_Sized

val coq_Sized_N : coq_N coq_Sized

val coq_Sized_primitive : primitive coq_Sized

val size_list : ('a1 -> nat) -> 'a1 list -> nat

val size_prod : ('a1 -> nat) -> ('a2 -> nat) -> ('a1 * 'a2) -> nat

val coq_Size_list : 'a1 coq_Sized -> 'a1 list coq_Sized

val coq_Size_prod : 'a1 coq_Sized -> 'a2 coq_Sized -> ('a1 * 'a2) coq_Sized

val size_exp : exp -> nat

val size_fundefs : fundefs -> nat

val coq_Sized_exp : exp coq_Sized

val coq_Sized_fundefs : fundefs coq_Sized

val univ_size : exp_univ -> exp_univ univD -> nat
