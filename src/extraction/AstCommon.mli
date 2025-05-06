open BasicAst
open BinNat
open BinNums
open Byte
open Classes1
open Datatypes
open EAst
open FloatOps
open Kernames
open List0
open MCString
open RandyPrelude
open ReflectEq
open Show
open SpecFloat
open Specif
open Bytestring
open Classes2

type __ = Obj.t

val print_name : name -> String.t

val print_inductive : inductive -> String.t

val inductive_dec : inductive -> inductive -> bool

val coq_NEq : coq_N coq_Eq

type coq_Cnstr = { coq_CnstrNm : String.t; coq_CnstrArity : nat }

type ityp = { itypNm : String.t; itypCnstrs : coq_Cnstr list }

type itypPack = ityp list

type 'trm envClass =
| Coq_ecTrm of 'trm
| Coq_ecTyp of nat * itypPack

val ecAx : 'a1 envClass

type 'trm environ = (kername * 'trm envClass) list

val cnstr_Cnstr : constructor_body -> coq_Cnstr

val ibody_ityp : one_inductive_body -> ityp

val ibodies_itypPack : one_inductive_body list -> itypPack

type 'trm coq_Program = { main : 'trm; env : 'trm environ }

val lookup : kername -> 'a1 environ -> 'a1 envClass option

val timePhase : String.t -> ('a1 -> 'a2) -> 'a1 -> 'a2

type prim_tag =
| Coq_primInt
| Coq_primFloat

type prim_value = __

type primitive = (prim_tag, prim_value) sigT

val string_of_specfloat : spec_float -> String.t

val string_of_float : Float64.t -> String.t

val string_of_prim : primitive -> String.t
