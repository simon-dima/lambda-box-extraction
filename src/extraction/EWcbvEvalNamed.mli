open BasicAst
open Byte
open Datatypes
open EAst
open EPrimitive
open Kernames
open List0
open MCList
open MCString
open Nat0
open ReflectEq
open Bytestring
open Ssrbool

type value =
| Coq_vClos of ident * term * (ident * value) list
| Coq_vConstruct of inductive * nat * value list
| Coq_vRecClos of (ident * term) list * nat * (ident * value) list
| Coq_vPrim of value prim_val
| Coq_vLazy of term * (ident * value) list

val gen_fresh_aux : ident -> String.t list -> nat -> ident

val gen_fresh : String.t -> String.t list -> ident

val gen_many_fresh : String.t list -> name list -> ident list

val map_def_name : (name -> name) -> ('a1 -> 'a1) -> 'a1 def -> 'a1 def

val annotate : ident list -> term -> term

val annotate_env :
  ident list -> global_declarations -> (kername * global_decl) list
