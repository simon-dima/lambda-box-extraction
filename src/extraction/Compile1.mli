open BasicAst
open Byte
open Datatypes
open EAst
open EGlobalEnv
open EPrimitive
open EProgram
open Kernames
open List0
open MCList
open Malfunction
open Nat0
open Specif
open Bytestring
open Utils_array

val coq_Mapply_ : (t * t list) -> t

val coq_Mlambda_ : (Ident.t list * t) -> t

val blocks_until : nat -> nat list -> nat

val nonblocks_until : nat -> nat list -> nat

val coq_Mcase : ((nat list * t) * (Ident.t list * t) list) -> t

val lookup_record_projs :
  global_declarations -> inductive -> ident list option

val lookup_constructor_args :
  global_declarations -> inductive -> nat list option

val coq_Mapply_u : t -> t -> t

val num_of_nat : nat -> t

val compile_array : t list -> t -> t

val is_wf_rec_body : t -> bool

val force_lambda : t -> t

val compile : global_declarations -> term -> t

val compile_constant_decl : global_declarations -> constant_body -> t option

val compile_env : (kername * global_decl) list -> (String.t * t option) list

val compile_program : eprogram -> program
