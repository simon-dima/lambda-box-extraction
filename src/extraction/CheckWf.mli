open Byte
open Datatypes
open EAst
open EGlobalEnv
open EPrimitive
open EWellformed
open ExAst
open Kernames
open List0
open MCString
open Nat0
open PeanoNat
open Primitive
open ReflectEq
open ResultMonad
open Bytestring
open Monad_utils

val metacoq_erasure_eflags : coq_EEnvFlags

val agda_typed_eflags : coq_EEnvFlags

val coq_assert : bool -> (unit -> String.t) -> (unit, String.t) result

val assert_some : 'a1 option -> (unit -> String.t) -> (unit, String.t) result

val result_forall :
  ('a1 -> (unit, String.t) result) -> 'a1 list -> (unit, String.t) result

val wf_fix_gen_ :
  (nat -> term -> (unit, String.t) result) -> nat -> term def list -> nat ->
  (unit, String.t) result

val bool_of_result : ('a1, 'a2) result -> bool

val has_prim_ :
  coq_EPrimitiveFlags -> term prim_val -> (unit, String.t) result

val wellformed :
  coq_EEnvFlags -> global_declarations -> nat -> term -> (unit, String.t)
  result

val wf_projections : EAst.one_inductive_body -> (unit, String.t) result

val wf_inductive : EAst.one_inductive_body -> (unit, String.t) result

val wf_minductive :
  coq_EEnvFlags -> EAst.mutual_inductive_body -> (unit, String.t) result

val wf_global_decl :
  coq_EEnvFlags -> global_declarations -> EAst.global_decl -> (unit,
  String.t) result

val check_fresh_global :
  kername -> global_declarations -> (unit, String.t) result

val check_wf_glob :
  coq_EEnvFlags -> global_declarations -> (unit, String.t) result

val check_wf_program : coq_EEnvFlags -> program -> (unit, String.t) result

module CheckWfExAst :
 sig
  val check_wf_typed_program :
    coq_EEnvFlags -> global_env -> (unit, String.t) result
 end
