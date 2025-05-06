open BasicAst
open BinNums
open Byte
open Clight
open Datatypes
open Kernames
open LambdaANF_to_Clight
open LambdaANF_to_Clight_stack
open LambdaBoxLocal_to_LambdaANF
open List0
open Monad0
open Pipeline_utils
open Bytestring
open CompM
open Cps
open Cps_util
open ExceptionMonad
open Toplevel0

val argsIdent : positive

val allocIdent : positive

val nallocIdent : positive

val limitIdent : positive

val gcIdent : positive

val mainIdent : positive

val bodyIdent : positive

val threadInfIdent : positive

val tinfIdent : positive

val heapInfIdent : positive

val numArgsIdent : positive

val isptrIdent : positive

val caseIdent : positive

val resultIdent : positive

val stackframeTIdent : positive

val frameIdent : positive

val rootIdent : positive

val fpIdent : positive

val nextFld : positive

val prevFld : positive

type coq_Cprogram = (name_env * program) * program

val add_prim_names :
  ((((kername * String.t) * bool) * nat) * positive) list ->
  LambdaBoxLocal_to_LambdaANF.name_env -> LambdaBoxLocal_to_LambdaANF.name_env

val coq_Clight_trans :
  String.t -> ((((kername * String.t) * bool) * nat) * positive) list -> nat
  -> coq_LambdaANF_FullTerm -> coq_Cprogram error

val coq_Clight_trans_ANF :
  String.t -> ((((kername * String.t) * bool) * nat) * positive) list -> nat
  -> coq_LambdaANF_FullTerm -> coq_Cprogram error * String.t

val compile_Clight :
  ((((kername * String.t) * bool) * nat) * positive) list ->
  (coq_LambdaANF_FullTerm, coq_Cprogram) coq_CertiCoqTrans
