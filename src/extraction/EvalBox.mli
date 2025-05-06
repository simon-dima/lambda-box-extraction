open AstCommon
open BinNums
open BinPos
open Byte
open CertiCoqPipeline
open Datatypes
open EAst
open List_util
open MCString
open Monad0
open PeanoNat
open Pipeline_utils
open Bytestring
open CompM
open Compile0
open Cps
open Cps_show
open Cps_util
open Eval
open ExceptionMonad
open Map_util
open Term0
open Toplevel0
open WcbvEval

val show_var : Cps_show.name_env -> positive -> String.t

val show_tag : fun_tag -> String.t

val show_tags : fun_tag -> fun_tag -> String.t

val bstep_f :
  prims -> ctor_env -> Cps_show.name_env -> env -> exp -> nat -> (env * exp,
  coq_val) sum coq_exception

val next_id : positive

val fuel : nat

val box_to_mut : program -> coq_Term coq_Program pipelineM

val box_to_anf : program -> coq_LambdaANF_FullTerm pipelineM

val eval_box : nat -> coq_Term coq_Program -> String.t pipelineM

val eval_anf : nat -> coq_LambdaANF_FullTerm -> String.t pipelineM

val eval : coq_Options -> bool -> program -> String.t error * String.t
