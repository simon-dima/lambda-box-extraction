open AstCommon
open BinNums
open BinPos
open Byte
open Datatypes
open Kernames
open LambdaBoxLocal_to_LambdaANF
open List0
open Monad0
open PeanoNat
open Pipeline_utils
open Bytestring
open CompM
open Cps
open Dead_param_elim
open Eval
open Hoisting
open Identifiers
open Inline
open Lambda_lifting
open Shrink_cps
open State
open Toplevel
open Uncurry_proto

type __ = Obj.t

type prim_env = (((kername * String.t) * bool) * nat) M.t

type coq_LambdaANFenv =
  ((((((prims * prim_env) * ctor_env) * ctor_tag) * ind_tag) * name_env) * fun_env) * env

type coq_LambdaANFterm = exp

type coq_LambdaANF_FullTerm = coq_LambdaANFenv * coq_LambdaANFterm

val default_ctor_tag : positive

val default_ind_tag : positive

val clo_tag : positive

val clo_ind_tag : positive

val fun_fun_tag : positive

val kon_fun_tag : positive

val make_prim_env :
  ((((kername * String.t) * bool) * nat) * positive) list -> prim_env

val compile_LambdaANF_CPS :
  positive -> ((((kername * String.t) * bool) * nat) * positive) list ->
  (coq_LambdaBoxLocalTerm, coq_LambdaANF_FullTerm) coq_CertiCoqTrans

val compile_LambdaANF_ANF :
  positive -> ((((kername * String.t) * bool) * nat) * positive) list ->
  (coq_LambdaBoxLocalTerm, coq_LambdaANF_FullTerm) coq_CertiCoqTrans

type anf_options = { time : bool; cps : bool; do_lambda_lift : bool;
                     args : nat; no_push : nat; inl_wrappers : bool;
                     inl_known : bool; inl_before : bool; inl_after : 
                     bool; dpe : bool }

type 'a anf_state = comp_data -> 'a error * comp_data

type anf_trans = exp -> exp anf_state

val coq_MonadState : __ anf_state coq_Monad

val id_trans : anf_trans

val time_anf :
  anf_options -> String.t -> ('a1 -> 'a2 anf_state) -> 'a1 -> 'a2 anf_state

val anf_pipeline : positive -> anf_options -> exp -> exp anf_state

val run_anf_pipeline :
  positive -> anf_options -> coq_LambdaANF_FullTerm -> coq_LambdaANF_FullTerm
  error * String.t

val make_anf_options : coq_Options -> anf_options

val compile_LambdaANF :
  positive -> (coq_LambdaANF_FullTerm, coq_LambdaANF_FullTerm)
  coq_CertiCoqTrans

val compile_LambdaANF_debug :
  positive -> (coq_LambdaANF_FullTerm, coq_LambdaANF_FullTerm)
  coq_CertiCoqTrans
