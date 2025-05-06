open BinNums
open BinPos
open BinPosDef
open Datatypes
open List0
open Monad0
open Nat0
open PeanoNat
open Bytestring
open CompM
open Cps
open Cps_show
open Ctx
open Identifiers
open Inline_letapp
open Map_util
open Rename
open Set_util
open Shrink_cps
open State

type r_map = var Cps.M.t

type 'st coq_InlineHeuristic = { update_funDef : (fundefs -> r_map -> 'st ->
                                                 'st * 'st);
                                 update_inFun : (var -> fun_tag -> var list
                                                -> exp -> r_map -> 'st -> 'st);
                                 update_App : (var -> fun_tag -> var list ->
                                              'st -> 'st * bool);
                                 update_letApp : (var -> fun_tag -> var list
                                                 -> 'st -> ('st * 'st) * bool) }

type fun_map = ((fun_tag * var list) * exp) Cps.M.t

type inline_state = bool * name_env

type 'x inlineM = (inline_state, 'x) compM'

val click : unit inlineM

val get_fresh_name : var -> var inlineM

val get_fresh_names : var list -> var list inlineM

val add_fundefs : fundefs -> fun_map -> fun_map

val split_fuel : nat -> nat * nat

val inline_exp :
  'a1 coq_InlineHeuristic -> nat -> nat -> exp -> var M.t -> ((fun_tag * var
  list) * exp) Cps.M.tree -> 'a1 -> exp inlineM

val restart_names : var -> exp -> comp_data -> comp_data * name_env

val inline_top' :
  'a1 coq_InlineHeuristic -> var -> nat -> 'a1 -> exp -> comp_data -> (exp
  error * comp_data) * bool

val inline_top :
  'a1 coq_InlineHeuristic -> var -> nat -> 'a1 -> exp -> comp_data -> exp
  error * comp_data

val inline_loop_aux :
  'a1 coq_InlineHeuristic -> var -> nat -> nat -> 'a1 -> exp -> comp_data ->
  exp error * comp_data

val inline_loop :
  'a1 coq_InlineHeuristic -> var -> nat -> 'a1 -> exp -> comp_data -> exp
  error * comp_data

val coq_CombineInlineHeuristic :
  (bool -> bool -> bool) -> 'a1 coq_InlineHeuristic -> 'a2
  coq_InlineHeuristic -> ('a1 * 'a2) coq_InlineHeuristic

val forall_fundefs : (exp -> bool) -> fundefs -> bool

val do_inline : var -> exp -> bool

val coq_InlineSmall : nat -> bool Cps.M.t coq_InlineHeuristic

val coq_InlinedUncurriedMarked : nat Cps.M.t coq_InlineHeuristic

val coq_InlineSmallOrUncurried :
  nat -> (bool Cps.M.t * nat Cps.M.t) coq_InlineHeuristic

val inline_uncurry :
  var -> nat -> nat -> exp -> comp_data -> exp error * comp_data

val inline_shrink_loop :
  var -> nat -> nat -> exp -> comp_data -> exp error * comp_data

val find_indirect_call : var -> exp -> bool Cps.M.t -> bool Cps.M.t

val coq_InineLifted : bool Cps.M.t coq_InlineHeuristic

val inline_lifted :
  var -> nat -> nat -> exp -> comp_data -> exp error * comp_data
