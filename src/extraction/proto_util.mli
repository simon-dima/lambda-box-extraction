open BinNums
open BinPos
open Frame
open Prototype
open Rewriting
open Cps
open Cps_proto_univ

val gensyms : var -> 'a1 list -> var * var list

val run_rewriter' :
  exp_univ -> bool -> exp_univ coq_Metric -> (exp_univ, 'a1) coq_Delayed ->
  (exp_univ, 'a2) coq_Preserves_R -> (exp_univ, 'a3) coq_Preserves_S_dn ->
  (exp_univ, 'a3) coq_Preserves_S_up -> (exp_univ, 'a1, 'a2, 'a3) rewriter ->
  exp_univ univD -> (exp_univ, 'a2) coq_Param -> (exp_univ, 'a3) coq_State ->
  (exp_univ, 'a3) result
