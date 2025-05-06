open BinNums
open BinPos
open Frame
open Prototype
open Rewriting
open Cps
open Cps_proto_univ

let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val gensyms : var -> 'a1 list -> var * var list **)

let rec gensyms x = function
| [] -> (x, [])
| _ :: xs0 ->
  let (x', xs') = gensyms (Pos.add x Coq_xH) xs0 in (x', (x :: xs'))

(** val run_rewriter' :
    exp_univ -> bool -> exp_univ coq_Metric -> (exp_univ, 'a1) coq_Delayed ->
    (exp_univ, 'a2) coq_Preserves_R -> (exp_univ, 'a3) coq_Preserves_S_dn ->
    (exp_univ, 'a3) coq_Preserves_S_up -> (exp_univ, 'a1, 'a2, 'a3) rewriter
    -> exp_univ univD -> (exp_univ, 'a2) coq_Param -> (exp_univ, 'a3)
    coq_State -> (exp_univ, 'a3) result **)

let run_rewriter' root fueled _ h _ _ _ rw e r s =
  if fueled
  then Obj.magic rw (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
         (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
         (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))))))))))))))) root __ __ e
         (h.delay_id root e) __ __ r s __
  else Obj.magic rw __ root __ __ e (h.delay_id root e) __ __ r s __
