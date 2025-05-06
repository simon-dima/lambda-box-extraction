open BinNums
open BinPos
open Datatypes
open List0
open List_util
open Cps
open Set_util

type coq_FVSet = PS.t

val fundefs_names : fundefs -> coq_FVSet

val add_list : coq_FVSet -> coq_FVSet -> PS.elt list -> coq_FVSet

val exp_fv_aux : exp -> coq_FVSet -> coq_FVSet -> coq_FVSet

val fundefs_fv_aux :
  fundefs -> coq_FVSet -> coq_FVSet -> coq_FVSet * coq_FVSet

val exp_fv : exp -> coq_FVSet

val fundefs_fv : fundefs -> coq_FVSet

val max_var : exp -> positive -> positive

val max_var_fundefs : fundefs -> positive -> positive

val eq_var : positive -> positive -> bool

val occurs_in_vars : var -> var list -> bool

val occurs_in_arms' :
  (var -> exp -> bool) -> var -> (ctor_tag * exp) list -> bool

val occurs_in_exp : var -> exp -> bool

val occurs_in_fundefs : var -> fundefs -> bool
