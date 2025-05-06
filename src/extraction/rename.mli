open BinNums
open List0
open Maps
open Cps
open Maps_util

type subst = var M.t

val apply_r : subst -> positive -> var

val apply_r_list : subst -> positive list -> var list

val all_fun_name : fundefs -> var list

val rename_all_ns : subst -> exp -> exp

val rename_all_fun_ns : subst -> fundefs -> fundefs
