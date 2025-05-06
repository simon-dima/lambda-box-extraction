open BinNums
open BinPos
open Cps

val eq_var : positive -> positive -> bool

val occurs_in_vars : var -> var list -> bool
