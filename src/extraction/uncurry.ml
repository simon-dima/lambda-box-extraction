open BinNums
open BinPos
open Cps

(** val eq_var : positive -> positive -> bool **)

let eq_var =
  Pos.eqb

(** val occurs_in_vars : var -> var list -> bool **)

let rec occurs_in_vars k = function
| [] -> false
| x :: xs1 -> (||) (eq_var k x) (occurs_in_vars k xs1)
