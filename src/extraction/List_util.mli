open BinNat
open BinNums
open BinPos
open Datatypes

val fromN : coq_N -> nat -> coq_N list

val nthN : 'a1 list -> coq_N -> 'a1 option

val max_list : positive list -> positive -> positive
