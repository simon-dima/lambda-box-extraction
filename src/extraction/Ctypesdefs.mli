open BinNums
open Ctypes

val tvoid : coq_type

val tptr : coq_type -> coq_type

val tattr : attr -> coq_type -> coq_type

val talignas : coq_N -> coq_type -> coq_type
