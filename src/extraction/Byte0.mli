open BinNums
open Bool
open Byte
open Datatypes

val eqb : byte -> byte -> bool

val to_nat : byte -> nat

val of_nat : nat -> byte option

val to_N : byte -> coq_N
