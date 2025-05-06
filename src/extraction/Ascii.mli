open BinNat
open BinNums
open Bool
open Byte
open Datatypes

type ascii =
| Ascii of bool * bool * bool * bool * bool * bool * bool * bool

val zero : ascii

val one : ascii

val shift : bool -> ascii -> ascii

val eqb : ascii -> ascii -> bool

val ascii_of_pos : positive -> ascii

val ascii_of_N : coq_N -> ascii

val ascii_of_nat : nat -> ascii

val coq_N_of_digits : bool list -> coq_N

val coq_N_of_ascii : ascii -> coq_N

val nat_of_ascii : ascii -> nat

val ascii_of_byte : byte -> ascii

val byte_of_ascii : ascii -> byte
