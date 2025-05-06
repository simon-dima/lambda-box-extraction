open Archi
open BinInt
open BinNums
open Datatypes
open Integers
open List0
open Values0

type quantity =
| Q32
| Q64

type memval =
| Undef
| Byte of Byte.int
| Fragment of coq_val * quantity * nat

val bytes_of_int : nat -> coq_Z -> Byte.int list

val rev_if_be : Byte.int list -> Byte.int list

val encode_int : nat -> coq_Z -> Byte.int list
