open BinNums
open Byte
open Datatypes
open List0
open Nat0

val byte_of_7_bits : bool list -> byte

val rebalance : byte list -> bool list -> bool -> byte list * bool list

val binary_of_aux2 : byte list -> bool list -> positive -> byte list

val incr_mod : nat -> nat -> nat

val bits_of_pos_pad : bool list -> nat -> nat -> positive -> bool list

val complement_of_one_two_aux : nat -> bool list -> bool list

val complement_of_one_two : bool list -> bool list

val bytes_of_bits : bool list -> byte list

val make_msb_one : byte -> byte

val make_msb_of_non_first_byte_one : byte list -> byte list

val encode_unsigned_aux : coq_N -> byte list

val encode_unsigned : coq_N -> byte list

val encode_signed_aux : coq_Z -> byte list

val encode_signed : coq_Z -> byte list
