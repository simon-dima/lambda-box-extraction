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

(** val bytes_of_int : nat -> coq_Z -> Byte.int list **)

let rec bytes_of_int n x =
  match n with
  | O -> []
  | S m ->
    (Byte.repr x) :: (bytes_of_int m
                       (Z.div x (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                         (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))))))

(** val rev_if_be : Byte.int list -> Byte.int list **)

let rev_if_be l =
  if big_endian then rev l else l

(** val encode_int : nat -> coq_Z -> Byte.int list **)

let encode_int sz x =
  rev_if_be (bytes_of_int sz x)
