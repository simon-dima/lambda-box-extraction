open BinInt
open Byte
open Byte0

type byte = Integers.Byte.int

type bytes = byte list

(** val byte_of_compcert_byte : byte -> Byte.byte **)

let byte_of_compcert_byte b =
  match of_nat (Z.to_nat (Integers.Byte.intval b)) with
  | Some b' -> b'
  | None -> Coq_x00
