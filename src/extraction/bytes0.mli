open BinInt
open Byte
open Byte0

type byte = Integers.Byte.int

type bytes = byte list

val byte_of_compcert_byte : byte -> Byte.byte
