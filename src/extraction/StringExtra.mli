open BinNat
open BinNums
open BinPos
open Byte
open Byte0
open Datatypes
open Bytestring

val coq_Nlog2up_nat : coq_N -> nat

val string_of_N : coq_N -> String.t

val string_of_nat : nat -> String.t

val replace_char : byte -> byte -> String.t -> String.t

val remove_char : byte -> String.t -> String.t

val starts_with_cont :
  byte -> String.t -> (String.t -> 'a1) -> String.t -> 'a1 option

val replace : String.t -> String.t -> String.t -> String.t

val is_letter : byte -> bool

val char_to_upper : byte -> byte

val capitalize : String.t -> String.t
