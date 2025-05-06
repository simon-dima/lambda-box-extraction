open Ascii
open BinInt
open BinNums
open Bool
open Datatypes
open DecimalString
open Nat0
open PeanoNat
open String0

val compcomp : comparison -> comparison -> comparison

val compb : bool -> bool -> comparison

val eqb_ascii : ascii -> ascii -> bool

val ascii_compare : ascii -> ascii -> comparison

val leb_ascii : ascii -> ascii -> bool

val eqb_string : string -> string -> bool

val string_elem : ascii -> string -> bool

val _string_reverse : string -> string -> string

val string_reverse : string -> string

val comma_sep : string list -> string

val is_printable : ascii -> bool

val is_whitespace : ascii -> bool

val is_digit : ascii -> bool

val is_upper : ascii -> bool

val is_lower : ascii -> bool

val is_alphanum : ascii -> bool

val _units_digit : nat -> ascii

val _three_digit : nat -> string

val _escape_string : string -> string -> string

val escape_string : string -> string

val string_of_nat : nat -> string

val string_of_Z : coq_Z -> string

val string_of_N : coq_N -> string

module DString :
 sig
  type t = string -> string

  val of_string : string -> t

  val of_ascii : ascii -> t

  val app_string : t -> string -> string
 end
