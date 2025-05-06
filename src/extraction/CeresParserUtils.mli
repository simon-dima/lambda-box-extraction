open Ascii
open BinNums
open CeresString
open String0

type loc = coq_N

val pretty_loc : loc -> string

type error =
| UnmatchedClose of loc
| UnmatchedOpen of loc
| UnknownEscape of loc * ascii
| UnterminatedString of loc
| EmptyInput
| InvalidChar of ascii * loc
| InvalidStringChar of ascii * loc

val pretty_error : error -> string

val is_atom_char : ascii -> bool
