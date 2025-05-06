open Ascii
open Byte
open Datatypes
open List0

type string =
| EmptyString
| String of ascii * string

val append : string -> string -> string

val length : string -> nat

val string_of_list_ascii : ascii list -> string

val string_of_list_byte : byte list -> string
