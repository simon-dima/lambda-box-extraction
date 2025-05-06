open Byte
open CeresS
open CeresString
open List0
open Bytestring

val byte_to_string : byte -> String.t

val dstring_of_sexp : ('a1 -> Tree.t) -> 'a1 sexp_ -> Tree.t

val string_of_sexp_ : ('a1 -> String.t) -> 'a1 sexp_ -> Tree.t

val string_of_atom : atom -> String.t

val string_of_sexp : atom sexp_ -> String.t
