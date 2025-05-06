open Ascii
open CeresS
open CeresString
open List0
open String0

val dstring_of_sexp : ('a1 -> DString.t) -> 'a1 sexp_ -> DString.t

val string_of_sexp_ : ('a1 -> string) -> 'a1 sexp_ -> string

val string_of_atom : atom -> string

val string_of_sexp : atom sexp_ -> string
