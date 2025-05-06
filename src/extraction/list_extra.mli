open Datatypes
open List0
open Nat0

val those_aux : 'a1 list option -> 'a1 option list -> 'a1 list option

val those : 'a1 option list -> 'a1 list option

val mapi_aux : (nat * 'a2 list) -> (nat -> 'a1 -> 'a2) -> 'a1 list -> 'a2 list

val mapi : (nat -> 'a1 -> 'a2) -> 'a1 list -> 'a2 list
