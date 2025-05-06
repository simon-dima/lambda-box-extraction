open BinNums
open String0

type 'a sexp_ =
| Atom_ of 'a
| List of 'a sexp_ list

type atom =
| Num of coq_Z
| Str of string
| Raw of string
