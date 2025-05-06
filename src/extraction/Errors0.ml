open BinNums
open String0

type errcode =
| MSG of string
| CTX of positive
| POS of positive

type errmsg = errcode list

type 'a res =
| OK of 'a
| Error of errmsg

(** val bind : 'a1 res -> ('a1 -> 'a2 res) -> 'a2 res **)

let bind f g =
  match f with
  | OK x -> g x
  | Error msg -> Error msg
