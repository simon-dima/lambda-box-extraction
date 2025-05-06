open BinNat
open BinNums
open BinPos
open Datatypes

(** val fromN : coq_N -> nat -> coq_N list **)

let rec fromN n = function
| O -> []
| S m' -> n :: (fromN (N.succ n) m')

(** val nthN : 'a1 list -> coq_N -> 'a1 option **)

let rec nthN al n =
  match al with
  | [] -> None
  | a :: al' ->
    (match n with
     | N0 -> Some a
     | Npos _ -> nthN al' (N.sub n (Npos Coq_xH)))

(** val max_list : positive list -> positive -> positive **)

let rec max_list ls acc =
  match ls with
  | [] -> acc
  | x :: xs -> max_list xs (Pos.max x acc)
