open BinNums
open Datatypes
open Nat0

(** val addn_rec : nat -> nat -> nat **)

let addn_rec =
  add

(** val addn : nat -> nat -> nat **)

let addn =
  addn_rec

module NatTrec =
 struct
  (** val add : nat -> nat -> nat **)

  let rec add m n =
    match m with
    | O -> n
    | S m' -> add m' (S n)

  (** val double : nat -> nat **)

  let double n = match n with
  | O -> O
  | S n' -> add n' (S n)
 end

(** val nat_of_pos : positive -> nat **)

let rec nat_of_pos = function
| Coq_xI p -> S (NatTrec.double (nat_of_pos p))
| Coq_xO p -> NatTrec.double (nat_of_pos p)
| Coq_xH -> S O

(** val nat_of_bin : coq_N -> nat **)

let nat_of_bin = function
| N0 -> O
| Npos p -> nat_of_pos p

(** val pos_of_nat : nat -> nat -> positive **)

let rec pos_of_nat n0 m0 =
  match n0 with
  | O -> Coq_xH
  | S n ->
    (match m0 with
     | O -> Coq_xI (pos_of_nat n n)
     | S n1 ->
       (match n1 with
        | O -> Coq_xO (pos_of_nat n n)
        | S m -> pos_of_nat n m))

(** val bin_of_nat : nat -> coq_N **)

let bin_of_nat = function
| O -> N0
| S n -> Npos (pos_of_nat n n)
