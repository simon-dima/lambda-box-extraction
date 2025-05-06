open Datatypes
open Decimal

module Nat =
 struct
  (** val pred : nat -> nat **)

  let pred n = match n with
  | O -> n
  | S u -> u

  (** val add : nat -> nat -> nat **)

  let rec add n m =
    match n with
    | O -> m
    | S p -> S (add p m)

  (** val mul : nat -> nat -> nat **)

  let rec mul n m =
    match n with
    | O -> O
    | S p -> add m (mul p m)

  (** val sub : nat -> nat -> nat **)

  let rec sub n m =
    match n with
    | O -> n
    | S k -> (match m with
              | O -> n
              | S l -> sub k l)

  (** val eqb : nat -> nat -> bool **)

  let rec eqb n m =
    match n with
    | O -> (match m with
            | O -> true
            | S _ -> false)
    | S n' -> (match m with
               | O -> false
               | S m' -> eqb n' m')

  (** val leb : nat -> nat -> bool **)

  let rec leb n m =
    match n with
    | O -> true
    | S n' -> (match m with
               | O -> false
               | S m' -> leb n' m')

  (** val ltb : nat -> nat -> bool **)

  let ltb n m =
    leb (S n) m

  (** val compare : nat -> nat -> comparison **)

  let rec compare n m =
    match n with
    | O -> (match m with
            | O -> Eq
            | S _ -> Lt)
    | S n' -> (match m with
               | O -> Gt
               | S m' -> compare n' m')

  (** val max : nat -> nat -> nat **)

  let rec max n m =
    match n with
    | O -> m
    | S n' -> (match m with
               | O -> n
               | S m' -> S (max n' m'))

  (** val even : nat -> bool **)

  let rec even = function
  | O -> true
  | S n0 -> (match n0 with
             | O -> false
             | S n' -> even n')

  (** val odd : nat -> bool **)

  let odd n =
    negb (even n)

  (** val pow : nat -> nat -> nat **)

  let rec pow n = function
  | O -> S O
  | S m0 -> mul n (pow n m0)

  (** val to_little_uint : nat -> uint -> uint **)

  let rec to_little_uint n acc =
    match n with
    | O -> acc
    | S n0 -> to_little_uint n0 (Little.succ acc)

  (** val to_uint : nat -> uint **)

  let to_uint n =
    rev (to_little_uint n (D0 Nil))

  (** val divmod : nat -> nat -> nat -> nat -> nat * nat **)

  let rec divmod x y q u =
    match x with
    | O -> (q, u)
    | S x' ->
      (match u with
       | O -> divmod x' y (S q) y
       | S u' -> divmod x' y q u')

  (** val div : nat -> nat -> nat **)

  let div x y = match y with
  | O -> y
  | S y' -> fst (divmod x y' O y')

  (** val modulo : nat -> nat -> nat **)

  let modulo x = function
  | O -> x
  | S y' -> sub y' (snd (divmod x y' O y'))

  (** val div2 : nat -> nat **)

  let rec div2 = function
  | O -> O
  | S n0 -> (match n0 with
             | O -> O
             | S n' -> S (div2 n'))

  (** val eq_dec : nat -> nat -> bool **)

  let rec eq_dec n m =
    match n with
    | O -> (match m with
            | O -> true
            | S _ -> false)
    | S n0 -> (match m with
               | O -> false
               | S n1 -> eq_dec n0 n1)

  (** val b2n : bool -> nat **)

  let b2n = function
  | true -> S O
  | false -> O
 end
