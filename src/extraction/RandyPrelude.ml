open Byte
open Datatypes
open Nat0
open PeanoNat
open Bytestring

(** val digit_to_string : nat -> String.t **)

let digit_to_string = function
| O -> String.String (Coq_x30, String.EmptyString)
| S n0 ->
  (match n0 with
   | O -> String.String (Coq_x31, String.EmptyString)
   | S n1 ->
     (match n1 with
      | O -> String.String (Coq_x32, String.EmptyString)
      | S n2 ->
        (match n2 with
         | O -> String.String (Coq_x33, String.EmptyString)
         | S n3 ->
           (match n3 with
            | O -> String.String (Coq_x34, String.EmptyString)
            | S n4 ->
              (match n4 with
               | O -> String.String (Coq_x35, String.EmptyString)
               | S n5 ->
                 (match n5 with
                  | O -> String.String (Coq_x36, String.EmptyString)
                  | S n6 ->
                    (match n6 with
                     | O -> String.String (Coq_x37, String.EmptyString)
                     | S n7 ->
                       (match n7 with
                        | O -> String.String (Coq_x38, String.EmptyString)
                        | S _ -> String.String (Coq_x39, String.EmptyString)))))))))

(** val nat_to_string : nat -> String.t **)

let rec nat_to_string x =
  if Nat.ltb x (S (S (S (S (S (S (S (S (S (S O))))))))))
  then digit_to_string x
  else let m = Nat.div x (S (S (S (S (S (S (S (S (S (S O)))))))))) in
       String.append (nat_to_string m)
         (digit_to_string
           (sub x (mul (S (S (S (S (S (S (S (S (S (S O)))))))))) m)))

(** val list_to_zero : nat -> nat list **)

let rec list_to_zero = function
| O -> []
| S n0 -> n0 :: (list_to_zero n0)
