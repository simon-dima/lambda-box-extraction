open BinNums
open Byte
open Datatypes
open List0
open Nat0

(** val byte_of_7_bits : bool list -> byte **)

let byte_of_7_bits = function
| [] -> Coq_x00
| b1 :: l ->
  (match l with
   | [] -> Coq_x00
   | b2 :: l0 ->
     (match l0 with
      | [] -> Coq_x00
      | b3 :: l1 ->
        (match l1 with
         | [] -> Coq_x00
         | b4 :: l2 ->
           (match l2 with
            | [] -> Coq_x00
            | b5 :: l3 ->
              (match l3 with
               | [] -> Coq_x00
               | b6 :: l4 ->
                 (match l4 with
                  | [] -> Coq_x00
                  | b7 :: l5 ->
                    (match l5 with
                     | [] ->
                       of_bits (b7, (b6, (b5, (b4, (b3, (b2, (b1, false)))))))
                     | _ :: _ -> Coq_x00)))))))

(** val rebalance :
    byte list -> bool list -> bool -> byte list * bool list **)

let rebalance bytes_produced bits_produced the_bit =
  if eqb (length bits_produced) (S (S (S (S (S (S O))))))
  then (((byte_of_7_bits (the_bit :: bits_produced)) :: bytes_produced), [])
  else (bytes_produced, (the_bit :: bits_produced))

(** val binary_of_aux2 : byte list -> bool list -> positive -> byte list **)

let rec binary_of_aux2 acc1 acc2 = function
| Coq_xI n' ->
  let (acc1', acc2') = rebalance acc1 acc2 true in
  binary_of_aux2 acc1' acc2' n'
| Coq_xO n' ->
  let (acc1', acc2') = rebalance acc1 acc2 false in
  binary_of_aux2 acc1' acc2' n'
| Coq_xH ->
  let (acc1', acc2') = rebalance acc1 acc2 true in
  let acc2'' =
    app (repeat false (sub (S (S (S (S (S (S O)))))) (length acc2))) acc2'
  in
  (byte_of_7_bits acc2'') :: acc1'

(** val incr_mod : nat -> nat -> nat **)

let incr_mod len pad =
  if eqb (S len) pad then O else S len

(** val bits_of_pos_pad : bool list -> nat -> nat -> positive -> bool list **)

let rec bits_of_pos_pad acc len pad = function
| Coq_xI n' -> bits_of_pos_pad (true :: acc) (incr_mod len pad) pad n'
| Coq_xO n' -> bits_of_pos_pad (false :: acc) (incr_mod len pad) pad n'
| Coq_xH -> app (repeat false (sub (sub pad (S O)) len)) (true :: acc)

(** val complement_of_one_two_aux : nat -> bool list -> bool list **)

let rec complement_of_one_two_aux zeros = function
| [] -> repeat false zeros
| b :: bs' ->
  if b
  then app (repeat false zeros) (true :: (map negb bs'))
  else complement_of_one_two_aux (S zeros) bs'

(** val complement_of_one_two : bool list -> bool list **)

let complement_of_one_two bs =
  rev (complement_of_one_two_aux O (rev bs))

(** val bytes_of_bits : bool list -> byte list **)

let rec bytes_of_bits = function
| [] -> []
| b1 :: l ->
  (match l with
   | [] -> []
   | b2 :: l0 ->
     (match l0 with
      | [] -> []
      | b3 :: l1 ->
        (match l1 with
         | [] -> []
         | b4 :: l2 ->
           (match l2 with
            | [] -> []
            | b5 :: l3 ->
              (match l3 with
               | [] -> []
               | b6 :: l4 ->
                 (match l4 with
                  | [] -> []
                  | b7 :: bs' ->
                    (of_bits (b7, (b6, (b5, (b4, (b3, (b2, (b1, false)))))))) :: 
                      (bytes_of_bits bs')))))))

(** val make_msb_one : byte -> byte **)

let make_msb_one b =
  let (b1, p) = to_bits b in
  let (b2, p0) = p in
  let (b3, p1) = p0 in
  let (b4, p2) = p1 in
  let (b5, p3) = p2 in
  let (b6, p4) = p3 in
  let (b7, _) = p4 in of_bits (b1, (b2, (b3, (b4, (b5, (b6, (b7, true)))))))

(** val make_msb_of_non_first_byte_one : byte list -> byte list **)

let make_msb_of_non_first_byte_one = function
| [] -> []
| b :: bs' -> b :: (map make_msb_one bs')

(** val encode_unsigned_aux : coq_N -> byte list **)

let encode_unsigned_aux = function
| N0 -> Coq_x00 :: []
| Npos n' -> make_msb_of_non_first_byte_one (binary_of_aux2 [] [] n')

(** val encode_unsigned : coq_N -> byte list **)

let encode_unsigned n =
  rev (encode_unsigned_aux n)

(** val encode_signed_aux : coq_Z -> byte list **)

let encode_signed_aux = function
| Z0 -> Coq_x00 :: []
| Zpos n' -> make_msb_of_non_first_byte_one (binary_of_aux2 [] [] n')
| Zneg n' ->
  make_msb_of_non_first_byte_one
    (bytes_of_bits
      (complement_of_one_two
        (bits_of_pos_pad [] O (S (S (S (S (S (S (S O))))))) n')))

(** val encode_signed : coq_Z -> byte list **)

let encode_signed z =
  rev (encode_signed_aux z)
