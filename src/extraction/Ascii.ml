open BinNat
open BinNums
open Bool
open Byte
open Datatypes

type ascii =
| Ascii of bool * bool * bool * bool * bool * bool * bool * bool

(** val zero : ascii **)

let zero =
  Ascii (false, false, false, false, false, false, false, false)

(** val one : ascii **)

let one =
  Ascii (true, false, false, false, false, false, false, false)

(** val shift : bool -> ascii -> ascii **)

let shift c = function
| Ascii (a1, a2, a3, a4, a5, a6, a7, _) ->
  Ascii (c, a1, a2, a3, a4, a5, a6, a7)

(** val eqb : ascii -> ascii -> bool **)

let eqb a b =
  let Ascii (a0, a1, a2, a3, a4, a5, a6, a7) = a in
  let Ascii (b0, b1, b2, b3, b4, b5, b6, b7) = b in
  if if if if if if if eqb a0 b0 then eqb a1 b1 else false
                 then eqb a2 b2
                 else false
              then eqb a3 b3
              else false
           then eqb a4 b4
           else false
        then eqb a5 b5
        else false
     then eqb a6 b6
     else false
  then eqb a7 b7
  else false

(** val ascii_of_pos : positive -> ascii **)

let ascii_of_pos =
  let rec loop n p =
    match n with
    | O -> zero
    | S n' ->
      (match p with
       | Coq_xI p' -> shift true (loop n' p')
       | Coq_xO p' -> shift false (loop n' p')
       | Coq_xH -> one)
  in loop (S (S (S (S (S (S (S (S O))))))))

(** val ascii_of_N : coq_N -> ascii **)

let ascii_of_N = function
| N0 -> zero
| Npos p -> ascii_of_pos p

(** val ascii_of_nat : nat -> ascii **)

let ascii_of_nat a =
  ascii_of_N (N.of_nat a)

(** val coq_N_of_digits : bool list -> coq_N **)

let rec coq_N_of_digits = function
| [] -> N0
| b :: l' ->
  N.add (if b then Npos Coq_xH else N0)
    (N.mul (Npos (Coq_xO Coq_xH)) (coq_N_of_digits l'))

(** val coq_N_of_ascii : ascii -> coq_N **)

let coq_N_of_ascii = function
| Ascii (a0, a1, a2, a3, a4, a5, a6, a7) ->
  coq_N_of_digits
    (a0 :: (a1 :: (a2 :: (a3 :: (a4 :: (a5 :: (a6 :: (a7 :: []))))))))

(** val nat_of_ascii : ascii -> nat **)

let nat_of_ascii a =
  N.to_nat (coq_N_of_ascii a)

(** val ascii_of_byte : byte -> ascii **)

let ascii_of_byte b =
  let (b0, p) = to_bits b in
  let (b1, p0) = p in
  let (b2, p1) = p0 in
  let (b3, p2) = p1 in
  let (b4, p3) = p2 in
  let (b5, p4) = p3 in
  let (b6, b7) = p4 in Ascii (b0, b1, b2, b3, b4, b5, b6, b7)

(** val byte_of_ascii : ascii -> byte **)

let byte_of_ascii = function
| Ascii (b0, b1, b2, b3, b4, b5, b6, b7) ->
  of_bits (b0, (b1, (b2, (b3, (b4, (b5, (b6, b7)))))))
