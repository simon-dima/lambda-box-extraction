open BinInt
open BinNums
open Bool
open Datatypes
open Nat0
open PrimInt63

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val size : nat **)

let size =
  S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    O))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

module Uint63NotationsInternalB =
 struct
 end

(** val digits : Uint63.t **)

let digits =
  (Uint63.of_int (63))

(** val max_int : Uint63.t **)

let max_int =
  (Uint63.of_int (-1))

(** val get_digit : Uint63.t -> Uint63.t -> bool **)

let get_digit x p =
  ltb (Uint63.of_int (0)) (coq_land x (coq_lsl (Uint63.of_int (1)) p))

(** val set_digit : Uint63.t -> Uint63.t -> bool -> Uint63.t **)

let set_digit x p b =
  if if leb (Uint63.of_int (0)) p then ltb p digits else false
  then if b
       then coq_lor x (coq_lsl (Uint63.of_int (1)) p)
       else coq_land x (coq_lxor max_int (coq_lsl (Uint63.of_int (1)) p))
  else x

(** val is_zero : Uint63.t -> bool **)

let is_zero i =
  eqb i (Uint63.of_int (0))

(** val is_even : Uint63.t -> bool **)

let is_even i =
  is_zero (coq_land i (Uint63.of_int (1)))

(** val bit : Uint63.t -> Uint63.t -> bool **)

let bit i n =
  negb (is_zero (coq_lsl (coq_lsr i n) (sub digits (Uint63.of_int (1)))))

(** val opp : Uint63.t -> Uint63.t **)

let opp i =
  sub (Uint63.of_int (0)) i

(** val oppcarry : Uint63.t -> Uint63.t **)

let oppcarry i =
  sub max_int i

(** val succ : Uint63.t -> Uint63.t **)

let succ i =
  add i (Uint63.of_int (1))

(** val pred : Uint63.t -> Uint63.t **)

let pred i =
  sub i (Uint63.of_int (1))

(** val addcarry : Uint63.t -> Uint63.t -> Uint63.t **)

let addcarry i j =
  add (add i j) (Uint63.of_int (1))

(** val subcarry : Uint63.t -> Uint63.t -> Uint63.t **)

let subcarry i j =
  sub (sub i j) (Uint63.of_int (1))

(** val addc_def : Uint63.t -> Uint63.t -> Uint63.t Uint63.carry **)

let addc_def x y =
  let r = add x y in if ltb r x then Uint63.C1 r else Uint63.C0 r

(** val addcarryc_def : Uint63.t -> Uint63.t -> Uint63.t Uint63.carry **)

let addcarryc_def x y =
  let r = addcarry x y in if leb r x then Uint63.C1 r else Uint63.C0 r

(** val subc_def : Uint63.t -> Uint63.t -> Uint63.t Uint63.carry **)

let subc_def x y =
  if leb y x then Uint63.C0 (sub x y) else Uint63.C1 (sub x y)

(** val subcarryc_def : Uint63.t -> Uint63.t -> Uint63.t Uint63.carry **)

let subcarryc_def x y =
  if ltb y x
  then Uint63.C0 (sub (sub x y) (Uint63.of_int (1)))
  else Uint63.C1 (sub (sub x y) (Uint63.of_int (1)))

(** val diveucl_def : Uint63.t -> Uint63.t -> Uint63.t * Uint63.t **)

let diveucl_def x y =
  ((div x y), (coq_mod x y))

(** val addmuldiv_def : Uint63.t -> Uint63.t -> Uint63.t -> Uint63.t **)

let addmuldiv_def p x y =
  coq_lor (coq_lsl x p) (coq_lsr y (sub digits p))

module Uint63NotationsInternalC =
 struct
 end

(** val oppc : Uint63.t -> Uint63.t Uint63.carry **)

let oppc i =
  subc (Uint63.of_int (0)) i

(** val succc : Uint63.t -> Uint63.t Uint63.carry **)

let succc i =
  addc i (Uint63.of_int (1))

(** val predc : Uint63.t -> Uint63.t Uint63.carry **)

let predc i =
  subc i (Uint63.of_int (1))

(** val compare_def : Uint63.t -> Uint63.t -> comparison **)

let compare_def x y =
  if ltb x y then Lt else if eqb x y then Eq else Gt

(** val to_Z_rec : nat -> Uint63.t -> coq_Z **)

let rec to_Z_rec n i =
  match n with
  | O -> Z0
  | S n0 ->
    if is_even i
    then Z.double (to_Z_rec n0 (coq_lsr i (Uint63.of_int (1))))
    else Z.succ_double (to_Z_rec n0 (coq_lsr i (Uint63.of_int (1))))

(** val to_Z : Uint63.t -> coq_Z **)

let to_Z =
  to_Z_rec size

(** val of_pos_rec : nat -> positive -> Uint63.t **)

let rec of_pos_rec n p =
  match n with
  | O -> (Uint63.of_int (0))
  | S n0 ->
    (match p with
     | Coq_xI p0 ->
       coq_lor (coq_lsl (of_pos_rec n0 p0) (Uint63.of_int (1)))
         (Uint63.of_int (1))
     | Coq_xO p0 -> coq_lsl (of_pos_rec n0 p0) (Uint63.of_int (1))
     | Coq_xH -> (Uint63.of_int (1)))

(** val of_pos : positive -> Uint63.t **)

let of_pos =
  of_pos_rec size

(** val of_Z : coq_Z -> Uint63.t **)

let of_Z = function
| Z0 -> (Uint63.of_int (0))
| Zpos p -> of_pos p
| Zneg p -> opp (of_pos p)

(** val wB : coq_Z **)

let wB =
  Z.pow (Zpos (Coq_xO Coq_xH)) (Z.of_nat size)

module Uint63NotationsInternalD =
 struct
 end

(** val sqrt_step :
    (Uint63.t -> Uint63.t -> Uint63.t) -> Uint63.t -> Uint63.t -> Uint63.t **)

let sqrt_step rec0 i j =
  let quo = div i j in
  if ltb quo j then rec0 i (coq_lsr (add j quo) (Uint63.of_int (1))) else j

(** val iter_sqrt :
    nat -> (Uint63.t -> Uint63.t -> Uint63.t) -> Uint63.t -> Uint63.t ->
    Uint63.t **)

let rec iter_sqrt n rec0 i j =
  let quo = div i j in
  if ltb quo j
  then (match n with
        | O -> rec0 i (coq_lsr (add j quo) (Uint63.of_int (1)))
        | S n0 ->
          iter_sqrt n0 (iter_sqrt n0 rec0) i
            (coq_lsr (add j quo) (Uint63.of_int (1))))
  else j

(** val sqrt : Uint63.t -> Uint63.t **)

let sqrt i =
  match compare (Uint63.of_int (1)) i with
  | Eq -> (Uint63.of_int (1))
  | Lt -> iter_sqrt size (fun _ j -> j) i (coq_lsr i (Uint63.of_int (1)))
  | Gt -> (Uint63.of_int (0))

(** val high_bit : Uint63.t **)

let high_bit =
  coq_lsl (Uint63.of_int (1)) (sub digits (Uint63.of_int (1)))

(** val sqrt2_step :
    (Uint63.t -> Uint63.t -> Uint63.t -> Uint63.t) -> Uint63.t -> Uint63.t ->
    Uint63.t -> Uint63.t **)

let sqrt2_step rec0 ih il j =
  if ltb ih j
  then let (quo, _) = diveucl_21 ih il j in
       if ltb quo j
       then (match addc j quo with
             | Uint63.C0 m1 -> rec0 ih il (coq_lsr m1 (Uint63.of_int (1)))
             | Uint63.C1 m1 ->
               rec0 ih il (add (coq_lsr m1 (Uint63.of_int (1))) high_bit))
       else j
  else j

(** val iter2_sqrt :
    nat -> (Uint63.t -> Uint63.t -> Uint63.t -> Uint63.t) -> Uint63.t ->
    Uint63.t -> Uint63.t -> Uint63.t **)

let rec iter2_sqrt n rec0 ih il j =
  if ltb ih j
  then let (quo, _) = diveucl_21 ih il j in
       if ltb quo j
       then (match addc j quo with
             | Uint63.C0 m1 ->
               (match n with
                | O -> rec0 ih il (coq_lsr m1 (Uint63.of_int (1)))
                | S n0 ->
                  iter2_sqrt n0 (iter2_sqrt n0 rec0) ih il
                    (coq_lsr m1 (Uint63.of_int (1))))
             | Uint63.C1 m1 ->
               (match n with
                | O ->
                  rec0 ih il (add (coq_lsr m1 (Uint63.of_int (1))) high_bit)
                | S n0 ->
                  iter2_sqrt n0 (iter2_sqrt n0 rec0) ih il
                    (add (coq_lsr m1 (Uint63.of_int (1))) high_bit)))
       else j
  else j

(** val sqrt2 : Uint63.t -> Uint63.t -> Uint63.t * Uint63.t Uint63.carry **)

let sqrt2 ih il =
  let s = iter2_sqrt size (fun _ _ j -> j) ih il max_int in
  let (ih1, il1) = mulc s s in
  (match subc il il1 with
   | Uint63.C0 il2 ->
     if ltb ih1 ih then (s, (Uint63.C1 il2)) else (s, (Uint63.C0 il2))
   | Uint63.C1 il2 ->
     if ltb ih1 (sub ih (Uint63.of_int (1)))
     then (s, (Uint63.C1 il2))
     else (s, (Uint63.C0 il2)))

(** val gcd_rec : nat -> Uint63.t -> Uint63.t -> Uint63.t **)

let rec gcd_rec guard i j =
  match guard with
  | O -> (Uint63.of_int (1))
  | S p -> if eqb j (Uint63.of_int (0)) then i else gcd_rec p j (coq_mod i j)

(** val gcd : Uint63.t -> Uint63.t -> Uint63.t **)

let gcd =
  gcd_rec (mul (S (S O)) size)

(** val eqs : Uint63.t -> Uint63.t -> bool **)

let eqs i j =
  if eqb i j then true else false

(** val cast : Uint63.t -> Uint63.t -> (__ -> __ -> __) option **)

let cast i j =
  if eqb i j then Some (fun _ hi -> hi) else None

(** val eqo : Uint63.t -> Uint63.t -> __ option **)

let eqo i j =
  if eqb i j then Some __ else None

(** val eqbP : Uint63.t -> Uint63.t -> reflect **)

let eqbP x y =
  iff_reflect (eqb x y)

(** val ltbP : Uint63.t -> Uint63.t -> reflect **)

let ltbP x y =
  iff_reflect (ltb x y)

(** val lebP : Uint63.t -> Uint63.t -> reflect **)

let lebP x y =
  iff_reflect (leb x y)

(** val b2i : bool -> Uint63.t **)

let b2i = function
| true -> (Uint63.of_int (1))
| false -> (Uint63.of_int (0))

module Uint63Notations =
 struct
 end
