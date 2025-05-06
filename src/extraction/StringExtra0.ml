open BinNat
open BinNums
open BinPos
open Byte
open Byte0
open Datatypes
open Bytestring

(** val coq_Nlog2up_nat : coq_N -> nat **)

let coq_Nlog2up_nat = function
| N0 -> S O
| Npos p -> Pos.size_nat p

(** val string_of_N : coq_N -> String.t **)

let string_of_N n =
  let rec f n0 num acc =
    let (q, r) = N.div_eucl num (Npos (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) in
    let char = fun x ->
      match r with
      | N0 -> String.String (Coq_x30, x)
      | Npos p ->
        (match p with
         | Coq_xI p0 ->
           (match p0 with
            | Coq_xI p1 ->
              (match p1 with
               | Coq_xH -> String.String (Coq_x37, x)
               | _ -> String.String (Coq_x78, x))
            | Coq_xO p1 ->
              (match p1 with
               | Coq_xI _ -> String.String (Coq_x78, x)
               | Coq_xO p2 ->
                 (match p2 with
                  | Coq_xH -> String.String (Coq_x39, x)
                  | _ -> String.String (Coq_x78, x))
               | Coq_xH -> String.String (Coq_x35, x))
            | Coq_xH -> String.String (Coq_x33, x))
         | Coq_xO p0 ->
           (match p0 with
            | Coq_xI p1 ->
              (match p1 with
               | Coq_xH -> String.String (Coq_x36, x)
               | _ -> String.String (Coq_x78, x))
            | Coq_xO p1 ->
              (match p1 with
               | Coq_xI _ -> String.String (Coq_x78, x)
               | Coq_xO p2 ->
                 (match p2 with
                  | Coq_xH -> String.String (Coq_x38, x)
                  | _ -> String.String (Coq_x78, x))
               | Coq_xH -> String.String (Coq_x34, x))
            | Coq_xH -> String.String (Coq_x32, x))
         | Coq_xH -> String.String (Coq_x31, x))
    in
    let acc0 = char acc in
    if N.eqb q N0
    then acc0
    else (match n0 with
          | O -> String.EmptyString
          | S n1 -> f n1 q acc0)
  in f (coq_Nlog2up_nat n) n String.EmptyString

(** val string_of_nat : nat -> String.t **)

let string_of_nat n =
  string_of_N (N.of_nat n)

(** val replace_char : byte -> byte -> String.t -> String.t **)

let rec replace_char orig new0 = function
| String.EmptyString -> String.EmptyString
| String.String (c, s0) ->
  if eqb c orig
  then String.String (new0, (replace_char orig new0 s0))
  else String.String (c, (replace_char orig new0 s0))

(** val remove_char : byte -> String.t -> String.t **)

let rec remove_char c = function
| String.EmptyString -> String.EmptyString
| String.String (c', s0) ->
  if eqb c' c
  then remove_char c s0
  else String.String (c', (remove_char c s0))

(** val substring_from : nat -> String.t -> String.t **)

let rec substring_from from s =
  match from with
  | O -> s
  | S n ->
    (match s with
     | String.EmptyString -> String.EmptyString
     | String.String (_, s0) -> substring_from n s0)

(** val substring_count : nat -> String.t -> String.t **)

let rec substring_count count s =
  match count with
  | O -> String.EmptyString
  | S n ->
    (match s with
     | String.EmptyString -> String.EmptyString
     | String.String (c, s0) -> String.String (c, (substring_count n s0)))

(** val is_letter : byte -> bool **)

let is_letter c =
  let n = to_N c in
  (||)
    ((&&)
      (N.leb (Npos (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
        Coq_xH))))))) n)
      (N.leb n (Npos (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO
        Coq_xH)))))))))
    ((&&)
      (N.leb (Npos (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI
        Coq_xH))))))) n)
      (N.leb n (Npos (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI
        Coq_xH)))))))))

(** val char_to_upper : byte -> byte **)

let char_to_upper c =
  if is_letter c
  then let (b0, p) = to_bits c in
       let (b1, p0) = p in
       let (b2, p1) = p0 in
       let (b3, p2) = p1 in
       let (b4, p3) = p2 in
       let (_, p4) = p3 in of_bits (b0, (b1, (b2, (b3, (b4, (false, p4))))))
  else c

(** val char_to_lower : byte -> byte **)

let char_to_lower c =
  if is_letter c
  then let (b0, p) = to_bits c in
       let (b1, p0) = p in
       let (b2, p1) = p0 in
       let (b3, p2) = p1 in
       let (b4, p3) = p2 in
       let (_, p4) = p3 in of_bits (b0, (b1, (b2, (b3, (b4, (true, p4))))))
  else c

(** val capitalize : String.t -> String.t **)

let capitalize = function
| String.EmptyString -> String.EmptyString
| String.String (c, s0) -> String.String ((char_to_upper c), s0)

(** val uncapitalize : String.t -> String.t **)

let uncapitalize = function
| String.EmptyString -> String.EmptyString
| String.String (c, s0) -> String.String ((char_to_lower c), s0)
