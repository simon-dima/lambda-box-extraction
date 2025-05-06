open Ascii
open BinInt
open BinNums
open Bool
open Datatypes
open DecimalString
open Nat0
open PeanoNat
open String0

(** val compcomp : comparison -> comparison -> comparison **)

let compcomp x y =
  match x with
  | Eq -> y
  | x0 -> x0

(** val compb : bool -> bool -> comparison **)

let compb x y =
  if x then if y then Eq else Gt else if y then Lt else Eq

(** val eqb_ascii : ascii -> ascii -> bool **)

let eqb_ascii a b =
  let Ascii (a0, a1, a2, a3, a4, a5, a6, a7) = a in
  let Ascii (b0, b1, b2, b3, b4, b5, b6, b7) = b in
  if if if if if if if Bool.eqb a0 b0 then Bool.eqb a1 b1 else false
                 then Bool.eqb a2 b2
                 else false
              then Bool.eqb a3 b3
              else false
           then Bool.eqb a4 b4
           else false
        then Bool.eqb a5 b5
        else false
     then Bool.eqb a6 b6
     else false
  then Bool.eqb a7 b7
  else false

(** val ascii_compare : ascii -> ascii -> comparison **)

let ascii_compare a b =
  let Ascii (a0, a1, a2, a3, a4, a5, a6, a7) = a in
  let Ascii (b0, b1, b2, b3, b4, b5, b6, b7) = b in
  compcomp (compb a7 b7)
    (compcomp (compb a6 b6)
      (compcomp (compb a5 b5)
        (compcomp (compb a4 b4)
          (compcomp (compb a3 b3)
            (compcomp (compb a2 b2) (compcomp (compb a1 b1) (compb a0 b0)))))))

(** val leb_ascii : ascii -> ascii -> bool **)

let leb_ascii a b =
  match ascii_compare a b with
  | Gt -> false
  | _ -> true

(** val eqb_string : string -> string -> bool **)

let rec eqb_string s1 s2 =
  match s1 with
  | EmptyString ->
    (match s2 with
     | EmptyString -> true
     | String (_, _) -> false)
  | String (c1, s1') ->
    (match s2 with
     | EmptyString -> false
     | String (c2, s2') ->
       if eqb_ascii c1 c2 then eqb_string s1' s2' else false)

(** val string_elem : ascii -> string -> bool **)

let rec string_elem c = function
| EmptyString -> false
| String (c', s0) -> if eqb_ascii c c' then true else string_elem c s0

(** val _string_reverse : string -> string -> string **)

let rec _string_reverse r = function
| EmptyString -> r
| String (c, s0) -> _string_reverse (String (c, r)) s0

(** val string_reverse : string -> string **)

let string_reverse =
  _string_reverse EmptyString

(** val comma_sep : string list -> string **)

let rec comma_sep = function
| [] -> EmptyString
| x :: xs0 ->
  (match xs0 with
   | [] -> x
   | _ :: _ ->
     append x
       (append (String ((Ascii (false, false, true, true, false, true, false,
         false)), (String ((Ascii (false, false, false, false, false, true,
         false, false)), EmptyString)))) (comma_sep xs0)))

(** val is_printable : ascii -> bool **)

let is_printable c =
  (&&)
    (leb_ascii (Ascii (false, false, false, false, false, true, false,
      false)) c)
    (leb_ascii c (Ascii (false, true, true, true, true, true, true, false)))

(** val is_whitespace : ascii -> bool **)

let is_whitespace = function
| Ascii (b, b0, b1, b2, b3, b4, b5, b6) ->
  if b
  then if b0
       then false
       else if b1
            then if b2
                 then if b3
                      then false
                      else if b4
                           then false
                           else if b5
                                then false
                                else if b6 then false else true
                 else false
            else false
  else if b0
       then if b1
            then false
            else if b2
                 then if b3
                      then false
                      else if b4
                           then false
                           else if b5
                                then false
                                else if b6 then false else true
                 else false
       else if b1
            then false
            else if b2
                 then false
                 else if b3
                      then false
                      else if b4
                           then if b5
                                then false
                                else if b6 then false else true
                           else false

(** val is_digit : ascii -> bool **)

let is_digit c =
  if leb_ascii (Ascii (false, false, false, false, true, true, false, false))
       c
  then leb_ascii c (Ascii (true, false, false, true, true, true, false,
         false))
  else false

(** val is_upper : ascii -> bool **)

let is_upper c =
  if leb_ascii (Ascii (true, false, false, false, false, false, true, false))
       c
  then leb_ascii c (Ascii (false, true, false, true, true, false, true,
         false))
  else false

(** val is_lower : ascii -> bool **)

let is_lower c =
  if leb_ascii (Ascii (true, false, false, false, false, true, true, false)) c
  then leb_ascii c (Ascii (false, true, false, true, true, true, true, false))
  else false

(** val is_alphanum : ascii -> bool **)

let is_alphanum c =
  if if is_upper c then true else is_lower c then true else is_digit c

(** val _units_digit : nat -> ascii **)

let _units_digit n =
  ascii_of_nat
    (add (Nat.modulo n (S (S (S (S (S (S (S (S (S (S O))))))))))) (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      O)))))))))))))))))))))))))))))))))))))))))))))))))

(** val _three_digit : nat -> string **)

let _three_digit n =
  let n0 = _units_digit n in
  let n1 = _units_digit (Nat.div n (S (S (S (S (S (S (S (S (S (S O)))))))))))
  in
  let n2 =
    _units_digit
      (Nat.div n (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        (S (S (S (S (S (S (S (S (S (S (S
        O)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  in
  String (n2, (String (n1, (String (n0, EmptyString)))))

(** val _escape_string : string -> string -> string **)

let rec _escape_string _end = function
| EmptyString -> _end
| String (c, s') ->
  let escaped_s' = _escape_string _end s' in
  if eqb_ascii (Ascii (true, false, false, true, false, false, false, false))
       c
  then String ((Ascii (false, false, true, true, true, false, true, false)),
         (String ((Ascii (false, false, true, false, true, true, true,
         false)), escaped_s')))
  else if eqb_ascii (Ascii (false, true, false, true, false, false, false,
            false)) c
       then String ((Ascii (false, false, true, true, true, false, true,
              false)), (String ((Ascii (false, true, true, true, false, true,
              true, false)), escaped_s')))
       else if eqb_ascii (Ascii (true, false, true, true, false, false,
                 false, false)) c
            then String ((Ascii (false, false, true, true, true, false, true,
                   false)), (String ((Ascii (false, true, false, false, true,
                   true, true, false)), escaped_s')))
            else if eqb_ascii (Ascii (false, true, false, false, false, true,
                      false, false)) c
                 then String ((Ascii (false, false, true, true, true, false,
                        true, false)), (String ((Ascii (false, true, false,
                        false, false, true, false, false)), escaped_s')))
                 else if eqb_ascii (Ascii (false, false, true, true, true,
                           false, true, false)) c
                      then String ((Ascii (false, false, true, true, true,
                             false, true, false)), (String ((Ascii (false,
                             false, true, true, true, false, true, false)),
                             escaped_s')))
                      else if is_printable c
                           then String (c, escaped_s')
                           else let n = nat_of_ascii c in
                                String ((Ascii (false, false, true, true,
                                true, false, true, false)),
                                (append (_three_digit n) escaped_s'))

(** val escape_string : string -> string **)

let escape_string s =
  String ((Ascii (false, true, false, false, false, true, false, false)),
    (_escape_string (String ((Ascii (false, true, false, false, false, true,
      false, false)), EmptyString)) s))

(** val string_of_nat : nat -> string **)

let string_of_nat n =
  NilEmpty.string_of_uint (Nat.to_uint n)

(** val string_of_Z : coq_Z -> string **)

let string_of_Z n =
  NilEmpty.string_of_int (Z.to_int n)

(** val string_of_N : coq_N -> string **)

let string_of_N n =
  string_of_Z (Z.of_N n)

module DString =
 struct
  type t = string -> string

  (** val of_string : string -> t **)

  let of_string =
    append

  (** val of_ascii : ascii -> t **)

  let of_ascii c s =
    String (c, s)

  (** val app_string : t -> string -> string **)

  let app_string =
    Obj.magic id
 end
