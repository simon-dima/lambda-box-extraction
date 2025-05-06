open Ascii
open Datatypes
open Decimal
open String0

(** val uint_of_char : ascii -> uint option -> uint option **)

let uint_of_char a = function
| Some d0 ->
  let Ascii (b, b0, b1, b2, b3, b4, b5, b6) = a in
  if b
  then if b0
       then if b1
            then if b2
                 then None
                 else if b3
                      then if b4
                           then if b5
                                then None
                                else if b6 then None else Some (D7 d0)
                           else None
                      else None
            else if b2
                 then None
                 else if b3
                      then if b4
                           then if b5
                                then None
                                else if b6 then None else Some (D3 d0)
                           else None
                      else None
       else if b1
            then if b2
                 then None
                 else if b3
                      then if b4
                           then if b5
                                then None
                                else if b6 then None else Some (D5 d0)
                           else None
                      else None
            else if b2
                 then if b3
                      then if b4
                           then if b5
                                then None
                                else if b6 then None else Some (D9 d0)
                           else None
                      else None
                 else if b3
                      then if b4
                           then if b5
                                then None
                                else if b6 then None else Some (D1 d0)
                           else None
                      else None
  else if b0
       then if b1
            then if b2
                 then None
                 else if b3
                      then if b4
                           then if b5
                                then None
                                else if b6 then None else Some (D6 d0)
                           else None
                      else None
            else if b2
                 then None
                 else if b3
                      then if b4
                           then if b5
                                then None
                                else if b6 then None else Some (D2 d0)
                           else None
                      else None
       else if b1
            then if b2
                 then None
                 else if b3
                      then if b4
                           then if b5
                                then None
                                else if b6 then None else Some (D4 d0)
                           else None
                      else None
            else if b2
                 then if b3
                      then if b4
                           then if b5
                                then None
                                else if b6 then None else Some (D8 d0)
                           else None
                      else None
                 else if b3
                      then if b4
                           then if b5
                                then None
                                else if b6 then None else Some (D0 d0)
                           else None
                      else None
| None -> None

module NilEmpty =
 struct
  (** val string_of_uint : uint -> string **)

  let rec string_of_uint = function
  | Nil -> EmptyString
  | D0 d0 ->
    String ((Ascii (false, false, false, false, true, true, false, false)),
      (string_of_uint d0))
  | D1 d0 ->
    String ((Ascii (true, false, false, false, true, true, false, false)),
      (string_of_uint d0))
  | D2 d0 ->
    String ((Ascii (false, true, false, false, true, true, false, false)),
      (string_of_uint d0))
  | D3 d0 ->
    String ((Ascii (true, true, false, false, true, true, false, false)),
      (string_of_uint d0))
  | D4 d0 ->
    String ((Ascii (false, false, true, false, true, true, false, false)),
      (string_of_uint d0))
  | D5 d0 ->
    String ((Ascii (true, false, true, false, true, true, false, false)),
      (string_of_uint d0))
  | D6 d0 ->
    String ((Ascii (false, true, true, false, true, true, false, false)),
      (string_of_uint d0))
  | D7 d0 ->
    String ((Ascii (true, true, true, false, true, true, false, false)),
      (string_of_uint d0))
  | D8 d0 ->
    String ((Ascii (false, false, false, true, true, true, false, false)),
      (string_of_uint d0))
  | D9 d0 ->
    String ((Ascii (true, false, false, true, true, true, false, false)),
      (string_of_uint d0))

  (** val uint_of_string : string -> uint option **)

  let rec uint_of_string = function
  | EmptyString -> Some Nil
  | String (a, s0) -> uint_of_char a (uint_of_string s0)

  (** val string_of_int : signed_int -> string **)

  let string_of_int = function
  | Pos d0 -> string_of_uint d0
  | Neg d0 ->
    String ((Ascii (true, false, true, true, false, true, false, false)),
      (string_of_uint d0))
 end

module NilZero =
 struct
  (** val string_of_uint : uint -> string **)

  let string_of_uint d = match d with
  | Nil ->
    String ((Ascii (false, false, false, false, true, true, false, false)),
      EmptyString)
  | _ -> NilEmpty.string_of_uint d

  (** val uint_of_string : string -> uint option **)

  let uint_of_string s = match s with
  | EmptyString -> None
  | String (_, _) -> NilEmpty.uint_of_string s

  (** val string_of_int : signed_int -> string **)

  let string_of_int = function
  | Pos d0 -> string_of_uint d0
  | Neg d0 ->
    String ((Ascii (true, false, true, true, false, true, false, false)),
      (string_of_uint d0))

  (** val int_of_string : string -> signed_int option **)

  let int_of_string s = match s with
  | EmptyString -> None
  | String (a, s') ->
    if eqb a (Ascii (true, false, true, true, false, true, false, false))
    then option_map (fun x -> Neg x) (uint_of_string s')
    else option_map (fun x -> Pos x) (uint_of_string s)
 end
