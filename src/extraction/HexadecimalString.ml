open Ascii
open Hexadecimal
open String0

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
  | Da d0 ->
    String ((Ascii (true, false, false, false, false, true, true, false)),
      (string_of_uint d0))
  | Db d0 ->
    String ((Ascii (false, true, false, false, false, true, true, false)),
      (string_of_uint d0))
  | Dc d0 ->
    String ((Ascii (true, true, false, false, false, true, true, false)),
      (string_of_uint d0))
  | Dd d0 ->
    String ((Ascii (false, false, true, false, false, true, true, false)),
      (string_of_uint d0))
  | De d0 ->
    String ((Ascii (true, false, true, false, false, true, true, false)),
      (string_of_uint d0))
  | Df d0 ->
    String ((Ascii (false, true, true, false, false, true, true, false)),
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
 end
