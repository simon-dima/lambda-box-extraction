open Ascii
open CeresS
open CeresString
open List0
open String0

(** val dstring_of_sexp : ('a1 -> DString.t) -> 'a1 sexp_ -> DString.t **)

let rec dstring_of_sexp dstring_A = function
| Atom_ a -> dstring_A a
| List xs0 ->
  (match xs0 with
   | [] ->
     DString.of_string (String ((Ascii (false, false, false, true, false,
       true, false, false)), (String ((Ascii (true, false, false, true,
       false, true, false, false)), EmptyString))))
   | x0 :: xs ->
     (fun s0 -> String ((Ascii (false, false, false, true, false, true,
       false, false)),
       (dstring_of_sexp dstring_A x0
         (fold_right (fun x1 s ->
           DString.app_string
             (DString.of_ascii (Ascii (false, false, false, false, false,
               true, false, false)))
             (DString.app_string (dstring_of_sexp dstring_A x1) s)) (String
           ((Ascii (true, false, false, true, false, true, false, false)),
           s0)) xs)))))

(** val string_of_sexp_ : ('a1 -> string) -> 'a1 sexp_ -> string **)

let string_of_sexp_ string_A x =
  dstring_of_sexp (fun x0 -> DString.of_string (string_A x0)) x EmptyString

(** val string_of_atom : atom -> string **)

let string_of_atom = function
| Num n -> string_of_Z n
| Str s -> escape_string s
| Raw s -> s

(** val string_of_sexp : atom sexp_ -> string **)

let string_of_sexp =
  string_of_sexp_ string_of_atom
