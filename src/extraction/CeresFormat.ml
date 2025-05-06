open Byte
open CeresS
open CeresString
open List0
open Bytestring

(** val byte_to_string : byte -> String.t **)

let byte_to_string b =
  String.String (b, String.EmptyString)

(** val dstring_of_sexp : ('a1 -> Tree.t) -> 'a1 sexp_ -> Tree.t **)

let rec dstring_of_sexp dstring_A = function
| Atom_ a -> dstring_A a
| List xs0 ->
  (match xs0 with
   | [] ->
     Tree.Coq_string (String.String (Coq_x28, (String.String (Coq_x29,
       String.EmptyString))))
   | x0 :: xs ->
     Tree.Coq_append ((Tree.Coq_string (String.String (Coq_x28,
       String.EmptyString))), (Tree.Coq_append
       ((dstring_of_sexp dstring_A x0),
       (fold_right (fun x1 y -> Tree.Coq_append ((Tree.Coq_string
         (byte_to_string Coq_x20)), (Tree.Coq_append
         ((dstring_of_sexp dstring_A x1), y)))) (Tree.Coq_string
         (String.String (Coq_x29, String.EmptyString))) xs)))))

(** val string_of_sexp_ : ('a1 -> String.t) -> 'a1 sexp_ -> Tree.t **)

let string_of_sexp_ string_A x =
  dstring_of_sexp (fun x0 -> Tree.Coq_string (string_A x0)) x

(** val string_of_atom : atom -> String.t **)

let string_of_atom = function
| Num n -> String.of_string (string_of_Z n)
| Str s -> String.of_string (escape_string s)
| Raw s -> String.of_string s

(** val string_of_sexp : atom sexp_ -> String.t **)

let string_of_sexp s =
  Tree.to_string (string_of_sexp_ string_of_atom s)
