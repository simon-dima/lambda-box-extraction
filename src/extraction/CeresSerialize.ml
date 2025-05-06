open BinNums
open CeresFormat
open CeresS
open Datatypes
open List0
open Bytestring

type 'a coq_Serialize = 'a -> atom sexp_

(** val to_sexp : 'a1 coq_Serialize -> 'a1 -> atom sexp_ **)

let to_sexp serialize =
  serialize

(** val to_string : 'a1 coq_Serialize -> 'a1 -> String.t **)

let to_string h a =
  string_of_sexp (to_sexp h a)

type 'a coq_Integral = 'a -> coq_Z

(** val to_Z : 'a1 coq_Integral -> 'a1 -> coq_Z **)

let to_Z integral =
  integral

(** val coq_Serialize_Integral : 'a1 coq_Integral -> 'a1 coq_Serialize **)

let coq_Serialize_Integral h z =
  Atom_ (Num (to_Z h z))

(** val coq_Integral_Z : coq_Z coq_Integral **)

let coq_Integral_Z =
  Obj.magic id

(** val coq_Serialize_product :
    'a1 coq_Serialize -> 'a2 coq_Serialize -> ('a1 * 'a2) coq_Serialize **)

let coq_Serialize_product h h0 ab =
  List ((to_sexp h (fst ab)) :: ((to_sexp h0 (snd ab)) :: []))

(** val coq_Serialize_list : 'a1 coq_Serialize -> 'a1 list coq_Serialize **)

let coq_Serialize_list h xs =
  List (map (to_sexp h) xs)
