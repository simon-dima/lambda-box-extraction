open BinNums
open CeresFormat
open CeresS
open Datatypes
open List0
open Bytestring

type 'a coq_Serialize = 'a -> atom sexp_

val to_sexp : 'a1 coq_Serialize -> 'a1 -> atom sexp_

val to_string : 'a1 coq_Serialize -> 'a1 -> String.t

type 'a coq_Integral = 'a -> coq_Z

val to_Z : 'a1 coq_Integral -> 'a1 -> coq_Z

val coq_Serialize_Integral : 'a1 coq_Integral -> 'a1 coq_Serialize

val coq_Integral_Z : coq_Z coq_Integral

val coq_Serialize_product :
  'a1 coq_Serialize -> 'a2 coq_Serialize -> ('a1 * 'a2) coq_Serialize

val coq_Serialize_list : 'a1 coq_Serialize -> 'a1 list coq_Serialize
