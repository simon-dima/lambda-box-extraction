open Ascii
open CeresDeserialize
open CeresFormat0
open CeresParserUtils
open CeresString
open Datatypes
open List0
open String0

val con6 :
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7) -> 'a1 coq_FromSexp -> 'a2
  coq_FromSexp -> 'a3 coq_FromSexp -> 'a4 coq_FromSexp -> 'a5 coq_FromSexp ->
  'a6 coq_FromSexp -> 'a7 coq_FromSexpList

val con6_ :
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7) -> 'a1 coq_Deserialize ->
  'a2 coq_Deserialize -> 'a3 coq_Deserialize -> 'a4 coq_Deserialize -> 'a5
  coq_Deserialize -> 'a6 coq_Deserialize -> 'a7 coq_FromSexpList

val string_of_loc : CeresDeserialize.loc -> string

val string_of_message : bool -> message -> string

val string_of_error : bool -> bool -> CeresDeserialize.error -> string
