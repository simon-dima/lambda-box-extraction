open Ascii
open CeresDeserialize
open CeresS
open Datatypes
open EPrimitive
open Primitive
open String0

val prim_int_of_string : string -> Uint63.t

val prim_float_of_string : string -> Float64.t

val coq_Deserialize_prim_tag : prim_tag coq_Deserialize

val coq_Deserialize_prim_int : Uint63.t coq_Deserialize

val coq_Deserialize_prim_float : Float64.t coq_Deserialize

val coq_Deserialize_array_model :
  'a1 coq_Deserialize -> 'a1 array_model coq_Deserialize

val coq_Deserialize_prim_val :
  'a1 coq_Deserialize -> 'a1 prim_val coq_Deserialize
