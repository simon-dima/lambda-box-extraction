open Ascii
open BasicAst
open CeresDeserialize
open CeresS
open Datatypes
open Kernames
open String0
open Universes0
open Bytestring

val coq_Deserialize_ident : ident coq_Deserialize

val coq_Deserialize_dirpath : dirpath coq_Deserialize

val coq_Deserialize_modpath : modpath coq_Deserialize

val coq_Deserialize_kername : kername coq_Deserialize

val coq_Deserialize_inductive : inductive coq_Deserialize

val coq_Deserialize_projection : projection coq_Deserialize

val coq_Deserialize_name : name coq_Deserialize

val coq_Deserialize_recursivity_kind : recursivity_kind coq_Deserialize

val coq_Deserialize_allowed_eliminations :
  allowed_eliminations coq_Deserialize

val kername_of_string : string -> (error, kername) sum
