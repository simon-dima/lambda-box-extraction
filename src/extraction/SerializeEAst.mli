open Ascii
open CeresDeserialize
open CeresS
open Datatypes
open EAst
open SerializeCommon
open SerializePrimitives
open String0

val coq_Deserialize_def : 'a1 coq_Deserialize -> 'a1 def coq_Deserialize

val coq_Deserialize_mfixpoint :
  'a1 coq_Deserialize -> 'a1 mfixpoint coq_Deserialize

val deserialize_term : loc -> atom sexp_ -> (error, term) sum

val coq_Deserialize_term : term coq_Deserialize

val coq_Deserialize_constructor_body : constructor_body coq_Deserialize

val coq_Deserialize_projection_body : projection_body coq_Deserialize

val coq_Deserialize_one_inductive_body : one_inductive_body coq_Deserialize

val coq_Deserialize_mutual_inductive_body :
  mutual_inductive_body coq_Deserialize

val coq_Deserialize_constant_body : constant_body coq_Deserialize

val coq_Deserialize_global_decl : global_decl coq_Deserialize

val coq_Deserialize_global_declarations : global_declarations coq_Deserialize

val coq_Deserialize_program : program coq_Deserialize

val program_of_string : string -> (error, program) sum
