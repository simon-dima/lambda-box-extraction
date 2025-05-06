open Ascii
open CeresDeserialize
open CeresExtra
open Datatypes
open ExAst
open SerializeCommon
open SerializeEAst
open String0

val coq_Deserialize_box_type : box_type coq_Deserialize

val coq_Deserialize_type_var_info : type_var_info coq_Deserialize

val coq_Deserialize_constant_body : constant_body coq_Deserialize

val coq_Deserialize_one_inductive_body : one_inductive_body coq_Deserialize

val coq_Deserialize_mutual_inductive_body :
  mutual_inductive_body coq_Deserialize

val coq_Deserialize_global_decl : global_decl coq_Deserialize

val coq_Deserialize_global_env : global_env coq_Deserialize

val global_env_of_string : string -> (error, global_env) sum
