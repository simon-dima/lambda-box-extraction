open BinInt
open BinNums
open Byte
open Compile1
open Datatypes
open EAst
open EGlobalEnv
open EImplementBox
open EPrimitive
open EProgram
open EWcbvEvalNamed
open EWellformed
open Erasure0
open Kernames
open List0
open Malfunction
open PeanoNat
open SemanticsSpec
open Uint0
open Bytestring

type malfunction_pipeline_config = { erasure_config : erasure_configuration;
                                     reorder_cstrs : inductives_mapping;
                                     prims : primitives }

val array_length : Uint63.t

val ignore : 'a1 -> 'a2 -> 'a2

val bool_good_error : bool -> String.t -> bool

val array_length_Z : coq_Z

val wellformed_fast : coq_EEnvFlags -> global_declarations -> term -> bool

val check_good_for_extraction_rec :
  coq_EEnvFlags -> (kername * global_decl) list -> bool

val check_good_for_extraction :
  coq_EEnvFlags -> ((kername * global_decl) list, term)
  Transform.Transform.program -> bool

val extraction_term_flags_mlf : coq_ETermFlags

val extraction_env_flags_mlf : coq_EEnvFlags

val enforce_extraction_conditions :
  coq_Pointer -> coq_Pointer -> coq_Heap -> (global_declarations,
  global_declarations, term, term, term, term) Transform.Transform.t

val implement_box_transformation :
  (global_declarations, global_declarations, term, term, term, term)
  Transform.Transform.t

val name_annotation :
  (global_declarations, (kername * global_decl) list, term, term, term,
  EWcbvEvalNamed.value) Transform.Transform.t

val compile_to_malfunction :
  coq_Pointer -> coq_Heap -> ((kername * global_decl) list, (Ident.t * t
  option) list, term, t, EWcbvEvalNamed.value, value) Transform.Transform.t

val post_verified_named_erasure_pipeline :
  coq_Pointer -> coq_Heap -> (global_declarations, global_declarations, term,
  term, term, EWcbvEvalNamed.value) Transform.Transform.t

val default_malfunction_config : malfunction_pipeline_config
