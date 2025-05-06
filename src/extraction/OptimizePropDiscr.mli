open Datatypes
open EEnvMap
open EOptimizePropDiscr
open ExAst
open List0
open MCProd

val remove_match_on_box_constant_body :
  GlobalContextMap.t -> constant_body -> constant_body

val remove_match_on_box_decl :
  GlobalContextMap.t -> global_decl -> global_decl

val remove_match_on_box_env : global_env -> global_env
