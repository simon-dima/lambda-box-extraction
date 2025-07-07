open CeresSerialize
open Datatypes
open EAst
open EEnvMap
open Erasure0
open Kernames
open List0
open Malfunction
open Pipeline0
open SemanticsSpec
open Serialize
open Bytestring

val extract_names : term -> ident list

val print_program :
  malfunction_pipeline_config -> String.t list -> program -> String.t
  list * String.t

val unbox : bool

val unsafe_passes_cfg : unsafe_passes

val erasure_cfg : erasure_configuration

val malfunction_cfg : malfunction_pipeline_config

val malfunction_pipeline :
  (GlobalContextMap.t, (Ident.t * t option) list, term, t, term, value)
  Transform.Transform.t

val box_to_ocaml : EAst.program -> String.t list * String.t
