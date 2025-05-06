open CeresSerialize
open Datatypes
open EAst
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

val malfunction_pipeline :
  (global_declarations, (Ident.t * t option) list, term, t, term, value)
  Transform.Transform.t

val box_to_ocaml : EAst.program -> String.t list * String.t
