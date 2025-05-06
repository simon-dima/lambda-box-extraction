open AstCommon
open BinNums
open Byte
open Datatypes
open EAst
open Erasure0
open Kernames
open Monad0
open Nat0
open Pipeline_utils
open Bytestring
open CompM
open Compile0
open Cps_show
open Pipeline
open Toplevel0
open Toplevel

val make_opts : bool -> bool -> coq_Options

val find_arity : term -> nat

val find_global_decl_arity : global_decl -> nat error

val find_prim_arity : global_declarations -> kername -> nat error

val find_prim_arities :
  global_declarations -> ((kername * String.t) * bool) list ->
  ((((kername * String.t) * bool) * nat) * positive) list error

val register_prims :
  positive -> global_declarations ->
  (((((kername * String.t) * bool) * nat) * positive) list * positive)
  pipelineM

val anf_pipeline :
  program -> ((((kername * String.t) * bool) * nat) * positive) list ->
  positive -> coq_LambdaANF_FullTerm pipelineM

val show_IR : coq_Options -> program -> String.t error * String.t
