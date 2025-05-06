open BasicAst
open BinNums
open Byte
open Datatypes
open Kernames
open LambdaANF_to_Wasm
open List0
open Monad0
open Pipeline_utils
open Bytestring
open CompM
open Cps
open Cps_show
open Datatypes0
open Toplevel0

val add_prim_names :
  ((((kername * String.t) * bool) * nat) * positive) list -> name_env ->
  name_env

val ensure_top_level_Efun : exp -> exp

val coq_LambdaANF_to_Wasm_Wrapper :
  ((((kername * String.t) * bool) * nat) * positive) list -> nat ->
  coq_LambdaANF_FullTerm -> coq_module error * String.t

val compile_LambdaANF_to_Wasm :
  ((((kername * String.t) * bool) * nat) * positive) list ->
  (coq_LambdaANF_FullTerm, coq_module) coq_CertiCoqTrans
