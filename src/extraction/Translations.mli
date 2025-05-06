open EAst
open ExAst
open Extraction
open Kernames
open LambdaBoxToC
open LambdaBoxToElm
open LambdaBoxToOCaml
open LambdaBoxToRust
open LambdaBoxToWasm
open Pipeline_utils
open Printing
open ResultMonad
open RustExtract
open Bytestring
open CompM
open Toplevel1

val l_box_to_wasm : coq_Options -> program -> String.t error * String.t

val l_box_to_rust :
  remaps -> coq_Preamble -> ind_attr_map -> extract_pcuic_params ->
  global_env -> (String.t list, String.t) result

val l_box_to_elm :
  String.t -> String.t option -> (kername * String.t) list ->
  extract_pcuic_params -> global_env -> (String.t, String.t) result

val l_box_to_c : coq_Options -> program -> coq_Cprogram error * String.t

val l_box_to_ocaml : program -> String.t list * String.t
