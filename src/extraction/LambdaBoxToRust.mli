open Ascii
open Byte
open Datatypes
open ExAst
open Extraction
open PrettyPrinterMonad
open Printing
open ResultMonad
open RustExtract
open String0
open TypedTransforms
open Utils
open Bytestring
open Monad_utils

val plugin_extract_preamble : coq_Preamble

val coq_RustConfig : coq_RustPrintConfig

val default_attrs : ind_attr_map

val default_remaps : remaps

val mk_preamble : String.t option -> String.t option -> coq_Preamble

val box_to_rust :
  remaps -> coq_Preamble -> ind_attr_map -> extract_pcuic_params ->
  global_env -> (String.t list, String.t) result
