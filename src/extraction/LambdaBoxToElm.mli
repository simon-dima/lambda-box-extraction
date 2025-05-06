open Byte
open Common2
open Datatypes
open ElmExtract
open ExAst
open Extraction
open Kernames
open List0
open PrettyPrinterMonad0
open ResultMonad
open TypedTransforms
open Bytestring
open Monad_utils

val coq_ElmBoxes : coq_ElmPrintConfig

val mk_preamble : String.t -> String.t option -> String.t

val default_remaps : (kername * String.t) list

val box_to_elm :
  String.t -> String.t option -> (kername * String.t) list ->
  extract_pcuic_params -> global_env -> (String.t, String.t) result
