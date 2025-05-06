open CertiCoqPipeline
open Datatypes
open EAst
open Monad0
open Pipeline_utils
open Binary_format_printer
open Bytestring
open CompM
open Datatypes0
open Pipeline
open Toplevel2

val print_wasm : coq_module -> String.t

val box_to_wasm : program -> String.t pipelineM

val run_translation : coq_Options -> program -> String.t error * String.t
