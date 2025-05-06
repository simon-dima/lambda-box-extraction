open BinNums
open CertiCoqPipeline
open Datatypes
open EAst
open Monad0
open Pipeline_utils
open Bytestring
open CompM
open Toplevel1

val next_id : positive

val box_to_c : program -> coq_Cprogram pipelineM

val run_translation : coq_Options -> program -> coq_Cprogram error * String.t
