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

(** val print_wasm : coq_module -> String.t **)

let print_wasm p =
  String.parse (binary_of_module p)

(** val box_to_wasm : program -> String.t pipelineM **)

let box_to_wasm p =
  let genv = fst p in
  bind (coq_MonadErrorT coq_MonadState) (register_prims next_id genv)
    (fun x ->
    let (prs, next_id0) = x in
    bind (coq_MonadErrorT coq_MonadState) (anf_pipeline p prs next_id0)
      (fun p_anf ->
      bind (coq_MonadErrorT coq_MonadState)
        (compile_LambdaANF_to_Wasm prs p_anf) (fun p_wasm ->
        ret (coq_MonadErrorT coq_MonadState) (print_wasm p_wasm))))

(** val run_translation :
    coq_Options -> program -> String.t error * String.t **)

let run_translation opts p =
  run_pipeline opts p box_to_wasm
