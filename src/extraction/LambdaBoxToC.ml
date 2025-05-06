open BinNums
open CertiCoqPipeline
open Datatypes
open EAst
open Monad0
open Pipeline_utils
open Bytestring
open CompM
open Toplevel1

(** val next_id : positive **)

let next_id =
  Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))

(** val box_to_c : program -> coq_Cprogram pipelineM **)

let box_to_c p =
  let genv = fst p in
  bind (coq_MonadErrorT coq_MonadState) (register_prims next_id genv)
    (fun x ->
    let (prs, next_id0) = x in
    bind (coq_MonadErrorT coq_MonadState) (anf_pipeline p prs next_id0)
      (fun p_anf -> compile_Clight prs p_anf))

(** val run_translation :
    coq_Options -> program -> coq_Cprogram error * String.t **)

let run_translation opts p =
  run_pipeline opts p box_to_c
