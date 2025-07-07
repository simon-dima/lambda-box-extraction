open CeresSerialize
open Datatypes
open EAst
open EEnvMap
open Erasure0
open Kernames
open List0
open Malfunction
open Pipeline0
open SemanticsSpec
open Serialize
open Bytestring

(** val extract_names : term -> ident list **)

let extract_names = function
| Coq_tConst kn -> (string_of_kername kn) :: []
| _ -> []

(** val print_program :
    malfunction_pipeline_config -> String.t list -> program -> String.t
    list * String.t **)

let print_program config nms p =
  let serialize = fun p_c ->
    to_string (coq_Serialize_module config.prims Standalone (rev nms)) p_c
  in
  let code = serialize p in (nms, code)

(** val unbox : bool **)

let unbox =
  true

(** val unsafe_passes_cfg : unsafe_passes **)

let unsafe_passes_cfg =
  { cofix_to_lazy = false; inlining = false; unboxing = unbox; betared =
    false }

(** val erasure_cfg : erasure_configuration **)

let erasure_cfg =
  { enable_unsafe = unsafe_passes_cfg; enable_typed_erasure = true;
    dearging_config = default_dearging_config; inlined_constants =
    KernameSet.empty }

(** val malfunction_cfg : malfunction_pipeline_config **)

let malfunction_cfg =
  { erasure_config = erasure_cfg; reorder_cstrs = []; prims = [] }

(** val malfunction_pipeline :
    (GlobalContextMap.t, (Ident.t * t option) list, term, t, term, value)
    Transform.Transform.t **)

let malfunction_pipeline =
  Transform.Transform.compose
    (Transform.Transform.compose
      (Transform.Transform.compose
        (verified_lambdabox_pipeline fake_guard_impl)
        (optional_unsafe_transforms erasure_cfg))
      (post_verified_named_erasure_pipeline coq_CanonicalPointer
        coq_CanonicalHeap))
    (compile_to_malfunction coq_CanonicalPointer coq_CanonicalHeap)

(** val box_to_ocaml : EAst.program -> String.t list * String.t **)

let box_to_ocaml p =
  let nms = extract_names (snd p) in
  let globalcontextmap = GlobalContextMap.make (fst p) in
  let p0 = (globalcontextmap, (snd p)) in
  let p1 = Transform.Transform.run malfunction_pipeline p0 in
  print_program malfunction_cfg nms p1
