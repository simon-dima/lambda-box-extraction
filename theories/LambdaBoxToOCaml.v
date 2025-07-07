From MetaCoq.Erasure Require EAst.
From MetaCoq.Common Require Import Kernames.
From Malfunction Require Import Pipeline.
From Coq Require Import List.

Import ListNotations.



Definition extract_names (t : EAst.term) : list ident :=
  match t with
  | EAst.tConst kn => [Kernames.string_of_kername kn]
  (* | EAst.tApp (EAst.tConstruct _ _ _) [_ ; _ ; l ; Ast.tConst kn _ ] => (Kernames.string_of_kername kn) :: extract_names l *) (* TODO? *)
  | _ => []
  end.

From Malfunction Require Import CeresSerialize Serialize SemanticsSpec.
From MetaCoq.Common Require Import Transform.
Import Transform.

Definition print_program config nms p :=
  let serialize p_c := @to_string _ (Serialize_module config.(prims) Standalone (rev nms)) p_c in
  let code := serialize p in
  (nms, code).

Local Existing Instance CanonicalHeap.
Local Existing Instance CanonicalPointer.

From MetaCoq.ErasurePlugin Require Import Erasure.
From MetaCoq.Common Require Import EnvMap.

Definition unbox := true.

Definition unsafe_passes_cfg: unsafe_passes := {|
  cofix_to_lazy := false;
  inlining := false;
  unboxing := unbox;
  betared := false;
|}.

Definition erasure_cfg: erasure_configuration := {|
  enable_unsafe := unsafe_passes_cfg;
  dearging_config := default_dearging_config;
  enable_typed_erasure := true;
  inlined_constants := KernameSet.empty;
|}.

Definition malfunction_cfg: malfunction_pipeline_config := {|
  erasure_config := erasure_cfg;
  reorder_cstrs := [];
  prims := [];
|}.

Program Definition malfunction_pipeline :
  Transform.t _ _ _ _ _ _ _ _ :=
  verified_lambdabox_pipeline ▷
  optional_unsafe_transforms erasure_cfg ▷
  post_verified_named_erasure_pipeline ▷
  compile_to_malfunction.
Next Obligation.
  intuition auto; destruct H; intuition eauto.
Qed.

Axiom trust_coq_kernel : forall p, pre malfunction_pipeline p.
Axiom global_names_unique : forall (g: EAst.global_declarations) , EnvMap.fresh_globals g.

Definition box_to_ocaml (p : EAst.program) :=
  let nms := extract_names (snd p) in
  let globalcontextmap := EEnvMap.GlobalContextMap.make (fst p) (global_names_unique (fst p)) in
  let p := (globalcontextmap, snd p) in
  let p := run malfunction_pipeline p (trust_coq_kernel p) in
  print_program malfunction_cfg nms p.
