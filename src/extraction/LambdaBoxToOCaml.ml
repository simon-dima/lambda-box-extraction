open CeresSerialize
open Datatypes
open EAst
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

(** val malfunction_pipeline :
    (global_declarations, (Ident.t * t option) list, term, t, term, value)
    Transform.Transform.t **)

let malfunction_pipeline =
  Transform.Transform.compose
    (post_verified_named_erasure_pipeline coq_CanonicalPointer
      coq_CanonicalHeap)
    (compile_to_malfunction coq_CanonicalPointer coq_CanonicalHeap)

(** val box_to_ocaml : EAst.program -> String.t list * String.t **)

let box_to_ocaml p =
  let nms = extract_names (snd p) in
  let p0 = Transform.Transform.run malfunction_pipeline p in
  print_program default_malfunction_config nms p0
