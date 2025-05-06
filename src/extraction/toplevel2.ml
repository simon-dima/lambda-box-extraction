open BasicAst
open BinNums
open Byte
open Datatypes
open Kernames
open LambdaANF_to_Wasm
open List0
open Monad0
open Pipeline_utils
open Bytestring
open CompM
open Cps
open Cps_show
open Datatypes0
open Toplevel0

(** val add_prim_names :
    ((((kername * String.t) * bool) * nat) * positive) list -> name_env ->
    name_env **)

let add_prim_names prims nenv =
  fold_left (fun m pat ->
    let (y, p) = pat in
    let (y0, _) = y in
    let (y2, _) = y0 in let (_, s) = y2 in M.set p (Coq_nNamed s) m) prims
    nenv

(** val ensure_top_level_Efun : exp -> exp **)

let ensure_top_level_Efun prog = match prog with
| Efun (_, _) -> prog
| _ -> Efun (Fnil, prog)

(** val coq_LambdaANF_to_Wasm_Wrapper :
    ((((kername * String.t) * bool) * nat) * positive) list -> nat ->
    coq_LambdaANF_FullTerm -> coq_module error * String.t **)

let coq_LambdaANF_to_Wasm_Wrapper prims _ = function
| (l, prog) ->
  let (p, _) = l in
  let (p0, _) = p in
  let (p1, nenv) = p0 in
  let (p2, _) = p1 in
  let (p3, _) = p2 in
  let (p4, cenv) = p3 in
  let (_, pr_env) = p4 in
  let nenv' = add_prim_names prims nenv in
  let prog' = ensure_top_level_Efun prog in
  (match coq_LambdaANF_to_Wasm nenv' cenv pr_env prog' with
   | Err err -> ((Err err), String.EmptyString)
   | Ret res ->
     let (p5, _) = res in
     let (module0, _) = p5 in ((Ret module0), String.EmptyString))

(** val compile_LambdaANF_to_Wasm :
    ((((kername * String.t) * bool) * nat) * positive) list ->
    (coq_LambdaANF_FullTerm, coq_module) coq_CertiCoqTrans **)

let compile_LambdaANF_to_Wasm prims s =
  bind (coq_MonadErrorT CompM.coq_MonadState)
    (debug_msg (String.String (Coq_x54, (String.String (Coq_x72,
      (String.String (Coq_x61, (String.String (Coq_x6e, (String.String
      (Coq_x73, (String.String (Coq_x6c, (String.String (Coq_x61,
      (String.String (Coq_x74, (String.String (Coq_x69, (String.String
      (Coq_x6e, (String.String (Coq_x67, (String.String (Coq_x20,
      (String.String (Coq_x66, (String.String (Coq_x72, (String.String
      (Coq_x6f, (String.String (Coq_x6d, (String.String (Coq_x20,
      (String.String (Coq_x4c, (String.String (Coq_x61, (String.String
      (Coq_x6d, (String.String (Coq_x62, (String.String (Coq_x64,
      (String.String (Coq_x61, (String.String (Coq_x41, (String.String
      (Coq_x4e, (String.String (Coq_x46, (String.String (Coq_x20,
      (String.String (Coq_x74, (String.String (Coq_x6f, (String.String
      (Coq_x20, (String.String (Coq_x57, (String.String (Coq_x61,
      (String.String (Coq_x73, (String.String (Coq_x6d,
      String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    (fun _ ->
    bind (coq_MonadErrorT CompM.coq_MonadState) get_options (fun opts ->
      let args = opts.c_args in
      coq_LiftErrorLogCertiCoqTrans (String.String (Coq_x43, (String.String
        (Coq_x6f, (String.String (Coq_x64, (String.String (Coq_x65,
        (String.String (Coq_x67, (String.String (Coq_x65, (String.String
        (Coq_x6e, (String.String (Coq_x57, (String.String (Coq_x61,
        (String.String (Coq_x73, (String.String (Coq_x6d,
        String.EmptyString))))))))))))))))))))))
        (coq_LambdaANF_to_Wasm_Wrapper prims args) s))
