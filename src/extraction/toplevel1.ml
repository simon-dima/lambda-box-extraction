open BasicAst
open BinNums
open Byte
open Clight
open Datatypes
open Kernames
open LambdaANF_to_Clight
open LambdaANF_to_Clight_stack
open LambdaBoxLocal_to_LambdaANF
open List0
open Monad0
open Pipeline_utils
open Bytestring
open CompM
open Cps
open Cps_util
open ExceptionMonad
open Toplevel0

(** val argsIdent : positive **)

let argsIdent =
  Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))

(** val allocIdent : positive **)

let allocIdent =
  Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH)))

(** val nallocIdent : positive **)

let nallocIdent =
  Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))

(** val limitIdent : positive **)

let limitIdent =
  Coq_xI (Coq_xO (Coq_xI (Coq_xI Coq_xH)))

(** val gcIdent : positive **)

let gcIdent =
  Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))

(** val mainIdent : positive **)

let mainIdent =
  Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))

(** val bodyIdent : positive **)

let bodyIdent =
  Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))

(** val threadInfIdent : positive **)

let threadInfIdent =
  Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))

(** val tinfIdent : positive **)

let tinfIdent =
  Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))

(** val heapInfIdent : positive **)

let heapInfIdent =
  Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))

(** val numArgsIdent : positive **)

let numArgsIdent =
  Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))

(** val isptrIdent : positive **)

let isptrIdent =
  Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))

(** val caseIdent : positive **)

let caseIdent =
  Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))

(** val resultIdent : positive **)

let resultIdent =
  Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))

(** val stackframeTIdent : positive **)

let stackframeTIdent =
  Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))

(** val frameIdent : positive **)

let frameIdent =
  Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))

(** val rootIdent : positive **)

let rootIdent =
  Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))

(** val fpIdent : positive **)

let fpIdent =
  Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))

(** val nextFld : positive **)

let nextFld =
  Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))

(** val prevFld : positive **)

let prevFld =
  Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))

type coq_Cprogram = (name_env * program) * program

(** val add_prim_names :
    ((((kername * String.t) * bool) * nat) * positive) list ->
    LambdaBoxLocal_to_LambdaANF.name_env ->
    LambdaBoxLocal_to_LambdaANF.name_env **)

let add_prim_names prims nenv =
  fold_left (fun map pat ->
    let (y, p) = pat in
    let (y0, _) = y in
    let (y1, _) = y0 in let (_, s) = y1 in M.set p (Coq_nNamed s) map) prims
    nenv

(** val coq_Clight_trans :
    String.t -> ((((kername * String.t) * bool) * nat) * positive) list ->
    nat -> coq_LambdaANF_FullTerm -> coq_Cprogram error **)

let coq_Clight_trans bodyName prims args = function
| (l, prog) ->
  let (p, _) = l in
  let (p0, _) = p in
  let (p1, nenv) = p0 in
  let (p2, _) = p1 in
  let (p3, _) = p2 in
  let (p4, cenv) = p3 in
  let (_, p_env) = p4 in
  let p5 =
    LambdaANF_to_Clight.compile argsIdent allocIdent limitIdent gcIdent
      mainIdent bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent
      numArgsIdent isptrIdent caseIdent args p_env prog cenv nenv
  in
  (match p5 with
   | Exc s -> Err s
   | Ret p6 ->
     let (p7, head) = p6 in
     let (nenv0, prog0) = p7 in
     CompM.Ret (((add_prim_names prims nenv0),
     (stripOption mainIdent prog0)), (stripOption mainIdent head)))

(** val coq_Clight_trans_ANF :
    String.t -> ((((kername * String.t) * bool) * nat) * positive) list ->
    nat -> coq_LambdaANF_FullTerm -> coq_Cprogram error * String.t **)

let coq_Clight_trans_ANF bodyName prims args = function
| (l, prog) ->
  let (p, _) = l in
  let (p0, _) = p in
  let (p1, nenv) = p0 in
  let (p2, _) = p1 in
  let (p3, _) = p2 in
  let (p4, cenv) = p3 in
  let (_, pr_env) = p4 in
  let (p5, str) =
    compile argsIdent allocIdent nallocIdent limitIdent gcIdent mainIdent
      bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent numArgsIdent
      isptrIdent caseIdent resultIdent args pr_env stackframeTIdent
      frameIdent rootIdent fpIdent nextFld rootIdent prevFld false prog cenv
      nenv
  in
  (match p5 with
   | Err s -> ((Err s), str)
   | CompM.Ret p6 ->
     let (p7, head) = p6 in
     let (nenv0, prog0) = p7 in
     ((CompM.Ret (((add_prim_names prims nenv0), prog0), head)), str))

(** val compile_Clight :
    ((((kername * String.t) * bool) * nat) * positive) list ->
    (coq_LambdaANF_FullTerm, coq_Cprogram) coq_CertiCoqTrans **)

let compile_Clight prims s =
  Monad0.bind (coq_MonadErrorT CompM.coq_MonadState)
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
      (Coq_x20, (String.String (Coq_x43,
      String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    (fun _ ->
    Monad0.bind (coq_MonadErrorT CompM.coq_MonadState) get_options
      (fun opts ->
      let args = opts.c_args in
      let cps = negb opts.direct in
      if cps
      then coq_LiftErrorCertiCoqTrans (String.String (Coq_x43, (String.String
             (Coq_x6f, (String.String (Coq_x64, (String.String (Coq_x65,
             (String.String (Coq_x67, (String.String (Coq_x65, (String.String
             (Coq_x6e, String.EmptyString))))))))))))))
             (coq_Clight_trans opts.body_name prims args) s
      else coq_LiftErrorLogCertiCoqTrans (String.String (Coq_x43,
             (String.String (Coq_x6f, (String.String (Coq_x64, (String.String
             (Coq_x65, (String.String (Coq_x67, (String.String (Coq_x65,
             (String.String (Coq_x6e, String.EmptyString))))))))))))))
             (coq_Clight_trans_ANF opts.body_name prims args) s))
