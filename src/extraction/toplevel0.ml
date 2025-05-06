open AstCommon
open BinNums
open BinPos
open Byte
open Datatypes
open Kernames
open LambdaBoxLocal_to_LambdaANF
open List0
open Monad0
open PeanoNat
open Pipeline_utils
open Bytestring
open CompM
open Cps
open Dead_param_elim
open Eval
open Hoisting
open Identifiers
open Inline
open Lambda_lifting
open Shrink_cps
open State
open Toplevel
open Uncurry_proto

type __ = Obj.t

type prim_env = (((kername * String.t) * bool) * nat) M.t

type coq_LambdaANFenv =
  ((((((prims * prim_env) * ctor_env) * ctor_tag) * ind_tag) * name_env) * fun_env) * env

type coq_LambdaANFterm = exp

type coq_LambdaANF_FullTerm = coq_LambdaANFenv * coq_LambdaANFterm

(** val default_ctor_tag : positive **)

let default_ctor_tag =
  Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))

(** val default_ind_tag : positive **)

let default_ind_tag =
  Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))

(** val clo_tag : positive **)

let clo_tag =
  Coq_xI (Coq_xI (Coq_xI Coq_xH))

(** val clo_ind_tag : positive **)

let clo_ind_tag =
  Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))

(** val fun_fun_tag : positive **)

let fun_fun_tag =
  Coq_xI Coq_xH

(** val kon_fun_tag : positive **)

let kon_fun_tag =
  Coq_xO Coq_xH

(** val make_prim_env :
    ((((kername * String.t) * bool) * nat) * positive) list -> prim_env **)

let make_prim_env prims0 =
  fold_left (fun map pat -> let (y, p) = pat in M.set p y map) prims0 M.empty

(** val compile_LambdaANF_CPS :
    positive -> ((((kername * String.t) * bool) * nat) * positive) list ->
    (coq_LambdaBoxLocalTerm, coq_LambdaANF_FullTerm) coq_CertiCoqTrans **)

let compile_LambdaANF_CPS next_var0 prims0 src =
  bind (coq_MonadErrorT coq_MonadState)
    (debug_msg (String.String (Coq_x54, (String.String (Coq_x72,
      (String.String (Coq_x61, (String.String (Coq_x6e, (String.String
      (Coq_x73, (String.String (Coq_x6c, (String.String (Coq_x61,
      (String.String (Coq_x74, (String.String (Coq_x69, (String.String
      (Coq_x6e, (String.String (Coq_x67, (String.String (Coq_x20,
      (String.String (Coq_x66, (String.String (Coq_x72, (String.String
      (Coq_x6f, (String.String (Coq_x6d, (String.String (Coq_x20,
      (String.String (Coq_x4c, (String.String (Coq_x61, (String.String
      (Coq_x6d, (String.String (Coq_x62, (String.String (Coq_x64,
      (String.String (Coq_x61, (String.String (Coq_x42, (String.String
      (Coq_x6f, (String.String (Coq_x78, (String.String (Coq_x4c,
      (String.String (Coq_x6f, (String.String (Coq_x63, (String.String
      (Coq_x61, (String.String (Coq_x6c, (String.String (Coq_x20,
      (String.String (Coq_x74, (String.String (Coq_x6f, (String.String
      (Coq_x20, (String.String (Coq_x4c, (String.String (Coq_x61,
      (String.String (Coq_x6d, (String.String (Coq_x62, (String.String
      (Coq_x64, (String.String (Coq_x61, (String.String (Coq_x41,
      (String.String (Coq_x4e, (String.String (Coq_x46, (String.String
      (Coq_x20, (String.String (Coq_x28, (String.String (Coq_x43,
      (String.String (Coq_x50, (String.String (Coq_x53, (String.String
      (Coq_x29,
      String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    (fun _ ->
    coq_LiftErrorCertiCoqTrans (String.String (Coq_x4c, (String.String
      (Coq_x61, (String.String (Coq_x6d, (String.String (Coq_x62,
      (String.String (Coq_x64, (String.String (Coq_x61, (String.String
      (Coq_x41, (String.String (Coq_x4e, (String.String (Coq_x46,
      (String.String (Coq_x20, (String.String (Coq_x43, (String.String
      (Coq_x50, (String.String (Coq_x53,
      String.EmptyString)))))))))))))))))))))))))) (fun p ->
      let prim_env0 = make_prim_env prims0 in
      let (e0, data) =
        convert_top prim_env0 fun_fun_tag kon_fun_tag default_ctor_tag
          default_ind_tag next_var0 p
      in
      (match e0 with
       | Err s -> Err s
       | Ret e ->
         let { next_var = _; nect_ctor_tag = ctag; next_ind_tag = itag;
           next_fun_tag = _; cenv = cenv0; fenv = fenv0; nenv = nenv0;
           inline_map = _; log = _ } = data
         in
         Ret ((((((((M.empty, prim_env0), cenv0), ctag), itag), nenv0),
         fenv0), M.empty), e))) src)

(** val compile_LambdaANF_ANF :
    positive -> ((((kername * String.t) * bool) * nat) * positive) list ->
    (coq_LambdaBoxLocalTerm, coq_LambdaANF_FullTerm) coq_CertiCoqTrans **)

let compile_LambdaANF_ANF next_var0 prims0 src =
  bind (coq_MonadErrorT coq_MonadState)
    (debug_msg (String.String (Coq_x54, (String.String (Coq_x72,
      (String.String (Coq_x61, (String.String (Coq_x6e, (String.String
      (Coq_x73, (String.String (Coq_x6c, (String.String (Coq_x61,
      (String.String (Coq_x74, (String.String (Coq_x69, (String.String
      (Coq_x6e, (String.String (Coq_x67, (String.String (Coq_x20,
      (String.String (Coq_x66, (String.String (Coq_x72, (String.String
      (Coq_x6f, (String.String (Coq_x6d, (String.String (Coq_x20,
      (String.String (Coq_x4c, (String.String (Coq_x61, (String.String
      (Coq_x6d, (String.String (Coq_x62, (String.String (Coq_x64,
      (String.String (Coq_x61, (String.String (Coq_x42, (String.String
      (Coq_x6f, (String.String (Coq_x78, (String.String (Coq_x4c,
      (String.String (Coq_x6f, (String.String (Coq_x63, (String.String
      (Coq_x61, (String.String (Coq_x6c, (String.String (Coq_x20,
      (String.String (Coq_x74, (String.String (Coq_x6f, (String.String
      (Coq_x20, (String.String (Coq_x4c, (String.String (Coq_x61,
      (String.String (Coq_x6d, (String.String (Coq_x62, (String.String
      (Coq_x64, (String.String (Coq_x61, (String.String (Coq_x41,
      (String.String (Coq_x4e, (String.String (Coq_x46, (String.String
      (Coq_x20, (String.String (Coq_x28, (String.String (Coq_x41,
      (String.String (Coq_x4e, (String.String (Coq_x46, (String.String
      (Coq_x29,
      String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    (fun _ ->
    coq_LiftErrorCertiCoqTrans (String.String (Coq_x4c, (String.String
      (Coq_x61, (String.String (Coq_x6d, (String.String (Coq_x62,
      (String.String (Coq_x64, (String.String (Coq_x61, (String.String
      (Coq_x41, (String.String (Coq_x4e, (String.String (Coq_x46,
      (String.String (Coq_x20, (String.String (Coq_x41, (String.String
      (Coq_x4e, (String.String (Coq_x46,
      String.EmptyString)))))))))))))))))))))))))) (fun p ->
      let prim_env0 = make_prim_env prims0 in
      let (e0, data) =
        convert_top_anf prim_env0 fun_fun_tag default_ctor_tag
          default_ind_tag next_var0 p
      in
      (match e0 with
       | Err s -> Err s
       | Ret e ->
         let { next_var = _; nect_ctor_tag = ctag; next_ind_tag = itag;
           next_fun_tag = _; cenv = cenv0; fenv = fenv0; nenv = nenv0;
           inline_map = _; log = _ } = data
         in
         Ret ((((((((M.empty, prim_env0), cenv0), ctag), itag), nenv0),
         fenv0), M.empty), e))) src)

type anf_options = { time : bool; cps : bool; do_lambda_lift : bool;
                     args : nat; no_push : nat; inl_wrappers : bool;
                     inl_known : bool; inl_before : bool; inl_after : 
                     bool; dpe : bool }

type 'a anf_state = comp_data -> 'a error * comp_data

type anf_trans = exp -> exp anf_state

(** val coq_MonadState : __ anf_state coq_Monad **)

let coq_MonadState =
  { ret = (fun _ x c_data -> ((Ret x), c_data)); bind =
    (fun _ _ m f c_data ->
    let (a_err, c_data') = m c_data in
    (match a_err with
     | Err s -> ((Err s), c_data')
     | Ret a -> f a c_data')) }

(** val id_trans : anf_trans **)

let id_trans e c =
  ((Ret e), c)

(** val time_anf :
    anf_options -> String.t -> ('a1 -> 'a2 anf_state) -> 'a1 -> 'a2 anf_state **)

let time_anf anf_opts name f x s =
  if anf_opts.time then timePhase name (f x) s else f x s

(** val anf_pipeline : positive -> anf_options -> exp -> exp anf_state **)

let anf_pipeline next_var0 anf_opts e =
  bind (Obj.magic coq_MonadState)
    (time_anf anf_opts (String.String (Coq_x53, (String.String (Coq_x68,
      (String.String (Coq_x72, (String.String (Coq_x69, (String.String
      (Coq_x6e, (String.String (Coq_x6b, String.EmptyString))))))))))))
      shrink_err e) (fun e0 ->
    bind (Obj.magic coq_MonadState)
      (time_anf anf_opts (String.String (Coq_x55, (String.String (Coq_x6e,
        (String.String (Coq_x63, (String.String (Coq_x75, (String.String
        (Coq_x72, (String.String (Coq_x72, (String.String (Coq_x79,
        String.EmptyString)))))))))))))) (uncurry_top anf_opts.cps) e0)
      (fun e1 ->
      bind (Obj.magic coq_MonadState)
        (time_anf anf_opts (String.String (Coq_x49, (String.String (Coq_x6e,
          (String.String (Coq_x6c, (String.String (Coq_x69, (String.String
          (Coq_x6e, (String.String (Coq_x65, (String.String (Coq_x20,
          (String.String (Coq_x75, (String.String (Coq_x6e, (String.String
          (Coq_x63, (String.String (Coq_x75, (String.String (Coq_x72,
          (String.String (Coq_x72, (String.String (Coq_x79, (String.String
          (Coq_x20, (String.String (Coq_x77, (String.String (Coq_x72,
          (String.String (Coq_x61, (String.String (Coq_x70, (String.String
          (Coq_x70, (String.String (Coq_x65, (String.String (Coq_x72,
          (String.String (Coq_x73,
          String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))
          (inline_uncurry next_var0 (S (S (S (S (S (S (S (S (S (S O))))))))))
            (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
            (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
            (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
            (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
            (S (S (S (S (S (S (S (S (S (S (S (S
            O)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
          e1) (fun e2 ->
        bind (Obj.magic coq_MonadState)
          (if anf_opts.inl_before
           then time_anf anf_opts (String.String (Coq_x49, (String.String
                  (Coq_x6e, (String.String (Coq_x6c, (String.String (Coq_x69,
                  (String.String (Coq_x6e, (String.String (Coq_x65,
                  (String.String (Coq_x2f, (String.String (Coq_x73,
                  (String.String (Coq_x68, (String.String (Coq_x72,
                  (String.String (Coq_x69, (String.String (Coq_x6e,
                  (String.String (Coq_x6b, (String.String (Coq_x20,
                  (String.String (Coq_x6c, (String.String (Coq_x6f,
                  (String.String (Coq_x6f, (String.String (Coq_x70,
                  String.EmptyString))))))))))))))))))))))))))))))))))))
                  (inline_shrink_loop next_var0 (S (S (S (S (S (S (S (S (S (S
                    O)))))))))) (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                    (S (S (S (S (S (S (S (S (S
                    O)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                  e2
           else id_trans e2) (fun e3 ->
          bind (Obj.magic coq_MonadState)
            (if anf_opts.do_lambda_lift
             then time_anf anf_opts (String.String (Coq_x4c, (String.String
                    (Coq_x61, (String.String (Coq_x6d, (String.String
                    (Coq_x62, (String.String (Coq_x64, (String.String
                    (Coq_x61, (String.String (Coq_x20, (String.String
                    (Coq_x6c, (String.String (Coq_x69, (String.String
                    (Coq_x66, (String.String (Coq_x74,
                    String.EmptyString))))))))))))))))))))))
                    (lambda_lift anf_opts.args anf_opts.no_push
                      anf_opts.inl_wrappers) e3
             else id_trans e3) (fun e4 ->
            bind (Obj.magic coq_MonadState)
              (time_anf anf_opts (String.String (Coq_x53, (String.String
                (Coq_x68, (String.String (Coq_x72, (String.String (Coq_x69,
                (String.String (Coq_x6e, (String.String (Coq_x6b,
                String.EmptyString)))))))))))) shrink_err e4) (fun e5 ->
              bind (Obj.magic coq_MonadState)
                (time_anf anf_opts (String.String (Coq_x43, (String.String
                  (Coq_x6c, (String.String (Coq_x6f, (String.String (Coq_x73,
                  (String.String (Coq_x75, (String.String (Coq_x72,
                  (String.String (Coq_x65, (String.String (Coq_x20,
                  (String.String (Coq_x63, (String.String (Coq_x6f,
                  (String.String (Coq_x6e, (String.String (Coq_x76,
                  (String.String (Coq_x65, (String.String (Coq_x72,
                  (String.String (Coq_x73, (String.String (Coq_x69,
                  (String.String (Coq_x6f, (String.String (Coq_x6e,
                  (String.String (Coq_x20, (String.String (Coq_x61,
                  (String.String (Coq_x6e, (String.String (Coq_x64,
                  (String.String (Coq_x20, (String.String (Coq_x68,
                  (String.String (Coq_x6f, (String.String (Coq_x69,
                  (String.String (Coq_x73, (String.String (Coq_x74,
                  (String.String (Coq_x69, (String.String (Coq_x6e,
                  (String.String (Coq_x67,
                  String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                  (closure_conversion_hoist clo_tag clo_ind_tag) e5)
                (fun e6 ->
                bind (Obj.magic coq_MonadState)
                  (time_anf anf_opts (String.String (Coq_x53, (String.String
                    (Coq_x68, (String.String (Coq_x72, (String.String
                    (Coq_x69, (String.String (Coq_x6e, (String.String
                    (Coq_x6b, String.EmptyString)))))))))))) shrink_err e6)
                  (fun e7 ->
                  bind (Obj.magic coq_MonadState)
                    (if anf_opts.inl_after
                     then time_anf anf_opts (String.String (Coq_x49,
                            (String.String (Coq_x6e, (String.String (Coq_x6c,
                            (String.String (Coq_x69, (String.String (Coq_x6e,
                            (String.String (Coq_x65, (String.String (Coq_x2f,
                            (String.String (Coq_x73, (String.String (Coq_x68,
                            (String.String (Coq_x72, (String.String (Coq_x69,
                            (String.String (Coq_x6e, (String.String (Coq_x6b,
                            (String.String (Coq_x20, (String.String (Coq_x6c,
                            (String.String (Coq_x6f, (String.String (Coq_x6f,
                            (String.String (Coq_x70,
                            String.EmptyString))))))))))))))))))))))))))))))))))))
                            (inline_shrink_loop next_var0 (S (S (S (S (S (S
                              (S (S (S (S O)))))))))) (S (S (S (S (S (S (S (S
                              (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                              (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                              (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                              (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                              (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                              (S (S (S (S (S (S (S (S (S (S (S (S
                              O)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                            e7
                     else id_trans e7) (fun e8 ->
                    bind (Obj.magic coq_MonadState)
                      (if anf_opts.dpe
                       then time_anf anf_opts (String.String (Coq_x44,
                              (String.String (Coq_x65, (String.String
                              (Coq_x61, (String.String (Coq_x64,
                              (String.String (Coq_x20, (String.String
                              (Coq_x70, (String.String (Coq_x61,
                              (String.String (Coq_x72, (String.String
                              (Coq_x61, (String.String (Coq_x6d,
                              (String.String (Coq_x20, (String.String
                              (Coq_x65, (String.String (Coq_x6c,
                              (String.String (Coq_x69, (String.String
                              (Coq_x6d,
                              String.EmptyString))))))))))))))))))))))))))))))
                              coq_DPE e8
                       else id_trans e8) (fun e9 ->
                      bind (Obj.magic coq_MonadState)
                        (time_anf anf_opts (String.String (Coq_x53,
                          (String.String (Coq_x68, (String.String (Coq_x72,
                          (String.String (Coq_x69, (String.String (Coq_x6e,
                          (String.String (Coq_x6b,
                          String.EmptyString)))))))))))) shrink_err e9)
                        (fun e10 ->
                        bind (Obj.magic coq_MonadState)
                          (if anf_opts.inl_known
                           then time_anf anf_opts (String.String (Coq_x49,
                                  (String.String (Coq_x6e, (String.String
                                  (Coq_x6c, (String.String (Coq_x69,
                                  (String.String (Coq_x6e, (String.String
                                  (Coq_x65, (String.String (Coq_x20,
                                  (String.String (Coq_x6b, (String.String
                                  (Coq_x6e, (String.String (Coq_x6f,
                                  (String.String (Coq_x77, (String.String
                                  (Coq_x6e, (String.String (Coq_x20,
                                  (String.String (Coq_x66, (String.String
                                  (Coq_x75, (String.String (Coq_x6e,
                                  (String.String (Coq_x63, (String.String
                                  (Coq_x74, (String.String (Coq_x69,
                                  (String.String (Coq_x6f, (String.String
                                  (Coq_x6e, (String.String (Coq_x73,
                                  (String.String (Coq_x20, (String.String
                                  (Coq_x69, (String.String (Coq_x6e,
                                  (String.String (Coq_x73, (String.String
                                  (Coq_x69, (String.String (Coq_x64,
                                  (String.String (Coq_x65, (String.String
                                  (Coq_x20, (String.String (Coq_x77,
                                  (String.String (Coq_x72, (String.String
                                  (Coq_x61, (String.String (Coq_x70,
                                  (String.String (Coq_x70, (String.String
                                  (Coq_x65, (String.String (Coq_x72,
                                  (String.String (Coq_x73,
                                  String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                                  (inline_lifted next_var0 (S (S (S (S (S (S
                                    (S (S (S (S O)))))))))) (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    O)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                                  e10
                           else id_trans e10) (fun e11 ->
                          ret (Obj.magic coq_MonadState) e11))))))))))))

(** val run_anf_pipeline :
    positive -> anf_options -> coq_LambdaANF_FullTerm ->
    coq_LambdaANF_FullTerm error * String.t **)

let run_anf_pipeline next_var0 anf_opts = function
| (l, e0) ->
  let (p, _) = l in
  let (p0, fenv0) = p in
  let (p1, nenv0) = p0 in
  let (p2, itag) = p1 in
  let (p3, ctag) = p2 in
  let (prims0, cenv0) = p3 in
  let c_data =
    let next_var1 = Pos.add (max_var e0 Coq_xH) Coq_xH in
    let next_fun_tag0 =
      Pos.add (M.fold (fun cm ft _ -> Pos.max cm ft) fenv0 Coq_xH) Coq_xH
    in
    pack_data next_var1 ctag itag next_fun_tag0 cenv0 fenv0 nenv0 M.empty []
  in
  let (res, c_data') = anf_pipeline next_var0 anf_opts e0 c_data in
  (match res with
   | Err s ->
     ((Err
       (String.append (String.String (Coq_x46, (String.String (Coq_x61,
         (String.String (Coq_x69, (String.String (Coq_x6c, (String.String
         (Coq_x65, (String.String (Coq_x64, (String.String (Coq_x20,
         (String.String (Coq_x63, (String.String (Coq_x6f, (String.String
         (Coq_x6d, (String.String (Coq_x70, (String.String (Coq_x69,
         (String.String (Coq_x6c, (String.String (Coq_x69, (String.String
         (Coq_x6e, (String.String (Coq_x67, (String.String (Coq_x20,
         (String.String (Coq_x4c, (String.String (Coq_x61, (String.String
         (Coq_x6d, (String.String (Coq_x62, (String.String (Coq_x64,
         (String.String (Coq_x61, (String.String (Coq_x41, (String.String
         (Coq_x4e, (String.String (Coq_x46, (String.String (Coq_x20,
         (String.String (Coq_x70, (String.String (Coq_x72, (String.String
         (Coq_x6f, (String.String (Coq_x67, (String.String (Coq_x72,
         (String.String (Coq_x61, (String.String (Coq_x6d, (String.String
         (Coq_x3a, (String.String (Coq_x20,
         String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
         s)), String.EmptyString)
   | Ret e ->
     let { next_var = _; nect_ctor_tag = ctag0; next_ind_tag = itag0;
       next_fun_tag = _; cenv = cenv1; fenv = fenv1; nenv = nenv1;
       inline_map = _; log = log0 } = c_data'
     in
     ((Ret (((((((prims0, cenv1), ctag0), itag0), nenv1), fenv1), M.empty),
     e)), (log_to_string log0)))

(** val make_anf_options : coq_Options -> anf_options **)

let make_anf_options opts =
  let (p, inl_after0) =
    let default = ((((true, false), (S O)), true), true) in
    (match opts.anf_conf with
     | O -> default
     | S n ->
       (match n with
        | O -> ((((false, false), (S O)), true), true)
        | S n0 ->
          (match n0 with
           | O -> ((((true, true), (S O)), true), true)
           | S n1 ->
             (match n1 with
              | O -> ((((true, false), O), true), true)
              | S n2 ->
                (match n2 with
                 | O -> ((((true, false), (S (S O))), true), true)
                 | S n3 ->
                   (match n3 with
                    | O ->
                      ((((true, false), (S (S (S (S (S (S (S (S (S (S
                        O))))))))))), true), true)
                    | S n4 ->
                      (match n4 with
                       | O -> ((((true, false), (S O)), false), true)
                       | S n5 ->
                         (match n5 with
                          | O -> ((((true, false), (S O)), true), false)
                          | S n6 ->
                            (match n6 with
                             | O -> ((((true, false), (S O)), false), false)
                             | S n7 ->
                               (match n7 with
                                | O ->
                                  ((((true, false), (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                                    (S (S (S (S (S (S (S (S (S (S (S (S
                                    O))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))),
                                    true), true)
                                | S _ -> default))))))))))
  in
  let (p0, inl_before0) = p in
  let (p1, no_push0) = p0 in
  let (inl_wrappers0, inl_known0) = p1 in
  { time = opts.Pipeline_utils.time; cps = (negb opts.direct);
  do_lambda_lift = (Nat.leb (S O) opts.o_level); args =
  (if Nat.eqb opts.anf_conf (S (S (S (S (S (S (S (S (S O)))))))))
   then S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S
          O)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
   else opts.c_args); no_push = no_push0; inl_wrappers = inl_wrappers0;
  inl_known = inl_known0; inl_before = inl_before0; inl_after = inl_after0;
  dpe = true }

(** val compile_LambdaANF :
    positive -> (coq_LambdaANF_FullTerm, coq_LambdaANF_FullTerm)
    coq_CertiCoqTrans **)

let compile_LambdaANF next_var0 src =
  bind (coq_MonadErrorT CompM.coq_MonadState)
    (debug_msg (String.String (Coq_x43, (String.String (Coq_x6f,
      (String.String (Coq_x6d, (String.String (Coq_x70, (String.String
      (Coq_x69, (String.String (Coq_x6c, (String.String (Coq_x69,
      (String.String (Coq_x6e, (String.String (Coq_x67, (String.String
      (Coq_x20, (String.String (Coq_x4c, (String.String (Coq_x61,
      (String.String (Coq_x6d, (String.String (Coq_x62, (String.String
      (Coq_x64, (String.String (Coq_x61, (String.String (Coq_x41,
      (String.String (Coq_x4e, (String.String (Coq_x46,
      String.EmptyString))))))))))))))))))))))))))))))))))))))) (fun _ ->
    bind (coq_MonadErrorT CompM.coq_MonadState) get_options (fun opts ->
      let anf_opts = make_anf_options opts in
      coq_LiftErrorLogCertiCoqTrans (String.String (Coq_x4c, (String.String
        (Coq_x61, (String.String (Coq_x6d, (String.String (Coq_x62,
        (String.String (Coq_x64, (String.String (Coq_x61, (String.String
        (Coq_x41, (String.String (Coq_x4e, (String.String (Coq_x46,
        (String.String (Coq_x20, (String.String (Coq_x50, (String.String
        (Coq_x69, (String.String (Coq_x70, (String.String (Coq_x65,
        (String.String (Coq_x6c, (String.String (Coq_x69, (String.String
        (Coq_x6e, (String.String (Coq_x65,
        String.EmptyString))))))))))))))))))))))))))))))))))))
        (run_anf_pipeline next_var0 anf_opts) src))

(** val compile_LambdaANF_debug :
    positive -> (coq_LambdaANF_FullTerm, coq_LambdaANF_FullTerm)
    coq_CertiCoqTrans **)

let compile_LambdaANF_debug next_var0 src =
  bind (coq_MonadErrorT CompM.coq_MonadState)
    (debug_msg (String.String (Coq_x43, (String.String (Coq_x6f,
      (String.String (Coq_x6d, (String.String (Coq_x70, (String.String
      (Coq_x69, (String.String (Coq_x6c, (String.String (Coq_x69,
      (String.String (Coq_x6e, (String.String (Coq_x67, (String.String
      (Coq_x20, (String.String (Coq_x4c, (String.String (Coq_x61,
      (String.String (Coq_x6d, (String.String (Coq_x62, (String.String
      (Coq_x64, (String.String (Coq_x61, (String.String (Coq_x41,
      (String.String (Coq_x4e, (String.String (Coq_x46,
      String.EmptyString))))))))))))))))))))))))))))))))))))))) (fun _ ->
    bind (coq_MonadErrorT CompM.coq_MonadState) get_options (fun opts ->
      let anf_opts = make_anf_options opts in
      coq_LiftErrorLogCertiCoqTrans (String.String (Coq_x4c, (String.String
        (Coq_x61, (String.String (Coq_x6d, (String.String (Coq_x62,
        (String.String (Coq_x64, (String.String (Coq_x61, (String.String
        (Coq_x41, (String.String (Coq_x4e, (String.String (Coq_x46,
        (String.String (Coq_x20, (String.String (Coq_x50, (String.String
        (Coq_x69, (String.String (Coq_x70, (String.String (Coq_x65,
        (String.String (Coq_x6c, (String.String (Coq_x69, (String.String
        (Coq_x6e, (String.String (Coq_x65,
        String.EmptyString))))))))))))))))))))))))))))))))))))
        (run_anf_pipeline next_var0 anf_opts) src))
