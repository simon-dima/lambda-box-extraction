open AstCommon
open BinNums
open BinPos
open Byte
open CertiCoqPipeline
open Datatypes
open EAst
open List_util
open MCString
open Monad0
open PeanoNat
open Pipeline_utils
open Bytestring
open CompM
open Compile0
open Cps
open Cps_show
open Cps_util
open Eval
open ExceptionMonad
open Map_util
open Term0
open Toplevel0
open WcbvEval

(** val show_var : Cps_show.name_env -> positive -> String.t **)

let show_var nenv v =
  show_tree (show_var nenv v)

(** val show_tag : fun_tag -> String.t **)

let show_tag t0 =
  show_tree (show_ftag true t0)

(** val show_tags : fun_tag -> fun_tag -> String.t **)

let show_tags t0 t' =
  String.append (String.String (Coq_x28, String.EmptyString))
    (String.append (show_tag t0)
      (String.append (String.String (Coq_x2c, (String.String (Coq_x20,
        String.EmptyString))))
        (String.append (show_tag t') (String.String (Coq_x29,
          String.EmptyString)))))

(** val bstep_f :
    prims -> ctor_env -> Cps_show.name_env -> env -> exp -> nat ->
    (env * exp, coq_val) sum coq_exception **)

let rec bstep_f pr cenv nenv rho e = function
| O -> Ret (Coq_inl (rho, e))
| S n' ->
  (match e with
   | Econstr (x, t0, ys, e') ->
     bind
       (l_opt (get_list ys rho) (String.String (Coq_x45, (String.String
         (Coq_x63, (String.String (Coq_x6f, (String.String (Coq_x6e,
         (String.String (Coq_x73, (String.String (Coq_x74, (String.String
         (Coq_x72, (String.String (Coq_x3a, (String.String (Coq_x20,
         (String.String (Coq_x66, (String.String (Coq_x61, (String.String
         (Coq_x69, (String.String (Coq_x6c, (String.String (Coq_x65,
         (String.String (Coq_x64, (String.String (Coq_x20, (String.String
         (Coq_x74, (String.String (Coq_x6f, (String.String (Coq_x20,
         (String.String (Coq_x67, (String.String (Coq_x65, (String.String
         (Coq_x74, (String.String (Coq_x20, (String.String (Coq_x61,
         (String.String (Coq_x72, (String.String (Coq_x67, (String.String
         (Coq_x73,
         String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))
       (fun vs ->
       let rho' = Cps.M.set x (Vconstr (t0, vs)) rho in
       bstep_f pr cenv nenv rho' e' n')
   | Ecase (y, cl) ->
     (match Cps.M.get y rho with
      | Some v ->
        (match v with
         | Vconstr (t0, _) ->
           bind
             (l_opt (findtag cl t0)
               (String.append (String.String (Coq_x43, (String.String
                 (Coq_x61, (String.String (Coq_x73, (String.String (Coq_x65,
                 (String.String (Coq_x3a, (String.String (Coq_x20,
                 String.EmptyString))))))))))))
                 (String.append (show_tag t0) (String.String (Coq_x20,
                   (String.String (Coq_x62, (String.String (Coq_x72,
                   (String.String (Coq_x61, (String.String (Coq_x6e,
                   (String.String (Coq_x63, (String.String (Coq_x68,
                   (String.String (Coq_x20, (String.String (Coq_x6e,
                   (String.String (Coq_x6f, (String.String (Coq_x74,
                   (String.String (Coq_x20, (String.String (Coq_x66,
                   (String.String (Coq_x6f, (String.String (Coq_x75,
                   (String.String (Coq_x6e, (String.String (Coq_x64,
                   String.EmptyString)))))))))))))))))))))))))))))))))))))
             (fun e0 ->
             if caseConsistent_f cenv cl t0
             then bstep_f pr cenv nenv rho e0 n'
             else Exc (String.String (Coq_x43, (String.String (Coq_x61,
                    (String.String (Coq_x73, (String.String (Coq_x65,
                    (String.String (Coq_x3a, (String.String (Coq_x20,
                    (String.String (Coq_x63, (String.String (Coq_x6f,
                    (String.String (Coq_x6e, (String.String (Coq_x73,
                    (String.String (Coq_x69, (String.String (Coq_x73,
                    (String.String (Coq_x74, (String.String (Coq_x65,
                    (String.String (Coq_x6e, (String.String (Coq_x63,
                    (String.String (Coq_x79, (String.String (Coq_x20,
                    (String.String (Coq_x66, (String.String (Coq_x61,
                    (String.String (Coq_x69, (String.String (Coq_x6c,
                    (String.String (Coq_x75, (String.String (Coq_x72,
                    (String.String (Coq_x65,
                    String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))
         | _ ->
           Exc
             (String.append (String.String (Coq_x43, (String.String (Coq_x61,
               (String.String (Coq_x73, (String.String (Coq_x65,
               (String.String (Coq_x3a, (String.String (Coq_x20,
               String.EmptyString))))))))))))
               (String.append (show_var nenv y) (String.String (Coq_x20,
                 (String.String (Coq_x62, (String.String (Coq_x72,
                 (String.String (Coq_x61, (String.String (Coq_x6e,
                 (String.String (Coq_x63, (String.String (Coq_x68,
                 (String.String (Coq_x20, (String.String (Coq_x6e,
                 (String.String (Coq_x6f, (String.String (Coq_x74,
                 (String.String (Coq_x20, (String.String (Coq_x66,
                 (String.String (Coq_x6f, (String.String (Coq_x75,
                 (String.String (Coq_x6e, (String.String (Coq_x64,
                 String.EmptyString)))))))))))))))))))))))))))))))))))))
      | None ->
        Exc
          (String.append (String.String (Coq_x43, (String.String (Coq_x61,
            (String.String (Coq_x73, (String.String (Coq_x65, (String.String
            (Coq_x3a, (String.String (Coq_x20, String.EmptyString))))))))))))
            (String.append (show_var nenv y) (String.String (Coq_x20,
              (String.String (Coq_x62, (String.String (Coq_x72,
              (String.String (Coq_x61, (String.String (Coq_x6e,
              (String.String (Coq_x63, (String.String (Coq_x68,
              (String.String (Coq_x20, (String.String (Coq_x6e,
              (String.String (Coq_x6f, (String.String (Coq_x74,
              (String.String (Coq_x20, (String.String (Coq_x66,
              (String.String (Coq_x6f, (String.String (Coq_x75,
              (String.String (Coq_x6e, (String.String (Coq_x64,
              String.EmptyString)))))))))))))))))))))))))))))))))))))
   | Eproj (x, t0, m, y, e') ->
     (match Cps.M.get y rho with
      | Some v ->
        (match v with
         | Vconstr (t', vs) ->
           if Pos.eqb t0 t'
           then bind
                  (l_opt (nthN vs m) (String.String (Coq_x45, (String.String
                    (Coq_x70, (String.String (Coq_x72, (String.String
                    (Coq_x6f, (String.String (Coq_x6a, (String.String
                    (Coq_x3a, (String.String (Coq_x20, (String.String
                    (Coq_x70, (String.String (Coq_x72, (String.String
                    (Coq_x6f, (String.String (Coq_x6a, (String.String
                    (Coq_x65, (String.String (Coq_x63, (String.String
                    (Coq_x74, (String.String (Coq_x69, (String.String
                    (Coq_x6f, (String.String (Coq_x6e, (String.String
                    (Coq_x20, (String.String (Coq_x66, (String.String
                    (Coq_x61, (String.String (Coq_x69, (String.String
                    (Coq_x6c, (String.String (Coq_x65, (String.String
                    (Coq_x64,
                    String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))
                  (fun v0 ->
                  let rho' = Cps.M.set x v0 rho in
                  bstep_f pr cenv nenv rho' e' n')
           else Exc
                  (String.append (String.String (Coq_x50, (String.String
                    (Coq_x72, (String.String (Coq_x6f, (String.String
                    (Coq_x6a, (String.String (Coq_x3a, (String.String
                    (Coq_x20, (String.String (Coq_x74, (String.String
                    (Coq_x61, (String.String (Coq_x67, (String.String
                    (Coq_x20, (String.String (Coq_x63, (String.String
                    (Coq_x68, (String.String (Coq_x65, (String.String
                    (Coq_x63, (String.String (Coq_x6b, (String.String
                    (Coq_x20, (String.String (Coq_x66, (String.String
                    (Coq_x61, (String.String (Coq_x69, (String.String
                    (Coq_x6c, (String.String (Coq_x65, (String.String
                    (Coq_x64, (String.String (Coq_x20,
                    String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))
                    (show_tags t0 t'))
         | _ ->
           Exc
             (String.append (String.String (Coq_x50, (String.String (Coq_x72,
               (String.String (Coq_x6f, (String.String (Coq_x6a,
               (String.String (Coq_x3a, (String.String (Coq_x20,
               String.EmptyString))))))))))))
               (String.append (show_var nenv y) (String.String (Coq_x20,
                 (String.String (Coq_x76, (String.String (Coq_x61,
                 (String.String (Coq_x72, (String.String (Coq_x20,
                 (String.String (Coq_x6e, (String.String (Coq_x6f,
                 (String.String (Coq_x74, (String.String (Coq_x20,
                 (String.String (Coq_x66, (String.String (Coq_x6f,
                 (String.String (Coq_x75, (String.String (Coq_x6e,
                 (String.String (Coq_x64,
                 String.EmptyString)))))))))))))))))))))))))))))))
      | None ->
        Exc
          (String.append (String.String (Coq_x50, (String.String (Coq_x72,
            (String.String (Coq_x6f, (String.String (Coq_x6a, (String.String
            (Coq_x3a, (String.String (Coq_x20, String.EmptyString))))))))))))
            (String.append (show_var nenv y) (String.String (Coq_x20,
              (String.String (Coq_x76, (String.String (Coq_x61,
              (String.String (Coq_x72, (String.String (Coq_x20,
              (String.String (Coq_x6e, (String.String (Coq_x6f,
              (String.String (Coq_x74, (String.String (Coq_x20,
              (String.String (Coq_x66, (String.String (Coq_x6f,
              (String.String (Coq_x75, (String.String (Coq_x6e,
              (String.String (Coq_x64,
              String.EmptyString)))))))))))))))))))))))))))))))
   | Eletapp (x, f, t0, ys, e0) ->
     (match Cps.M.get f rho with
      | Some v ->
        (match v with
         | Vfun (rho', fl, f') ->
           bind
             (l_opt (get_list ys rho) (String.String (Coq_x45, (String.String
               (Coq_x6c, (String.String (Coq_x65, (String.String (Coq_x74,
               (String.String (Coq_x61, (String.String (Coq_x70,
               (String.String (Coq_x70, (String.String (Coq_x3a,
               (String.String (Coq_x20, (String.String (Coq_x66,
               (String.String (Coq_x61, (String.String (Coq_x69,
               (String.String (Coq_x6c, (String.String (Coq_x65,
               (String.String (Coq_x64, (String.String (Coq_x20,
               (String.String (Coq_x74, (String.String (Coq_x6f,
               (String.String (Coq_x20, (String.String (Coq_x67,
               (String.String (Coq_x65, (String.String (Coq_x74,
               (String.String (Coq_x20, (String.String (Coq_x61,
               (String.String (Coq_x72, (String.String (Coq_x67,
               (String.String (Coq_x73,
               String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))
             (fun vs ->
             match find_def f' fl with
             | Some p ->
               let (p0, e_body) = p in
               let (t', xs) = p0 in
               if Pos.eqb t0 t'
               then bind
                      (l_opt (set_lists xs vs (def_funs fl fl rho' rho'))
                        (String.String (Coq_x45, (String.String (Coq_x6c,
                        (String.String (Coq_x65, (String.String (Coq_x74,
                        (String.String (Coq_x61, (String.String (Coq_x70,
                        (String.String (Coq_x70, (String.String (Coq_x3a,
                        (String.String (Coq_x20, (String.String (Coq_x73,
                        (String.String (Coq_x65, (String.String (Coq_x74,
                        (String.String (Coq_x5f, (String.String (Coq_x6c,
                        (String.String (Coq_x69, (String.String (Coq_x73,
                        (String.String (Coq_x74, (String.String (Coq_x73,
                        (String.String (Coq_x20, (String.String (Coq_x66,
                        (String.String (Coq_x61, (String.String (Coq_x69,
                        (String.String (Coq_x6c, (String.String (Coq_x65,
                        (String.String (Coq_x64,
                        String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))
                      (fun rho'' ->
                      bind (bstep_f pr cenv nenv rho'' e_body n') (fun v0 ->
                        match v0 with
                        | Coq_inl st -> Ret (Coq_inl st)
                        | Coq_inr v1 ->
                          bstep_f pr cenv nenv (Cps.M.set x v1 rho) e0 n'))
               else Exc
                      (String.append (String.String (Coq_x45, (String.String
                        (Coq_x6c, (String.String (Coq_x65, (String.String
                        (Coq_x74, (String.String (Coq_x61, (String.String
                        (Coq_x70, (String.String (Coq_x70, (String.String
                        (Coq_x3a, (String.String (Coq_x20, (String.String
                        (Coq_x74, (String.String (Coq_x61, (String.String
                        (Coq_x67, (String.String (Coq_x20, (String.String
                        (Coq_x63, (String.String (Coq_x68, (String.String
                        (Coq_x65, (String.String (Coq_x63, (String.String
                        (Coq_x6b, (String.String (Coq_x20, (String.String
                        (Coq_x66, (String.String (Coq_x61, (String.String
                        (Coq_x69, (String.String (Coq_x6c, (String.String
                        (Coq_x65, (String.String (Coq_x64, (String.String
                        (Coq_x20,
                        String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))
                        (show_tags t0 t'))
             | None ->
               Exc (String.String (Coq_x45, (String.String (Coq_x6c,
                 (String.String (Coq_x65, (String.String (Coq_x74,
                 (String.String (Coq_x61, (String.String (Coq_x70,
                 (String.String (Coq_x70, (String.String (Coq_x3a,
                 (String.String (Coq_x20, (String.String (Coq_x66,
                 (String.String (Coq_x75, (String.String (Coq_x6e,
                 (String.String (Coq_x63, (String.String (Coq_x74,
                 (String.String (Coq_x69, (String.String (Coq_x6f,
                 (String.String (Coq_x6e, (String.String (Coq_x20,
                 (String.String (Coq_x6e, (String.String (Coq_x6f,
                 (String.String (Coq_x74, (String.String (Coq_x20,
                 (String.String (Coq_x66, (String.String (Coq_x6f,
                 (String.String (Coq_x75, (String.String (Coq_x6e,
                 (String.String (Coq_x64, (String.String (Coq_x20,
                 (String.String (Coq_x69, (String.String (Coq_x6e,
                 (String.String (Coq_x20, (String.String (Coq_x62,
                 (String.String (Coq_x75, (String.String (Coq_x6e,
                 (String.String (Coq_x64, (String.String (Coq_x6c,
                 (String.String (Coq_x65,
                 String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
         | _ ->
           Exc
             (String.append (String.String (Coq_x45, (String.String (Coq_x6c,
               (String.String (Coq_x65, (String.String (Coq_x74,
               (String.String (Coq_x61, (String.String (Coq_x70,
               (String.String (Coq_x70, (String.String (Coq_x3a,
               (String.String (Coq_x20, String.EmptyString))))))))))))))))))
               (String.append (show_var nenv f) (String.String (Coq_x20,
                 (String.String (Coq_x62, (String.String (Coq_x75,
                 (String.String (Coq_x6e, (String.String (Coq_x64,
                 (String.String (Coq_x6c, (String.String (Coq_x65,
                 (String.String (Coq_x20, (String.String (Coq_x6e,
                 (String.String (Coq_x6f, (String.String (Coq_x74,
                 (String.String (Coq_x20, (String.String (Coq_x66,
                 (String.String (Coq_x6f, (String.String (Coq_x75,
                 (String.String (Coq_x6e, (String.String (Coq_x64,
                 String.EmptyString)))))))))))))))))))))))))))))))))))))
      | None ->
        Exc
          (String.append (String.String (Coq_x45, (String.String (Coq_x6c,
            (String.String (Coq_x65, (String.String (Coq_x74, (String.String
            (Coq_x61, (String.String (Coq_x70, (String.String (Coq_x70,
            (String.String (Coq_x3a, (String.String (Coq_x20,
            String.EmptyString))))))))))))))))))
            (String.append (show_var nenv f) (String.String (Coq_x20,
              (String.String (Coq_x62, (String.String (Coq_x75,
              (String.String (Coq_x6e, (String.String (Coq_x64,
              (String.String (Coq_x6c, (String.String (Coq_x65,
              (String.String (Coq_x20, (String.String (Coq_x6e,
              (String.String (Coq_x6f, (String.String (Coq_x74,
              (String.String (Coq_x20, (String.String (Coq_x66,
              (String.String (Coq_x6f, (String.String (Coq_x75,
              (String.String (Coq_x6e, (String.String (Coq_x64,
              String.EmptyString)))))))))))))))))))))))))))))))))))))
   | Efun (fl, e') ->
     let rho' = def_funs fl fl rho rho in bstep_f pr cenv nenv rho' e' n'
   | Eapp (f, t0, ys) ->
     (match Cps.M.get f rho with
      | Some v ->
        (match v with
         | Vfun (rho', fl, f') ->
           bind
             (l_opt (get_list ys rho) (String.String (Coq_x45, (String.String
               (Coq_x61, (String.String (Coq_x70, (String.String (Coq_x70,
               (String.String (Coq_x3a, (String.String (Coq_x20,
               (String.String (Coq_x66, (String.String (Coq_x61,
               (String.String (Coq_x69, (String.String (Coq_x6c,
               (String.String (Coq_x65, (String.String (Coq_x64,
               (String.String (Coq_x20, (String.String (Coq_x74,
               (String.String (Coq_x6f, (String.String (Coq_x20,
               (String.String (Coq_x67, (String.String (Coq_x65,
               (String.String (Coq_x74, (String.String (Coq_x20,
               (String.String (Coq_x61, (String.String (Coq_x72,
               (String.String (Coq_x67, (String.String (Coq_x73,
               String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))
             (fun vs ->
             match find_def f' fl with
             | Some p ->
               let (p0, e0) = p in
               let (t', xs) = p0 in
               if Pos.eqb t0 t'
               then bind
                      (l_opt (set_lists xs vs (def_funs fl fl rho' rho'))
                        (String.String (Coq_x46, (String.String (Coq_x75,
                        (String.String (Coq_x6e, (String.String (Coq_x3a,
                        (String.String (Coq_x20, (String.String (Coq_x73,
                        (String.String (Coq_x65, (String.String (Coq_x74,
                        (String.String (Coq_x5f, (String.String (Coq_x6c,
                        (String.String (Coq_x69, (String.String (Coq_x73,
                        (String.String (Coq_x74, (String.String (Coq_x73,
                        (String.String (Coq_x20, (String.String (Coq_x66,
                        (String.String (Coq_x61, (String.String (Coq_x69,
                        (String.String (Coq_x6c, (String.String (Coq_x65,
                        (String.String (Coq_x64,
                        String.EmptyString)))))))))))))))))))))))))))))))))))))))))))
                      (fun rho'' -> bstep_f pr cenv nenv rho'' e0 n')
               else Exc
                      (String.append (String.String (Coq_x45, (String.String
                        (Coq_x61, (String.String (Coq_x70, (String.String
                        (Coq_x70, (String.String (Coq_x3a, (String.String
                        (Coq_x20, (String.String (Coq_x74, (String.String
                        (Coq_x61, (String.String (Coq_x67, (String.String
                        (Coq_x20, (String.String (Coq_x63, (String.String
                        (Coq_x68, (String.String (Coq_x65, (String.String
                        (Coq_x63, (String.String (Coq_x6b, (String.String
                        (Coq_x20, (String.String (Coq_x66, (String.String
                        (Coq_x61, (String.String (Coq_x69, (String.String
                        (Coq_x6c, (String.String (Coq_x65, (String.String
                        (Coq_x64, (String.String (Coq_x20,
                        String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))
                        (show_tags t0 t'))
             | None ->
               Exc (String.String (Coq_x45, (String.String (Coq_x61,
                 (String.String (Coq_x70, (String.String (Coq_x70,
                 (String.String (Coq_x3a, (String.String (Coq_x20,
                 (String.String (Coq_x66, (String.String (Coq_x75,
                 (String.String (Coq_x6e, (String.String (Coq_x63,
                 (String.String (Coq_x74, (String.String (Coq_x69,
                 (String.String (Coq_x6f, (String.String (Coq_x6e,
                 (String.String (Coq_x20, (String.String (Coq_x6e,
                 (String.String (Coq_x6f, (String.String (Coq_x74,
                 (String.String (Coq_x20, (String.String (Coq_x66,
                 (String.String (Coq_x6f, (String.String (Coq_x75,
                 (String.String (Coq_x6e, (String.String (Coq_x64,
                 (String.String (Coq_x20, (String.String (Coq_x69,
                 (String.String (Coq_x6e, (String.String (Coq_x20,
                 (String.String (Coq_x62, (String.String (Coq_x75,
                 (String.String (Coq_x6e, (String.String (Coq_x64,
                 (String.String (Coq_x6c, (String.String (Coq_x65,
                 String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
         | _ ->
           Exc
             (String.append (String.String (Coq_x45, (String.String (Coq_x61,
               (String.String (Coq_x70, (String.String (Coq_x70,
               (String.String (Coq_x3a, (String.String (Coq_x20,
               String.EmptyString))))))))))))
               (String.append (show_var nenv f) (String.String (Coq_x20,
                 (String.String (Coq_x62, (String.String (Coq_x75,
                 (String.String (Coq_x6e, (String.String (Coq_x64,
                 (String.String (Coq_x6c, (String.String (Coq_x65,
                 (String.String (Coq_x20, (String.String (Coq_x6e,
                 (String.String (Coq_x6f, (String.String (Coq_x74,
                 (String.String (Coq_x20, (String.String (Coq_x66,
                 (String.String (Coq_x6f, (String.String (Coq_x75,
                 (String.String (Coq_x6e, (String.String (Coq_x64,
                 String.EmptyString)))))))))))))))))))))))))))))))))))))
      | None ->
        Exc
          (String.append (String.String (Coq_x45, (String.String (Coq_x61,
            (String.String (Coq_x70, (String.String (Coq_x70, (String.String
            (Coq_x3a, (String.String (Coq_x20, String.EmptyString))))))))))))
            (String.append (show_var nenv f) (String.String (Coq_x20,
              (String.String (Coq_x62, (String.String (Coq_x75,
              (String.String (Coq_x6e, (String.String (Coq_x64,
              (String.String (Coq_x6c, (String.String (Coq_x65,
              (String.String (Coq_x20, (String.String (Coq_x6e,
              (String.String (Coq_x6f, (String.String (Coq_x74,
              (String.String (Coq_x20, (String.String (Coq_x66,
              (String.String (Coq_x6f, (String.String (Coq_x75,
              (String.String (Coq_x6e, (String.String (Coq_x64,
              String.EmptyString)))))))))))))))))))))))))))))))))))))
   | Eprim_val (x, p, e') ->
     let rho' = Cps.M.set x (Vprim p) rho in bstep_f pr cenv nenv rho' e' n'
   | Eprim (x, f, ys, e') ->
     bind
       (l_opt (get_list ys rho) (String.String (Coq_x45, (String.String
         (Coq_x70, (String.String (Coq_x72, (String.String (Coq_x69,
         (String.String (Coq_x6d, (String.String (Coq_x3a, (String.String
         (Coq_x20, (String.String (Coq_x66, (String.String (Coq_x61,
         (String.String (Coq_x69, (String.String (Coq_x6c, (String.String
         (Coq_x65, (String.String (Coq_x64, (String.String (Coq_x20,
         (String.String (Coq_x74, (String.String (Coq_x6f, (String.String
         (Coq_x20, (String.String (Coq_x67, (String.String (Coq_x65,
         (String.String (Coq_x74, (String.String (Coq_x5f, (String.String
         (Coq_x6c, (String.String (Coq_x69, (String.String (Coq_x73,
         (String.String (Coq_x74,
         String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))
       (fun vs ->
       bind
         (l_opt (Cps.M.get f pr) (String.String (Coq_x45, (String.String
           (Coq_x70, (String.String (Coq_x72, (String.String (Coq_x69,
           (String.String (Coq_x6d, (String.String (Coq_x3a, (String.String
           (Coq_x20, (String.String (Coq_x70, (String.String (Coq_x72,
           (String.String (Coq_x69, (String.String (Coq_x6d, (String.String
           (Coq_x20, (String.String (Coq_x6e, (String.String (Coq_x6f,
           (String.String (Coq_x74, (String.String (Coq_x20, (String.String
           (Coq_x66, (String.String (Coq_x6f, (String.String (Coq_x75,
           (String.String (Coq_x6e, (String.String (Coq_x64,
           String.EmptyString)))))))))))))))))))))))))))))))))))))))))))
         (fun f' ->
         bind
           (l_opt (f' vs) (String.String (Coq_x45, (String.String (Coq_x70,
             (String.String (Coq_x72, (String.String (Coq_x69, (String.String
             (Coq_x6d, (String.String (Coq_x3a, (String.String (Coq_x20,
             (String.String (Coq_x70, (String.String (Coq_x72, (String.String
             (Coq_x69, (String.String (Coq_x6d, (String.String (Coq_x20,
             (String.String (Coq_x64, (String.String (Coq_x69, (String.String
             (Coq_x64, (String.String (Coq_x20, (String.String (Coq_x6e,
             (String.String (Coq_x6f, (String.String (Coq_x74, (String.String
             (Coq_x20, (String.String (Coq_x63, (String.String (Coq_x6f,
             (String.String (Coq_x6d, (String.String (Coq_x70, (String.String
             (Coq_x75, (String.String (Coq_x74, (String.String (Coq_x65,
             String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))
           (fun v ->
           let rho' = Cps.M.set x v rho in bstep_f pr cenv nenv rho' e' n')))
   | Ehalt x ->
     (match Cps.M.get x rho with
      | Some v -> Ret (Coq_inr v)
      | None ->
        Exc
          (String.append (String.String (Coq_x48, (String.String (Coq_x61,
            (String.String (Coq_x6c, (String.String (Coq_x74, (String.String
            (Coq_x3a, (String.String (Coq_x20, String.EmptyString))))))))))))
            (String.append (show_var nenv x) (String.String (Coq_x20,
              (String.String (Coq_x76, (String.String (Coq_x61,
              (String.String (Coq_x6c, (String.String (Coq_x75,
              (String.String (Coq_x65, (String.String (Coq_x20,
              (String.String (Coq_x6e, (String.String (Coq_x6f,
              (String.String (Coq_x74, (String.String (Coq_x20,
              (String.String (Coq_x66, (String.String (Coq_x6f,
              (String.String (Coq_x75, (String.String (Coq_x6e,
              (String.String (Coq_x64,
              String.EmptyString))))))))))))))))))))))))))))))))))))

(** val next_id : positive **)

let next_id =
  Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))

(** val fuel : nat **)

let fuel =
  Nat.pow (S (S O)) (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    O))))))))))))))))))

(** val box_to_mut : program -> coq_Term coq_Program pipelineM **)

let box_to_mut p =
  Monad0.ret (coq_MonadErrorT CompM.coq_MonadState) { main =
    (compile (snd p)); env = (compile_ctx (fst p)) }

(** val box_to_anf : program -> coq_LambdaANF_FullTerm pipelineM **)

let box_to_anf p =
  let genv = fst p in
  Monad0.bind (coq_MonadErrorT CompM.coq_MonadState)
    (register_prims next_id genv) (fun x ->
    let (prs, next_id0) = x in CertiCoqPipeline.anf_pipeline p prs next_id0)

(** val eval_box : nat -> coq_Term coq_Program -> String.t pipelineM **)

let eval_box n p =
  match wcbvEval p.env n p.main with
  | Exc s ->
    failwith
      (String.append (String.String (Coq_x43, (String.String (Coq_x6f,
        (String.String (Coq_x75, (String.String (Coq_x6c, (String.String
        (Coq_x64, (String.String (Coq_x20, (String.String (Coq_x6e,
        (String.String (Coq_x6f, (String.String (Coq_x74, (String.String
        (Coq_x20, (String.String (Coq_x65, (String.String (Coq_x76,
        (String.String (Coq_x61, (String.String (Coq_x6c, (String.String
        (Coq_x75, (String.String (Coq_x61, (String.String (Coq_x74,
        (String.String (Coq_x65, (String.String (Coq_x20, (String.String
        (Coq_x70, (String.String (Coq_x72, (String.String (Coq_x6f,
        (String.String (Coq_x67, (String.String (Coq_x72, (String.String
        (Coq_x61, (String.String (Coq_x6d, (String.String (Coq_x3a,
        String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))
        (String.append nl s))
  | Ret p0 ->
    Monad0.ret (coq_MonadErrorT CompM.coq_MonadState) (print_term p0)

(** val eval_anf : nat -> coq_LambdaANF_FullTerm -> String.t pipelineM **)

let eval_anf n = function
| (env0, p0) ->
  let (p1, env1) = env0 in
  let (p2, _) = p1 in
  let (p3, nenv) = p2 in
  let (p4, _) = p3 in
  let (p5, _) = p4 in
  let (p6, ctor_env0) = p5 in
  let (prims0, _) = p6 in
  (match bstep_f prims0 ctor_env0 nenv env1 p0 n with
   | Exc s ->
     failwith
       (String.append (String.String (Coq_x43, (String.String (Coq_x6f,
         (String.String (Coq_x75, (String.String (Coq_x6c, (String.String
         (Coq_x64, (String.String (Coq_x20, (String.String (Coq_x6e,
         (String.String (Coq_x6f, (String.String (Coq_x74, (String.String
         (Coq_x20, (String.String (Coq_x65, (String.String (Coq_x76,
         (String.String (Coq_x61, (String.String (Coq_x6c, (String.String
         (Coq_x75, (String.String (Coq_x61, (String.String (Coq_x74,
         (String.String (Coq_x65, (String.String (Coq_x20, (String.String
         (Coq_x70, (String.String (Coq_x72, (String.String (Coq_x6f,
         (String.String (Coq_x67, (String.String (Coq_x72, (String.String
         (Coq_x61, (String.String (Coq_x6d, (String.String (Coq_x3a,
         String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))
         (String.append nl s))
   | Ret s ->
     (match s with
      | Coq_inl p7 ->
        let (_, e) = p7 in
        Monad0.ret (coq_MonadErrorT CompM.coq_MonadState)
          (show_exp nenv ctor_env0 true e)
      | Coq_inr v ->
        Monad0.ret (coq_MonadErrorT CompM.coq_MonadState)
          (show_val nenv ctor_env0 true v)))

(** val eval : coq_Options -> bool -> program -> String.t error * String.t **)

let eval opts anf p =
  let pipeline = fun p0 ->
    if anf
    then Monad0.bind (coq_MonadErrorT CompM.coq_MonadState) (box_to_anf p0)
           (fun p1 -> eval_anf fuel p1)
    else Monad0.bind (coq_MonadErrorT CompM.coq_MonadState) (box_to_mut p0)
           (fun p1 -> eval_box fuel p1)
  in
  run_pipeline opts p pipeline
