open AstCommon
open Byte
open Datatypes
open Kernames
open Monad0
open MonadExc
open RandyPrelude
open Bytestring
open Compile0
open ExceptionMonad
open Term0

type __ = Obj.t

(** val fix_bug : (String.t, __ coq_exception) coq_MonadExc **)

let fix_bug =
  exn_monad_exc

(** val wcbvEval :
    coq_Term environ -> nat -> coq_Term -> coq_Term coq_exception **)

let wcbvEval p =
  let rec wcbvEval0 tmr t0 =
    match tmr with
    | O ->
      MonadExc.raise (Obj.magic fix_bug)
        (String.append (String.String (Coq_x6f, (String.String (Coq_x75,
          (String.String (Coq_x74, (String.String (Coq_x20, (String.String
          (Coq_x6f, (String.String (Coq_x66, (String.String (Coq_x20,
          (String.String (Coq_x74, (String.String (Coq_x69, (String.String
          (Coq_x6d, (String.String (Coq_x65, (String.String (Coq_x3a,
          (String.String (Coq_x20,
          String.EmptyString)))))))))))))))))))))))))) (print_term t0))
    | S n ->
      (match t0 with
       | TRel _ ->
         MonadExc.raise (Obj.magic fix_bug) (String.String (Coq_x77,
           (String.String (Coq_x63, (String.String (Coq_x62, (String.String
           (Coq_x76, (String.String (Coq_x45, (String.String (Coq_x76,
           (String.String (Coq_x61, (String.String (Coq_x6c, (String.String
           (Coq_x3a, (String.String (Coq_x75, (String.String (Coq_x6e,
           (String.String (Coq_x62, (String.String (Coq_x6f, (String.String
           (Coq_x75, (String.String (Coq_x6e, (String.String (Coq_x64,
           (String.String (Coq_x20, (String.String (Coq_x52, (String.String
           (Coq_x65, (String.String (Coq_x6c,
           String.EmptyString))))))))))))))))))))))))))))))))))))))))
       | TLetIn (_, df, bod) ->
         (match wcbvEval0 n df with
          | Exc s ->
            MonadExc.raise (Obj.magic fix_bug)
              (String.append (String.String (Coq_x77, (String.String
                (Coq_x63, (String.String (Coq_x62, (String.String (Coq_x76,
                (String.String (Coq_x45, (String.String (Coq_x76,
                (String.String (Coq_x61, (String.String (Coq_x6c,
                (String.String (Coq_x2c, (String.String (Coq_x54,
                (String.String (Coq_x4c, (String.String (Coq_x65,
                (String.String (Coq_x74, (String.String (Coq_x49,
                (String.String (Coq_x6e, (String.String (Coq_x2c,
                (String.String (Coq_x64, (String.String (Coq_x65,
                (String.String (Coq_x66, (String.String (Coq_x3a,
                (String.String (Coq_x20,
                String.EmptyString))))))))))))))))))))))))))))))))))))))))))
                s)
          | Ret df' -> wcbvEval0 n (instantiate df' O bod))
       | TApp (fn, a1) ->
         (match wcbvEval0 n fn with
          | Exc str ->
            MonadExc.raise (Obj.magic fix_bug)
              (String.append (String.String (Coq_x28, (String.String
                (Coq_x77, (String.String (Coq_x63, (String.String (Coq_x62,
                (String.String (Coq_x76, (String.String (Coq_x45,
                (String.String (Coq_x76, (String.String (Coq_x61,
                (String.String (Coq_x6c, (String.String (Coq_x3b,
                (String.String (Coq_x54, (String.String (Coq_x41,
                (String.String (Coq_x70, (String.String (Coq_x70,
                (String.String (Coq_x3a, (String.String (Coq_x66,
                (String.String (Coq_x6e,
                String.EmptyString))))))))))))))))))))))))))))))))))
                (String.append str (String.String (Coq_x29,
                  String.EmptyString))))
          | Ret u ->
            (match u with
             | TProof ->
               (match wcbvEval0 n a1 with
                | Exc s ->
                  MonadExc.raise (Obj.magic fix_bug)
                    (String.append (String.String (Coq_x77, (String.String
                      (Coq_x63, (String.String (Coq_x62, (String.String
                      (Coq_x76, (String.String (Coq_x45, (String.String
                      (Coq_x76, (String.String (Coq_x61, (String.String
                      (Coq_x6c, (String.String (Coq_x2c, (String.String
                      (Coq_x50, (String.String (Coq_x72, (String.String
                      (Coq_x6f, (String.String (Coq_x6f, (String.String
                      (Coq_x66, (String.String (Coq_x52, (String.String
                      (Coq_x65, (String.String (Coq_x64, (String.String
                      (Coq_x65, (String.String (Coq_x78, (String.String
                      (Coq_x2c, (String.String (Coq_x61, (String.String
                      (Coq_x72, (String.String (Coq_x67, (String.String
                      (Coq_x3a, (String.String (Coq_x20,
                      String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))
                      s)
                | Ret _ -> Ret TProof)
             | TLambda (_, bod) ->
               (match wcbvEval0 n a1 with
                | Exc s ->
                  MonadExc.raise (Obj.magic fix_bug)
                    (String.append (String.String (Coq_x77, (String.String
                      (Coq_x63, (String.String (Coq_x62, (String.String
                      (Coq_x76, (String.String (Coq_x45, (String.String
                      (Coq_x76, (String.String (Coq_x61, (String.String
                      (Coq_x6c, (String.String (Coq_x2c, (String.String
                      (Coq_x62, (String.String (Coq_x65, (String.String
                      (Coq_x74, (String.String (Coq_x61, (String.String
                      (Coq_x2c, (String.String (Coq_x61, (String.String
                      (Coq_x72, (String.String (Coq_x67, (String.String
                      (Coq_x3a, (String.String (Coq_x20,
                      String.EmptyString))))))))))))))))))))))))))))))))))))))
                      s)
                | Ret b1 -> wcbvEval0 n (whBetaStep bod b1))
             | TConstruct (i, cn, _) ->
               MonadExc.raise (Obj.magic fix_bug)
                 (String.append (String.String (Coq_x77, (String.String
                   (Coq_x63, (String.String (Coq_x62, (String.String
                   (Coq_x76, (String.String (Coq_x45, (String.String
                   (Coq_x76, (String.String (Coq_x61, (String.String
                   (Coq_x6c, (String.String (Coq_x2c, (String.String
                   (Coq_x43, (String.String (Coq_x6f, (String.String
                   (Coq_x6e, (String.String (Coq_x67, (String.String
                   (Coq_x2c, (String.String (Coq_x43, (String.String
                   (Coq_x6f, (String.String (Coq_x6e, (String.String
                   (Coq_x73, (String.String (Coq_x74, (String.String
                   (Coq_x72, (String.String (Coq_x75, (String.String
                   (Coq_x63, (String.String (Coq_x74, (String.String
                   (Coq_x6f, (String.String (Coq_x72, (String.String
                   (Coq_x3a, (String.String (Coq_x20,
                   String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))
                   (String.append (String.String (Coq_x20,
                     String.EmptyString))
                     (String.append (print_inductive i)
                       (String.append (String.String (Coq_x20,
                         String.EmptyString)) (nat_to_string cn)))))
             | TFix (dts, m) ->
               (match whFixStep dts m with
                | Some f ->
                  (match wcbvEval0 n a1 with
                   | Exc s ->
                     MonadExc.raise (Obj.magic fix_bug)
                       (String.append (String.String (Coq_x77, (String.String
                         (Coq_x63, (String.String (Coq_x62, (String.String
                         (Coq_x76, (String.String (Coq_x45, (String.String
                         (Coq_x76, (String.String (Coq_x61, (String.String
                         (Coq_x6c, (String.String (Coq_x2c, (String.String
                         (Coq_x66, (String.String (Coq_x69, (String.String
                         (Coq_x78, (String.String (Coq_x2c, (String.String
                         (Coq_x61, (String.String (Coq_x72, (String.String
                         (Coq_x67, (String.String (Coq_x3a, (String.String
                         (Coq_x20,
                         String.EmptyString))))))))))))))))))))))))))))))))))))
                         s)
                   | Ret b1 -> wcbvEval0 n (TApp (f, b1)))
                | None ->
                  MonadExc.raise (Obj.magic fix_bug) (String.String (Coq_x77,
                    (String.String (Coq_x63, (String.String (Coq_x62,
                    (String.String (Coq_x76, (String.String (Coq_x45,
                    (String.String (Coq_x76, (String.String (Coq_x61,
                    (String.String (Coq_x6c, (String.String (Coq_x3b,
                    (String.String (Coq_x54, (String.String (Coq_x41,
                    (String.String (Coq_x70, (String.String (Coq_x70,
                    (String.String (Coq_x3a, (String.String (Coq_x77,
                    (String.String (Coq_x68, (String.String (Coq_x46,
                    (String.String (Coq_x69, (String.String (Coq_x78,
                    (String.String (Coq_x53, (String.String (Coq_x74,
                    (String.String (Coq_x65, (String.String (Coq_x70,
                    (String.String (Coq_x20, (String.String (Coq_x64,
                    (String.String (Coq_x6f, (String.String (Coq_x65,
                    (String.String (Coq_x73, (String.String (Coq_x6e,
                    (String.String (Coq_x27, (String.String (Coq_x74,
                    (String.String (Coq_x20, (String.String (Coq_x65,
                    (String.String (Coq_x76, (String.String (Coq_x61,
                    (String.String (Coq_x6c,
                    String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
             | TPrim _ ->
               MonadExc.raise (Obj.magic fix_bug) (String.String (Coq_x77,
                 (String.String (Coq_x63, (String.String (Coq_x62,
                 (String.String (Coq_x76, (String.String (Coq_x45,
                 (String.String (Coq_x76, (String.String (Coq_x61,
                 (String.String (Coq_x6c, (String.String (Coq_x2c,
                 (String.String (Coq_x43, (String.String (Coq_x6f,
                 (String.String (Coq_x6e, (String.String (Coq_x67,
                 (String.String (Coq_x2c, (String.String (Coq_x50,
                 (String.String (Coq_x72, (String.String (Coq_x69,
                 (String.String (Coq_x6d,
                 String.EmptyString))))))))))))))))))))))))))))))))))))
             | _ ->
               (match wcbvEval0 n a1 with
                | Exc s ->
                  MonadExc.raise (Obj.magic fix_bug)
                    (String.append (String.String (Coq_x77, (String.String
                      (Coq_x63, (String.String (Coq_x62, (String.String
                      (Coq_x76, (String.String (Coq_x45, (String.String
                      (Coq_x76, (String.String (Coq_x61, (String.String
                      (Coq_x6c, (String.String (Coq_x2c, (String.String
                      (Coq_x43, (String.String (Coq_x6f, (String.String
                      (Coq_x6e, (String.String (Coq_x67, (String.String
                      (Coq_x2c, (String.String (Coq_x61, (String.String
                      (Coq_x72, (String.String (Coq_x67, (String.String
                      (Coq_x3a, (String.String (Coq_x20,
                      String.EmptyString))))))))))))))))))))))))))))))))))))))
                      s)
                | Ret b1 -> Monad0.ret (Obj.magic exn_monad) (TApp (u, b1)))))
       | TConst nm ->
         (match lookup nm p with
          | Some e ->
            (match e with
             | Coq_ecTrm t1 -> wcbvEval0 n t1
             | Coq_ecTyp (_, _) ->
               MonadExc.raise (Obj.magic fix_bug)
                 (String.append (String.String (Coq_x77, (String.String
                   (Coq_x63, (String.String (Coq_x62, (String.String
                   (Coq_x76, (String.String (Coq_x45, (String.String
                   (Coq_x76, (String.String (Coq_x61, (String.String
                   (Coq_x6c, (String.String (Coq_x3b, (String.String
                   (Coq_x54, (String.String (Coq_x43, (String.String
                   (Coq_x6f, (String.String (Coq_x6e, (String.String
                   (Coq_x73, (String.String (Coq_x74, (String.String
                   (Coq_x3b, (String.String (Coq_x65, (String.String
                   (Coq_x63, (String.String (Coq_x54, (String.String
                   (Coq_x79, (String.String (Coq_x70, (String.String
                   (Coq_x3a, (String.String (Coq_x20,
                   String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))
                   (string_of_kername nm)))
          | None ->
            MonadExc.raise (Obj.magic fix_bug) (String.String (Coq_x77,
              (String.String (Coq_x63, (String.String (Coq_x62,
              (String.String (Coq_x76, (String.String (Coq_x45,
              (String.String (Coq_x76, (String.String (Coq_x61,
              (String.String (Coq_x6c, (String.String (Coq_x3a,
              (String.String (Coq_x20, (String.String (Coq_x54,
              (String.String (Coq_x43, (String.String (Coq_x6f,
              (String.String (Coq_x6e, (String.String (Coq_x73,
              (String.String (Coq_x74, (String.String (Coq_x20,
              (String.String (Coq_x65, (String.String (Coq_x6e,
              (String.String (Coq_x76, (String.String (Coq_x69,
              (String.String (Coq_x72, (String.String (Coq_x6f,
              (String.String (Coq_x6e, (String.String (Coq_x6d,
              (String.String (Coq_x65, (String.String (Coq_x6e,
              (String.String (Coq_x74, (String.String (Coq_x20,
              (String.String (Coq_x6d, (String.String (Coq_x69,
              (String.String (Coq_x73, (String.String (Coq_x73,
              String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
       | TConstruct (i, cn, args) ->
         (match wcbvEvals n args with
          | Exc s ->
            MonadExc.raise (Obj.magic fix_bug)
              (String.append (String.String (Coq_x77, (String.String
                (Coq_x63, (String.String (Coq_x62, (String.String (Coq_x76,
                (String.String (Coq_x45, (String.String (Coq_x76,
                (String.String (Coq_x61, (String.String (Coq_x6c,
                (String.String (Coq_x3a, (String.String (Coq_x54,
                (String.String (Coq_x43, (String.String (Coq_x6f,
                (String.String (Coq_x6e, (String.String (Coq_x73,
                (String.String (Coq_x74, (String.String (Coq_x72,
                (String.String (Coq_x75, (String.String (Coq_x63,
                (String.String (Coq_x74, (String.String (Coq_x3a,
                (String.String (Coq_x61, (String.String (Coq_x72,
                (String.String (Coq_x67, (String.String (Coq_x73,
                (String.String (Coq_x3a, (String.String (Coq_x20,
                String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))
                s)
          | Ret args' ->
            Monad0.ret (Obj.magic exn_monad) (TConstruct (i, cn, args')))
       | TCase (i, mch, brs) ->
         (match wcbvEval0 n mch with
          | Exc str ->
            MonadExc.raise (Obj.magic fix_bug)
              (String.append (String.String (Coq_x77, (String.String
                (Coq_x63, (String.String (Coq_x62, (String.String (Coq_x76,
                (String.String (Coq_x45, (String.String (Coq_x76,
                (String.String (Coq_x61, (String.String (Coq_x6c,
                (String.String (Coq_x2c, (String.String (Coq_x54,
                (String.String (Coq_x43, (String.String (Coq_x61,
                (String.String (Coq_x73, (String.String (Coq_x65,
                (String.String (Coq_x2c, (String.String (Coq_x6d,
                (String.String (Coq_x63, (String.String (Coq_x68,
                (String.String (Coq_x3a, (String.String (Coq_x20,
                String.EmptyString))))))))))))))))))))))))))))))))))))))))
                str)
          | Ret x ->
            (match x with
             | TConstruct (j, r, args) ->
               (match whCaseStep r args brs with
                | Some cs ->
                  if inductive_dec i j
                  then wcbvEval0 n cs
                  else MonadExc.raise (Obj.magic fix_bug) (String.String
                         (Coq_x77, (String.String (Coq_x63, (String.String
                         (Coq_x62, (String.String (Coq_x76, (String.String
                         (Coq_x45, (String.String (Coq_x76, (String.String
                         (Coq_x61, (String.String (Coq_x6c, (String.String
                         (Coq_x3a, (String.String (Coq_x43, (String.String
                         (Coq_x61, (String.String (Coq_x73, (String.String
                         (Coq_x65, (String.String (Coq_x2c, (String.String
                         (Coq_x77, (String.String (Coq_x68, (String.String
                         (Coq_x43, (String.String (Coq_x61, (String.String
                         (Coq_x73, (String.String (Coq_x65, (String.String
                         (Coq_x53, (String.String (Coq_x74, (String.String
                         (Coq_x65, (String.String (Coq_x70, (String.String
                         (Coq_x2c, (String.String (Coq_x20, (String.String
                         (Coq_x69, (String.String (Coq_x6e, (String.String
                         (Coq_x64, (String.String (Coq_x75, (String.String
                         (Coq_x63, (String.String (Coq_x74, (String.String
                         (Coq_x69, (String.String (Coq_x76, (String.String
                         (Coq_x65, (String.String (Coq_x73,
                         String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                | None ->
                  MonadExc.raise (Obj.magic fix_bug) (String.String (Coq_x77,
                    (String.String (Coq_x63, (String.String (Coq_x62,
                    (String.String (Coq_x76, (String.String (Coq_x45,
                    (String.String (Coq_x76, (String.String (Coq_x61,
                    (String.String (Coq_x6c, (String.String (Coq_x3a,
                    (String.String (Coq_x43, (String.String (Coq_x61,
                    (String.String (Coq_x73, (String.String (Coq_x65,
                    (String.String (Coq_x2c, (String.String (Coq_x77,
                    (String.String (Coq_x68, (String.String (Coq_x43,
                    (String.String (Coq_x61, (String.String (Coq_x73,
                    (String.String (Coq_x65, (String.String (Coq_x53,
                    (String.String (Coq_x74, (String.String (Coq_x65,
                    (String.String (Coq_x70,
                    String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))
             | _ ->
               MonadExc.raise (Obj.magic fix_bug) (String.String (Coq_x77,
                 (String.String (Coq_x63, (String.String (Coq_x62,
                 (String.String (Coq_x76, (String.String (Coq_x45,
                 (String.String (Coq_x76, (String.String (Coq_x61,
                 (String.String (Coq_x6c, (String.String (Coq_x3a,
                 (String.String (Coq_x43, (String.String (Coq_x61,
                 (String.String (Coq_x73, (String.String (Coq_x65,
                 (String.String (Coq_x2c, (String.String (Coq_x64,
                 (String.String (Coq_x69, (String.String (Coq_x73,
                 (String.String (Coq_x63, (String.String (Coq_x72,
                 (String.String (Coq_x69, (String.String (Coq_x6d,
                 (String.String (Coq_x69, (String.String (Coq_x6e,
                 (String.String (Coq_x65, (String.String (Coq_x65,
                 String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))
       | TWrong _ -> MonadExc.raise (Obj.magic fix_bug) (print_term t0)
       | x -> Monad0.ret (Obj.magic exn_monad) x)
  and wcbvEvals tmr ts =
    match tmr with
    | O ->
      MonadExc.raise (Obj.magic fix_bug) (String.String (Coq_x6f,
        (String.String (Coq_x75, (String.String (Coq_x74, (String.String
        (Coq_x20, (String.String (Coq_x6f, (String.String (Coq_x66,
        (String.String (Coq_x20, (String.String (Coq_x74, (String.String
        (Coq_x69, (String.String (Coq_x6d, (String.String (Coq_x65,
        String.EmptyString))))))))))))))))))))))
    | S n ->
      (match ts with
       | Coq_tnil -> Monad0.ret (Obj.magic exn_monad) Coq_tnil
       | Coq_tcons (s, ss) ->
         (match wcbvEval0 n s with
          | Exc s0 ->
            MonadExc.raise (Obj.magic fix_bug)
              (String.append (String.String (Coq_x77, (String.String
                (Coq_x63, (String.String (Coq_x62, (String.String (Coq_x76,
                (String.String (Coq_x45, (String.String (Coq_x76,
                (String.String (Coq_x61, (String.String (Coq_x6c,
                (String.String (Coq_x73, (String.String (Coq_x3a,
                (String.String (Coq_x68, (String.String (Coq_x64,
                (String.String (Coq_x3a, (String.String (Coq_x20,
                String.EmptyString)))))))))))))))))))))))))))) s0)
          | Ret es ->
            (match wcbvEvals n ss with
             | Exc s0 ->
               MonadExc.raise (Obj.magic fix_bug)
                 (String.append (String.String (Coq_x77, (String.String
                   (Coq_x63, (String.String (Coq_x62, (String.String
                   (Coq_x76, (String.String (Coq_x45, (String.String
                   (Coq_x76, (String.String (Coq_x61, (String.String
                   (Coq_x6c, (String.String (Coq_x73, (String.String
                   (Coq_x3a, (String.String (Coq_x74, (String.String
                   (Coq_x6c, (String.String (Coq_x3a, (String.String
                   (Coq_x20, String.EmptyString))))))))))))))))))))))))))))
                   s0)
             | Ret ess ->
               Monad0.ret (Obj.magic exn_monad) (Coq_tcons (es, ess)))))
  in wcbvEval0
