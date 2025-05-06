open AstCommon
open BasicAst
open BinNat
open BinNums
open Byte
open Datatypes
open List0
open MCString
open Monad0
open MonadState
open Nat0
open Show
open Specif
open StateMonad
open Bytestring
open Cps

type name_env = name M.tree

(** val show_nat : nat -> String.t **)

let show_nat =
  string_of_nat

(** val show_pos : positive -> String.t **)

let show_pos =
  string_of_positive

(** val show_binnat : coq_N -> String.t **)

let show_binnat x =
  show_nat (N.to_nat x)

(** val sep : 'a1 -> 'a1 list -> 'a1 list **)

let rec sep s = function
| [] -> []
| h :: t0 -> (match t0 with
              | [] -> h :: []
              | _ :: _ -> h :: (s :: (sep s t0)))

type string_tree =
| Emp
| Str of String.t
| App of string_tree * string_tree

(** val show_tree_c : string_tree -> String.t -> String.t **)

let rec show_tree_c t0 acc =
  match t0 with
  | Emp -> acc
  | Str s -> String.append s acc
  | App (t1, t2) -> show_tree_c t1 (show_tree_c t2 acc)

(** val show_tree : string_tree -> String.t **)

let show_tree t0 =
  show_tree_c t0 String.EmptyString

(** val show_var : name_env -> positive -> string_tree **)

let show_var nenv x =
  match M.get x nenv with
  | Some n ->
    (match n with
     | Coq_nAnon ->
       App ((Str (String.String (Coq_x78, String.EmptyString))), (Str
         (show_pos x)))
     | Coq_nNamed s ->
       App ((App ((Str s), (Str (String.String (Coq_x5f,
         String.EmptyString))))), (Str (show_pos x))))
  | None ->
    App ((Str (String.String (Coq_x78, String.EmptyString))), (Str
      (show_pos x)))

(** val show_con : ctor_env -> ctor_tag -> string_tree **)

let show_con cenv tg =
  match M.get tg cenv with
  | Some c ->
    let { ctor_name = ctor_name0; ctor_ind_name = _; ctor_ind_tag = _;
      ctor_arity = _; ctor_ordinal = _ } = c
    in
    (match ctor_name0 with
     | Coq_nAnon ->
       App ((Str (String.String (Coq_x63, (String.String (Coq_x6f,
         (String.String (Coq_x6e, (String.String (Coq_x5f,
         String.EmptyString))))))))), (Str (show_pos tg)))
     | Coq_nNamed s -> Str s)
  | None ->
    App ((Str (String.String (Coq_x63, (String.String (Coq_x6f,
      (String.String (Coq_x6e, (String.String (Coq_x5f,
      String.EmptyString))))))))), (Str (show_pos tg)))

(** val show_ftag : bool -> fun_tag -> string_tree **)

let show_ftag ftag_flag tg =
  if ftag_flag
  then App ((App ((Str (String.String (Coq_x3c, String.EmptyString))), (Str
         (show_pos tg)))), (Str (String.String (Coq_x3e,
         String.EmptyString))))
  else Str String.EmptyString

(** val show_vars : name_env -> positive list -> string_tree **)

let show_vars nenv xs =
  App ((App ((Str (String.String (Coq_x28, String.EmptyString))),
    (fold_right (fun x y -> App (x, y)) (Str String.EmptyString)
      (sep (Str (String.String (Coq_x2c, String.EmptyString)))
        (map (show_var nenv) xs))))), (Str (String.String (Coq_x29,
    String.EmptyString))))

type 't coq_M = (string_tree, 't) state

(** val emit : string_tree -> unit coq_M **)

let emit s =
  bind (Obj.magic coq_Monad_state) (Obj.magic coq_MonadState_state).get
    (fun st -> (Obj.magic coq_MonadState_state).put (App (st, s)))

(** val tab : nat -> unit coq_M **)

let rec tab = function
| O -> ret (Obj.magic coq_Monad_state) ()
| S n0 ->
  bind (Obj.magic coq_Monad_state)
    (emit (Str (String.String (Coq_x20, String.EmptyString)))) (fun _ ->
    tab n0)

(** val chr_newline : byte **)

let chr_newline =
  Coq_x0a

(** val newline : unit coq_M **)

let newline =
  emit (Str (String.String (chr_newline, String.EmptyString)))

(** val emit_prim : primitive -> unit coq_M **)

let emit_prim p =
  let f = projT2 p in
  (match projT1 p with
   | Coq_primInt ->
     bind (Obj.magic coq_Monad_state)
       (emit (Str (String.String (Coq_x28, (String.String (Coq_x69,
         (String.String (Coq_x6e, (String.String (Coq_x74, (String.String
         (Coq_x3a, (String.String (Coq_x20, String.EmptyString))))))))))))))
       (fun _ ->
       bind (Obj.magic coq_Monad_state)
         (emit (Str (string_of_prim_int (Obj.magic f)))) (fun _ ->
         emit (Str (String.String (Coq_x29, String.EmptyString)))))
   | Coq_primFloat ->
     bind (Obj.magic coq_Monad_state)
       (emit (Str (String.String (Coq_x28, (String.String (Coq_x66,
         (String.String (Coq_x6c, (String.String (Coq_x6f, (String.String
         (Coq_x61, (String.String (Coq_x74, (String.String (Coq_x3a,
         (String.String (Coq_x20, String.EmptyString))))))))))))))))))
       (fun _ ->
       bind (Obj.magic coq_Monad_state)
         (emit (Str (AstCommon.string_of_float (Obj.magic f)))) (fun _ ->
         emit (Str (String.String (Coq_x29, String.EmptyString))))))

(** val emit_exp :
    name_env -> ctor_env -> bool -> nat -> exp -> unit coq_M **)

let rec emit_exp nenv cenv ftag_flag indent e =
  bind (Obj.magic coq_Monad_state) (tab indent) (fun _ ->
    match e with
    | Econstr (x, tg, xs, e0) ->
      bind (Obj.magic coq_Monad_state)
        (emit (Str (String.String (Coq_x6c, (String.String (Coq_x65,
          (String.String (Coq_x74, (String.String (Coq_x20,
          String.EmptyString)))))))))) (fun _ ->
        bind (Obj.magic coq_Monad_state) (emit (show_var nenv x)) (fun _ ->
          bind (Obj.magic coq_Monad_state)
            (emit (Str (String.String (Coq_x20, (String.String (Coq_x3a,
              (String.String (Coq_x3d, (String.String (Coq_x20,
              String.EmptyString)))))))))) (fun _ ->
            bind (Obj.magic coq_Monad_state) (emit (show_con cenv tg))
              (fun _ ->
              bind (Obj.magic coq_Monad_state) (emit (show_vars nenv xs))
                (fun _ ->
                bind (Obj.magic coq_Monad_state)
                  (emit (Str (String.String (Coq_x20, (String.String
                    (Coq_x69, (String.String (Coq_x6e, (String.String
                    (Coq_x20, String.EmptyString)))))))))) (fun _ ->
                  bind (Obj.magic coq_Monad_state) newline (fun _ ->
                    emit_exp nenv cenv ftag_flag indent e0)))))))
    | Ecase (x, arms) ->
      bind (Obj.magic coq_Monad_state)
        (emit (Str (String.String (Coq_x63, (String.String (Coq_x61,
          (String.String (Coq_x73, (String.String (Coq_x65, (String.String
          (Coq_x20, String.EmptyString)))))))))))) (fun _ ->
        bind (Obj.magic coq_Monad_state) (emit (show_var nenv x)) (fun _ ->
          bind (Obj.magic coq_Monad_state)
            (emit (Str (String.String (Coq_x20, (String.String (Coq_x6f,
              (String.String (Coq_x66, (String.String (Coq_x20,
              (String.String (Coq_x7b, String.EmptyString))))))))))))
            (fun _ ->
            bind (Obj.magic coq_Monad_state) newline (fun _ ->
              bind (Obj.magic coq_Monad_state)
                (let rec iter = function
                 | [] -> ret (Obj.magic coq_Monad_state) ()
                 | p :: tail ->
                   let (tg, e0) = p in
                   bind (Obj.magic coq_Monad_state) (tab indent) (fun _ ->
                     bind (Obj.magic coq_Monad_state)
                       (emit (Str (String.String (Coq_x7c, (String.String
                         (Coq_x20, String.EmptyString)))))) (fun _ ->
                       bind (Obj.magic coq_Monad_state)
                         (emit (show_con cenv tg)) (fun _ ->
                         bind (Obj.magic coq_Monad_state)
                           (emit (Str (String.String (Coq_x20, (String.String
                             (Coq_x3d, (String.String (Coq_x3e,
                             (String.String (Coq_x20,
                             String.EmptyString)))))))))) (fun _ ->
                           bind (Obj.magic coq_Monad_state) newline (fun _ ->
                             bind (Obj.magic coq_Monad_state)
                               (emit_exp nenv cenv ftag_flag
                                 (add (S (S O)) indent) e0) (fun _ ->
                               iter tail))))))
                 in iter arms) (fun _ ->
                bind (Obj.magic coq_Monad_state) (tab indent) (fun _ ->
                  bind (Obj.magic coq_Monad_state)
                    (emit (Str (String.String (Coq_x7d, String.EmptyString))))
                    (fun _ -> newline)))))))
    | Eproj (x, tg, n, y, e0) ->
      bind (Obj.magic coq_Monad_state)
        (emit (Str (String.String (Coq_x6c, (String.String (Coq_x65,
          (String.String (Coq_x74, (String.String (Coq_x20,
          String.EmptyString)))))))))) (fun _ ->
        bind (Obj.magic coq_Monad_state) (emit (show_var nenv x)) (fun _ ->
          bind (Obj.magic coq_Monad_state)
            (emit (Str (String.String (Coq_x20, (String.String (Coq_x3a,
              (String.String (Coq_x3d, (String.String (Coq_x20,
              (String.String (Coq_x70, (String.String (Coq_x72,
              (String.String (Coq_x6f, (String.String (Coq_x6a,
              (String.String (Coq_x5f, String.EmptyString))))))))))))))))))))
            (fun _ ->
            bind (Obj.magic coq_Monad_state) (emit (Str (show_binnat n)))
              (fun _ ->
              bind (Obj.magic coq_Monad_state)
                (emit (Str (String.String (Coq_x20, String.EmptyString))))
                (fun _ ->
                bind (Obj.magic coq_Monad_state) (emit (Str (show_pos tg)))
                  (fun _ ->
                  bind (Obj.magic coq_Monad_state)
                    (emit (Str (String.String (Coq_x20, String.EmptyString))))
                    (fun _ ->
                    bind (Obj.magic coq_Monad_state) (emit (show_var nenv y))
                      (fun _ ->
                      bind (Obj.magic coq_Monad_state)
                        (emit (Str (String.String (Coq_x20, (String.String
                          (Coq_x69, (String.String (Coq_x6e, (String.String
                          (Coq_x20, String.EmptyString)))))))))) (fun _ ->
                        bind (Obj.magic coq_Monad_state) newline (fun _ ->
                          emit_exp nenv cenv ftag_flag indent e0))))))))))
    | Eletapp (x, f, ft, ys, e0) ->
      bind (Obj.magic coq_Monad_state)
        (emit (Str (String.String (Coq_x6c, (String.String (Coq_x65,
          (String.String (Coq_x74, (String.String (Coq_x20,
          String.EmptyString)))))))))) (fun _ ->
        bind (Obj.magic coq_Monad_state) (emit (show_var nenv x)) (fun _ ->
          bind (Obj.magic coq_Monad_state)
            (emit (Str (String.String (Coq_x20, (String.String (Coq_x3a,
              (String.String (Coq_x3d, (String.String (Coq_x20,
              (String.String (Coq_x61, (String.String (Coq_x70,
              (String.String (Coq_x70, (String.String (Coq_x20,
              String.EmptyString)))))))))))))))))) (fun _ ->
            bind (Obj.magic coq_Monad_state) (emit (show_var nenv f))
              (fun _ ->
              bind (Obj.magic coq_Monad_state)
                (emit (show_ftag ftag_flag ft)) (fun _ ->
                bind (Obj.magic coq_Monad_state) (emit (show_vars nenv ys))
                  (fun _ ->
                  bind (Obj.magic coq_Monad_state)
                    (emit (Str (String.String (Coq_x20, (String.String
                      (Coq_x69, (String.String (Coq_x6e, (String.String
                      (Coq_x20, String.EmptyString)))))))))) (fun _ ->
                    bind (Obj.magic coq_Monad_state) newline (fun _ ->
                      emit_exp nenv cenv ftag_flag indent e0))))))))
    | Efun (fds, e0) ->
      bind (Obj.magic coq_Monad_state)
        (emit (Str (String.String (Coq_x6c, (String.String (Coq_x65,
          (String.String (Coq_x74, (String.String (Coq_x72, (String.String
          (Coq_x65, (String.String (Coq_x63, (String.String (Coq_x20,
          (String.String (Coq_x5b, String.EmptyString))))))))))))))))))
        (fun _ ->
        bind (Obj.magic coq_Monad_state) newline (fun _ ->
          bind (Obj.magic coq_Monad_state)
            (let rec iter = function
             | Fcons (x, tg, xs, e1, fds') ->
               bind (Obj.magic coq_Monad_state) (tab (add (S (S O)) indent))
                 (fun _ ->
                 bind (Obj.magic coq_Monad_state)
                   (emit (Str (String.String (Coq_x66, (String.String
                     (Coq_x75, (String.String (Coq_x6e, (String.String
                     (Coq_x20, String.EmptyString)))))))))) (fun _ ->
                   bind (Obj.magic coq_Monad_state) (emit (show_var nenv x))
                     (fun _ ->
                     bind (Obj.magic coq_Monad_state)
                       (emit (show_ftag ftag_flag tg)) (fun _ ->
                       bind (Obj.magic coq_Monad_state)
                         (emit (show_vars nenv xs)) (fun _ ->
                         bind (Obj.magic coq_Monad_state)
                           (emit (Str (String.String (Coq_x20, (String.String
                             (Coq_x3a, (String.String (Coq_x3d,
                             (String.String (Coq_x20,
                             String.EmptyString)))))))))) (fun _ ->
                           bind (Obj.magic coq_Monad_state) newline (fun _ ->
                             bind (Obj.magic coq_Monad_state)
                               (emit_exp nenv cenv ftag_flag
                                 (add (S (S (S (S O)))) indent) e1) (fun _ ->
                               iter fds'))))))))
             | Fnil -> ret (Obj.magic coq_Monad_state) ()
             in iter fds) (fun _ ->
            bind (Obj.magic coq_Monad_state) (tab indent) (fun _ ->
              bind (Obj.magic coq_Monad_state)
                (emit (Str (String.String (Coq_x5d, (String.String (Coq_x20,
                  (String.String (Coq_x69, (String.String (Coq_x6e,
                  String.EmptyString)))))))))) (fun _ ->
                bind (Obj.magic coq_Monad_state) newline (fun _ ->
                  emit_exp nenv cenv ftag_flag indent e0))))))
    | Eapp (x, ft, ys) ->
      bind (Obj.magic coq_Monad_state) (emit (show_var nenv x)) (fun _ ->
        bind (Obj.magic coq_Monad_state) (emit (show_ftag ftag_flag ft))
          (fun _ ->
          bind (Obj.magic coq_Monad_state) (emit (show_vars nenv ys))
            (fun _ -> newline)))
    | Eprim_val (x, p, e0) ->
      bind (Obj.magic coq_Monad_state)
        (emit (Str (String.String (Coq_x6c, (String.String (Coq_x65,
          (String.String (Coq_x74, (String.String (Coq_x20,
          String.EmptyString)))))))))) (fun _ ->
        bind (Obj.magic coq_Monad_state) (emit (show_var nenv x)) (fun _ ->
          bind (Obj.magic coq_Monad_state)
            (emit (Str (String.String (Coq_x20, (String.String (Coq_x3a,
              (String.String (Coq_x3d, (String.String (Coq_x20,
              (String.String (Coq_x70, (String.String (Coq_x72,
              (String.String (Coq_x69, (String.String (Coq_x6d,
              (String.String (Coq_x3a, (String.String (Coq_x20,
              String.EmptyString)))))))))))))))))))))) (fun _ ->
            bind (Obj.magic coq_Monad_state) (emit_prim p) (fun _ ->
              bind (Obj.magic coq_Monad_state)
                (emit (Str (String.String (Coq_x20, (String.String (Coq_x69,
                  (String.String (Coq_x6e, (String.String (Coq_x20,
                  String.EmptyString)))))))))) (fun _ ->
                bind (Obj.magic coq_Monad_state) newline (fun _ ->
                  emit_exp nenv cenv ftag_flag indent e0))))))
    | Eprim (x, p, ys, e0) ->
      bind (Obj.magic coq_Monad_state)
        (emit (Str (String.String (Coq_x6c, (String.String (Coq_x65,
          (String.String (Coq_x74, (String.String (Coq_x20,
          String.EmptyString)))))))))) (fun _ ->
        bind (Obj.magic coq_Monad_state) (emit (show_var nenv x)) (fun _ ->
          bind (Obj.magic coq_Monad_state)
            (emit (Str (String.String (Coq_x20, (String.String (Coq_x3a,
              (String.String (Coq_x3d, (String.String (Coq_x20,
              (String.String (Coq_x70, (String.String (Coq_x72,
              (String.String (Coq_x69, (String.String (Coq_x6d,
              (String.String (Coq_x5f, String.EmptyString))))))))))))))))))))
            (fun _ ->
            bind (Obj.magic coq_Monad_state) (emit (Str (show_pos p)))
              (fun _ ->
              bind (Obj.magic coq_Monad_state) (emit (show_vars nenv ys))
                (fun _ ->
                bind (Obj.magic coq_Monad_state)
                  (emit (Str (String.String (Coq_x20, (String.String
                    (Coq_x69, (String.String (Coq_x6e, (String.String
                    (Coq_x20, String.EmptyString)))))))))) (fun _ ->
                  bind (Obj.magic coq_Monad_state) newline (fun _ ->
                    emit_exp nenv cenv ftag_flag indent e0)))))))
    | Ehalt x ->
      bind (Obj.magic coq_Monad_state)
        (emit (Str (String.String (Coq_x68, (String.String (Coq_x61,
          (String.String (Coq_x6c, (String.String (Coq_x74, (String.String
          (Coq_x20, String.EmptyString)))))))))))) (fun _ ->
        bind (Obj.magic coq_Monad_state) (emit (show_var nenv x)) (fun _ ->
          newline)))

(** val emit_val :
    name_env -> ctor_env -> bool -> nat -> coq_val -> unit coq_M **)

let rec emit_val nenv cenv ftag_flag indent v =
  bind (Obj.magic coq_Monad_state) (tab indent) (fun _ ->
    match v with
    | Vconstr (tg, l) ->
      bind (Obj.magic coq_Monad_state)
        (emit (Str (String.String (Coq_x63, (String.String (Coq_x6f,
          (String.String (Coq_x6e, (String.String (Coq_x73, (String.String
          (Coq_x74, (String.String (Coq_x72, (String.String (Coq_x20,
          String.EmptyString)))))))))))))))) (fun _ ->
        bind (Obj.magic coq_Monad_state) (emit (show_con cenv tg)) (fun _ ->
          bind (Obj.magic coq_Monad_state)
            (emit (Str (String.String (Coq_x20, String.EmptyString))))
            (fun _ ->
            bind (Obj.magic coq_Monad_state) newline (fun _ ->
              fold_left (fun _ v0 ->
                emit_val nenv cenv ftag_flag (add indent (S O)) v0) l newline))))
    | Vfun (_, fds, f) ->
      (match find_def f fds with
       | Some p ->
         let (p0, e) = p in
         let (_, xs) = p0 in
         bind (Obj.magic coq_Monad_state)
           (emit (Str (String.String (Coq_x66, (String.String (Coq_x75,
             (String.String (Coq_x6e, (String.String (Coq_x20,
             String.EmptyString)))))))))) (fun _ ->
           bind (Obj.magic coq_Monad_state) (emit (show_var nenv f))
             (fun _ ->
             bind (Obj.magic coq_Monad_state) (emit (show_vars nenv xs))
               (fun _ ->
               bind (Obj.magic coq_Monad_state)
                 (emit (Str (String.String (Coq_x3a, (String.String (Coq_x3d,
                   String.EmptyString)))))) (fun _ ->
                 bind (Obj.magic coq_Monad_state) newline (fun _ ->
                   bind (Obj.magic coq_Monad_state)
                     (emit_exp nenv cenv ftag_flag
                       (add (S (S (S (S O)))) indent) e) (fun _ -> newline))))))
       | None ->
         bind (Obj.magic coq_Monad_state)
           (emit (Str (String.String (Coq_x45, (String.String (Coq_x52,
             (String.String (Coq_x52, (String.String (Coq_x4f, (String.String
             (Coq_x52, (String.String (Coq_x21, (String.String (Coq_x20,
             (String.String (Coq_x46, (String.String (Coq_x55, (String.String
             (Coq_x4e, (String.String (Coq_x20,
             String.EmptyString)))))))))))))))))))))))) (fun _ ->
           bind (Obj.magic coq_Monad_state) (emit (show_var nenv f))
             (fun _ ->
             bind (Obj.magic coq_Monad_state)
               (emit (Str (String.String (Coq_x20, (String.String (Coq_x4e,
                 (String.String (Coq_x4f, (String.String (Coq_x54,
                 (String.String (Coq_x20, (String.String (Coq_x46,
                 (String.String (Coq_x4f, (String.String (Coq_x55,
                 (String.String (Coq_x4e, (String.String (Coq_x44,
                 (String.String (Coq_x21,
                 String.EmptyString)))))))))))))))))))))))) (fun _ -> newline))))
    | Vprim p ->
      bind (Obj.magic coq_Monad_state)
        (emit (Str (String.String (Coq_x50, (String.String (Coq_x72,
          (String.String (Coq_x69, (String.String (Coq_x6d, (String.String
          (Coq_x69, (String.String (Coq_x74, (String.String (Coq_x69,
          (String.String (Coq_x76, (String.String (Coq_x65, (String.String
          (Coq_x20, String.EmptyString)))))))))))))))))))))) (fun _ ->
        bind (Obj.magic coq_Monad_state) (emit_prim p) (fun _ -> newline))
    | Vint _ ->
      bind (Obj.magic coq_Monad_state)
        (emit (Str (String.String (Coq_x49, (String.String (Coq_x6e,
          (String.String (Coq_x74, (String.String (Coq_x20,
          String.EmptyString)))))))))) (fun _ -> newline))

(** val show_val : name_env -> ctor_env -> bool -> coq_val -> String.t **)

let show_val nenv cenv ftag_flag v =
  String.String (chr_newline,
    (show_tree (snd (emit_val nenv cenv ftag_flag O v Emp))))

(** val show_exp : name_env -> ctor_env -> bool -> exp -> String.t **)

let show_exp nenv cenv ftag_flag x =
  String.String (chr_newline,
    (show_tree (snd (emit_exp nenv cenv ftag_flag O x Emp))))
