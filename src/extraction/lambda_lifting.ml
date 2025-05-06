open BinNat
open Byte
open Datatypes
open List0
open Maps
open Monad0
open Nat0
open PeanoNat
open Bytestring
open CompM
open Cps
open Identifiers
open Set_util
open State
open Uncurry

type coq_VarInfo =
| FreeVar of var
| WrapperFun of var

type coq_VarInfoMap = coq_VarInfo PTree.t

type coq_FunInfo =
| Fun of var * fun_tag * var list * PS.t * var list
| NoLiftFun of var list * PS.t

type coq_FunInfoMap = coq_FunInfo PTree.t

type coq_GFunInfo =
| GFun
| LGFun

type coq_GFunMap = coq_GFunInfo M.t

type 'a lambdaM = (unit, 'a) compM'

(** val add_functions :
    fundefs -> var list -> PS.t -> var list -> coq_FunInfoMap -> coq_GFunMap
    -> (coq_FunInfoMap * coq_GFunMap) lambdaM **)

let rec add_functions b fvs sfvs args m gfuns =
  match b with
  | Fcons (f, _, xs, _, b0) ->
    bind (coq_MonadErrorT coq_MonadState)
      (add_functions b0 fvs sfvs args m gfuns) (fun x ->
      let (m', gfuns') = x in
      let is_closed = match fvs with
                      | [] -> true
                      | _ :: _ -> false in
      let is_closed_lifted = Nat.eqb (length fvs) (length args) in
      let gfuns'' = if is_closed then M.set f GFun gfuns' else gfuns' in
      if is_closed_lifted
      then bind (coq_MonadErrorT coq_MonadState)
             (get_name f (String.String (Coq_x5f, (String.String (Coq_x6b,
               (String.String (Coq_x6e, (String.String (Coq_x6f,
               (String.String (Coq_x77, (String.String (Coq_x6e,
               String.EmptyString))))))))))))) (fun f' ->
             bind (coq_MonadErrorT coq_MonadState)
               (get_ftag (N.of_nat (length xs))) (fun ft' ->
               let gfuns''' = M.set f' LGFun gfuns'' in
               ret (coq_MonadErrorT coq_MonadState)
                 ((M.set f (Fun (f', ft', fvs, sfvs, args)) m'), gfuns''')))
      else ret (coq_MonadErrorT coq_MonadState)
             ((M.set f (NoLiftFun (fvs, sfvs)) m'), gfuns''))
  | Fnil -> ret (coq_MonadErrorT coq_MonadState) (m, gfuns)

(** val rename : coq_VarInfoMap -> var -> var **)

let rename map0 x =
  match M.get x map0 with
  | Some inf -> (match inf with
                 | FreeVar y -> y
                 | WrapperFun y -> y)
  | None -> x

(** val rename_lst : coq_VarInfoMap -> var list -> var list **)

let rename_lst map0 xs =
  map (rename map0) xs

(** val add_free_vars :
    var list -> coq_VarInfoMap -> (var list * coq_VarInfoMap) lambdaM **)

let rec add_free_vars fvs m =
  match fvs with
  | [] -> ret (coq_MonadErrorT coq_MonadState) ([], m)
  | y :: ys ->
    bind (coq_MonadErrorT coq_MonadState) (add_free_vars ys m) (fun p ->
      bind (coq_MonadErrorT coq_MonadState) (get_name y String.EmptyString)
        (fun y' ->
        let (ys', m') = p in
        ret (coq_MonadErrorT coq_MonadState) ((y' :: ys'),
          (M.set y (FreeVar y') m'))))

(** val make_wrappers :
    fundefs -> coq_VarInfoMap -> coq_FunInfoMap -> (fundefs
    option * coq_VarInfoMap) lambdaM **)

let rec make_wrappers b fvm fm =
  match b with
  | Fcons (f, ft, xs, _, b0) ->
    (match M.get f fm with
     | Some inf ->
       (match inf with
        | Fun (f', ft', _, _, args) ->
          bind (coq_MonadErrorT coq_MonadState)
            (get_name f (String.String (Coq_x5f, (String.String (Coq_x77,
              (String.String (Coq_x72, (String.String (Coq_x61,
              (String.String (Coq_x70, (String.String (Coq_x70,
              (String.String (Coq_x65, (String.String (Coq_x72,
              String.EmptyString))))))))))))))))) (fun g ->
            bind (coq_MonadErrorT coq_MonadState)
              (get_names_lst xs String.EmptyString) (fun xs' ->
              bind (coq_MonadErrorT coq_MonadState) (make_wrappers b0 fvm fm)
                (fun cm ->
                let (mb, fvm') = cm in
                let fvm'' = M.set f (WrapperFun g) fvm' in
                (match mb with
                 | Some bw ->
                   ret (coq_MonadErrorT coq_MonadState) ((Some (Fcons (g, ft,
                     xs', (Eapp (f', ft', (app xs' (rename_lst fvm args)))),
                     bw))), fvm'')
                 | None ->
                   ret (coq_MonadErrorT coq_MonadState) ((Some (Fcons (g, ft,
                     xs', (Eapp (f', ft', (app xs' (rename_lst fvm args)))),
                     Fnil))), fvm'')))))
        | NoLiftFun (_, _) -> ret (coq_MonadErrorT coq_MonadState) (None, fvm))
     | None ->
       bind (coq_MonadErrorT coq_MonadState) (get_pp_name f) (fun f_str ->
         failwith
           (String.append (String.String (Coq_x49, (String.String (Coq_x6e,
             (String.String (Coq_x74, (String.String (Coq_x65, (String.String
             (Coq_x72, (String.String (Coq_x6e, (String.String (Coq_x61,
             (String.String (Coq_x6c, (String.String (Coq_x20, (String.String
             (Coq_x65, (String.String (Coq_x72, (String.String (Coq_x72,
             (String.String (Coq_x6f, (String.String (Coq_x72, (String.String
             (Coq_x20, (String.String (Coq_x69, (String.String (Coq_x6e,
             (String.String (Coq_x20, (String.String (Coq_x6d, (String.String
             (Coq_x61, (String.String (Coq_x6b, (String.String (Coq_x65,
             (String.String (Coq_x5f, (String.String (Coq_x77, (String.String
             (Coq_x72, (String.String (Coq_x61, (String.String (Coq_x70,
             (String.String (Coq_x70, (String.String (Coq_x65, (String.String
             (Coq_x72, (String.String (Coq_x73, (String.String (Coq_x3a,
             (String.String (Coq_x20, (String.String (Coq_x41, (String.String
             (Coq_x6c, (String.String (Coq_x6c, (String.String (Coq_x20,
             (String.String (Coq_x6b, (String.String (Coq_x6e, (String.String
             (Coq_x6f, (String.String (Coq_x77, (String.String (Coq_x6e,
             (String.String (Coq_x20, (String.String (Coq_x66, (String.String
             (Coq_x75, (String.String (Coq_x6e, (String.String (Coq_x63,
             (String.String (Coq_x74, (String.String (Coq_x69, (String.String
             (Coq_x6f, (String.String (Coq_x6e, (String.String (Coq_x73,
             (String.String (Coq_x20, (String.String (Coq_x73, (String.String
             (Coq_x68, (String.String (Coq_x6f, (String.String (Coq_x75,
             (String.String (Coq_x6c, (String.String (Coq_x64, (String.String
             (Coq_x20, (String.String (Coq_x62, (String.String (Coq_x65,
             (String.String (Coq_x20, (String.String (Coq_x69, (String.String
             (Coq_x6e, (String.String (Coq_x20, (String.String (Coq_x6d,
             (String.String (Coq_x61, (String.String (Coq_x70, (String.String
             (Coq_x2e, (String.String (Coq_x20, (String.String (Coq_x43,
             (String.String (Coq_x6f, (String.String (Coq_x75, (String.String
             (Coq_x6c, (String.String (Coq_x64, (String.String (Coq_x20,
             (String.String (Coq_x6e, (String.String (Coq_x6f, (String.String
             (Coq_x74, (String.String (Coq_x20, (String.String (Coq_x66,
             (String.String (Coq_x69, (String.String (Coq_x6e, (String.String
             (Coq_x64, (String.String (Coq_x20,
             String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
             f_str)))
  | Fnil -> ret (coq_MonadErrorT coq_MonadState) ((Some Fnil), fvm)

(** val fundefs_max_params : fundefs -> nat **)

let fundefs_max_params b =
  let rec aux b0 p =
    match b0 with
    | Fcons (_, _, xs, _, b1) -> aux b1 (max (length xs) p)
    | Fnil -> O
  in aux b O

(** val fundefs_true_fv_aux :
    (var list -> coq_FVSet -> bool) -> coq_FunInfoMap -> fundefs -> coq_FVSet
    -> PS.t -> coq_FVSet * PS.t **)

let fundefs_true_fv_aux lift_dec funmap =
  let rec exp_true_fv_aux e scope fvset =
    match e with
    | Econstr (x, _, ys, e0) ->
      let fvset' = add_list scope fvset ys in
      exp_true_fv_aux e0 (PS.add x scope) fvset'
    | Ecase (x, pats) ->
      let fvset' =
        fold_left (fun fvs p -> exp_true_fv_aux (snd p) scope fvs) pats fvset
      in
      if PS.mem x scope then fvset' else PS.add x fvset'
    | Eproj (x, _, _, y, e0) ->
      let fvset' = if PS.mem y scope then fvset else PS.add y fvset in
      exp_true_fv_aux e0 (PS.add x scope) fvset'
    | Eletapp (x, f, _, ys, e0) ->
      let fvset' =
        match PTree.get f funmap with
        | Some f0 ->
          (match f0 with
           | Fun (f', _, _, _, lfvs) ->
             if lift_dec lfvs fvset
             then union_list fvset (f' :: lfvs)
             else if PS.mem f scope then fvset else PS.add f fvset
           | NoLiftFun (_, _) ->
             if PS.mem f scope then fvset else PS.add f fvset)
        | None -> if PS.mem f scope then fvset else PS.add f fvset
      in
      let fvset'' = add_list scope fvset' ys in
      exp_true_fv_aux e0 (PS.add x scope) fvset''
    | Efun (defs, e0) ->
      let (scope', fvset') = fundefs_true_fv_aux0 defs scope fvset in
      exp_true_fv_aux e0 scope' fvset'
    | Eapp (f, _, xs) ->
      let fvset' =
        match PTree.get f funmap with
        | Some f0 ->
          (match f0 with
           | Fun (f', _, _, _, lfvs) ->
             if lift_dec lfvs fvset
             then union_list fvset (f' :: lfvs)
             else if PS.mem f scope then fvset else PS.add f fvset
           | NoLiftFun (_, _) ->
             if PS.mem f scope then fvset else PS.add f fvset)
        | None -> if PS.mem f scope then fvset else PS.add f fvset
      in
      add_list scope fvset' xs
    | Eprim_val (x, _, e0) -> exp_true_fv_aux e0 (PS.add x scope) fvset
    | Eprim (x, _, ys, e0) ->
      let fvset' = add_list scope fvset ys in
      exp_true_fv_aux e0 (PS.add x scope) fvset'
    | Ehalt x -> if PS.mem x scope then fvset else PS.add x fvset
  and fundefs_true_fv_aux0 defs scope fvset =
    match defs with
    | Fcons (f, _, ys, e, defs') ->
      let (scope', fvset') = fundefs_true_fv_aux0 defs' (PS.add f scope) fvset
      in
      (scope', (exp_true_fv_aux e (union_list scope' ys) fvset'))
    | Fnil -> (scope, fvset)
  in fundefs_true_fv_aux0

(** val fundefs_true_fv :
    (var list -> coq_FVSet -> bool) -> coq_FunInfoMap -> fundefs -> PS.t **)

let fundefs_true_fv lift_dec funmap b =
  snd (fundefs_true_fv_aux lift_dec funmap b PS.empty PS.empty)

(** val occurs_in_exp : var -> PS.t -> exp -> bool **)

let rec occurs_in_exp k curr_f = function
| Econstr (z, _, xs, e1) ->
  (||) ((||) (eq_var z k) (occurs_in_vars k xs)) (occurs_in_exp k curr_f e1)
| Ecase (x, arms) ->
  (||) (eq_var k x)
    (let rec occurs_in_arms = function
     | [] -> false
     | p :: arms1 ->
       let (_, e0) = p in
       (||) (occurs_in_exp k curr_f e0) (occurs_in_arms arms1)
     in occurs_in_arms arms)
| Eproj (z, _, _, x, e1) ->
  (||) ((||) (eq_var z k) (eq_var k x)) (occurs_in_exp k curr_f e1)
| Eletapp (z, f, _, xs, e1) ->
  (||)
    ((||) ((||) ((||) (eq_var z k) (eq_var f k)) (PS.mem f curr_f))
      (occurs_in_vars k xs)) (occurs_in_exp k curr_f e1)
| Efun (fds, e0) ->
  (||) (occurs_in_fundefs k curr_f fds) (occurs_in_exp k curr_f e0)
| Eapp (x, _, xs) ->
  (||) ((||) (eq_var k x) (PS.mem x curr_f)) (occurs_in_vars k xs)
| Eprim_val (z, _, e1) -> (||) (eq_var z k) (occurs_in_exp k curr_f e1)
| Eprim (z, _, xs, e1) ->
  (||) ((||) (eq_var z k) (occurs_in_vars k xs)) (occurs_in_exp k curr_f e1)
| Ehalt x -> eq_var x k

(** val occurs_in_fundefs : var -> PS.t -> fundefs -> bool **)

and occurs_in_fundefs k curr_f = function
| Fcons (z, _, zs, e, fds1) ->
  (||)
    ((||) ((||) (eq_var z k) (occurs_in_vars k zs))
      (occurs_in_exp k curr_f e)) (occurs_in_fundefs k curr_f fds1)
| Fnil -> false

(** val stack_push : var -> PS.t -> exp -> nat **)

let rec stack_push x curr_f = function
| Econstr (_, _, _, e0) -> stack_push x curr_f e0
| Ecase (_, p) ->
  fold_left (fun n br -> max n (stack_push x curr_f (snd br))) p O
| Eproj (_, _, _, _, e0) -> stack_push x curr_f e0
| Eletapp (_, _, _, _, e0) ->
  let n = stack_push x curr_f e0 in
  if occurs_in_exp x curr_f e0 then add n (S O) else n
| Efun (_, e0) -> stack_push x curr_f e0
| Eprim_val (_, _, e0) -> stack_push x curr_f e0
| Eprim (_, _, _, e0) -> stack_push x curr_f e0
| _ -> O

(** val stack_push_fundefs_aux : var -> PS.t -> fundefs -> nat **)

let rec stack_push_fundefs_aux x curr_f = function
| Fcons (_, _, _, e, b') ->
  max (stack_push x curr_f e) (stack_push_fundefs_aux x curr_f b')
| Fnil -> O

(** val stack_push_fundefs : var -> fundefs -> nat **)

let stack_push_fundefs x b =
  stack_push_fundefs_aux x (fundefs_names b) b

(** val exp_lambda_lift :
    nat -> nat -> (var list -> coq_FVSet -> bool) -> exp -> PS.t -> PS.t ->
    coq_VarInfoMap -> coq_FunInfoMap -> coq_GFunMap -> exp lambdaM **)

let exp_lambda_lift max_args max_push lift_dec =
  let rec exp_lambda_lift0 e scope active_funs fvm fm gfuns =
    match e with
    | Econstr (x, t0, ys, e0) ->
      bind (coq_MonadErrorT coq_MonadState)
        (exp_lambda_lift0 e0 (PS.add x scope) active_funs fvm fm gfuns)
        (fun e' ->
        ret (coq_MonadErrorT coq_MonadState) (Econstr (x, t0,
          (rename_lst fvm ys), e')))
    | Ecase (x, p) ->
      bind (coq_MonadErrorT coq_MonadState)
        (let rec mapM_ll = function
         | [] -> ret (coq_MonadErrorT coq_MonadState) []
         | y :: p0 ->
           let (c, e0) = y in
           bind (coq_MonadErrorT coq_MonadState)
             (exp_lambda_lift0 e0 scope active_funs fvm fm gfuns) (fun e' ->
             bind (coq_MonadErrorT coq_MonadState) (mapM_ll p0) (fun p' ->
               ret (coq_MonadErrorT coq_MonadState) ((c, e') :: p')))
         in mapM_ll p) (fun p' ->
        ret (coq_MonadErrorT coq_MonadState) (Ecase ((rename fvm x), p')))
    | Eproj (x, t0, n, y, e0) ->
      bind (coq_MonadErrorT coq_MonadState)
        (exp_lambda_lift0 e0 (PS.add x scope) active_funs fvm fm gfuns)
        (fun e' ->
        ret (coq_MonadErrorT coq_MonadState) (Eproj (x, t0, n,
          (rename fvm y), e')))
    | Eletapp (x, f, ft, ys, e0) ->
      bind (coq_MonadErrorT coq_MonadState)
        (exp_lambda_lift0 e0 (PS.add x scope) active_funs fvm fm gfuns)
        (fun e' ->
        match PTree.get f fm with
        | Some f0 ->
          (match f0 with
           | Fun (f', ft', _, _, args) ->
             if lift_dec args scope
             then ret (coq_MonadErrorT coq_MonadState) (Eletapp (x,
                    (rename fvm f'), ft', (rename_lst fvm (app ys args)), e'))
             else ret (coq_MonadErrorT coq_MonadState) (Eletapp (x,
                    (rename fvm f), ft, (rename_lst fvm ys), e'))
           | NoLiftFun (_, _) ->
             ret (coq_MonadErrorT coq_MonadState) (Eletapp (x,
               (rename fvm f), ft, (rename_lst fvm ys), e')))
        | None ->
          ret (coq_MonadErrorT coq_MonadState) (Eletapp (x, (rename fvm f),
            ft, (rename_lst fvm ys), e')))
    | Efun (b, e0) ->
      let sfvsi = fundefs_true_fv lift_dec fm b in
      let sfvs =
        PS.filter (fun x ->
          match M.get x gfuns with
          | Some _ -> false
          | None -> true) sfvsi
      in
      let fvs = PS.elements sfvs in
      let lfvs =
        if existsb (fun x ->
             let n = stack_push_fundefs x b in Nat.ltb max_push n) fvs
        then []
        else fvs
      in
      let lifted_args = sub max_args (fundefs_max_params b) in
      let lfvs0 = if Nat.ltb lifted_args (length lfvs) then [] else lfvs in
      bind (coq_MonadErrorT coq_MonadState)
        (add_functions b fvs sfvs lfvs0 fm gfuns) (fun x ->
        let (fm', gfuns') = x in
        let names = fundefs_names b in
        bind (coq_MonadErrorT coq_MonadState)
          (fundefs_lambda_lift b b names (PS.union names active_funs) fvm fm'
            gfuns' lifted_args) (fun b' ->
          bind (coq_MonadErrorT coq_MonadState) (make_wrappers b fvm fm')
            (fun x0 ->
            let (ob, fvm') = x0 in
            bind (coq_MonadErrorT coq_MonadState)
              (exp_lambda_lift0 e0 (PS.union names scope) active_funs fvm'
                fm' gfuns') (fun e' ->
              match ob with
              | Some bw ->
                ret (coq_MonadErrorT coq_MonadState) (Efun (b', (Efun (bw,
                  e'))))
              | None -> ret (coq_MonadErrorT coq_MonadState) (Efun (b', e'))))))
    | Eapp (f, ft, xs) ->
      (match PTree.get f fm with
       | Some f0 ->
         (match f0 with
          | Fun (f', ft', _, _, args) ->
            if lift_dec args scope
            then ret (coq_MonadErrorT coq_MonadState) (Eapp ((rename fvm f'),
                   ft', (rename_lst fvm (app xs args))))
            else ret (coq_MonadErrorT coq_MonadState) (Eapp ((rename fvm f),
                   ft, (rename_lst fvm xs)))
          | NoLiftFun (_, _) ->
            ret (coq_MonadErrorT coq_MonadState) (Eapp ((rename fvm f), ft,
              (rename_lst fvm xs))))
       | None ->
         ret (coq_MonadErrorT coq_MonadState) (Eapp ((rename fvm f), ft,
           (rename_lst fvm xs))))
    | Eprim_val (x, p, e0) ->
      bind (coq_MonadErrorT coq_MonadState)
        (exp_lambda_lift0 e0 (PS.add x scope) active_funs fvm fm gfuns)
        (fun e' ->
        ret (coq_MonadErrorT coq_MonadState) (Eprim_val (x, p, e')))
    | Eprim (x, f, ys, e0) ->
      bind (coq_MonadErrorT coq_MonadState)
        (exp_lambda_lift0 e0 (PS.add x scope) active_funs fvm fm gfuns)
        (fun e' ->
        ret (coq_MonadErrorT coq_MonadState) (Eprim (x, f,
          (rename_lst fvm ys), e')))
    | Ehalt x -> ret (coq_MonadErrorT coq_MonadState) (Ehalt (rename fvm x))
  and fundefs_lambda_lift b bfull fnames active_funs fvm fm gfuns fv_no =
    match b with
    | Fcons (f, ft, xs, e, b0) ->
      (match M.get f fm with
       | Some f0 ->
         (match f0 with
          | Fun (f', ft', _, sfvs, args) ->
            bind (coq_MonadErrorT coq_MonadState) (add_free_vars args fvm)
              (fun x ->
              let (ys, fvm') = x in
              bind (coq_MonadErrorT coq_MonadState)
                (make_wrappers bfull fvm' fm) (fun x0 ->
                let (ob, fvm'') = x0 in
                bind (coq_MonadErrorT coq_MonadState)
                  (exp_lambda_lift0 e (PS.union fnames (union_list sfvs xs))
                    active_funs fvm'' fm gfuns) (fun e' ->
                  bind (coq_MonadErrorT coq_MonadState)
                    (fundefs_lambda_lift b0 bfull fnames active_funs fvm fm
                      gfuns fv_no) (fun b' ->
                    match ob with
                    | Some bw ->
                      ret (coq_MonadErrorT coq_MonadState) (Fcons (f', ft',
                        (app xs ys), (Efun (bw, e')), b'))
                    | None ->
                      bind (coq_MonadErrorT coq_MonadState) (get_pp_name f)
                        (fun f_str ->
                        failwith
                          (String.append (String.String (Coq_x49,
                            (String.String (Coq_x6e, (String.String (Coq_x74,
                            (String.String (Coq_x65, (String.String (Coq_x72,
                            (String.String (Coq_x6e, (String.String (Coq_x61,
                            (String.String (Coq_x6c, (String.String (Coq_x20,
                            (String.String (Coq_x65, (String.String (Coq_x72,
                            (String.String (Coq_x72, (String.String (Coq_x6f,
                            (String.String (Coq_x72, (String.String (Coq_x20,
                            (String.String (Coq_x69, (String.String (Coq_x6e,
                            (String.String (Coq_x20, (String.String (Coq_x66,
                            (String.String (Coq_x75, (String.String (Coq_x6e,
                            (String.String (Coq_x64, (String.String (Coq_x65,
                            (String.String (Coq_x66, (String.String (Coq_x73,
                            (String.String (Coq_x5f, (String.String (Coq_x6c,
                            (String.String (Coq_x61, (String.String (Coq_x6d,
                            (String.String (Coq_x62, (String.String (Coq_x64,
                            (String.String (Coq_x61, (String.String (Coq_x5f,
                            (String.String (Coq_x6c, (String.String (Coq_x69,
                            (String.String (Coq_x66, (String.String (Coq_x74,
                            (String.String (Coq_x3a, (String.String (Coq_x20,
                            (String.String (Coq_x57, (String.String (Coq_x72,
                            (String.String (Coq_x61, (String.String (Coq_x70,
                            (String.String (Coq_x70, (String.String (Coq_x65,
                            (String.String (Coq_x72, (String.String (Coq_x73,
                            (String.String (Coq_x20, (String.String (Coq_x63,
                            (String.String (Coq_x61, (String.String (Coq_x6e,
                            (String.String (Coq_x6e, (String.String (Coq_x6f,
                            (String.String (Coq_x74, (String.String (Coq_x20,
                            (String.String (Coq_x62, (String.String (Coq_x65,
                            (String.String (Coq_x20, (String.String (Coq_x65,
                            (String.String (Coq_x6d, (String.String (Coq_x70,
                            (String.String (Coq_x74, (String.String (Coq_x79,
                            (String.String (Coq_x20, (String.String (Coq_x69,
                            (String.String (Coq_x6e, (String.String (Coq_x20,
                            (String.String (Coq_x6c, (String.String (Coq_x61,
                            (String.String (Coq_x6d, (String.String (Coq_x62,
                            (String.String (Coq_x64, (String.String (Coq_x61,
                            (String.String (Coq_x20, (String.String (Coq_x6c,
                            (String.String (Coq_x69, (String.String (Coq_x66,
                            (String.String (Coq_x74, (String.String (Coq_x65,
                            (String.String (Coq_x64, (String.String (Coq_x20,
                            (String.String (Coq_x66, (String.String (Coq_x75,
                            (String.String (Coq_x6e, (String.String (Coq_x63,
                            (String.String (Coq_x74, (String.String (Coq_x69,
                            (String.String (Coq_x6f, (String.String (Coq_x6e,
                            (String.String (Coq_x20,
                            String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                            f_str))))))
          | NoLiftFun (_, sfvs) ->
            bind (coq_MonadErrorT coq_MonadState)
              (exp_lambda_lift0 e (PS.union fnames (union_list sfvs xs))
                active_funs fvm fm gfuns) (fun e' ->
              bind (coq_MonadErrorT coq_MonadState)
                (fundefs_lambda_lift b0 bfull fnames active_funs fvm fm gfuns
                  fv_no) (fun b' ->
                ret (coq_MonadErrorT coq_MonadState) (Fcons (f, ft, xs, e',
                  b')))))
       | None ->
         bind (coq_MonadErrorT coq_MonadState) (get_pp_name f) (fun f_str ->
           failwith
             (String.append (String.String (Coq_x49, (String.String (Coq_x6e,
               (String.String (Coq_x74, (String.String (Coq_x65,
               (String.String (Coq_x72, (String.String (Coq_x6e,
               (String.String (Coq_x61, (String.String (Coq_x6c,
               (String.String (Coq_x20, (String.String (Coq_x65,
               (String.String (Coq_x72, (String.String (Coq_x72,
               (String.String (Coq_x6f, (String.String (Coq_x72,
               (String.String (Coq_x20, (String.String (Coq_x69,
               (String.String (Coq_x6e, (String.String (Coq_x20,
               (String.String (Coq_x66, (String.String (Coq_x75,
               (String.String (Coq_x6e, (String.String (Coq_x64,
               (String.String (Coq_x65, (String.String (Coq_x66,
               (String.String (Coq_x73, (String.String (Coq_x5f,
               (String.String (Coq_x6c, (String.String (Coq_x61,
               (String.String (Coq_x6d, (String.String (Coq_x62,
               (String.String (Coq_x64, (String.String (Coq_x61,
               (String.String (Coq_x5f, (String.String (Coq_x6c,
               (String.String (Coq_x69, (String.String (Coq_x66,
               (String.String (Coq_x74, (String.String (Coq_x3a,
               (String.String (Coq_x20, (String.String (Coq_x41,
               (String.String (Coq_x6c, (String.String (Coq_x6c,
               (String.String (Coq_x20, (String.String (Coq_x6b,
               (String.String (Coq_x6e, (String.String (Coq_x6f,
               (String.String (Coq_x77, (String.String (Coq_x6e,
               (String.String (Coq_x20, (String.String (Coq_x66,
               (String.String (Coq_x75, (String.String (Coq_x6e,
               (String.String (Coq_x63, (String.String (Coq_x74,
               (String.String (Coq_x69, (String.String (Coq_x6f,
               (String.String (Coq_x6e, (String.String (Coq_x73,
               (String.String (Coq_x20, (String.String (Coq_x73,
               (String.String (Coq_x68, (String.String (Coq_x6f,
               (String.String (Coq_x75, (String.String (Coq_x6c,
               (String.String (Coq_x64, (String.String (Coq_x20,
               (String.String (Coq_x62, (String.String (Coq_x65,
               (String.String (Coq_x20, (String.String (Coq_x69,
               (String.String (Coq_x6e, (String.String (Coq_x20,
               (String.String (Coq_x6d, (String.String (Coq_x61,
               (String.String (Coq_x70, (String.String (Coq_x2e,
               (String.String (Coq_x20, (String.String (Coq_x43,
               (String.String (Coq_x6f, (String.String (Coq_x75,
               (String.String (Coq_x6c, (String.String (Coq_x64,
               (String.String (Coq_x20, (String.String (Coq_x6e,
               (String.String (Coq_x6f, (String.String (Coq_x74,
               (String.String (Coq_x20, (String.String (Coq_x66,
               (String.String (Coq_x69, (String.String (Coq_x6e,
               (String.String (Coq_x64, (String.String (Coq_x20,
               String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
               f_str)))
    | Fnil -> ret (coq_MonadErrorT coq_MonadState) Fnil
  in exp_lambda_lift0

(** val lift_all : var list -> coq_FVSet -> bool **)

let lift_all _ _ =
  true

(** val lift_conservative : var list -> coq_FVSet -> bool **)

let lift_conservative fvs scope =
  PS.subset (union_list PS.empty fvs) scope

(** val lambda_lift :
    nat -> nat -> bool -> exp -> comp_data -> exp error * comp_data **)

let lambda_lift args no_push inl e c =
  let (e', p) =
    run_compM
      (exp_lambda_lift args no_push
        (if inl then lift_all else lift_conservative) e PS.empty PS.empty
        PTree.empty PTree.empty M.empty) c ()
  in
  let (c', _) = p in (e', c')
