open BinNums
open BinPos
open Datatypes
open List0
open List_util
open Cps
open Set_util

type coq_FVSet = PS.t

(** val fundefs_names : fundefs -> coq_FVSet **)

let rec fundefs_names = function
| Fcons (f, _, _, _, defs') -> PS.add f (fundefs_names defs')
| Fnil -> PS.empty

(** val add_list : coq_FVSet -> coq_FVSet -> PS.elt list -> coq_FVSet **)

let add_list scope fvset ys =
  fold_left (fun fvs y -> if PS.mem y scope then fvs else PS.add y fvs) ys
    fvset

(** val exp_fv_aux : exp -> coq_FVSet -> coq_FVSet -> coq_FVSet **)

let rec exp_fv_aux e scope fvset =
  match e with
  | Econstr (x, _, ys, e0) ->
    let fvset' = add_list scope fvset ys in
    exp_fv_aux e0 (PS.add x scope) fvset'
  | Ecase (x, pats) ->
    let fvset' =
      fold_left (fun fvs p -> exp_fv_aux (snd p) scope fvs) pats fvset
    in
    if PS.mem x scope then fvset' else PS.add x fvset'
  | Eproj (x, _, _, y, e0) ->
    let fvset' = if PS.mem y scope then fvset else PS.add y fvset in
    exp_fv_aux e0 (PS.add x scope) fvset'
  | Eletapp (x, f, _, ys, e0) ->
    let fvset' = add_list scope fvset (f :: ys) in
    exp_fv_aux e0 (PS.add x scope) fvset'
  | Efun (defs, e0) ->
    let (scope', fvset') = fundefs_fv_aux defs scope fvset in
    exp_fv_aux e0 scope' fvset'
  | Eapp (x, _, xs) ->
    let fvset' = add_list scope fvset xs in
    if PS.mem x scope then fvset' else PS.add x fvset'
  | Eprim_val (x, _, e0) -> exp_fv_aux e0 (PS.add x scope) fvset
  | Eprim (x, _, ys, e0) ->
    let fvset' = add_list scope fvset ys in
    exp_fv_aux e0 (PS.add x scope) fvset'
  | Ehalt x -> if PS.mem x scope then fvset else PS.add x fvset

(** val fundefs_fv_aux :
    fundefs -> coq_FVSet -> coq_FVSet -> coq_FVSet * coq_FVSet **)

and fundefs_fv_aux defs scope fvset =
  match defs with
  | Fcons (f, _, ys, e, defs') ->
    let (scope', fvset') = fundefs_fv_aux defs' (PS.add f scope) fvset in
    (scope', (exp_fv_aux e (union_list scope' ys) fvset'))
  | Fnil -> (scope, fvset)

(** val exp_fv : exp -> coq_FVSet **)

let exp_fv e =
  exp_fv_aux e PS.empty PS.empty

(** val fundefs_fv : fundefs -> coq_FVSet **)

let fundefs_fv b =
  snd (fundefs_fv_aux b PS.empty PS.empty)

(** val max_var : exp -> positive -> positive **)

let rec max_var e z =
  match e with
  | Econstr (x, _, ys, e0) -> max_var e0 (max_list (x :: ys) z)
  | Ecase (x, p) ->
    let rec aux p0 z0 =
      match p0 with
      | [] -> Pos.max z0 x
      | y :: p1 -> let (_, e0) = y in aux p1 (max_var e0 z0)
    in aux p z
  | Eproj (x, _, _, y, e0) -> max_var e0 (max_list (x :: (y :: [])) z)
  | Eletapp (x, f, _, ys, e0) -> max_var e0 (max_list (x :: (f :: ys)) z)
  | Efun (defs, e0) -> let z' = max_var_fundefs defs z in max_var e0 z'
  | Eapp (f, _, xs) -> max_list (f :: xs) z
  | Eprim_val (x, _, e0) -> max_var e0 (max_list (x :: []) z)
  | Eprim (x, _, ys, e0) -> max_var e0 (max_list (x :: ys) z)
  | Ehalt x -> Pos.max z x

(** val max_var_fundefs : fundefs -> positive -> positive **)

and max_var_fundefs defs z =
  match defs with
  | Fcons (f, _, ys, e, defs0) ->
    let z' = max_var e z in max_var_fundefs defs0 (max_list (f :: ys) z')
  | Fnil -> z

(** val eq_var : positive -> positive -> bool **)

let eq_var =
  Pos.eqb

(** val occurs_in_vars : var -> var list -> bool **)

let rec occurs_in_vars k = function
| [] -> false
| x :: xs1 -> (||) (eq_var k x) (occurs_in_vars k xs1)

(** val occurs_in_arms' :
    (var -> exp -> bool) -> var -> (ctor_tag * exp) list -> bool **)

let rec occurs_in_arms' occurs_in_exp0 k = function
| [] -> false
| p :: arms1 ->
  let (_, e) = p in
  (||) (occurs_in_exp0 k e) (occurs_in_arms' occurs_in_exp0 k arms1)

(** val occurs_in_exp : var -> exp -> bool **)

let rec occurs_in_exp k = function
| Econstr (z, _, xs, e1) ->
  (||) ((||) (eq_var z k) (occurs_in_vars k xs)) (occurs_in_exp k e1)
| Ecase (x, arms) -> (||) (eq_var k x) (occurs_in_arms' occurs_in_exp k arms)
| Eproj (z, _, _, x, e1) ->
  (||) ((||) (eq_var z k) (eq_var k x)) (occurs_in_exp k e1)
| Eletapp (z, f, _, xs, e1) ->
  (||) ((||) ((||) (eq_var z k) (eq_var f k)) (occurs_in_vars k xs))
    (occurs_in_exp k e1)
| Efun (fds, e0) -> (||) (occurs_in_fundefs k fds) (occurs_in_exp k e0)
| Eapp (x, _, xs) -> (||) (eq_var k x) (occurs_in_vars k xs)
| Eprim_val (z, _, e1) -> (||) (eq_var z k) (occurs_in_exp k e1)
| Eprim (z, _, xs, e1) ->
  (||) ((||) (eq_var z k) (occurs_in_vars k xs)) (occurs_in_exp k e1)
| Ehalt x -> eq_var x k

(** val occurs_in_fundefs : var -> fundefs -> bool **)

and occurs_in_fundefs k = function
| Fcons (z, _, zs, e, fds1) ->
  (||) ((||) ((||) (eq_var z k) (occurs_in_vars k zs)) (occurs_in_exp k e))
    (occurs_in_fundefs k fds1)
| Fnil -> false
