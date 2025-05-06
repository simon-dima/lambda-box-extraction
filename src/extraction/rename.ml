open BinNums
open List0
open Maps
open Cps
open Maps_util

type subst = var M.t

(** val apply_r : subst -> positive -> var **)

let apply_r sigma y =
  match PTree.get y sigma with
  | Some v -> v
  | None -> y

(** val apply_r_list : subst -> positive list -> var list **)

let apply_r_list sigma ys =
  map (apply_r sigma) ys

(** val all_fun_name : fundefs -> var list **)

let rec all_fun_name = function
| Fcons (f, _, _, _, fds') -> f :: (all_fun_name fds')
| Fnil -> []

(** val rename_all_ns : subst -> exp -> exp **)

let rec rename_all_ns sigma = function
| Econstr (x, t0, ys, e') ->
  Econstr (x, t0, (apply_r_list sigma ys), (rename_all_ns sigma e'))
| Ecase (v, cl) ->
  Ecase ((apply_r sigma v),
    (map (fun p -> let (k, e0) = p in (k, (rename_all_ns sigma e0))) cl))
| Eproj (v, t0, n, y, e') ->
  Eproj (v, t0, n, (apply_r sigma y), (rename_all_ns sigma e'))
| Eletapp (v, f, t0, ys, e') ->
  Eletapp (v, (apply_r sigma f), t0, (apply_r_list sigma ys),
    (rename_all_ns sigma e'))
| Efun (fl, e') ->
  let fl' = rename_all_fun_ns sigma fl in Efun (fl', (rename_all_ns sigma e'))
| Eapp (f, t0, ys) -> Eapp ((apply_r sigma f), t0, (apply_r_list sigma ys))
| Eprim_val (x, p, e') -> Eprim_val (x, p, (rename_all_ns sigma e'))
| Eprim (x, f, ys, e') ->
  Eprim (x, f, (apply_r_list sigma ys), (rename_all_ns sigma e'))
| Ehalt v -> Ehalt (apply_r sigma v)

(** val rename_all_fun_ns : subst -> fundefs -> fundefs **)

and rename_all_fun_ns sigma = function
| Fcons (v', t0, ys, e, fds') ->
  Fcons (v', t0, ys, (rename_all_ns sigma e), (rename_all_fun_ns sigma fds'))
| Fnil -> Fnil
