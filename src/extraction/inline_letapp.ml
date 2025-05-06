open Monad0
open Cps
open Cps_util
open Ctx

(** val inline_letapp : exp -> var -> (exp_ctx * var) option **)

let rec inline_letapp e z =
  match e with
  | Econstr (x, ct, xs, e0) ->
    bind (Obj.magic coq_OptMonad) (inline_letapp e0 z) (fun res ->
      let (c, v) = res in
      ret (Obj.magic coq_OptMonad) ((Econstr_c (x, ct, xs, c)), v))
  | Ecase (_, _) -> None
  | Eproj (x, n, ct, y, e0) ->
    bind (Obj.magic coq_OptMonad) (inline_letapp e0 z) (fun res ->
      let (c, v) = res in
      ret (Obj.magic coq_OptMonad) ((Eproj_c (x, n, ct, y, c)), v))
  | Eletapp (x, f, ft, ys, e0) ->
    bind (Obj.magic coq_OptMonad) (inline_letapp e0 z) (fun res ->
      let (c, v) = res in
      ret (Obj.magic coq_OptMonad) ((Eletapp_c (x, f, ft, ys, c)), v))
  | Efun (b, e0) ->
    bind (Obj.magic coq_OptMonad) (inline_letapp e0 z) (fun res ->
      let (c, v) = res in ret (Obj.magic coq_OptMonad) ((Efun1_c (b, c)), v))
  | Eapp (f, ft, ys) ->
    ret (Obj.magic coq_OptMonad) ((Eletapp_c (z, f, ft, ys, Hole_c)), z)
  | Eprim_val (x, p, e0) ->
    bind (Obj.magic coq_OptMonad) (inline_letapp e0 z) (fun res ->
      let (c, v) = res in
      ret (Obj.magic coq_OptMonad) ((Eprim_val_c (x, p, c)), v))
  | Eprim (x, p, ys, e0) ->
    bind (Obj.magic coq_OptMonad) (inline_letapp e0 z) (fun res ->
      let (c, v) = res in
      ret (Obj.magic coq_OptMonad) ((Eprim_c (x, p, ys, c)), v))
  | Ehalt x -> ret (Obj.magic coq_OptMonad) (Hole_c, x)
