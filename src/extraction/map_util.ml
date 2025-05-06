open BinNums
open Datatypes
open List0
open Maps

type __ = Obj.t

module M = PTree

(** val get_list : M.elt list -> 'a1 M.t -> 'a1 list option **)

let rec get_list xs rho =
  match xs with
  | [] -> Some []
  | x :: xs' ->
    (match M.get x rho with
     | Some v ->
       (match get_list xs' rho with
        | Some vs -> Some (v :: vs)
        | None -> None)
     | None -> None)

(** val set_lists : M.elt list -> 'a1 list -> 'a1 M.t -> 'a1 M.t option **)

let rec set_lists xs vs rho =
  match xs with
  | [] -> (match vs with
           | [] -> Some rho
           | _ :: _ -> None)
  | x :: xs' ->
    (match vs with
     | [] -> None
     | v :: vs' ->
       (match set_lists xs' vs' rho with
        | Some rho' -> Some (M.set x v rho')
        | None -> None))

(** val set_list : (M.elt * 'a1) list -> 'a1 M.t -> 'a1 M.t **)

let set_list l map0 =
  fold_right (fun xv cmap -> M.set (fst xv) (snd xv) cmap) map0 l
