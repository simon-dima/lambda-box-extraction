open AstCommon
open BinNums
open Datatypes
open Frame
open List0
open Nat0
open Cps
open Cps_proto_univ

type 'a coq_Sized = 'a -> nat

(** val size : 'a1 coq_Sized -> 'a1 -> nat **)

let size sized =
  sized

(** val coq_Sized_pos : positive coq_Sized **)

let coq_Sized_pos _ =
  S O

(** val coq_Sized_N : coq_N coq_Sized **)

let coq_Sized_N _ =
  S O

(** val coq_Sized_primitive : primitive coq_Sized **)

let coq_Sized_primitive _ =
  S O

(** val size_list : ('a1 -> nat) -> 'a1 list -> nat **)

let size_list size0 =
  fold_right (fun x n -> S (add (size0 x) n)) (S O)

(** val size_prod : ('a1 -> nat) -> ('a2 -> nat) -> ('a1 * 'a2) -> nat **)

let size_prod sizeA sizeB = function
| (x, y) -> S (add (sizeA x) (sizeB y))

(** val coq_Size_list : 'a1 coq_Sized -> 'a1 list coq_Sized **)

let coq_Size_list h =
  size_list (size h)

(** val coq_Size_prod :
    'a1 coq_Sized -> 'a2 coq_Sized -> ('a1 * 'a2) coq_Sized **)

let coq_Size_prod h h0 =
  size_prod (size h) (size h0)

(** val size_exp : exp -> nat **)

let rec size_exp = function
| Econstr (x, c, ys, e0) ->
  S
    (add
      (add (add (size coq_Sized_pos x) (size coq_Sized_pos c))
        (size (coq_Size_list coq_Sized_pos) ys)) (size_exp e0))
| Ecase (x, ces) ->
  S
    (add (size coq_Sized_pos x)
      (size_list (size_prod (size coq_Sized_pos) size_exp) ces))
| Eproj (x, c, n, y, e0) ->
  S
    (add
      (add
        (add (add (size coq_Sized_pos x) (size coq_Sized_pos c))
          (size coq_Sized_N n)) (size coq_Sized_pos y)) (size_exp e0))
| Eletapp (x, f, ft, ys, e0) ->
  S
    (add
      (add
        (add (add (size coq_Sized_pos x) (size coq_Sized_pos f))
          (size coq_Sized_pos ft)) (size (coq_Size_list coq_Sized_pos) ys))
      (size_exp e0))
| Efun (fds, e0) -> S (add (size_fundefs fds) (size_exp e0))
| Eapp (f, ft, xs) ->
  S
    (add (add (size coq_Sized_pos f) (size coq_Sized_pos ft))
      (size (coq_Size_list coq_Sized_pos) xs))
| Eprim_val (x, p, e0) ->
  S
    (add (add (size coq_Sized_pos x) (size coq_Sized_primitive p))
      (size_exp e0))
| Eprim (x, p, ys, e0) ->
  S
    (add
      (add (add (size coq_Sized_pos x) (size coq_Sized_pos p))
        (size (coq_Size_list coq_Sized_pos) ys)) (size_exp e0))
| Ehalt x -> S (size coq_Sized_pos x)

(** val size_fundefs : fundefs -> nat **)

and size_fundefs = function
| Fcons (f, ft, xs, e, fds0) ->
  S
    (add
      (add
        (add (add (size coq_Sized_pos f) (size coq_Sized_pos ft))
          (size (coq_Size_list coq_Sized_pos) xs)) (size_exp e))
      (size_fundefs fds0))
| Fnil -> S O

(** val coq_Sized_exp : exp coq_Sized **)

let coq_Sized_exp =
  size_exp

(** val coq_Sized_fundefs : fundefs coq_Sized **)

let coq_Sized_fundefs =
  size_fundefs

(** val univ_size : exp_univ -> exp_univ univD -> nat **)

let univ_size = function
| Coq_exp_univ_prod_ctor_tag_exp ->
  size (Obj.magic coq_Size_prod coq_Sized_pos coq_Sized_exp)
| Coq_exp_univ_list_prod_ctor_tag_exp ->
  size (Obj.magic coq_Size_list (coq_Size_prod coq_Sized_pos coq_Sized_exp))
| Coq_exp_univ_fundefs -> size (Obj.magic coq_Sized_fundefs)
| Coq_exp_univ_exp -> size (Obj.magic coq_Sized_exp)
| Coq_exp_univ_N -> size (Obj.magic coq_Sized_N)
| Coq_exp_univ_list_var -> size (Obj.magic coq_Size_list coq_Sized_pos)
| Coq_exp_univ_primitive -> size (Obj.magic coq_Sized_primitive)
| _ -> size (Obj.magic coq_Sized_pos)
