open AstCommon
open BasicAst
open BinNums
open Maps

type __ = Obj.t

module M = PTree

type var = M.elt

type fun_tag = M.elt

type ind_tag = M.elt

type ctor_tag = M.elt

type prim = M.elt

(** val findtag : (ctor_tag * 'a1) list -> ctor_tag -> 'a1 option **)

let rec findtag cl c =
  match cl with
  | [] -> None
  | p :: cl' ->
    let (c', a) = p in if M.elt_eq c' c then Some a else findtag cl' c

type exp =
| Econstr of var * ctor_tag * var list * exp
| Ecase of var * (ctor_tag * exp) list
| Eproj of var * ctor_tag * coq_N * var * exp
| Eletapp of var * var * fun_tag * var list * exp
| Efun of fundefs * exp
| Eapp of var * fun_tag * var list
| Eprim_val of var * primitive * exp
| Eprim of var * prim * var list * exp
| Ehalt of var
and fundefs =
| Fcons of var * fun_tag * var list * exp * fundefs
| Fnil

type coq_val =
| Vconstr of ctor_tag * coq_val list
| Vfun of coq_val M.t * fundefs * var
| Vprim of primitive
| Vint of coq_Z

(** val def_funs :
    fundefs -> fundefs -> coq_val M.t -> coq_val M.t -> coq_val M.t **)

let rec def_funs fl0 fl rho0 rho =
  match fl with
  | Fcons (f, _, _, _, fl') ->
    M.set f (Vfun (rho0, fl0, f)) (def_funs fl0 fl' rho0 rho)
  | Fnil -> rho

(** val find_def : var -> fundefs -> ((fun_tag * var list) * exp) option **)

let rec find_def f = function
| Fcons (f', t0, ys, e, fl') ->
  if M.elt_eq f f' then Some ((t0, ys), e) else find_def f fl'
| Fnil -> None

type ctor_ty_info = { ctor_name : name; ctor_ind_name : name;
                      ctor_ind_tag : ind_tag; ctor_arity : coq_N;
                      ctor_ordinal : coq_N }

type ind_ty_info = (ctor_tag * coq_N) list

type ctor_env = ctor_ty_info M.tree

type ind_env = ind_ty_info M.tree

type fun_ty_info = coq_N * coq_N list

type fun_env = fun_ty_info M.tree

(** val add_closure_tag : positive -> positive -> ctor_env -> ctor_env **)

let add_closure_tag c i cenv =
  let info = { ctor_name = Coq_nAnon; ctor_ind_name = Coq_nAnon;
    ctor_ind_tag = i; ctor_arity = (Npos (Coq_xO Coq_xH)); ctor_ordinal = N0 }
  in
  M.set c info cenv
