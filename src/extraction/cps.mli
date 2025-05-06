open AstCommon
open BasicAst
open BinNums
open Maps

type __ = Obj.t

module M :
 sig
  type elt = positive

  val elt_eq : positive -> positive -> bool

  type 'a tree' = 'a PTree.tree' =
  | Node001 of 'a tree'
  | Node010 of 'a
  | Node011 of 'a * 'a tree'
  | Node100 of 'a tree'
  | Node101 of 'a tree' * 'a tree'
  | Node110 of 'a tree' * 'a
  | Node111 of 'a tree' * 'a * 'a tree'

  type 'a tree = 'a PTree.tree =
  | Empty
  | Nodes of 'a tree'

  type 'a t = 'a tree

  val coq_Node : 'a1 tree -> 'a1 option -> 'a1 tree -> 'a1 tree

  val empty : 'a1 t

  val get' : positive -> 'a1 tree' -> 'a1 option

  val get : positive -> 'a1 tree -> 'a1 option

  val set0 : positive -> 'a1 -> 'a1 tree'

  val set' : positive -> 'a1 -> 'a1 tree' -> 'a1 tree'

  val set : positive -> 'a1 -> 'a1 tree -> 'a1 tree

  val rem' : positive -> 'a1 tree' -> 'a1 tree

  val remove' : positive -> 'a1 tree' -> 'a1 tree

  val remove : positive -> 'a1 tree -> 'a1 tree

  val tree_case :
    'a2 -> ('a1 tree -> 'a1 option -> 'a1 tree -> 'a2) -> 'a1 tree -> 'a2

  val tree_rec' :
    'a2 -> ('a1 tree -> 'a2 -> 'a1 option -> 'a1 tree -> 'a2 -> 'a2) -> 'a1
    tree' -> 'a2

  val tree_rec :
    'a2 -> ('a1 tree -> 'a2 -> 'a1 option -> 'a1 tree -> 'a2 -> 'a2) -> 'a1
    tree -> 'a2

  val tree_rec2' :
    'a3 -> ('a2 tree -> 'a3) -> ('a1 tree -> 'a3) -> ('a1 tree -> 'a1 option
    -> 'a1 tree -> 'a2 tree -> 'a2 option -> 'a2 tree -> 'a3 -> 'a3 -> 'a3)
    -> 'a1 tree' -> 'a2 tree' -> 'a3

  val tree_rec2 :
    'a3 -> ('a2 tree -> 'a3) -> ('a1 tree -> 'a3) -> ('a1 tree -> 'a1 option
    -> 'a1 tree -> 'a2 tree -> 'a2 option -> 'a2 tree -> 'a3 -> 'a3 -> 'a3)
    -> 'a1 tree -> 'a2 tree -> 'a3

  val tree_ind' :
    'a2 -> ('a1 tree -> 'a2 -> 'a1 option -> 'a1 tree -> 'a2 -> __ -> 'a2) ->
    'a1 tree' -> 'a2

  val tree_ind :
    'a2 -> ('a1 tree -> 'a2 -> 'a1 option -> 'a1 tree -> 'a2 -> __ -> 'a2) ->
    'a1 tree -> 'a2

  val beq' : ('a1 -> 'a1 -> bool) -> 'a1 tree' -> 'a1 tree' -> bool

  val beq : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool

  val prev_append : positive -> positive -> positive

  val prev : positive -> positive

  val map' : (positive -> 'a1 -> 'a2) -> 'a1 tree' -> positive -> 'a2 tree'

  val map : (positive -> 'a1 -> 'a2) -> 'a1 tree -> 'a2 tree

  val map1' : ('a1 -> 'a2) -> 'a1 tree' -> 'a2 tree'

  val map1 : ('a1 -> 'a2) -> 'a1 t -> 'a2 t

  val map_filter1_nonopt : ('a1 -> 'a2 option) -> 'a1 tree -> 'a2 tree

  val map_filter1 : ('a1 -> 'a2 option) -> 'a1 tree -> 'a2 tree

  val filter1 : ('a1 -> bool) -> 'a1 t -> 'a1 t

  val combine :
    ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 tree -> 'a2 tree -> 'a3
    tree

  val xelements' :
    'a1 tree' -> positive -> (positive * 'a1) list -> (positive * 'a1) list

  val elements : 'a1 t -> (positive * 'a1) list

  val xelements : 'a1 t -> positive -> (positive * 'a1) list

  val xkeys : 'a1 t -> positive -> positive list

  val fold' :
    ('a2 -> positive -> 'a1 -> 'a2) -> positive -> 'a1 tree' -> 'a2 -> 'a2

  val fold : ('a2 -> positive -> 'a1 -> 'a2) -> 'a1 t -> 'a2 -> 'a2

  val fold1' : ('a2 -> 'a1 -> 'a2) -> 'a1 tree' -> 'a2 -> 'a2

  val fold1 : ('a2 -> 'a1 -> 'a2) -> 'a1 t -> 'a2 -> 'a2
 end

type var = M.elt

type fun_tag = M.elt

type ind_tag = M.elt

type ctor_tag = M.elt

type prim = M.elt

val findtag : (ctor_tag * 'a1) list -> ctor_tag -> 'a1 option

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

val def_funs : fundefs -> fundefs -> coq_val M.t -> coq_val M.t -> coq_val M.t

val find_def : var -> fundefs -> ((fun_tag * var list) * exp) option

type ctor_ty_info = { ctor_name : name; ctor_ind_name : name;
                      ctor_ind_tag : ind_tag; ctor_arity : coq_N;
                      ctor_ordinal : coq_N }

type ind_ty_info = (ctor_tag * coq_N) list

type ctor_env = ctor_ty_info M.tree

type ind_env = ind_ty_info M.tree

type fun_ty_info = coq_N * coq_N list

type fun_env = fun_ty_info M.tree

val add_closure_tag : positive -> positive -> ctor_env -> ctor_env
