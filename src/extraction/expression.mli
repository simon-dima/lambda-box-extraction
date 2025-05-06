open AstCommon
open BasicAst
open BinNums
open Datatypes
open Kernames
open List0
open Bytestring

type dcon = inductive * coq_N

type exp =
| Var_e of coq_N
| Lam_e of name * exp
| App_e of exp * exp
| Con_e of dcon * exps
| Match_e of exp * coq_N * branches_e
| Let_e of name * exp * exp
| Fix_e of efnlst * coq_N
| Prf_e
| Prim_val_e of primitive
| Prim_e of positive
and exps =
| Coq_enil
| Coq_econs of exp * exps
and efnlst =
| Coq_eflnil
| Coq_eflcons of name * exp * efnlst
and branches_e =
| Coq_brnil_e
| Coq_brcons_e of dcon * (coq_N * name list) * exp * branches_e

val nNameds : String.t -> name

val exps_as_list : exps -> exp list

val efnlst_as_list : efnlst -> (name * exp) list

val fnames : efnlst -> name list
