open AstCommon
open BinNums
open Datatypes
open Cps

type exp_ctx =
| Hole_c
| Econstr_c of var * ctor_tag * var list * exp_ctx
| Eproj_c of var * ctor_tag * coq_N * var * exp_ctx
| Eprim_val_c of var * primitive * exp_ctx
| Eprim_c of var * prim * var list * exp_ctx
| Eletapp_c of var * var * fun_tag * var list * exp_ctx
| Ecase_c of var * (ctor_tag * exp) list * ctor_tag * exp_ctx
   * (ctor_tag * exp) list
| Efun1_c of fundefs * exp_ctx
| Efun2_c of fundefs_ctx * exp
and fundefs_ctx =
| Fcons1_c of var * ctor_tag * var list * exp_ctx * fundefs
| Fcons2_c of var * ctor_tag * var list * exp * fundefs_ctx

val app_ctx_f : exp_ctx -> exp -> exp

val app_f_ctx_f : fundefs_ctx -> exp -> fundefs

val comp_ctx_f : exp_ctx -> exp_ctx -> exp_ctx

val comp_f_ctx_f : fundefs_ctx -> exp_ctx -> fundefs_ctx
