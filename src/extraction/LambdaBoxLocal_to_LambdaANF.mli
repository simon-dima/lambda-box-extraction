open AstCommon
open BasicAst
open BinNat
open BinNums
open BinPos
open Byte
open Datatypes
open Kernames
open List0
open Monad0
open Nat0
open PeanoNat
open Bytestring
open CompM
open Cps
open Ctx
open Expression
open State

type conId_map = (dcon * ctor_tag) list

val conId_dec : dcon -> dcon -> bool

val dcon_to_info : positive -> dcon -> conId_map -> positive

type constr_env = conId_map

val dcon_to_tag : positive -> dcon -> conId_map -> positive

type name_env = name M.t

type ienv = (kername * itypPack) list

val fromN : positive -> nat -> positive list * positive

val ctx_bind_proj : ctor_tag -> positive -> var list -> nat -> exp_ctx

val convert_cnstrs :
  String.t -> ctor_tag list -> coq_Cnstr list -> inductive -> coq_N -> coq_N
  -> coq_N -> ind_tag -> ctor_env -> conId_map -> ctor_env * conId_map

val convert_typack :
  ityp list -> kername -> nat ->
  ((((ind_env * ctor_env) * ctor_tag) * ind_tag) * conId_map) ->
  (((ind_env * ctor_env) * ctor_tag) * ind_tag) * conId_map

val convert_env' :
  ienv -> ((((ind_env * ctor_env) * ctor_tag) * ind_tag) * conId_map) ->
  (((ind_env * ctor_env) * ctor_tag) * ind_tag) * conId_map

val convert_env :
  positive -> positive -> ienv ->
  (((ind_env * ctor_env) * ctor_tag) * ind_tag) * conId_map

type 'a cpsM = (unit, 'a) compM'

val get_named_str_lst : String.t list -> var list cpsM

val convert_prim :
  positive -> positive -> nat -> positive -> var list -> var -> Cps.exp cpsM

val names_lst_len : name list -> nat -> name list

val cps_cvt :
  (((kername * String.t) * bool) * nat) M.t -> positive -> positive ->
  positive -> exp -> var list -> var -> constr_env -> Cps.exp cpsM

val convert_whole_exp :
  (((kername * String.t) * bool) * nat) M.t -> positive -> positive ->
  positive -> exp -> conId_map -> Cps.exp cpsM

val convert_top :
  (((kername * String.t) * bool) * nat) M.t -> positive -> positive ->
  positive -> positive -> positive -> (ienv * exp) -> Cps.exp
  error * comp_data

type 'a anfM = (unit, 'a) compM'

type var_map = var M.t * coq_N

val get_var_name : var_map -> coq_N -> var option

val add_var_name : var_map -> var -> var M.tree * coq_N

val new_var_map : var M.t * coq_N

type anf_value =
| Anf_Var of var
| Anf_App of var * var
| Constr of ctor_tag * var list
| Proj of ctor_tag * coq_N * var
| Fun of fun_tag * var * Cps.exp
| Prim_val of primitive
| Prim of positive * var list

type anf_term = anf_value * exp_ctx

val anf_term_to_exp : positive -> anf_term -> Cps.exp anfM

val anf_term_to_ctx : positive -> anf_term -> name -> (var * exp_ctx) anfM

val def_name : name

val proj_ctx :
  name list -> coq_N -> var -> var_map -> ctor_tag -> (exp_ctx * var_map) anfM

val add_fix_names : efnlst -> var_map -> (var list * var_map) anfM

val convert_prim_anf :
  positive -> nat -> positive -> var list -> anf_term anfM

val convert_anf :
  (((kername * String.t) * bool) * nat) M.t -> positive -> positive ->
  conId_map -> exp -> var_map -> anf_term anfM

val convert_anf_exp :
  (((kername * String.t) * bool) * nat) M.t -> positive -> positive ->
  conId_map -> exp -> Cps.exp anfM

val convert_top_anf :
  (((kername * String.t) * bool) * nat) M.t -> positive -> positive ->
  positive -> positive -> (ienv * exp) -> Cps.exp error * comp_data
