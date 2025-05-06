open BinNums
open BinPos
open Datatypes
open List0
open List_util
open Nat0
open Specif
open CompM
open Cps
open Cps_util
open Ctx
open Identifiers
open Inline_letapp
open Map_util
open Rename
open Set_util
open State

type __ = Obj.t

type svalue =
| SVconstr of ctor_tag * var list
| SVfun of fun_tag * var list * exp

type ctx_map = svalue Cps.M.t

type r_map = var Cps.M.t

type c_map = nat Cps.M.t

type b_map = bool Cps.M.t

val getd : 'a1 -> positive -> 'a1 Cps.M.tree -> 'a1

val term_size : exp -> nat

val funs_size : fundefs -> nat

val update_census_list :
  r_map -> var list -> (var -> c_map -> nat) -> c_map -> c_map

val update_census : r_map -> exp -> (var -> c_map -> nat) -> c_map -> c_map

val update_census_f :
  r_map -> fundefs -> (var -> c_map -> nat) -> c_map -> c_map

val init_census : exp -> c_map

val dec_census : r_map -> exp -> c_map -> c_map

val dec_census_list : r_map -> var list -> c_map -> c_map

val dec_census_all_case : r_map -> (var * exp) list -> c_map -> c_map

val dec_census_case : r_map -> (var * exp) list -> var -> c_map -> c_map

val update_count_inlined : var list -> var list -> c_map -> c_map

val update_count_letapp : var -> var -> c_map -> c_map

type 'a shrinkT = ((('a * nat) * c_map) * b_map, __) sigT

val precontractfun :
  r_map -> c_map -> ctx_map -> fundefs -> (fundefs * c_map) * ctx_map

val contractcases :
  ((exp * ctx_map) * b_map) -> (r_map -> c_map -> ((exp * ctx_map) * b_map)
  -> __ -> exp shrinkT) -> r_map -> c_map -> b_map -> ctx_map -> (var * exp)
  list -> (var * exp) list shrinkT

val postcontractfun :
  ((exp * ctx_map) * b_map) -> (r_map -> c_map -> ((exp * ctx_map) * b_map)
  -> __ -> exp shrinkT) -> r_map -> c_map -> b_map -> ctx_map -> fundefs ->
  nat -> fundefs shrinkT

type contractT = exp shrinkT

val incr_steps : b_map -> contractT -> contractT

val contract_func :
  (r_map, (c_map, (exp, (ctx_map, b_map) sigT) sigT) sigT) sigT -> contractT

val contract : r_map -> c_map -> exp -> ctx_map -> b_map -> contractT

val shrink_top : exp -> exp * nat

val shrink_err : exp -> comp_data -> exp error * comp_data
