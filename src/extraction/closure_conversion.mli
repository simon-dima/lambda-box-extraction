open BinNat
open BinNums
open BinPos
open Byte
open Datatypes
open List0
open Maps
open Monad0
open Bytestring
open CompM
open Cps
open Identifiers
open Set_util
open State

type __ = Obj.t

type coq_VarInfo =
| FVar of coq_N
| MRFun of var
| BoundVar

type coq_VarInfoMap = coq_VarInfo M.t

type coq_GFunMap = __ M.t

type 'a ccstate = (unit, 'a) compM'

val clo_env_suffix : String.t

val clo_suffix : String.t

val code_suffix : String.t

val proj_suffix : String.t

val get_var :
  ctor_tag -> var -> coq_VarInfoMap -> coq_GFunMap -> ctor_tag -> var ->
  (var * (exp -> exp)) ccstate

val get_vars :
  ctor_tag -> var list -> coq_VarInfoMap -> coq_GFunMap -> ctor_tag -> var ->
  (var list * (exp -> exp)) ccstate

val add_params : positive list -> coq_VarInfoMap -> coq_VarInfoMap

val make_env :
  ctor_tag -> var list -> coq_VarInfoMap -> coq_VarInfoMap -> ctor_tag -> var
  -> var -> coq_GFunMap -> ((ctor_tag * coq_VarInfoMap) * (exp -> exp))
  ccstate

val add_closures : fundefs -> coq_VarInfoMap -> var -> coq_VarInfoMap

val add_closures_gfuns : fundefs -> coq_GFunMap -> bool -> coq_GFunMap

val exp_closure_conv :
  ctor_tag -> exp -> coq_VarInfoMap -> coq_GFunMap -> ctor_tag -> var ->
  (exp * (exp -> exp)) ccstate

val populate_map : coq_FVSet -> coq_VarInfoMap -> coq_VarInfoMap

val get_name : comp_data -> var * comp_data

val closure_conversion_top :
  ctor_tag -> ind_tag -> exp -> comp_data -> exp error * comp_data
