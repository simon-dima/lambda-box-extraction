open BinNat
open Byte
open Datatypes
open List0
open Maps
open Monad0
open Nat0
open PeanoNat
open Bytestring
open CompM
open Cps
open Identifiers
open Set_util
open State
open Uncurry

type coq_VarInfo =
| FreeVar of var
| WrapperFun of var

type coq_VarInfoMap = coq_VarInfo PTree.t

type coq_FunInfo =
| Fun of var * fun_tag * var list * PS.t * var list
| NoLiftFun of var list * PS.t

type coq_FunInfoMap = coq_FunInfo PTree.t

type coq_GFunInfo =
| GFun
| LGFun

type coq_GFunMap = coq_GFunInfo M.t

type 'a lambdaM = (unit, 'a) compM'

val add_functions :
  fundefs -> var list -> PS.t -> var list -> coq_FunInfoMap -> coq_GFunMap ->
  (coq_FunInfoMap * coq_GFunMap) lambdaM

val rename : coq_VarInfoMap -> var -> var

val rename_lst : coq_VarInfoMap -> var list -> var list

val add_free_vars :
  var list -> coq_VarInfoMap -> (var list * coq_VarInfoMap) lambdaM

val make_wrappers :
  fundefs -> coq_VarInfoMap -> coq_FunInfoMap -> (fundefs
  option * coq_VarInfoMap) lambdaM

val fundefs_max_params : fundefs -> nat

val fundefs_true_fv_aux :
  (var list -> coq_FVSet -> bool) -> coq_FunInfoMap -> fundefs -> coq_FVSet
  -> PS.t -> coq_FVSet * PS.t

val fundefs_true_fv :
  (var list -> coq_FVSet -> bool) -> coq_FunInfoMap -> fundefs -> PS.t

val occurs_in_exp : var -> PS.t -> exp -> bool

val occurs_in_fundefs : var -> PS.t -> fundefs -> bool

val stack_push : var -> PS.t -> exp -> nat

val stack_push_fundefs_aux : var -> PS.t -> fundefs -> nat

val stack_push_fundefs : var -> fundefs -> nat

val exp_lambda_lift :
  nat -> nat -> (var list -> coq_FVSet -> bool) -> exp -> PS.t -> PS.t ->
  coq_VarInfoMap -> coq_FunInfoMap -> coq_GFunMap -> exp lambdaM

val lift_all : var list -> coq_FVSet -> bool

val lift_conservative : var list -> coq_FVSet -> bool

val lambda_lift :
  nat -> nat -> bool -> exp -> comp_data -> exp error * comp_data
