open BinNat
open BinNatDef
open BinNums
open BinPos
open Bool
open Byte
open Datatypes
open Frame
open List0
open Nat0
open Rewriting
open Bytestring
open CompM
open Cps
open Cps_proto
open Cps_proto_univ
open Cps_util
open Identifiers
open Proto_util
open State

val set_name : var -> var -> String.t -> comp_data -> comp_data

val set_names_lst : var list -> var list -> String.t -> comp_data -> comp_data

type coq_St = nat * nat M.tree

type arity_map = fun_tag M.tree

type local_map = bool M.tree

type coq_S_misc = (((bool * arity_map) * local_map) * coq_St) * comp_data

val get_fun_tag : coq_N -> coq_S_misc -> fun_tag * coq_S_misc

val metadata_update :
  var -> var -> var -> nat -> var list -> var list -> var list -> var list ->
  coq_S_misc -> coq_S_misc

val rw_uncurry :
  coq_Fuel -> exp_univ -> exp_univ univD -> (exp_univ, unit) coq_Delay ->
  (exp_univ, bool) coq_Param -> (exp_univ, coq_S_misc * var) coq_State ->
  (exp_univ, coq_S_misc * var) result

val uncurry_top : bool -> exp -> comp_data -> exp error * comp_data
