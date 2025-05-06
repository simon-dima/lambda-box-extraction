open BinNat
open BinNums
open BinPos
open Bool
open Byte
open Datatypes
open List0
open List_util
open Monad0
open Nat0
open OrdersEx
open Bytestring
open CompM
open Cps
open Set_util
open State

type live_fun = bool list M.t

val get_fun_vars : live_fun -> var -> bool list option

val set_fun_vars : live_fun -> var -> bool list -> bool list M.tree

val live_args : 'a1 list -> bool list -> 'a1 list

val get_bool_false : 'a1 list -> bool list

val init_live_fun_aux : live_fun -> fundefs -> live_fun

val init_live_fun : fundefs -> live_fun

val remove_escaping : live_fun -> var -> live_fun

val remove_escapings : live_fun -> var list -> live_fun

val escaping_fun_exp : exp -> live_fun -> live_fun

val escaping_fun_fundefs : fundefs -> live_fun -> live_fun

val add_fun_vars : live_fun -> var -> var list -> PS.t -> PS.t

val live_expr : live_fun -> exp -> PS.t -> PS.t

val update_bs : PS.t -> PS.elt list -> bool list -> bool list * bool

val update_live_fun : live_fun -> var -> var list -> PS.t -> live_fun * bool

val live : fundefs -> live_fun -> bool -> live_fun * bool

val find_live_helper : fundefs -> live_fun -> nat -> live_fun error

val num_vars : fundefs -> nat -> nat

val find_live : exp -> live_fun error

type arityMap = fun_tag M.t

type ftagMap = fun_tag M.t

type 'a elimM = (unit, 'a) compM'

val make_arityMap : exp -> arityMap -> arityMap

val make_arityMap_fundefs : fundefs -> arityMap -> arityMap

val make_ftag : nat -> comp_data -> fun_tag * comp_data

val create_fun_tag :
  live_fun -> arityMap -> fundefs -> comp_data -> ftagMap ->
  ftagMap * comp_data

val is_hoisted_exp : exp -> bool

val is_hoisted_fundefs : fundefs -> bool

val is_hoisted : exp -> bool

val get_fun_tag : ftagMap -> var -> fun_tag

val eliminate_expr : ftagMap -> live_fun -> exp -> exp elimM

val eliminate_fundefs : ftagMap -> live_fun -> fundefs -> fundefs elimM

val coq_DPE : exp -> comp_data -> exp error * comp_data
