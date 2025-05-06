open BasicAst
open BinNat
open BinNums
open BinPos
open Byte
open Datatypes
open List_util
open Monad0
open Bytestring
open CompM
open Cps
open Cps_show
open Cps_util

type comp_data = { next_var : var; nect_ctor_tag : ctor_tag;
                   next_ind_tag : ind_tag; next_fun_tag : fun_tag;
                   cenv : ctor_env; fenv : fun_env; nenv : Cps_show.name_env;
                   inline_map : nat M.tree; log : String.t list }

type ('s, 'a) compM' = (unit, comp_data * 's, 'a) compM

val get_name_env : unit -> ('a1, Cps_show.name_env) compM'

val get_name : var -> String.t -> ('a1, var) compM'

val add_entry_from_map :
  Cps_show.name_env -> Cps_show.name_env -> var -> var -> String.t ->
  Cps_show.name_env

val get_name' : var -> String.t -> Cps_show.name_env -> ('a1, var) compM'

val get_names_lst : var list -> String.t -> ('a1, var list) compM'

val get_names_lst' :
  var list -> String.t -> Cps_show.name_env -> ('a1, var list) compM'

val get_named : name -> ('a1, var) compM'

val get_named_lst : name list -> ('a1, var list) compM'

val get_named_str : String.t -> ('a1, var) compM'

val make_record_ctor_tag : coq_N -> ('a1, ctor_tag) compM'

val get_pp_name : var -> ('a1, String.t) compM'

val add_log : String.t -> comp_data -> comp_data

val get_state : unit -> ('a1, 'a1) compM'

val put_state : 'a1 -> ('a1, unit) compM'

val get_ftag : coq_N -> ('a1, fun_tag) compM'

val run_compM :
  ('a1, 'a2) compM' -> comp_data -> 'a1 -> 'a2 error * (comp_data * 'a1)

val pack_data :
  var -> ctor_tag -> ind_tag -> fun_tag -> ctor_env -> fun_env ->
  Cps_show.name_env -> nat M.tree -> String.t list -> comp_data

val put_ctor_env : ctor_env -> comp_data -> comp_data
