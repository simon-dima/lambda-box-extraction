open Datatypes
open List0
open Nat0
open Closure_conversion
open CompM
open Cps
open State

val erase_fundefs :
  exp -> fundefs -> (((exp * fundefs) * nat) -> (exp * fundefs) * nat) ->
  (exp * fundefs) * nat

val erase_nested_fundefs :
  fundefs -> exp -> fundefs -> (((exp * fundefs) * nat) ->
  (exp * fundefs) * nat) -> (exp * fundefs) * nat

val exp_hoist : exp -> exp * nat

val closure_conversion_hoist :
  ctor_tag -> ind_tag -> exp -> comp_data -> exp error * comp_data
