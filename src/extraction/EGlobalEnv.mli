open Datatypes
open EAst
open ELiftSubst
open Kernames
open List0
open MCOption
open MCProd
open Monad_utils

val lookup_env : global_declarations -> kername -> global_decl option

val lookup_constant : global_declarations -> kername -> constant_body option

val lookup_minductive :
  global_declarations -> kername -> mutual_inductive_body option

val lookup_inductive :
  global_declarations -> inductive ->
  (mutual_inductive_body * one_inductive_body) option

val lookup_constructor :
  global_declarations -> inductive -> nat ->
  ((mutual_inductive_body * one_inductive_body) * constructor_body) option

val lookup_constructor_pars_args :
  global_declarations -> inductive -> nat -> (nat * nat) option

val lookup_projection :
  global_declarations -> projection ->
  (((mutual_inductive_body * one_inductive_body) * constructor_body) * projection_body)
  option

val closed_decl : global_decl -> bool

val closed_env : global_declarations -> bool
