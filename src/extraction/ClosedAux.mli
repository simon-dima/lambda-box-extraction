open Datatypes
open EAst
open ELiftSubst
open List0

val decl_closed : global_decl -> bool

val env_closed : global_declarations -> bool
