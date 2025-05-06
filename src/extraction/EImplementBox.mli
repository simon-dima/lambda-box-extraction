open BasicAst
open Byte
open Datatypes
open EAst
open ELiftSubst
open EPrimitive
open EProgram
open Extract
open Kernames
open List0
open MCList
open MCProd
open Bytestring

val iBox : term

val implement_box : term -> term

val implement_box_constant_decl : constant_body -> E.constant_body

val implement_box_decl : global_decl -> global_decl

val implement_box_env : global_declarations -> (kername * global_decl) list

val implement_box_program : eprogram -> (kername * global_decl) list * term
