open AstCommon
open BasicAst
open BinNat
open BinNums
open Byte
open Datatypes
open List0
open MCString
open Monad0
open MonadState
open Nat0
open Show
open Specif
open StateMonad
open Bytestring
open Cps

type name_env = name M.tree

val show_nat : nat -> String.t

val show_pos : positive -> String.t

val show_binnat : coq_N -> String.t

val sep : 'a1 -> 'a1 list -> 'a1 list

type string_tree =
| Emp
| Str of String.t
| App of string_tree * string_tree

val show_tree_c : string_tree -> String.t -> String.t

val show_tree : string_tree -> String.t

val show_var : name_env -> positive -> string_tree

val show_con : ctor_env -> ctor_tag -> string_tree

val show_ftag : bool -> fun_tag -> string_tree

val show_vars : name_env -> positive list -> string_tree

type 't coq_M = (string_tree, 't) state

val emit : string_tree -> unit coq_M

val tab : nat -> unit coq_M

val chr_newline : byte

val newline : unit coq_M

val emit_prim : primitive -> unit coq_M

val emit_exp : name_env -> ctor_env -> bool -> nat -> exp -> unit coq_M

val emit_val : name_env -> ctor_env -> bool -> nat -> coq_val -> unit coq_M

val show_val : name_env -> ctor_env -> bool -> coq_val -> String.t

val show_exp : name_env -> ctor_env -> bool -> exp -> String.t
