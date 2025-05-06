open BasicAst
open BinNums
open BinPos
open Byte
open Datatypes
open Monad0
open Nat0
open Bytestring
open Cps
open Map_util

type __ = Obj.t

val var_dec : positive -> positive -> bool

type name_env = name M.t

val add_entry : name_env -> var -> var -> String.t -> name_env

val add_entry_str : name_env -> var -> String.t -> name_env

val caseConsistent_f : ctor_env -> (ctor_tag * exp) list -> ctor_tag -> bool

val numOf_fundefs : fundefs -> nat

val coq_OptMonad : __ option coq_Monad
