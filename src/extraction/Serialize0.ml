open CeresDeserialize
open CeresExtra
open Datatypes
open EAst
open ExAst
open Kernames
open SerializeCommon
open SerializeEAst
open SerializeExAst
open String0

(** val program_of_string : string -> (error, program) sum **)

let program_of_string =
  program_of_string

(** val global_env_of_string : string -> (error, global_env) sum **)

let global_env_of_string =
  global_env_of_string

(** val kername_of_string : string -> (error, kername) sum **)

let kername_of_string =
  kername_of_string

(** val string_of_error : bool -> bool -> error -> string **)

let string_of_error =
  string_of_error
