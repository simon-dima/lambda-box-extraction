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

val program_of_string : string -> (error, program) sum

val global_env_of_string : string -> (error, global_env) sum

val kername_of_string : string -> (error, kername) sum

val string_of_error : bool -> bool -> error -> string
