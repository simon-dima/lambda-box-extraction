open BinInt
open BinNums
open Byte
open Datatypes
open List0
open MCString
open Monad0
open Bytestring
open CompM
open Cps
open Cps_util
open Numerics

val max_function_args : coq_Z

val max_num_functions : coq_Z

val max_constr_args : coq_Z

val coq_assert : bool -> String.t -> unit error

val get_ctor_ord : ctor_env -> ctor_tag -> coq_N error

val check_restrictions : ctor_env -> exp -> unit error
