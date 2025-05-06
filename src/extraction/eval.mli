open Bytestring
open Cps
open ExceptionMonad
open Map_util

type env = coq_val M.t

type prims = (coq_val list -> coq_val option) M.t

val l_opt : 'a1 option -> String.t -> 'a1 coq_exception
