open Monad0
open Cps
open Cps_util
open Ctx

val inline_letapp : exp -> var -> (exp_ctx * var) option
