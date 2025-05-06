open Ascii
open ExAst
open Extraction
open Optimize
open OptimizePropDiscr
open ResultMonad
open String0
open Transform0
open Utils
open Bytestring

val mk_params : bool -> bool -> extract_pcuic_params

val typed_transfoms :
  extract_pcuic_params -> global_env -> (global_env, String.t) result
