open AstCommon
open BinNums
open Byte
open Datatypes
open Kernames
open LambdaBoxMut_to_LambdaBoxLocal
open Monad0
open Pipeline_utils
open Bytestring
open CompM
open Compile0
open Expression

type coq_LambdaBoxLocalTerm = ienv * exp

val compile_LambdaBoxLocal :
  ((((kername * String.t) * bool) * nat) * positive) list -> (coq_Term
  coq_Program, coq_LambdaBoxLocalTerm) coq_CertiCoqTrans
