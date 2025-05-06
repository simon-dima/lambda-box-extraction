open AstCommon
open Byte
open Datatypes
open Kernames
open Monad0
open MonadExc
open RandyPrelude
open Bytestring
open Compile0
open ExceptionMonad
open Term0

type __ = Obj.t

val fix_bug : (String.t, __ coq_exception) coq_MonadExc

val wcbvEval : coq_Term environ -> nat -> coq_Term -> coq_Term coq_exception
