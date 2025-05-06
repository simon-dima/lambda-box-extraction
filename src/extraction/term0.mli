open AstCommon
open BasicAst
open Byte
open Datatypes
open EAst
open Kernames
open List0
open Nat0
open PeanoNat
open RandyPrelude
open Bytestring
open Compile0

val print_term : coq_Term -> String.t

val dnth : nat -> coq_Defs -> coq_Term def option

val bnth : nat -> coq_Brs -> (name list * coq_Term) option

val instantiate : coq_Term -> nat -> coq_Term -> coq_Term

val instantiatel : coq_Terms -> nat -> coq_Term -> coq_Term

val whBetaStep : coq_Term -> coq_Term -> coq_Term

val whCaseStep : nat -> coq_Terms -> coq_Brs -> coq_Term option

val whFixStep : coq_Defs -> nat -> coq_Term option
