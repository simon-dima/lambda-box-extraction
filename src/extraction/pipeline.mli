open AstCommon
open BinNums
open BinPos
open Byte
open Datatypes
open Kernames
open List0
open MCString
open Monad0
open Pipeline_utils
open Bytestring
open CompM
open Compile0

val pick_prim_ident :
  positive -> ((((kername * String.t) * bool) * nat) * positive) list ->
  ((((kername * String.t) * bool) * nat) * positive) list * positive

val find_axioms :
  ((((kername * String.t) * bool) * nat) * positive) list -> kername list ->
  'a1 environ -> kername list

val check_axioms :
  ((((kername * String.t) * bool) * nat) * positive) list -> coq_Term
  coq_Program -> unit pipelineM

val next_id : positive
