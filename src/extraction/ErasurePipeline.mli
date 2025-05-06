open Byte
open EAst
open EImplementBox
open EWellformed
open Bytestring

val implement_box_transformation :
  coq_EEnvFlags -> (global_declarations, global_declarations, term, term,
  term, term) Transform.Transform.t

val implement_box :
  coq_EEnvFlags -> (global_declarations, term) Transform.Transform.program ->
  (global_declarations, term) Transform.Transform.program
