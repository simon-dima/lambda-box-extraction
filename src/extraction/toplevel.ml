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

(** val compile_LambdaBoxLocal :
    ((((kername * String.t) * bool) * nat) * positive) list -> (coq_Term
    coq_Program, coq_LambdaBoxLocalTerm) coq_CertiCoqTrans **)

let compile_LambdaBoxLocal prims src =
  bind (coq_MonadErrorT coq_MonadState)
    (debug_msg (String.String (Coq_x54, (String.String (Coq_x72,
      (String.String (Coq_x61, (String.String (Coq_x6e, (String.String
      (Coq_x73, (String.String (Coq_x6c, (String.String (Coq_x61,
      (String.String (Coq_x74, (String.String (Coq_x69, (String.String
      (Coq_x6e, (String.String (Coq_x67, (String.String (Coq_x20,
      (String.String (Coq_x66, (String.String (Coq_x72, (String.String
      (Coq_x6f, (String.String (Coq_x6d, (String.String (Coq_x20,
      (String.String (Coq_x4c, (String.String (Coq_x61, (String.String
      (Coq_x6d, (String.String (Coq_x62, (String.String (Coq_x64,
      (String.String (Coq_x61, (String.String (Coq_x42, (String.String
      (Coq_x6f, (String.String (Coq_x78, (String.String (Coq_x4d,
      (String.String (Coq_x75, (String.String (Coq_x74, (String.String
      (Coq_x20, (String.String (Coq_x74, (String.String (Coq_x6f,
      (String.String (Coq_x20, (String.String (Coq_x4c, (String.String
      (Coq_x61, (String.String (Coq_x6d, (String.String (Coq_x62,
      (String.String (Coq_x64, (String.String (Coq_x61, (String.String
      (Coq_x42, (String.String (Coq_x6f, (String.String (Coq_x78,
      (String.String (Coq_x4c, (String.String (Coq_x6f, (String.String
      (Coq_x63, (String.String (Coq_x61, (String.String (Coq_x6c,
      String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    (fun _ ->
    coq_LiftCertiCoqTrans (String.String (Coq_x4c, (String.String (Coq_x61,
      (String.String (Coq_x6d, (String.String (Coq_x62, (String.String
      (Coq_x64, (String.String (Coq_x61, (String.String (Coq_x42,
      (String.String (Coq_x6f, (String.String (Coq_x78, (String.String
      (Coq_x4c, (String.String (Coq_x6f, (String.String (Coq_x63,
      (String.String (Coq_x61, (String.String (Coq_x6c,
      String.EmptyString)))))))))))))))))))))))))))) (fun p ->
      ((inductive_env p.env), (translate_program prims p.env p.main))) src)
