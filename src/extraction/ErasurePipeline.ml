open Byte
open EAst
open EImplementBox
open EWellformed
open Bytestring

(** val implement_box_transformation :
    coq_EEnvFlags -> (global_declarations, global_declarations, term, term,
    term, term) Transform.Transform.t **)

let implement_box_transformation _ =
  { Transform.Transform.name = (String.String (Coq_x69, (String.String
    (Coq_x6d, (String.String (Coq_x70, (String.String (Coq_x6c,
    (String.String (Coq_x65, (String.String (Coq_x6d, (String.String
    (Coq_x65, (String.String (Coq_x6e, (String.String (Coq_x74,
    (String.String (Coq_x69, (String.String (Coq_x6e, (String.String
    (Coq_x67, (String.String (Coq_x20, (String.String (Coq_x62,
    (String.String (Coq_x6f, (String.String (Coq_x78,
    String.EmptyString))))))))))))))))))))))))))))))));
    Transform.Transform.transform = (fun p _ -> implement_box_program p) }

(** val implement_box :
    coq_EEnvFlags -> (global_declarations, term) Transform.Transform.program
    -> (global_declarations, term) Transform.Transform.program **)

let implement_box efl p =
  Transform.Transform.run (implement_box_transformation efl) p
