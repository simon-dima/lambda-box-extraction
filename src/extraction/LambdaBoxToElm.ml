open Byte
open Common2
open Datatypes
open ElmExtract
open ExAst
open Extraction
open Kernames
open List0
open PrettyPrinterMonad0
open ResultMonad
open TypedTransforms
open Bytestring
open Monad_utils

(** val coq_ElmBoxes : coq_ElmPrintConfig **)

let coq_ElmBoxes =
  { term_box_symbol = (String.String (Coq_x28, (String.String (Coq_x29,
    String.EmptyString)))); type_box_symbol = (String.String (Coq_x28,
    (String.String (Coq_x29, String.EmptyString)))); any_type_symbol =
    (String.String (Coq_x28, (String.String (Coq_x29, String.EmptyString))));
    false_elim_def = (String.String (Coq_x66, (String.String (Coq_x61,
    (String.String (Coq_x6c, (String.String (Coq_x73, (String.String
    (Coq_x65, (String.String (Coq_x5f, (String.String (Coq_x72,
    (String.String (Coq_x65, (String.String (Coq_x63, (String.String
    (Coq_x20, (String.String (Coq_x28, (String.String (Coq_x29,
    String.EmptyString)))))))))))))))))))))))); print_full_names = true }

(** val mk_preamble : String.t -> String.t option -> String.t **)

let mk_preamble mod_name preamble =
  let preamble0 = match preamble with
                  | Some s -> s
                  | None -> String.EmptyString
  in
  String.append (String.String (Coq_x6d, (String.String (Coq_x6f,
    (String.String (Coq_x64, (String.String (Coq_x75, (String.String
    (Coq_x6c, (String.String (Coq_x65, (String.String (Coq_x20,
    String.EmptyString))))))))))))))
    (String.append mod_name
      (String.append (String.String (Coq_x20, (String.String (Coq_x65,
        (String.String (Coq_x78, (String.String (Coq_x70, (String.String
        (Coq_x6f, (String.String (Coq_x73, (String.String (Coq_x69,
        (String.String (Coq_x6e, (String.String (Coq_x67, (String.String
        (Coq_x20, (String.String (Coq_x28, (String.String (Coq_x2e,
        (String.String (Coq_x2e, (String.String (Coq_x29,
        String.EmptyString))))))))))))))))))))))))))))
        (String.append nl
          (String.append preamble0 (String.append nl elm_false_rec)))))

(** val default_remaps : (kername * String.t) list **)

let default_remaps =
  []

(** val box_to_elm :
    String.t -> String.t option -> (kername * String.t) list ->
    extract_pcuic_params -> global_env -> (String.t, String.t) result **)

let box_to_elm mod_name preamble remaps params _UU03a3_ =
  let remaps_fun = fun kn ->
    option_map snd
      (find (fun pat -> let (kn', _) = pat in Kername.reflect_kername kn kn')
        remaps)
  in
  let preamble0 = mk_preamble mod_name preamble in
  bind (Obj.magic coq_Monad_result)
    (Obj.magic typed_transfoms params _UU03a3_) (fun _UU03a3_0 ->
    bind (Obj.magic coq_Monad_result)
      (Obj.magic finish_print (print_env _UU03a3_0 remaps_fun coq_ElmBoxes))
      (fun x ->
      let (_, s) = x in
      ret (Obj.magic coq_Monad_result)
        (String.append preamble0 (String.append nl (String.append s nl)))))
