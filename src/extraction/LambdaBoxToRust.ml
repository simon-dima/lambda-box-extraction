open Ascii
open Byte
open Datatypes
open ExAst
open Extraction
open PrettyPrinterMonad
open Printing
open ResultMonad
open RustExtract
open String0
open TypedTransforms
open Utils
open Bytestring
open Monad_utils

(** val plugin_extract_preamble : coq_Preamble **)

let plugin_extract_preamble =
  { top_preamble = ((String.String (Coq_x23, (String.String (Coq_x21,
    (String.String (Coq_x5b, (String.String (Coq_x61, (String.String
    (Coq_x6c, (String.String (Coq_x6c, (String.String (Coq_x6f,
    (String.String (Coq_x77, (String.String (Coq_x28, (String.String
    (Coq_x64, (String.String (Coq_x65, (String.String (Coq_x61,
    (String.String (Coq_x64, (String.String (Coq_x5f, (String.String
    (Coq_x63, (String.String (Coq_x6f, (String.String (Coq_x64,
    (String.String (Coq_x65, (String.String (Coq_x29, (String.String
    (Coq_x5d,
    String.EmptyString)))))))))))))))))))))))))))))))))))))))) :: ((String.String
    (Coq_x23, (String.String (Coq_x21, (String.String (Coq_x5b,
    (String.String (Coq_x61, (String.String (Coq_x6c, (String.String
    (Coq_x6c, (String.String (Coq_x6f, (String.String (Coq_x77,
    (String.String (Coq_x28, (String.String (Coq_x6e, (String.String
    (Coq_x6f, (String.String (Coq_x6e, (String.String (Coq_x5f,
    (String.String (Coq_x63, (String.String (Coq_x61, (String.String
    (Coq_x6d, (String.String (Coq_x65, (String.String (Coq_x6c,
    (String.String (Coq_x5f, (String.String (Coq_x63, (String.String
    (Coq_x61, (String.String (Coq_x73, (String.String (Coq_x65,
    (String.String (Coq_x5f, (String.String (Coq_x74, (String.String
    (Coq_x79, (String.String (Coq_x70, (String.String (Coq_x65,
    (String.String (Coq_x73, (String.String (Coq_x29, (String.String
    (Coq_x5d,
    String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) :: ((String.String
    (Coq_x23, (String.String (Coq_x21, (String.String (Coq_x5b,
    (String.String (Coq_x61, (String.String (Coq_x6c, (String.String
    (Coq_x6c, (String.String (Coq_x6f, (String.String (Coq_x77,
    (String.String (Coq_x28, (String.String (Coq_x75, (String.String
    (Coq_x6e, (String.String (Coq_x75, (String.String (Coq_x73,
    (String.String (Coq_x65, (String.String (Coq_x64, (String.String
    (Coq_x5f, (String.String (Coq_x69, (String.String (Coq_x6d,
    (String.String (Coq_x70, (String.String (Coq_x6f, (String.String
    (Coq_x72, (String.String (Coq_x74, (String.String (Coq_x73,
    (String.String (Coq_x29, (String.String (Coq_x5d,
    String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))) :: ((String.String
    (Coq_x23, (String.String (Coq_x21, (String.String (Coq_x5b,
    (String.String (Coq_x61, (String.String (Coq_x6c, (String.String
    (Coq_x6c, (String.String (Coq_x6f, (String.String (Coq_x77,
    (String.String (Coq_x28, (String.String (Coq_x6e, (String.String
    (Coq_x6f, (String.String (Coq_x6e, (String.String (Coq_x5f,
    (String.String (Coq_x73, (String.String (Coq_x6e, (String.String
    (Coq_x61, (String.String (Coq_x6b, (String.String (Coq_x65,
    (String.String (Coq_x5f, (String.String (Coq_x63, (String.String
    (Coq_x61, (String.String (Coq_x73, (String.String (Coq_x65,
    (String.String (Coq_x29, (String.String (Coq_x5d,
    String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))) :: ((String.String
    (Coq_x23, (String.String (Coq_x21, (String.String (Coq_x5b,
    (String.String (Coq_x61, (String.String (Coq_x6c, (String.String
    (Coq_x6c, (String.String (Coq_x6f, (String.String (Coq_x77,
    (String.String (Coq_x28, (String.String (Coq_x75, (String.String
    (Coq_x6e, (String.String (Coq_x75, (String.String (Coq_x73,
    (String.String (Coq_x65, (String.String (Coq_x64, (String.String
    (Coq_x5f, (String.String (Coq_x76, (String.String (Coq_x61,
    (String.String (Coq_x72, (String.String (Coq_x69, (String.String
    (Coq_x61, (String.String (Coq_x62, (String.String (Coq_x6c,
    (String.String (Coq_x65, (String.String (Coq_x73, (String.String
    (Coq_x29, (String.String (Coq_x5d,
    String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))) :: (String.EmptyString :: ((String.String
    (Coq_x75, (String.String (Coq_x73, (String.String (Coq_x65,
    (String.String (Coq_x20, (String.String (Coq_x73, (String.String
    (Coq_x74, (String.String (Coq_x64, (String.String (Coq_x3a,
    (String.String (Coq_x3a, (String.String (Coq_x6d, (String.String
    (Coq_x61, (String.String (Coq_x72, (String.String (Coq_x6b,
    (String.String (Coq_x65, (String.String (Coq_x72, (String.String
    (Coq_x3a, (String.String (Coq_x3a, (String.String (Coq_x50,
    (String.String (Coq_x68, (String.String (Coq_x61, (String.String
    (Coq_x6e, (String.String (Coq_x74, (String.String (Coq_x6f,
    (String.String (Coq_x6d, (String.String (Coq_x44, (String.String
    (Coq_x61, (String.String (Coq_x74, (String.String (Coq_x61,
    (String.String (Coq_x3b,
    String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))) :: (String.EmptyString :: ((String.String
    (Coq_x66, (String.String (Coq_x6e, (String.String (Coq_x20,
    (String.String (Coq_x68, (String.String (Coq_x69, (String.String
    (Coq_x6e, (String.String (Coq_x74, (String.String (Coq_x5f,
    (String.String (Coq_x61, (String.String (Coq_x70, (String.String
    (Coq_x70, (String.String (Coq_x3c, (String.String (Coq_x54,
    (String.String (Coq_x41, (String.String (Coq_x72, (String.String
    (Coq_x67, (String.String (Coq_x2c, (String.String (Coq_x20,
    (String.String (Coq_x54, (String.String (Coq_x52, (String.String
    (Coq_x65, (String.String (Coq_x74, (String.String (Coq_x3e,
    (String.String (Coq_x28, (String.String (Coq_x66, (String.String
    (Coq_x3a, (String.String (Coq_x20, (String.String (Coq_x26,
    (String.String (Coq_x64, (String.String (Coq_x79, (String.String
    (Coq_x6e, (String.String (Coq_x20, (String.String (Coq_x46,
    (String.String (Coq_x6e, (String.String (Coq_x28, (String.String
    (Coq_x54, (String.String (Coq_x41, (String.String (Coq_x72,
    (String.String (Coq_x67, (String.String (Coq_x29, (String.String
    (Coq_x20, (String.String (Coq_x2d, (String.String (Coq_x3e,
    (String.String (Coq_x20, (String.String (Coq_x54, (String.String
    (Coq_x52, (String.String (Coq_x65, (String.String (Coq_x74,
    (String.String (Coq_x29, (String.String (Coq_x20, (String.String
    (Coq_x2d, (String.String (Coq_x3e, (String.String (Coq_x20,
    (String.String (Coq_x26, (String.String (Coq_x64, (String.String
    (Coq_x79, (String.String (Coq_x6e, (String.String (Coq_x20,
    (String.String (Coq_x46, (String.String (Coq_x6e, (String.String
    (Coq_x28, (String.String (Coq_x54, (String.String (Coq_x41,
    (String.String (Coq_x72, (String.String (Coq_x67, (String.String
    (Coq_x29, (String.String (Coq_x20, (String.String (Coq_x2d,
    (String.String (Coq_x3e, (String.String (Coq_x20, (String.String
    (Coq_x54, (String.String (Coq_x52, (String.String (Coq_x65,
    (String.String (Coq_x74, (String.String (Coq_x20, (String.String
    (Coq_x7b,
    String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) :: ((String.String
    (Coq_x20, (String.String (Coq_x20, (String.String (Coq_x66,
    String.EmptyString)))))) :: ((String.String (Coq_x7d,
    String.EmptyString)) :: []))))))))))); program_preamble = ((String.String
    (Coq_x66, (String.String (Coq_x6e, (String.String (Coq_x20,
    (String.String (Coq_x61, (String.String (Coq_x6c, (String.String
    (Coq_x6c, (String.String (Coq_x6f, (String.String (Coq_x63,
    (String.String (Coq_x3c, (String.String (Coq_x54, (String.String
    (Coq_x3e, (String.String (Coq_x28, (String.String (Coq_x26,
    (String.String (Coq_x27, (String.String (Coq_x61, (String.String
    (Coq_x20, (String.String (Coq_x73, (String.String (Coq_x65,
    (String.String (Coq_x6c, (String.String (Coq_x66, (String.String
    (Coq_x2c, (String.String (Coq_x20, (String.String (Coq_x74,
    (String.String (Coq_x3a, (String.String (Coq_x20, (String.String
    (Coq_x54, (String.String (Coq_x29, (String.String (Coq_x20,
    (String.String (Coq_x2d, (String.String (Coq_x3e, (String.String
    (Coq_x20, (String.String (Coq_x26, (String.String (Coq_x27,
    (String.String (Coq_x61, (String.String (Coq_x20, (String.String
    (Coq_x54, (String.String (Coq_x20, (String.String (Coq_x7b,
    String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) :: ((String.String
    (Coq_x20, (String.String (Coq_x20, (String.String (Coq_x73,
    (String.String (Coq_x65, (String.String (Coq_x6c, (String.String
    (Coq_x66, (String.String (Coq_x2e, (String.String (Coq_x5f,
    (String.String (Coq_x5f, (String.String (Coq_x61, (String.String
    (Coq_x6c, (String.String (Coq_x6c, (String.String (Coq_x6f,
    (String.String (Coq_x63, (String.String (Coq_x2e, (String.String
    (Coq_x61, (String.String (Coq_x6c, (String.String (Coq_x6c,
    (String.String (Coq_x6f, (String.String (Coq_x63, (String.String
    (Coq_x28, (String.String (Coq_x74, (String.String (Coq_x29,
    String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))) :: ((String.String
    (Coq_x7d, String.EmptyString)) :: (String.EmptyString :: ((String.String
    (Coq_x66, (String.String (Coq_x6e, (String.String (Coq_x20,
    (String.String (Coq_x63, (String.String (Coq_x6c, (String.String
    (Coq_x6f, (String.String (Coq_x73, (String.String (Coq_x75,
    (String.String (Coq_x72, (String.String (Coq_x65, (String.String
    (Coq_x3c, (String.String (Coq_x54, (String.String (Coq_x41,
    (String.String (Coq_x72, (String.String (Coq_x67, (String.String
    (Coq_x2c, (String.String (Coq_x20, (String.String (Coq_x54,
    (String.String (Coq_x52, (String.String (Coq_x65, (String.String
    (Coq_x74, (String.String (Coq_x3e, (String.String (Coq_x28,
    (String.String (Coq_x26, (String.String (Coq_x27, (String.String
    (Coq_x61, (String.String (Coq_x20, (String.String (Coq_x73,
    (String.String (Coq_x65, (String.String (Coq_x6c, (String.String
    (Coq_x66, (String.String (Coq_x2c, (String.String (Coq_x20,
    (String.String (Coq_x46, (String.String (Coq_x3a, (String.String
    (Coq_x20, (String.String (Coq_x69, (String.String (Coq_x6d,
    (String.String (Coq_x70, (String.String (Coq_x6c, (String.String
    (Coq_x20, (String.String (Coq_x46, (String.String (Coq_x6e,
    (String.String (Coq_x28, (String.String (Coq_x54, (String.String
    (Coq_x41, (String.String (Coq_x72, (String.String (Coq_x67,
    (String.String (Coq_x29, (String.String (Coq_x20, (String.String
    (Coq_x2d, (String.String (Coq_x3e, (String.String (Coq_x20,
    (String.String (Coq_x54, (String.String (Coq_x52, (String.String
    (Coq_x65, (String.String (Coq_x74, (String.String (Coq_x20,
    (String.String (Coq_x2b, (String.String (Coq_x20, (String.String
    (Coq_x27, (String.String (Coq_x61, (String.String (Coq_x29,
    (String.String (Coq_x20, (String.String (Coq_x2d, (String.String
    (Coq_x3e, (String.String (Coq_x20, (String.String (Coq_x26,
    (String.String (Coq_x27, (String.String (Coq_x61, (String.String
    (Coq_x20, (String.String (Coq_x64, (String.String (Coq_x79,
    (String.String (Coq_x6e, (String.String (Coq_x20, (String.String
    (Coq_x46, (String.String (Coq_x6e, (String.String (Coq_x28,
    (String.String (Coq_x54, (String.String (Coq_x41, (String.String
    (Coq_x72, (String.String (Coq_x67, (String.String (Coq_x29,
    (String.String (Coq_x20, (String.String (Coq_x2d, (String.String
    (Coq_x3e, (String.String (Coq_x20, (String.String (Coq_x54,
    (String.String (Coq_x52, (String.String (Coq_x65, (String.String
    (Coq_x74, (String.String (Coq_x20, (String.String (Coq_x7b,
    String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) :: ((String.String
    (Coq_x20, (String.String (Coq_x20, (String.String (Coq_x73,
    (String.String (Coq_x65, (String.String (Coq_x6c, (String.String
    (Coq_x66, (String.String (Coq_x2e, (String.String (Coq_x5f,
    (String.String (Coq_x5f, (String.String (Coq_x61, (String.String
    (Coq_x6c, (String.String (Coq_x6c, (String.String (Coq_x6f,
    (String.String (Coq_x63, (String.String (Coq_x2e, (String.String
    (Coq_x61, (String.String (Coq_x6c, (String.String (Coq_x6c,
    (String.String (Coq_x6f, (String.String (Coq_x63, (String.String
    (Coq_x28, (String.String (Coq_x46, (String.String (Coq_x29,
    String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))) :: ((String.String
    (Coq_x7d, String.EmptyString)) :: []))))))) }

(** val coq_RustConfig : coq_RustPrintConfig **)

let coq_RustConfig =
  { term_box_symbol = (String.String (Coq_x28, (String.String (Coq_x29,
    String.EmptyString)))); type_box_symbol = (String.String (Coq_x28,
    (String.String (Coq_x29, String.EmptyString)))); any_type_symbol =
    (String.String (Coq_x28, (String.String (Coq_x29, String.EmptyString))));
    print_full_names = true }

(** val default_attrs : ind_attr_map **)

let default_attrs _ =
  String.String (Coq_x23, (String.String (Coq_x5b, (String.String (Coq_x64,
    (String.String (Coq_x65, (String.String (Coq_x72, (String.String
    (Coq_x69, (String.String (Coq_x76, (String.String (Coq_x65,
    (String.String (Coq_x28, (String.String (Coq_x44, (String.String
    (Coq_x65, (String.String (Coq_x62, (String.String (Coq_x75,
    (String.String (Coq_x67, (String.String (Coq_x2c, (String.String
    (Coq_x20, (String.String (Coq_x43, (String.String (Coq_x6c,
    (String.String (Coq_x6f, (String.String (Coq_x6e, (String.String
    (Coq_x65, (String.String (Coq_x29, (String.String (Coq_x5d,
    String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))

(** val default_remaps : remaps **)

let default_remaps =
  no_remaps

(** val mk_preamble : String.t option -> String.t option -> coq_Preamble **)

let mk_preamble top program =
  { top_preamble =
    (match top with
     | Some top0 -> app plugin_extract_preamble.top_preamble (top0 :: [])
     | None -> plugin_extract_preamble.top_preamble); program_preamble =
    (match program with
     | Some program0 ->
       app plugin_extract_preamble.program_preamble (program0 :: [])
     | None -> plugin_extract_preamble.program_preamble) }

(** val box_to_rust :
    remaps -> coq_Preamble -> ind_attr_map -> extract_pcuic_params ->
    global_env -> (String.t list, String.t) result **)

let box_to_rust remaps0 preamble attrs params _UU03a3_ =
  bind (Obj.magic coq_Monad_result)
    (Obj.magic typed_transfoms params _UU03a3_) (fun _UU03a3_0 ->
    let p = print_program _UU03a3_0 remaps0 coq_RustConfig attrs preamble in
    bind (Obj.magic coq_Monad_result)
      (timed (String ((Ascii (false, false, false, false, true, false, true,
        false)), (String ((Ascii (false, true, false, false, true, true,
        true, false)), (String ((Ascii (true, false, false, true, false,
        true, true, false)), (String ((Ascii (false, true, true, true, false,
        true, true, false)), (String ((Ascii (false, false, true, false,
        true, true, true, false)), (String ((Ascii (true, false, false, true,
        false, true, true, false)), (String ((Ascii (false, true, true, true,
        false, true, true, false)), (String ((Ascii (true, true, true, false,
        false, true, true, false)), EmptyString)))))))))))))))) (fun _ ->
        Obj.magic finish_print_lines p)) (fun x -> let (_, s) = x in Ok s))
