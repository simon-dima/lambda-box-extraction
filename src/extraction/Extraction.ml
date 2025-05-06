open ExAst
open Transform0

(** val is_empty_type_decl : global_decl -> bool **)

let is_empty_type_decl = function
| InductiveDecl mib ->
  (match mib.ind_bodies with
   | [] -> false
   | oib :: _ -> (match oib.ind_ctors with
                  | [] -> true
                  | _ :: _ -> false))
| _ -> false

type extract_pcuic_params = { optimize_prop_discr : bool;
                              extract_transforms : coq_ExtractTransform list }
