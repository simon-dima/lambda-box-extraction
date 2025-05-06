open ExAst
open Transform0

val is_empty_type_decl : global_decl -> bool

type extract_pcuic_params = { optimize_prop_discr : bool;
                              extract_transforms : coq_ExtractTransform list }
