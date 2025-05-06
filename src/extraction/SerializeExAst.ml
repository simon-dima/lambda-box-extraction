open Ascii
open CeresDeserialize
open CeresExtra
open Datatypes
open ExAst
open SerializeCommon
open SerializeEAst
open String0

(** val coq_Deserialize_box_type : box_type coq_Deserialize **)

let rec coq_Deserialize_box_type l e =
  Deser.match_con (String ((Ascii (false, true, false, false, false, true,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), (String ((Ascii (false, false, false, true, true, true,
    true, false)), (String ((Ascii (true, true, true, true, true, false,
    true, false)), (String ((Ascii (false, false, true, false, true, true,
    true, false)), (String ((Ascii (true, false, false, true, true, true,
    true, false)), (String ((Ascii (false, false, false, false, true, true,
    true, false)), (String ((Ascii (true, false, true, false, false, true,
    true, false)), EmptyString)))))))))))))))) (((String ((Ascii (false,
    false, true, false, true, false, true, false)), (String ((Ascii (false,
    true, false, false, false, false, true, false)), (String ((Ascii (true,
    true, true, true, false, true, true, false)), (String ((Ascii (false,
    false, false, true, true, true, true, false)), EmptyString)))))))),
    TBox) :: (((String ((Ascii (false, false, true, false, true, false, true,
    false)), (String ((Ascii (true, false, false, false, false, false, true,
    false)), (String ((Ascii (false, true, true, true, false, true, true,
    false)), (String ((Ascii (true, false, false, true, true, true, true,
    false)), EmptyString)))))))), TAny) :: [])) (((String ((Ascii (false,
    false, true, false, true, false, true, false)), (String ((Ascii (true,
    false, false, false, false, false, true, false)), (String ((Ascii (false,
    true, false, false, true, true, true, false)), (String ((Ascii (false,
    true, false, false, true, true, true, false)), EmptyString)))))))),
    (Deser.con2 (fun x x0 -> TArr (x, x0)) coq_Deserialize_box_type
      coq_Deserialize_box_type)) :: (((String ((Ascii (false, false, true,
    false, true, false, true, false)), (String ((Ascii (true, false, false,
    false, false, false, true, false)), (String ((Ascii (false, false, false,
    false, true, true, true, false)), (String ((Ascii (false, false, false,
    false, true, true, true, false)), EmptyString)))))))),
    (Deser.con2 (fun x x0 -> TApp (x, x0)) coq_Deserialize_box_type
      coq_Deserialize_box_type)) :: (((String ((Ascii (false, false, true,
    false, true, false, true, false)), (String ((Ascii (false, true, true,
    false, true, false, true, false)), (String ((Ascii (true, false, false,
    false, false, true, true, false)), (String ((Ascii (false, true, false,
    false, true, true, true, false)), EmptyString)))))))),
    (Deser.con1_ (fun x -> TVar x)
      (coq_Deserialize_SemiIntegral coq_SemiIntegral_nat))) :: (((String
    ((Ascii (false, false, true, false, true, false, true, false)), (String
    ((Ascii (true, false, false, true, false, false, true, false)), (String
    ((Ascii (false, true, true, true, false, true, true, false)), (String
    ((Ascii (false, false, true, false, false, true, true, false)),
    EmptyString)))))))),
    (Deser.con1_ (fun x -> TInd x) coq_Deserialize_inductive)) :: (((String
    ((Ascii (false, false, true, false, true, false, true, false)), (String
    ((Ascii (true, true, false, false, false, false, true, false)), (String
    ((Ascii (true, true, true, true, false, true, true, false)), (String
    ((Ascii (false, true, true, true, false, true, true, false)), (String
    ((Ascii (true, true, false, false, true, true, true, false)), (String
    ((Ascii (false, false, true, false, true, true, true, false)),
    EmptyString)))))))))))),
    (Deser.con1_ (fun x -> TConst x) coq_Deserialize_kername)) :: []))))) l e

(** val coq_Deserialize_type_var_info : type_var_info coq_Deserialize **)

let coq_Deserialize_type_var_info l e =
  Deser.match_con (String ((Ascii (false, false, true, false, true, true,
    true, false)), (String ((Ascii (true, false, false, true, true, true,
    true, false)), (String ((Ascii (false, false, false, false, true, true,
    true, false)), (String ((Ascii (true, false, true, false, false, true,
    true, false)), (String ((Ascii (true, true, true, true, true, false,
    true, false)), (String ((Ascii (false, true, true, false, true, true,
    true, false)), (String ((Ascii (true, false, false, false, false, true,
    true, false)), (String ((Ascii (false, true, false, false, true, true,
    true, false)), (String ((Ascii (true, true, true, true, true, false,
    true, false)), (String ((Ascii (true, false, false, true, false, true,
    true, false)), (String ((Ascii (false, true, true, true, false, true,
    true, false)), (String ((Ascii (false, true, true, false, false, true,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), EmptyString)))))))))))))))))))))))))) [] (((String ((Ascii
    (false, false, true, false, true, true, true, false)), (String ((Ascii
    (true, false, false, true, true, true, true, false)), (String ((Ascii
    (false, false, false, false, true, true, true, false)), (String ((Ascii
    (true, false, true, false, false, true, true, false)), (String ((Ascii
    (true, true, true, true, true, false, true, false)), (String ((Ascii
    (false, true, true, false, true, true, true, false)), (String ((Ascii
    (true, false, false, false, false, true, true, false)), (String ((Ascii
    (false, true, false, false, true, true, true, false)), (String ((Ascii
    (true, true, true, true, true, false, true, false)), (String ((Ascii
    (true, false, false, true, false, true, true, false)), (String ((Ascii
    (false, true, true, true, false, true, true, false)), (String ((Ascii
    (false, true, true, false, false, true, true, false)), (String ((Ascii
    (true, true, true, true, false, true, true, false)),
    EmptyString)))))))))))))))))))))))))),
    (Deser.con4_ (fun x x0 x1 x2 -> { tvar_name = x; tvar_is_logical = x0;
      tvar_is_arity = x1; tvar_is_sort = x2 }) coq_Deserialize_name
      coq_Deserialize_bool coq_Deserialize_bool coq_Deserialize_bool)) :: [])
    l e

(** val coq_Deserialize_constant_body : constant_body coq_Deserialize **)

let coq_Deserialize_constant_body l e =
  Deser.match_con (String ((Ascii (true, true, false, false, false, true,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), (String ((Ascii (false, true, true, true, false, true,
    true, false)), (String ((Ascii (true, true, false, false, true, true,
    true, false)), (String ((Ascii (false, false, true, false, true, true,
    true, false)), (String ((Ascii (true, false, false, false, false, true,
    true, false)), (String ((Ascii (false, true, true, true, false, true,
    true, false)), (String ((Ascii (false, false, true, false, true, true,
    true, false)), (String ((Ascii (true, true, true, true, true, false,
    true, false)), (String ((Ascii (false, true, false, false, false, true,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), (String ((Ascii (false, false, true, false, false, true,
    true, false)), (String ((Ascii (true, false, false, true, true, true,
    true, false)), EmptyString)))))))))))))))))))))))))) [] (((String ((Ascii
    (true, true, false, false, false, true, true, false)), (String ((Ascii
    (true, true, true, true, false, true, true, false)), (String ((Ascii
    (false, true, true, true, false, true, true, false)), (String ((Ascii
    (true, true, false, false, true, true, true, false)), (String ((Ascii
    (false, false, true, false, true, true, true, false)), (String ((Ascii
    (true, false, false, false, false, true, true, false)), (String ((Ascii
    (false, true, true, true, false, true, true, false)), (String ((Ascii
    (false, false, true, false, true, true, true, false)), (String ((Ascii
    (true, true, true, true, true, false, true, false)), (String ((Ascii
    (false, true, false, false, false, true, true, false)), (String ((Ascii
    (true, true, true, true, false, true, true, false)), (String ((Ascii
    (false, false, true, false, false, true, true, false)), (String ((Ascii
    (true, false, false, true, true, true, true, false)),
    EmptyString)))))))))))))))))))))))))),
    (Deser.con2_ (fun x x0 -> { cst_type = x; cst_body = x0 })
      (coq_Deserialize_prod (coq_Deserialize_list coq_Deserialize_name)
        coq_Deserialize_box_type)
      (coq_Deserialize_option coq_Deserialize_term))) :: []) l e

(** val coq_Deserialize_one_inductive_body :
    one_inductive_body coq_Deserialize **)

let coq_Deserialize_one_inductive_body l e =
  Deser.match_con (String ((Ascii (true, true, true, true, false, true, true,
    false)), (String ((Ascii (false, true, true, true, false, true, true,
    false)), (String ((Ascii (true, false, true, false, false, true, true,
    false)), (String ((Ascii (true, true, true, true, true, false, true,
    false)), (String ((Ascii (true, false, false, true, false, true, true,
    false)), (String ((Ascii (false, true, true, true, false, true, true,
    false)), (String ((Ascii (false, false, true, false, false, true, true,
    false)), (String ((Ascii (true, false, true, false, true, true, true,
    false)), (String ((Ascii (true, true, false, false, false, true, true,
    false)), (String ((Ascii (false, false, true, false, true, true, true,
    false)), (String ((Ascii (true, false, false, true, false, true, true,
    false)), (String ((Ascii (false, true, true, false, true, true, true,
    false)), (String ((Ascii (true, false, true, false, false, true, true,
    false)), (String ((Ascii (true, true, true, true, true, false, true,
    false)), (String ((Ascii (false, true, false, false, false, true, true,
    false)), (String ((Ascii (true, true, true, true, false, true, true,
    false)), (String ((Ascii (false, false, true, false, false, true, true,
    false)), (String ((Ascii (true, false, false, true, true, true, true,
    false)), EmptyString)))))))))))))))))))))))))))))))))))) [] (((String
    ((Ascii (true, true, true, true, false, true, true, false)), (String
    ((Ascii (false, true, true, true, false, true, true, false)), (String
    ((Ascii (true, false, true, false, false, true, true, false)), (String
    ((Ascii (true, true, true, true, true, false, true, false)), (String
    ((Ascii (true, false, false, true, false, true, true, false)), (String
    ((Ascii (false, true, true, true, false, true, true, false)), (String
    ((Ascii (false, false, true, false, false, true, true, false)), (String
    ((Ascii (true, false, true, false, true, true, true, false)), (String
    ((Ascii (true, true, false, false, false, true, true, false)), (String
    ((Ascii (false, false, true, false, true, true, true, false)), (String
    ((Ascii (true, false, false, true, false, true, true, false)), (String
    ((Ascii (false, true, true, false, true, true, true, false)), (String
    ((Ascii (true, false, true, false, false, true, true, false)), (String
    ((Ascii (true, true, true, true, true, false, true, false)), (String
    ((Ascii (false, true, false, false, false, true, true, false)), (String
    ((Ascii (true, true, true, true, false, true, true, false)), (String
    ((Ascii (false, false, true, false, false, true, true, false)), (String
    ((Ascii (true, false, false, true, true, true, true, false)),
    EmptyString)))))))))))))))))))))))))))))))))))),
    (con6_ (fun x x0 x1 x2 x3 x4 -> { ind_name = x; ind_propositional = x0;
      ind_kelim = x1; ind_type_vars = x2; ind_ctors = x3; ind_projs = x4 })
      coq_Deserialize_ident coq_Deserialize_bool
      coq_Deserialize_allowed_eliminations
      (coq_Deserialize_list coq_Deserialize_type_var_info)
      (coq_Deserialize_list
        (coq_Deserialize_prod
          (coq_Deserialize_prod coq_Deserialize_ident
            (coq_Deserialize_list
              (coq_Deserialize_prod coq_Deserialize_name
                coq_Deserialize_box_type)))
          (coq_Deserialize_SemiIntegral coq_SemiIntegral_nat)))
      (coq_Deserialize_list
        (coq_Deserialize_prod coq_Deserialize_ident coq_Deserialize_box_type)))) :: [])
    l e

(** val coq_Deserialize_mutual_inductive_body :
    mutual_inductive_body coq_Deserialize **)

let coq_Deserialize_mutual_inductive_body l e =
  Deser.match_con (String ((Ascii (true, false, true, true, false, true,
    true, false)), (String ((Ascii (true, false, true, false, true, true,
    true, false)), (String ((Ascii (false, false, true, false, true, true,
    true, false)), (String ((Ascii (true, false, true, false, true, true,
    true, false)), (String ((Ascii (true, false, false, false, false, true,
    true, false)), (String ((Ascii (false, false, true, true, false, true,
    true, false)), (String ((Ascii (true, true, true, true, true, false,
    true, false)), (String ((Ascii (true, false, false, true, false, true,
    true, false)), (String ((Ascii (false, true, true, true, false, true,
    true, false)), (String ((Ascii (false, false, true, false, false, true,
    true, false)), (String ((Ascii (true, false, true, false, true, true,
    true, false)), (String ((Ascii (true, true, false, false, false, true,
    true, false)), (String ((Ascii (false, false, true, false, true, true,
    true, false)), (String ((Ascii (true, false, false, true, false, true,
    true, false)), (String ((Ascii (false, true, true, false, true, true,
    true, false)), (String ((Ascii (true, false, true, false, false, true,
    true, false)), (String ((Ascii (true, true, true, true, true, false,
    true, false)), (String ((Ascii (false, true, false, false, false, true,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), (String ((Ascii (false, false, true, false, false, true,
    true, false)), (String ((Ascii (true, false, false, true, true, true,
    true, false)), EmptyString)))))))))))))))))))))))))))))))))))))))))) []
    (((String ((Ascii (true, false, true, true, false, true, true, false)),
    (String ((Ascii (true, false, true, false, true, true, true, false)),
    (String ((Ascii (false, false, true, false, true, true, true, false)),
    (String ((Ascii (true, false, true, false, true, true, true, false)),
    (String ((Ascii (true, false, false, false, false, true, true, false)),
    (String ((Ascii (false, false, true, true, false, true, true, false)),
    (String ((Ascii (true, true, true, true, true, false, true, false)),
    (String ((Ascii (true, false, false, true, false, true, true, false)),
    (String ((Ascii (false, true, true, true, false, true, true, false)),
    (String ((Ascii (false, false, true, false, false, true, true, false)),
    (String ((Ascii (true, false, true, false, true, true, true, false)),
    (String ((Ascii (true, true, false, false, false, true, true, false)),
    (String ((Ascii (false, false, true, false, true, true, true, false)),
    (String ((Ascii (true, false, false, true, false, true, true, false)),
    (String ((Ascii (false, true, true, false, true, true, true, false)),
    (String ((Ascii (true, false, true, false, false, true, true, false)),
    (String ((Ascii (true, true, true, true, true, false, true, false)),
    (String ((Ascii (false, true, false, false, false, true, true, false)),
    (String ((Ascii (true, true, true, true, false, true, true, false)),
    (String ((Ascii (false, false, true, false, false, true, true, false)),
    (String ((Ascii (true, false, false, true, true, true, true, false)),
    EmptyString)))))))))))))))))))))))))))))))))))))))))),
    (Deser.con3_ (fun x x0 x1 -> { ind_finite = x; ind_npars = x0;
      ind_bodies = x1 }) coq_Deserialize_recursivity_kind
      (coq_Deserialize_SemiIntegral coq_SemiIntegral_nat)
      (coq_Deserialize_list coq_Deserialize_one_inductive_body))) :: []) l e

(** val coq_Deserialize_global_decl : global_decl coq_Deserialize **)

let coq_Deserialize_global_decl l e =
  Deser.match_con (String ((Ascii (true, true, true, false, false, true,
    true, false)), (String ((Ascii (false, false, true, true, false, true,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), (String ((Ascii (false, true, false, false, false, true,
    true, false)), (String ((Ascii (true, false, false, false, false, true,
    true, false)), (String ((Ascii (false, false, true, true, false, true,
    true, false)), (String ((Ascii (true, true, true, true, true, false,
    true, false)), (String ((Ascii (false, false, true, false, false, true,
    true, false)), (String ((Ascii (true, false, true, false, false, true,
    true, false)), (String ((Ascii (true, true, false, false, false, true,
    true, false)), (String ((Ascii (false, false, true, true, false, true,
    true, false)), EmptyString)))))))))))))))))))))) [] (((String ((Ascii
    (true, true, false, false, false, false, true, false)), (String ((Ascii
    (true, true, true, true, false, true, true, false)), (String ((Ascii
    (false, true, true, true, false, true, true, false)), (String ((Ascii
    (true, true, false, false, true, true, true, false)), (String ((Ascii
    (false, false, true, false, true, true, true, false)), (String ((Ascii
    (true, false, false, false, false, true, true, false)), (String ((Ascii
    (false, true, true, true, false, true, true, false)), (String ((Ascii
    (false, false, true, false, true, true, true, false)), (String ((Ascii
    (false, false, true, false, false, false, true, false)), (String ((Ascii
    (true, false, true, false, false, true, true, false)), (String ((Ascii
    (true, true, false, false, false, true, true, false)), (String ((Ascii
    (false, false, true, true, false, true, true, false)),
    EmptyString)))))))))))))))))))))))),
    (Deser.con1_ (fun x -> ConstantDecl x) coq_Deserialize_constant_body)) :: (((String
    ((Ascii (true, false, false, true, false, false, true, false)), (String
    ((Ascii (false, true, true, true, false, true, true, false)), (String
    ((Ascii (false, false, true, false, false, true, true, false)), (String
    ((Ascii (true, false, true, false, true, true, true, false)), (String
    ((Ascii (true, true, false, false, false, true, true, false)), (String
    ((Ascii (false, false, true, false, true, true, true, false)), (String
    ((Ascii (true, false, false, true, false, true, true, false)), (String
    ((Ascii (false, true, true, false, true, true, true, false)), (String
    ((Ascii (true, false, true, false, false, true, true, false)), (String
    ((Ascii (false, false, true, false, false, false, true, false)), (String
    ((Ascii (true, false, true, false, false, true, true, false)), (String
    ((Ascii (true, true, false, false, false, true, true, false)), (String
    ((Ascii (false, false, true, true, false, true, true, false)),
    EmptyString)))))))))))))))))))))))))),
    (Deser.con1_ (fun x -> InductiveDecl x)
      coq_Deserialize_mutual_inductive_body)) :: (((String ((Ascii (false,
    false, true, false, true, false, true, false)), (String ((Ascii (true,
    false, false, true, true, true, true, false)), (String ((Ascii (false,
    false, false, false, true, true, true, false)), (String ((Ascii (true,
    false, true, false, false, true, true, false)), (String ((Ascii (true,
    false, false, false, false, false, true, false)), (String ((Ascii (false,
    false, true, true, false, true, true, false)), (String ((Ascii (true,
    false, false, true, false, true, true, false)), (String ((Ascii (true,
    false, false, false, false, true, true, false)), (String ((Ascii (true,
    true, false, false, true, true, true, false)), (String ((Ascii (false,
    false, true, false, false, false, true, false)), (String ((Ascii (true,
    false, true, false, false, true, true, false)), (String ((Ascii (true,
    true, false, false, false, true, true, false)), (String ((Ascii (false,
    false, true, true, false, true, true, false)),
    EmptyString)))))))))))))))))))))))))),
    (Deser.con1_ (fun x -> TypeAliasDecl x)
      (coq_Deserialize_option
        (coq_Deserialize_prod
          (coq_Deserialize_list coq_Deserialize_type_var_info)
          coq_Deserialize_box_type)))) :: []))) l e

(** val coq_Deserialize_global_env : global_env coq_Deserialize **)

let coq_Deserialize_global_env l e =
  _from_sexp
    (coq_Deserialize_list
      (coq_Deserialize_prod
        (coq_Deserialize_prod coq_Deserialize_kername coq_Deserialize_bool)
        coq_Deserialize_global_decl)) l e

(** val global_env_of_string : string -> (error, global_env) sum **)

let global_env_of_string s =
  from_string coq_Deserialize_global_env s
