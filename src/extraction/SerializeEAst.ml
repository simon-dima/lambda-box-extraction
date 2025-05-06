open Ascii
open CeresDeserialize
open CeresS
open Datatypes
open EAst
open SerializeCommon
open SerializePrimitives
open String0

(** val coq_Deserialize_def :
    'a1 coq_Deserialize -> 'a1 def coq_Deserialize **)

let coq_Deserialize_def h l e =
  Deser.match_con (String ((Ascii (false, false, true, false, false, true,
    true, false)), (String ((Ascii (true, false, true, false, false, true,
    true, false)), (String ((Ascii (false, true, true, false, false, true,
    true, false)), EmptyString)))))) [] (((String ((Ascii (false, false,
    true, false, false, true, true, false)), (String ((Ascii (true, false,
    true, false, false, true, true, false)), (String ((Ascii (false, true,
    true, false, false, true, true, false)), EmptyString)))))),
    (Deser.con3_ (fun x x0 x1 -> { dname = x; dbody = x0; rarg = x1 })
      coq_Deserialize_name h
      (coq_Deserialize_SemiIntegral coq_SemiIntegral_nat))) :: []) l e

(** val coq_Deserialize_mfixpoint :
    'a1 coq_Deserialize -> 'a1 mfixpoint coq_Deserialize **)

let coq_Deserialize_mfixpoint h l e =
  _from_sexp (coq_Deserialize_list (coq_Deserialize_def h)) l e

(** val deserialize_term : loc -> atom sexp_ -> (error, term) sum **)

let rec deserialize_term l e =
  let ds_term_list = fun l0 e0 ->
    _from_sexp (coq_Deserialize_list deserialize_term) l0 e0
  in
  let ds_mfixpoint = _from_sexp (coq_Deserialize_mfixpoint deserialize_term)
  in
  let ds_cases =
    _from_sexp
      (coq_Deserialize_list
        (coq_Deserialize_prod
          (_from_sexp (coq_Deserialize_list coq_Deserialize_name))
          deserialize_term))
  in
  let ds_prim = _from_sexp (coq_Deserialize_prim_val deserialize_term) in
  Deser.match_con (String ((Ascii (false, false, true, false, true, true,
    true, false)), (String ((Ascii (true, false, true, false, false, true,
    true, false)), (String ((Ascii (false, true, false, false, true, true,
    true, false)), (String ((Ascii (true, false, true, true, false, true,
    true, false)), EmptyString)))))))) (((String ((Ascii (false, false, true,
    false, true, true, true, false)), (String ((Ascii (false, true, false,
    false, false, false, true, false)), (String ((Ascii (true, true, true,
    true, false, true, true, false)), (String ((Ascii (false, false, false,
    true, true, true, true, false)), EmptyString)))))))), Coq_tBox) :: [])
    (((String ((Ascii (false, false, true, false, true, true, true, false)),
    (String ((Ascii (false, true, false, false, true, false, true, false)),
    (String ((Ascii (true, false, true, false, false, true, true, false)),
    (String ((Ascii (false, false, true, true, false, true, true, false)),
    EmptyString)))))))),
    (Deser.con1_ (fun x -> Coq_tRel x)
      (coq_Deserialize_SemiIntegral coq_SemiIntegral_nat))) :: (((String
    ((Ascii (false, false, true, false, true, true, true, false)), (String
    ((Ascii (false, true, true, false, true, false, true, false)), (String
    ((Ascii (true, false, false, false, false, true, true, false)), (String
    ((Ascii (false, true, false, false, true, true, true, false)),
    EmptyString)))))))),
    (Deser.con1_ (fun x -> Coq_tVar x) coq_Deserialize_ident)) :: (((String
    ((Ascii (false, false, true, false, true, true, true, false)), (String
    ((Ascii (true, false, true, false, false, false, true, false)), (String
    ((Ascii (false, true, true, false, true, true, true, false)), (String
    ((Ascii (true, false, false, false, false, true, true, false)), (String
    ((Ascii (false, true, false, false, true, true, true, false)),
    EmptyString)))))))))),
    (Deser.con2 (fun x x0 -> Coq_tEvar (x, x0))
      (_from_sexp (coq_Deserialize_SemiIntegral coq_SemiIntegral_nat))
      ds_term_list)) :: (((String ((Ascii (false, false, true, false, true,
    true, true, false)), (String ((Ascii (false, false, true, true, false,
    false, true, false)), (String ((Ascii (true, false, false, false, false,
    true, true, false)), (String ((Ascii (true, false, true, true, false,
    true, true, false)), (String ((Ascii (false, true, false, false, false,
    true, true, false)), (String ((Ascii (false, false, true, false, false,
    true, true, false)), (String ((Ascii (true, false, false, false, false,
    true, true, false)), EmptyString)))))))))))))),
    (Deser.con2 (fun x x0 -> Coq_tLambda (x, x0))
      (_from_sexp coq_Deserialize_name) deserialize_term)) :: (((String
    ((Ascii (false, false, true, false, true, true, true, false)), (String
    ((Ascii (false, false, true, true, false, false, true, false)), (String
    ((Ascii (true, false, true, false, false, true, true, false)), (String
    ((Ascii (false, false, true, false, true, true, true, false)), (String
    ((Ascii (true, false, false, true, false, false, true, false)), (String
    ((Ascii (false, true, true, true, false, true, true, false)),
    EmptyString)))))))))))),
    (Deser.con3 (fun x x0 x1 -> Coq_tLetIn (x, x0, x1))
      (_from_sexp coq_Deserialize_name) deserialize_term deserialize_term)) :: (((String
    ((Ascii (false, false, true, false, true, true, true, false)), (String
    ((Ascii (true, false, false, false, false, false, true, false)), (String
    ((Ascii (false, false, false, false, true, true, true, false)), (String
    ((Ascii (false, false, false, false, true, true, true, false)),
    EmptyString)))))))),
    (Deser.con2 (fun x x0 -> Coq_tApp (x, x0)) deserialize_term
      deserialize_term)) :: (((String ((Ascii (false, false, true, false,
    true, true, true, false)), (String ((Ascii (true, true, false, false,
    false, false, true, false)), (String ((Ascii (true, true, true, true,
    false, true, true, false)), (String ((Ascii (false, true, true, true,
    false, true, true, false)), (String ((Ascii (true, true, false, false,
    true, true, true, false)), (String ((Ascii (false, false, true, false,
    true, true, true, false)), EmptyString)))))))))))),
    (Deser.con1_ (fun x -> Coq_tConst x) coq_Deserialize_kername)) :: (((String
    ((Ascii (false, false, true, false, true, true, true, false)), (String
    ((Ascii (true, true, false, false, false, false, true, false)), (String
    ((Ascii (true, true, true, true, false, true, true, false)), (String
    ((Ascii (false, true, true, true, false, true, true, false)), (String
    ((Ascii (true, true, false, false, true, true, true, false)), (String
    ((Ascii (false, false, true, false, true, true, true, false)), (String
    ((Ascii (false, true, false, false, true, true, true, false)), (String
    ((Ascii (true, false, true, false, true, true, true, false)), (String
    ((Ascii (true, true, false, false, false, true, true, false)), (String
    ((Ascii (false, false, true, false, true, true, true, false)),
    EmptyString)))))))))))))))))))),
    (Deser.con3 (fun x x0 x1 -> Coq_tConstruct (x, x0, x1))
      (_from_sexp coq_Deserialize_inductive)
      (_from_sexp (coq_Deserialize_SemiIntegral coq_SemiIntegral_nat))
      ds_term_list)) :: (((String ((Ascii (false, false, true, false, true,
    true, true, false)), (String ((Ascii (true, true, false, false, false,
    false, true, false)), (String ((Ascii (true, false, false, false, false,
    true, true, false)), (String ((Ascii (true, true, false, false, true,
    true, true, false)), (String ((Ascii (true, false, true, false, false,
    true, true, false)), EmptyString)))))))))),
    (Deser.con3 (fun x x0 x1 -> Coq_tCase (x, x0, x1))
      (_from_sexp
        (coq_Deserialize_prod coq_Deserialize_inductive
          (coq_Deserialize_SemiIntegral coq_SemiIntegral_nat)))
      deserialize_term ds_cases)) :: (((String ((Ascii (false, false, true,
    false, true, true, true, false)), (String ((Ascii (false, false, false,
    false, true, false, true, false)), (String ((Ascii (false, true, false,
    false, true, true, true, false)), (String ((Ascii (true, true, true,
    true, false, true, true, false)), (String ((Ascii (false, true, false,
    true, false, true, true, false)), EmptyString)))))))))),
    (Deser.con2 (fun x x0 -> Coq_tProj (x, x0))
      (_from_sexp coq_Deserialize_projection) deserialize_term)) :: (((String
    ((Ascii (false, false, true, false, true, true, true, false)), (String
    ((Ascii (false, true, true, false, false, false, true, false)), (String
    ((Ascii (true, false, false, true, false, true, true, false)), (String
    ((Ascii (false, false, false, true, true, true, true, false)),
    EmptyString)))))))),
    (Deser.con2 (fun x x0 -> Coq_tFix (x, x0)) ds_mfixpoint
      (_from_sexp (coq_Deserialize_SemiIntegral coq_SemiIntegral_nat)))) :: (((String
    ((Ascii (false, false, true, false, true, true, true, false)), (String
    ((Ascii (true, true, false, false, false, false, true, false)), (String
    ((Ascii (true, true, true, true, false, true, true, false)), (String
    ((Ascii (false, true, true, false, false, false, true, false)), (String
    ((Ascii (true, false, false, true, false, true, true, false)), (String
    ((Ascii (false, false, false, true, true, true, true, false)),
    EmptyString)))))))))))),
    (Deser.con2 (fun x x0 -> Coq_tCoFix (x, x0)) ds_mfixpoint
      (_from_sexp (coq_Deserialize_SemiIntegral coq_SemiIntegral_nat)))) :: (((String
    ((Ascii (false, false, true, false, true, true, true, false)), (String
    ((Ascii (false, false, false, false, true, false, true, false)), (String
    ((Ascii (false, true, false, false, true, true, true, false)), (String
    ((Ascii (true, false, false, true, false, true, true, false)), (String
    ((Ascii (true, false, true, true, false, true, true, false)),
    EmptyString)))))))))),
    (Deser.con1 (fun x -> Coq_tPrim x) ds_prim)) :: (((String ((Ascii (false,
    false, true, false, true, true, true, false)), (String ((Ascii (false,
    false, true, true, false, false, true, false)), (String ((Ascii (true,
    false, false, false, false, true, true, false)), (String ((Ascii (false,
    true, false, true, true, true, true, false)), (String ((Ascii (true,
    false, false, true, true, true, true, false)), EmptyString)))))))))),
    (Deser.con1 (fun x -> Coq_tLazy x) deserialize_term)) :: (((String
    ((Ascii (false, false, true, false, true, true, true, false)), (String
    ((Ascii (false, true, true, false, false, false, true, false)), (String
    ((Ascii (true, true, true, true, false, true, true, false)), (String
    ((Ascii (false, true, false, false, true, true, true, false)), (String
    ((Ascii (true, true, false, false, false, true, true, false)), (String
    ((Ascii (true, false, true, false, false, true, true, false)),
    EmptyString)))))))))))),
    (Deser.con1 (fun x -> Coq_tForce x) deserialize_term)) :: [])))))))))))))))
    l e

(** val coq_Deserialize_term : term coq_Deserialize **)

let coq_Deserialize_term =
  deserialize_term

(** val coq_Deserialize_constructor_body :
    constructor_body coq_Deserialize **)

let coq_Deserialize_constructor_body l e =
  Deser.match_con (String ((Ascii (true, true, false, false, false, true,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), (String ((Ascii (false, true, true, true, false, true,
    true, false)), (String ((Ascii (true, true, false, false, true, true,
    true, false)), (String ((Ascii (false, false, true, false, true, true,
    true, false)), (String ((Ascii (false, true, false, false, true, true,
    true, false)), (String ((Ascii (true, false, true, false, true, true,
    true, false)), (String ((Ascii (true, true, false, false, false, true,
    true, false)), (String ((Ascii (false, false, true, false, true, true,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), (String ((Ascii (false, true, false, false, true, true,
    true, false)), (String ((Ascii (true, true, true, true, true, false,
    true, false)), (String ((Ascii (false, true, false, false, false, true,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), (String ((Ascii (false, false, true, false, false, true,
    true, false)), (String ((Ascii (true, false, false, true, true, true,
    true, false)), EmptyString)))))))))))))))))))))))))))))))) [] (((String
    ((Ascii (true, true, false, false, false, true, true, false)), (String
    ((Ascii (true, true, true, true, false, true, true, false)), (String
    ((Ascii (false, true, true, true, false, true, true, false)), (String
    ((Ascii (true, true, false, false, true, true, true, false)), (String
    ((Ascii (false, false, true, false, true, true, true, false)), (String
    ((Ascii (false, true, false, false, true, true, true, false)), (String
    ((Ascii (true, false, true, false, true, true, true, false)), (String
    ((Ascii (true, true, false, false, false, true, true, false)), (String
    ((Ascii (false, false, true, false, true, true, true, false)), (String
    ((Ascii (true, true, true, true, false, true, true, false)), (String
    ((Ascii (false, true, false, false, true, true, true, false)), (String
    ((Ascii (true, true, true, true, true, false, true, false)), (String
    ((Ascii (false, true, false, false, false, true, true, false)), (String
    ((Ascii (true, true, true, true, false, true, true, false)), (String
    ((Ascii (false, false, true, false, false, true, true, false)), (String
    ((Ascii (true, false, false, true, true, true, true, false)),
    EmptyString)))))))))))))))))))))))))))))))),
    (Deser.con2_ (fun x x0 -> { cstr_name = x; cstr_nargs = x0 })
      coq_Deserialize_ident
      (coq_Deserialize_SemiIntegral coq_SemiIntegral_nat))) :: []) l e

(** val coq_Deserialize_projection_body : projection_body coq_Deserialize **)

let coq_Deserialize_projection_body l e =
  Deser.match_con (String ((Ascii (false, false, false, false, true, true,
    true, false)), (String ((Ascii (false, true, false, false, true, true,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), (String ((Ascii (false, true, false, true, false, true,
    true, false)), (String ((Ascii (true, false, true, false, false, true,
    true, false)), (String ((Ascii (true, true, false, false, false, true,
    true, false)), (String ((Ascii (false, false, true, false, true, true,
    true, false)), (String ((Ascii (true, false, false, true, false, true,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), (String ((Ascii (false, true, true, true, false, true,
    true, false)), (String ((Ascii (true, true, true, true, true, false,
    true, false)), (String ((Ascii (false, true, false, false, false, true,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), (String ((Ascii (false, false, true, false, false, true,
    true, false)), (String ((Ascii (true, false, false, true, true, true,
    true, false)), EmptyString)))))))))))))))))))))))))))))) [] (((String
    ((Ascii (false, false, false, false, true, true, true, false)), (String
    ((Ascii (false, true, false, false, true, true, true, false)), (String
    ((Ascii (true, true, true, true, false, true, true, false)), (String
    ((Ascii (false, true, false, true, false, true, true, false)), (String
    ((Ascii (true, false, true, false, false, true, true, false)), (String
    ((Ascii (true, true, false, false, false, true, true, false)), (String
    ((Ascii (false, false, true, false, true, true, true, false)), (String
    ((Ascii (true, false, false, true, false, true, true, false)), (String
    ((Ascii (true, true, true, true, false, true, true, false)), (String
    ((Ascii (false, true, true, true, false, true, true, false)), (String
    ((Ascii (true, true, true, true, true, false, true, false)), (String
    ((Ascii (false, true, false, false, false, true, true, false)), (String
    ((Ascii (true, true, true, true, false, true, true, false)), (String
    ((Ascii (false, false, true, false, false, true, true, false)), (String
    ((Ascii (true, false, false, true, true, true, true, false)),
    EmptyString)))))))))))))))))))))))))))))),
    (Deser.con1_ (fun x -> x) coq_Deserialize_ident)) :: []) l e

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
    (Deser.con5_ (fun x x0 x1 x2 x3 -> { ind_name = x; ind_propositional =
      x0; ind_kelim = x1; ind_ctors = x2; ind_projs = x3 })
      coq_Deserialize_ident coq_Deserialize_bool
      coq_Deserialize_allowed_eliminations
      (coq_Deserialize_list coq_Deserialize_constructor_body)
      (coq_Deserialize_list coq_Deserialize_projection_body))) :: []) l e

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
    (Deser.con1_ (fun x -> x) (coq_Deserialize_option coq_Deserialize_term))) :: [])
    l e

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
      coq_Deserialize_mutual_inductive_body)) :: [])) l e

(** val coq_Deserialize_global_declarations :
    global_declarations coq_Deserialize **)

let coq_Deserialize_global_declarations l e =
  _from_sexp
    (coq_Deserialize_list
      (coq_Deserialize_prod coq_Deserialize_kername
        coq_Deserialize_global_decl)) l e

(** val coq_Deserialize_program : program coq_Deserialize **)

let coq_Deserialize_program l e =
  _from_sexp
    (coq_Deserialize_prod coq_Deserialize_global_declarations
      coq_Deserialize_term) l e

(** val program_of_string : string -> (error, program) sum **)

let program_of_string s =
  from_string coq_Deserialize_program s
