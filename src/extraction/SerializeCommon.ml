open Ascii
open BasicAst
open CeresDeserialize
open CeresS
open Datatypes
open Kernames
open String0
open Universes0
open Bytestring

(** val coq_Deserialize_ident : ident coq_Deserialize **)

let coq_Deserialize_ident l = function
| Atom_ a ->
  (match a with
   | Str s -> Coq_inr (String.of_string s)
   | _ ->
     Coq_inl (DeserError (l, (MsgStr (String ((Ascii (true, false, true,
       false, false, true, true, false)), (String ((Ascii (false, true,
       false, false, true, true, true, false)), (String ((Ascii (false, true,
       false, false, true, true, true, false)), (String ((Ascii (true, true,
       true, true, false, true, true, false)), (String ((Ascii (false, true,
       false, false, true, true, true, false)), EmptyString))))))))))))))
| List _ ->
  Coq_inl (DeserError (l, (MsgStr (String ((Ascii (true, false, true, false,
    false, true, true, false)), (String ((Ascii (false, true, false, false,
    true, true, true, false)), (String ((Ascii (false, true, false, false,
    true, true, true, false)), (String ((Ascii (true, true, true, true,
    false, true, true, false)), (String ((Ascii (false, true, false, false,
    true, true, true, false)), EmptyString)))))))))))))

(** val coq_Deserialize_dirpath : dirpath coq_Deserialize **)

let coq_Deserialize_dirpath l e =
  _from_sexp (coq_Deserialize_list coq_Deserialize_ident) l e

(** val coq_Deserialize_modpath : modpath coq_Deserialize **)

let rec coq_Deserialize_modpath l e =
  Deser.match_con (String ((Ascii (true, false, true, true, false, true,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), (String ((Ascii (false, false, true, false, false, true,
    true, false)), (String ((Ascii (false, false, false, false, true, true,
    true, false)), (String ((Ascii (true, false, false, false, false, true,
    true, false)), (String ((Ascii (false, false, true, false, true, true,
    true, false)), (String ((Ascii (false, false, false, true, false, true,
    true, false)), EmptyString)))))))))))))) [] (((String ((Ascii (true,
    false, true, true, false, false, true, false)), (String ((Ascii (false,
    false, false, false, true, false, true, false)), (String ((Ascii (false,
    true, true, false, false, true, true, false)), (String ((Ascii (true,
    false, false, true, false, true, true, false)), (String ((Ascii (false,
    false, true, true, false, true, true, false)), (String ((Ascii (true,
    false, true, false, false, true, true, false)), EmptyString)))))))))))),
    (Deser.con1_ (fun x -> MPfile x) coq_Deserialize_dirpath)) :: (((String
    ((Ascii (true, false, true, true, false, false, true, false)), (String
    ((Ascii (false, false, false, false, true, false, true, false)), (String
    ((Ascii (false, true, false, false, false, true, true, false)), (String
    ((Ascii (true, true, true, true, false, true, true, false)), (String
    ((Ascii (true, false, true, false, true, true, true, false)), (String
    ((Ascii (false, true, true, true, false, true, true, false)), (String
    ((Ascii (false, false, true, false, false, true, true, false)),
    EmptyString)))))))))))))),
    (Deser.con3_ (fun x x0 x1 -> MPbound (x, x0, x1)) coq_Deserialize_dirpath
      coq_Deserialize_ident
      (coq_Deserialize_SemiIntegral coq_SemiIntegral_nat))) :: (((String
    ((Ascii (true, false, true, true, false, false, true, false)), (String
    ((Ascii (false, false, false, false, true, false, true, false)), (String
    ((Ascii (false, false, true, false, false, true, true, false)), (String
    ((Ascii (true, true, true, true, false, true, true, false)), (String
    ((Ascii (false, false, true, false, true, true, true, false)),
    EmptyString)))))))))),
    (Deser.con2 (fun x x0 -> MPdot (x, x0)) coq_Deserialize_modpath
      (_from_sexp coq_Deserialize_ident))) :: []))) l e

(** val coq_Deserialize_kername : kername coq_Deserialize **)

let coq_Deserialize_kername l e =
  _from_sexp
    (coq_Deserialize_prod coq_Deserialize_modpath coq_Deserialize_ident) l e

(** val coq_Deserialize_inductive : inductive coq_Deserialize **)

let coq_Deserialize_inductive l e =
  Deser.match_con (String ((Ascii (true, false, false, true, false, true,
    true, false)), (String ((Ascii (false, true, true, true, false, true,
    true, false)), (String ((Ascii (false, false, true, false, false, true,
    true, false)), (String ((Ascii (true, false, true, false, true, true,
    true, false)), (String ((Ascii (true, true, false, false, false, true,
    true, false)), (String ((Ascii (false, false, true, false, true, true,
    true, false)), (String ((Ascii (true, false, false, true, false, true,
    true, false)), (String ((Ascii (false, true, true, false, true, true,
    true, false)), (String ((Ascii (true, false, true, false, false, true,
    true, false)), EmptyString)))))))))))))))))) [] (((String ((Ascii (true,
    false, false, true, false, true, true, false)), (String ((Ascii (false,
    true, true, true, false, true, true, false)), (String ((Ascii (false,
    false, true, false, false, true, true, false)), (String ((Ascii (true,
    false, true, false, true, true, true, false)), (String ((Ascii (true,
    true, false, false, false, true, true, false)), (String ((Ascii (false,
    false, true, false, true, true, true, false)), (String ((Ascii (true,
    false, false, true, false, true, true, false)), (String ((Ascii (false,
    true, true, false, true, true, true, false)), (String ((Ascii (true,
    false, true, false, false, true, true, false)),
    EmptyString)))))))))))))))))),
    (Deser.con2_ (fun x x0 -> { inductive_mind = x; inductive_ind = x0 })
      coq_Deserialize_kername
      (coq_Deserialize_SemiIntegral coq_SemiIntegral_nat))) :: []) l e

(** val coq_Deserialize_projection : projection coq_Deserialize **)

let coq_Deserialize_projection l e =
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
    true, false)), EmptyString)))))))))))))))))))) [] (((String ((Ascii
    (false, false, false, false, true, true, true, false)), (String ((Ascii
    (false, true, false, false, true, true, true, false)), (String ((Ascii
    (true, true, true, true, false, true, true, false)), (String ((Ascii
    (false, true, false, true, false, true, true, false)), (String ((Ascii
    (true, false, true, false, false, true, true, false)), (String ((Ascii
    (true, true, false, false, false, true, true, false)), (String ((Ascii
    (false, false, true, false, true, true, true, false)), (String ((Ascii
    (true, false, false, true, false, true, true, false)), (String ((Ascii
    (true, true, true, true, false, true, true, false)), (String ((Ascii
    (false, true, true, true, false, true, true, false)),
    EmptyString)))))))))))))))))))),
    (Deser.con3_ (fun x x0 x1 -> { proj_ind = x; proj_npars = x0; proj_arg =
      x1 }) coq_Deserialize_inductive
      (coq_Deserialize_SemiIntegral coq_SemiIntegral_nat)
      (coq_Deserialize_SemiIntegral coq_SemiIntegral_nat))) :: []) l e

(** val coq_Deserialize_name : name coq_Deserialize **)

let coq_Deserialize_name l e =
  Deser.match_con (String ((Ascii (false, true, true, true, false, true,
    true, false)), (String ((Ascii (true, false, false, false, false, true,
    true, false)), (String ((Ascii (true, false, true, true, false, true,
    true, false)), (String ((Ascii (true, false, true, false, false, true,
    true, false)), EmptyString)))))))) (((String ((Ascii (false, true, true,
    true, false, true, true, false)), (String ((Ascii (true, false, false,
    false, false, false, true, false)), (String ((Ascii (false, true, true,
    true, false, true, true, false)), (String ((Ascii (true, true, true,
    true, false, true, true, false)), (String ((Ascii (false, true, true,
    true, false, true, true, false)), EmptyString)))))))))),
    Coq_nAnon) :: []) (((String ((Ascii (false, true, true, true, false,
    true, true, false)), (String ((Ascii (false, true, true, true, false,
    false, true, false)), (String ((Ascii (true, false, false, false, false,
    true, true, false)), (String ((Ascii (true, false, true, true, false,
    true, true, false)), (String ((Ascii (true, false, true, false, false,
    true, true, false)), (String ((Ascii (false, false, true, false, false,
    true, true, false)), EmptyString)))))))))))),
    (Deser.con1_ (fun x -> Coq_nNamed x) coq_Deserialize_ident)) :: []) l e

(** val coq_Deserialize_recursivity_kind :
    recursivity_kind coq_Deserialize **)

let coq_Deserialize_recursivity_kind l e =
  Deser.match_con (String ((Ascii (false, true, false, false, true, true,
    true, false)), (String ((Ascii (true, false, true, false, false, true,
    true, false)), (String ((Ascii (true, true, false, false, false, true,
    true, false)), (String ((Ascii (true, false, true, false, true, true,
    true, false)), (String ((Ascii (false, true, false, false, true, true,
    true, false)), (String ((Ascii (true, true, false, false, true, true,
    true, false)), (String ((Ascii (true, false, false, true, false, true,
    true, false)), (String ((Ascii (false, true, true, false, true, true,
    true, false)), (String ((Ascii (true, false, false, true, false, true,
    true, false)), (String ((Ascii (false, false, true, false, true, true,
    true, false)), (String ((Ascii (true, false, false, true, true, true,
    true, false)), (String ((Ascii (true, true, true, true, true, false,
    true, false)), (String ((Ascii (true, true, false, true, false, true,
    true, false)), (String ((Ascii (true, false, false, true, false, true,
    true, false)), (String ((Ascii (false, true, true, true, false, true,
    true, false)), (String ((Ascii (false, false, true, false, false, true,
    true, false)), EmptyString)))))))))))))))))))))))))))))))) (((String
    ((Ascii (false, true, true, false, false, false, true, false)), (String
    ((Ascii (true, false, false, true, false, true, true, false)), (String
    ((Ascii (false, true, true, true, false, true, true, false)), (String
    ((Ascii (true, false, false, true, false, true, true, false)), (String
    ((Ascii (false, false, true, false, true, true, true, false)), (String
    ((Ascii (true, false, true, false, false, true, true, false)),
    EmptyString)))))))))))), Finite) :: (((String ((Ascii (true, true, false,
    false, false, false, true, false)), (String ((Ascii (true, true, true,
    true, false, true, true, false)), (String ((Ascii (false, true, true,
    false, false, false, true, false)), (String ((Ascii (true, false, false,
    true, false, true, true, false)), (String ((Ascii (false, true, true,
    true, false, true, true, false)), (String ((Ascii (true, false, false,
    true, false, true, true, false)), (String ((Ascii (false, false, true,
    false, true, true, true, false)), (String ((Ascii (true, false, true,
    false, false, true, true, false)), EmptyString)))))))))))))))),
    CoFinite) :: (((String ((Ascii (false, true, false, false, false, false,
    true, false)), (String ((Ascii (true, false, false, true, false, true,
    true, false)), (String ((Ascii (false, true, true, false, false, false,
    true, false)), (String ((Ascii (true, false, false, true, false, true,
    true, false)), (String ((Ascii (false, true, true, true, false, true,
    true, false)), (String ((Ascii (true, false, false, true, false, true,
    true, false)), (String ((Ascii (false, false, true, false, true, true,
    true, false)), (String ((Ascii (true, false, true, false, false, true,
    true, false)), EmptyString)))))))))))))))), BiFinite) :: []))) [] l e

(** val coq_Deserialize_allowed_eliminations :
    allowed_eliminations coq_Deserialize **)

let coq_Deserialize_allowed_eliminations l e =
  Deser.match_con (String ((Ascii (true, false, false, false, false, true,
    true, false)), (String ((Ascii (false, false, true, true, false, true,
    true, false)), (String ((Ascii (false, false, true, true, false, true,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), (String ((Ascii (true, true, true, false, true, true,
    true, false)), (String ((Ascii (true, false, true, false, false, true,
    true, false)), (String ((Ascii (false, false, true, false, false, true,
    true, false)), (String ((Ascii (true, true, true, true, true, false,
    true, false)), (String ((Ascii (true, false, true, false, false, true,
    true, false)), (String ((Ascii (false, false, true, true, false, true,
    true, false)), (String ((Ascii (true, false, false, true, false, true,
    true, false)), (String ((Ascii (true, false, true, true, false, true,
    true, false)), (String ((Ascii (true, false, false, true, false, true,
    true, false)), (String ((Ascii (false, true, true, true, false, true,
    true, false)), (String ((Ascii (true, false, false, false, false, true,
    true, false)), (String ((Ascii (false, false, true, false, true, true,
    true, false)), (String ((Ascii (true, false, false, true, false, true,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), (String ((Ascii (false, true, true, true, false, true,
    true, false)), (String ((Ascii (true, true, false, false, true, true,
    true, false)), EmptyString))))))))))))))))))))))))))))))))))))))))
    (((String ((Ascii (true, false, false, true, false, false, true, false)),
    (String ((Ascii (false, true, true, true, false, true, true, false)),
    (String ((Ascii (false, false, true, false, true, true, true, false)),
    (String ((Ascii (true, true, true, true, false, true, true, false)),
    (String ((Ascii (true, true, false, false, true, false, true, false)),
    (String ((Ascii (false, false, false, false, true, false, true, false)),
    (String ((Ascii (false, true, false, false, true, true, true, false)),
    (String ((Ascii (true, true, true, true, false, true, true, false)),
    (String ((Ascii (false, false, false, false, true, true, true, false)),
    EmptyString)))))))))))))))))), IntoSProp) :: (((String ((Ascii (true,
    false, false, true, false, false, true, false)), (String ((Ascii (false,
    true, true, true, false, true, true, false)), (String ((Ascii (false,
    false, true, false, true, true, true, false)), (String ((Ascii (true,
    true, true, true, false, true, true, false)), (String ((Ascii (false,
    false, false, false, true, false, true, false)), (String ((Ascii (false,
    true, false, false, true, true, true, false)), (String ((Ascii (true,
    true, true, true, false, true, true, false)), (String ((Ascii (false,
    false, false, false, true, true, true, false)), (String ((Ascii (true,
    true, false, false, true, false, true, false)), (String ((Ascii (false,
    false, false, false, true, false, true, false)), (String ((Ascii (false,
    true, false, false, true, true, true, false)), (String ((Ascii (true,
    true, true, true, false, true, true, false)), (String ((Ascii (false,
    false, false, false, true, true, true, false)),
    EmptyString)))))))))))))))))))))))))), IntoPropSProp) :: (((String
    ((Ascii (true, false, false, true, false, false, true, false)), (String
    ((Ascii (false, true, true, true, false, true, true, false)), (String
    ((Ascii (false, false, true, false, true, true, true, false)), (String
    ((Ascii (true, true, true, true, false, true, true, false)), (String
    ((Ascii (true, true, false, false, true, false, true, false)), (String
    ((Ascii (true, false, true, false, false, true, true, false)), (String
    ((Ascii (false, false, true, false, true, true, true, false)), (String
    ((Ascii (false, false, false, false, true, false, true, false)), (String
    ((Ascii (false, true, false, false, true, true, true, false)), (String
    ((Ascii (true, true, true, true, false, true, true, false)), (String
    ((Ascii (false, false, false, false, true, true, true, false)), (String
    ((Ascii (true, true, false, false, true, false, true, false)), (String
    ((Ascii (false, false, false, false, true, false, true, false)), (String
    ((Ascii (false, true, false, false, true, true, true, false)), (String
    ((Ascii (true, true, true, true, false, true, true, false)), (String
    ((Ascii (false, false, false, false, true, true, true, false)),
    EmptyString)))))))))))))))))))))))))))))))),
    IntoSetPropSProp) :: (((String ((Ascii (true, false, false, true, false,
    false, true, false)), (String ((Ascii (false, true, true, true, false,
    true, true, false)), (String ((Ascii (false, false, true, false, true,
    true, true, false)), (String ((Ascii (true, true, true, true, false,
    true, true, false)), (String ((Ascii (true, false, false, false, false,
    false, true, false)), (String ((Ascii (false, true, true, true, false,
    true, true, false)), (String ((Ascii (true, false, false, true, true,
    true, true, false)), EmptyString)))))))))))))), IntoAny) :: [])))) [] l e

(** val kername_of_string : string -> (error, kername) sum **)

let kername_of_string s =
  from_string coq_Deserialize_kername s
