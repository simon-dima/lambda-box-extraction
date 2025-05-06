open BasicAst
open BinNums
open BinPos
open Byte
open Datatypes
open Monad0
open Nat0
open Bytestring
open Cps
open Map_util

type __ = Obj.t

(** val var_dec : positive -> positive -> bool **)

let var_dec =
  M.elt_eq

type name_env = name M.t

(** val add_entry : name_env -> var -> var -> String.t -> name_env **)

let add_entry nenv x x_origin suff =
  match M.get x_origin nenv with
  | Some n ->
    (match n with
     | Coq_nAnon ->
       M.set x (Coq_nNamed
         (String.append (String.String (Coq_x61, (String.String (Coq_x6e,
           (String.String (Coq_x6f, (String.String (Coq_x6e,
           String.EmptyString)))))))) suff)) nenv
     | Coq_nNamed s -> M.set x (Coq_nNamed (String.append s suff)) nenv)
  | None ->
    M.set x (Coq_nNamed
      (String.append (String.String (Coq_x61, (String.String (Coq_x6e,
        (String.String (Coq_x6f, (String.String (Coq_x6e,
        String.EmptyString)))))))) suff)) nenv

(** val add_entry_str : name_env -> var -> String.t -> name_env **)

let add_entry_str nenv x name0 =
  M.set x (Coq_nNamed name0) nenv

(** val caseConsistent_f :
    ctor_env -> (ctor_tag * exp) list -> ctor_tag -> bool **)

let rec caseConsistent_f cenv l t0 =
  match l with
  | [] -> true
  | p :: l' ->
    let (t', _) = p in
    (&&) (caseConsistent_f cenv l' t0)
      (match M.get t0 cenv with
       | Some info ->
         (match M.get t' cenv with
          | Some info' -> Pos.eqb info.ctor_ind_tag info'.ctor_ind_tag
          | None -> false)
       | None -> false)

(** val numOf_fundefs : fundefs -> nat **)

let rec numOf_fundefs = function
| Fcons (_, _, _, _, b0) -> add (S O) (numOf_fundefs b0)
| Fnil -> O

(** val coq_OptMonad : __ option coq_Monad **)

let coq_OptMonad =
  { ret = (fun _ x -> Some x); bind = (fun _ _ x f ->
    match x with
    | Some a -> f a
    | None -> None) }
