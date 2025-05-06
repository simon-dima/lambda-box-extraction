open Datatypes
open EAst
open EGlobalEnv
open EPrimitive
open Kernames
open List0
open Nat0
open PeanoNat
open Primitive
open ReflectEq
open Specif

(** val isSome : 'a1 option -> bool **)

let isSome = function
| Some _ -> true
| None -> false

type coq_EPrimitiveFlags = { has_primint : bool; has_primfloat : bool;
                             has_primarray : bool }

(** val has_prim : coq_EPrimitiveFlags -> term prim_val -> bool **)

let has_prim epfl p =
  match projT1 p with
  | Coq_primInt -> epfl.has_primint
  | Coq_primFloat -> epfl.has_primfloat
  | Coq_primArray -> epfl.has_primarray

type coq_ETermFlags = { has_tBox : bool; has_tRel : bool; has_tVar : 
                        bool; has_tEvar : bool; has_tLambda : bool;
                        has_tLetIn : bool; has_tApp : bool;
                        has_tConst : bool; has_tConstruct : bool;
                        has_tCase : bool; has_tProj : bool; has_tFix : 
                        bool; has_tCoFix : bool;
                        has_tPrim : coq_EPrimitiveFlags;
                        has_tLazy_Force : bool }

type coq_EEnvFlags = { has_axioms : bool; has_cstr_params : bool;
                       term_switches : coq_ETermFlags; cstr_as_blocks : 
                       bool }

(** val all_primitive_flags : coq_EPrimitiveFlags **)

let all_primitive_flags =
  { has_primint = true; has_primfloat = true; has_primarray = true }

(** val all_term_flags : coq_ETermFlags **)

let all_term_flags =
  { has_tBox = true; has_tRel = true; has_tVar = true; has_tEvar = true;
    has_tLambda = true; has_tLetIn = true; has_tApp = true; has_tConst =
    true; has_tConstruct = true; has_tCase = true; has_tProj = true;
    has_tFix = true; has_tCoFix = true; has_tPrim = all_primitive_flags;
    has_tLazy_Force = true }

(** val all_env_flags : coq_EEnvFlags **)

let all_env_flags =
  { has_axioms = true; has_cstr_params = true; term_switches =
    all_term_flags; cstr_as_blocks = false }

(** val wf_fix_gen :
    (nat -> term -> bool) -> nat -> term def list -> nat -> bool **)

let wf_fix_gen wf k mfix idx =
  let k' = add (length mfix) k in
  (&&) (Nat.ltb idx (length mfix)) (forallb (test_def (wf k')) mfix)

(** val is_nil : 'a1 list -> bool **)

let is_nil = function
| [] -> true
| _ :: _ -> false

(** val wf_brs : global_declarations -> inductive -> nat -> bool **)

let wf_brs _UU03a3_ ind brsl =
  match lookup_inductive _UU03a3_ ind with
  | Some p -> let (_, oib) = p in reflect_nat (length oib.ind_ctors) brsl
  | None -> false

(** val wf_projections : one_inductive_body -> bool **)

let wf_projections idecl =
  match idecl.ind_projs with
  | [] -> true
  | _ :: _ ->
    (match idecl.ind_ctors with
     | [] -> false
     | cstr :: l ->
       (match l with
        | [] -> reflect_nat (length idecl.ind_projs) cstr.cstr_nargs
        | _ :: _ -> false))

(** val wf_inductive : one_inductive_body -> bool **)

let wf_inductive =
  wf_projections

(** val wf_minductive : coq_EEnvFlags -> mutual_inductive_body -> bool **)

let wf_minductive efl mdecl =
  (&&) ((||) efl.has_cstr_params (reflect_nat mdecl.ind_npars O))
    (forallb wf_inductive mdecl.ind_bodies)
