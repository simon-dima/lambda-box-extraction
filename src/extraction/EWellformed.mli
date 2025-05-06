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

val isSome : 'a1 option -> bool

type coq_EPrimitiveFlags = { has_primint : bool; has_primfloat : bool;
                             has_primarray : bool }

val has_prim : coq_EPrimitiveFlags -> term prim_val -> bool

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

val all_primitive_flags : coq_EPrimitiveFlags

val all_term_flags : coq_ETermFlags

val all_env_flags : coq_EEnvFlags

val wf_fix_gen : (nat -> term -> bool) -> nat -> term def list -> nat -> bool

val is_nil : 'a1 list -> bool

val wf_brs : global_declarations -> inductive -> nat -> bool

val wf_projections : one_inductive_body -> bool

val wf_inductive : one_inductive_body -> bool

val wf_minductive : coq_EEnvFlags -> mutual_inductive_body -> bool
