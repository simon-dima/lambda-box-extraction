open All_Forall
open Ascii
open BasicAst
open Byte
open CRelationClasses
open Classes1
open Datatypes
open EAst
open EPrimitive
open Erasure
open ErasureFunction
open ExAst
open Kernames
open MCProd
open Optimize
open PCUICAst
open PCUICAstUtils
open PCUICWfEnv
open PCUICWfEnvImpl
open Primitive
open ResultMonad
open Signature
open Specif
open String0
open Universes0
open Utils
open Bytestring
open Config0
open Monad_utils

type __ = Obj.t

module E :
 sig
  type 'term def = 'term EAst.def = { dname : name; dbody : 'term; rarg : nat }

  val dname : 'a1 def -> name

  val dbody : 'a1 def -> 'a1

  val rarg : 'a1 def -> nat

  val coq_NoConfusionPackage_def : 'a1 def coq_NoConfusionPackage

  val map_def : ('a1 -> 'a1) -> 'a1 def -> 'a1 def

  val test_def : ('a1 -> bool) -> 'a1 def -> bool

  type 'term mfixpoint = 'term def list

  type term = EAst.term =
  | Coq_tBox
  | Coq_tRel of nat
  | Coq_tVar of ident
  | Coq_tEvar of nat * term list
  | Coq_tLambda of name * term
  | Coq_tLetIn of name * term * term
  | Coq_tApp of term * term
  | Coq_tConst of kername
  | Coq_tConstruct of inductive * nat * term list
  | Coq_tCase of (inductive * nat) * term * (name list * term) list
  | Coq_tProj of projection * term
  | Coq_tFix of term mfixpoint * nat
  | Coq_tCoFix of term mfixpoint * nat
  | Coq_tPrim of term prim_val
  | Coq_tLazy of term
  | Coq_tForce of term

  val term_rect :
    'a1 -> (nat -> 'a1) -> (ident -> 'a1) -> (nat -> term list -> 'a1) ->
    (name -> term -> 'a1 -> 'a1) -> (name -> term -> 'a1 -> term -> 'a1 ->
    'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) -> (kername -> 'a1) ->
    (inductive -> nat -> term list -> 'a1) -> ((inductive * nat) -> term ->
    'a1 -> (name list * term) list -> 'a1) -> (projection -> term -> 'a1 ->
    'a1) -> (term mfixpoint -> nat -> 'a1) -> (term mfixpoint -> nat -> 'a1)
    -> (term prim_val -> 'a1) -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> 'a1)
    -> term -> 'a1

  val term_rec :
    'a1 -> (nat -> 'a1) -> (ident -> 'a1) -> (nat -> term list -> 'a1) ->
    (name -> term -> 'a1 -> 'a1) -> (name -> term -> 'a1 -> term -> 'a1 ->
    'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) -> (kername -> 'a1) ->
    (inductive -> nat -> term list -> 'a1) -> ((inductive * nat) -> term ->
    'a1 -> (name list * term) list -> 'a1) -> (projection -> term -> 'a1 ->
    'a1) -> (term mfixpoint -> nat -> 'a1) -> (term mfixpoint -> nat -> 'a1)
    -> (term prim_val -> 'a1) -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> 'a1)
    -> term -> 'a1

  val coq_NoConfusionPackage_term : term coq_NoConfusionPackage

  val mkApps : term -> term list -> term

  val mkApp : term -> term -> term

  val isApp : term -> bool

  val isLambda : term -> bool

  type definition_entry = EAst.definition_entry = { definition_entry_body : 
                                                    term;
                                                    definition_entry_opaque : 
                                                    bool }

  val definition_entry_body : definition_entry -> term

  val definition_entry_opaque : definition_entry -> bool

  type constant_entry = EAst.constant_entry =
  | ParameterEntry
  | DefinitionEntry of definition_entry

  val constant_entry_rect :
    (__ -> 'a1) -> (definition_entry -> 'a1) -> constant_entry -> 'a1

  val constant_entry_rec :
    (__ -> 'a1) -> (definition_entry -> 'a1) -> constant_entry -> 'a1

  type local_entry = EAst.local_entry =
  | LocalDef of term
  | LocalAssum of term

  val local_entry_rect : (term -> 'a1) -> (term -> 'a1) -> local_entry -> 'a1

  val local_entry_rec : (term -> 'a1) -> (term -> 'a1) -> local_entry -> 'a1

  type one_inductive_entry = EAst.one_inductive_entry = { mind_entry_typename : 
                                                          ident;
                                                          mind_entry_arity : 
                                                          term;
                                                          mind_entry_template : 
                                                          bool;
                                                          mind_entry_consnames : 
                                                          ident list;
                                                          mind_entry_lc : 
                                                          term list }

  val mind_entry_typename : one_inductive_entry -> ident

  val mind_entry_arity : one_inductive_entry -> term

  val mind_entry_template : one_inductive_entry -> bool

  val mind_entry_consnames : one_inductive_entry -> ident list

  val mind_entry_lc : one_inductive_entry -> term list

  type mutual_inductive_entry = EAst.mutual_inductive_entry = { mind_entry_record : 
                                                                ident option
                                                                option;
                                                                mind_entry_finite : 
                                                                recursivity_kind;
                                                                mind_entry_params : 
                                                                (ident * local_entry)
                                                                list;
                                                                mind_entry_inds : 
                                                                one_inductive_entry
                                                                list;
                                                                mind_entry_private : 
                                                                bool option }

  val mind_entry_record : mutual_inductive_entry -> ident option option

  val mind_entry_finite : mutual_inductive_entry -> recursivity_kind

  val mind_entry_params : mutual_inductive_entry -> (ident * local_entry) list

  val mind_entry_inds : mutual_inductive_entry -> one_inductive_entry list

  val mind_entry_private : mutual_inductive_entry -> bool option

  type context_decl = EAst.context_decl = { decl_name : name;
                                            decl_body : term option }

  val decl_name : context_decl -> name

  val decl_body : context_decl -> term option

  val vass : name -> context_decl

  val vdef : name -> term -> context_decl

  type context = context_decl list

  val map_decl : (term -> term) -> context_decl -> context_decl

  val map_context : (term -> term) -> context_decl list -> context_decl list

  val snoc : 'a1 list -> 'a1 -> 'a1 list

  type constructor_body = EAst.constructor_body = { cstr_name : ident;
                                                    cstr_nargs : nat }

  val cstr_name : constructor_body -> ident

  val cstr_nargs : constructor_body -> nat

  val coq_NoConfusionPackage_constructor_body :
    constructor_body coq_NoConfusionPackage

  type projection_body =
    ident
    (* singleton inductive, whose constructor was mkProjection *)

  val proj_name : projection_body -> ident

  val coq_NoConfusionPackage_projection_body :
    projection_body coq_NoConfusionPackage

  type one_inductive_body = EAst.one_inductive_body = { ind_name : ident;
                                                        ind_propositional : 
                                                        bool;
                                                        ind_kelim : allowed_eliminations;
                                                        ind_ctors : constructor_body
                                                                    list;
                                                        ind_projs : projection_body
                                                                    list }

  val ind_name : one_inductive_body -> ident

  val ind_propositional : one_inductive_body -> bool

  val ind_kelim : one_inductive_body -> allowed_eliminations

  val ind_ctors : one_inductive_body -> constructor_body list

  val ind_projs : one_inductive_body -> projection_body list

  val coq_NoConfusionPackage_one_inductive_body :
    one_inductive_body coq_NoConfusionPackage

  type mutual_inductive_body = EAst.mutual_inductive_body = { ind_finite : 
                                                              recursivity_kind;
                                                              ind_npars : 
                                                              nat;
                                                              ind_bodies : 
                                                              one_inductive_body
                                                              list }

  val ind_finite : mutual_inductive_body -> recursivity_kind

  val ind_npars : mutual_inductive_body -> nat

  val ind_bodies : mutual_inductive_body -> one_inductive_body list

  val coq_NoConfusionPackage_mutual_inductive_body :
    mutual_inductive_body coq_NoConfusionPackage

  val cstr_arity : mutual_inductive_body -> constructor_body -> nat

  type constant_body =
    term option
    (* singleton inductive, whose constructor was Build_constant_body *)

  val cst_body : constant_body -> term option

  type global_decl = EAst.global_decl =
  | ConstantDecl of constant_body
  | InductiveDecl of mutual_inductive_body

  val global_decl_rect :
    (constant_body -> 'a1) -> (mutual_inductive_body -> 'a1) -> global_decl
    -> 'a1

  val global_decl_rec :
    (constant_body -> 'a1) -> (mutual_inductive_body -> 'a1) -> global_decl
    -> 'a1

  val coq_NoConfusionPackage_global_decl : global_decl coq_NoConfusionPackage

  type global_declarations = (kername * global_decl) list

  type program = global_declarations * term
 end

module PEnv :
 sig
  type judgment = (Sort.t, term) judgment_

  val vass : aname -> term -> term BasicAst.context_decl

  val vdef : aname -> term -> term -> term BasicAst.context_decl

  type context = term BasicAst.context_decl list

  val lift_decl :
    nat -> nat -> term BasicAst.context_decl -> term BasicAst.context_decl

  val lift_context : nat -> nat -> context -> context

  val subst_context : term list -> nat -> context -> context

  val subst_decl :
    term list -> nat -> term BasicAst.context_decl -> term
    BasicAst.context_decl

  val subst_telescope : term list -> nat -> context -> context

  val subst_instance_decl : term BasicAst.context_decl coq_UnivSubst

  val subst_instance_context : context coq_UnivSubst

  val set_binder_name :
    aname -> term BasicAst.context_decl -> term BasicAst.context_decl

  val context_assumptions : context -> nat

  val is_assumption_context : context -> bool

  val smash_context : context -> context -> context

  val extended_subst : context -> nat -> term list

  val expand_lets_k : context -> nat -> term -> term

  val expand_lets : context -> term -> term

  val expand_lets_k_ctx : context -> nat -> context -> context

  val expand_lets_ctx : context -> context -> context

  val fix_context : term BasicAst.mfixpoint -> context

  type constructor_body = PCUICEnvironment.constructor_body = { cstr_name : 
                                                                ident;
                                                                cstr_args : 
                                                                context;
                                                                cstr_indices : 
                                                                term list;
                                                                cstr_type : 
                                                                term;
                                                                cstr_arity : 
                                                                nat }

  val cstr_name : constructor_body -> ident

  val cstr_args : constructor_body -> context

  val cstr_indices : constructor_body -> term list

  val cstr_type : constructor_body -> term

  val cstr_arity : constructor_body -> nat

  type projection_body = PCUICEnvironment.projection_body = { proj_name : 
                                                              ident;
                                                              proj_relevance : 
                                                              relevance;
                                                              proj_type : 
                                                              term }

  val proj_name : projection_body -> ident

  val proj_relevance : projection_body -> relevance

  val proj_type : projection_body -> term

  val map_constructor_body :
    nat -> nat -> (nat -> term -> term) -> constructor_body ->
    constructor_body

  val map_projection_body :
    nat -> (nat -> term -> term) -> projection_body -> projection_body

  type one_inductive_body = PCUICEnvironment.one_inductive_body = { ind_name : 
                                                                    ident;
                                                                    ind_indices : 
                                                                    context;
                                                                    ind_sort : 
                                                                    Sort.t;
                                                                    ind_type : 
                                                                    term;
                                                                    ind_kelim : 
                                                                    allowed_eliminations;
                                                                    ind_ctors : 
                                                                    constructor_body
                                                                    list;
                                                                    ind_projs : 
                                                                    projection_body
                                                                    list;
                                                                    ind_relevance : 
                                                                    relevance }

  val ind_name : one_inductive_body -> ident

  val ind_indices : one_inductive_body -> context

  val ind_sort : one_inductive_body -> Sort.t

  val ind_type : one_inductive_body -> term

  val ind_kelim : one_inductive_body -> allowed_eliminations

  val ind_ctors : one_inductive_body -> constructor_body list

  val ind_projs : one_inductive_body -> projection_body list

  val ind_relevance : one_inductive_body -> relevance

  val map_one_inductive_body :
    nat -> nat -> (nat -> term -> term) -> one_inductive_body ->
    one_inductive_body

  type mutual_inductive_body = PCUICEnvironment.mutual_inductive_body = { 
  ind_finite : recursivity_kind; ind_npars : nat; ind_params : context;
  ind_bodies : one_inductive_body list; ind_universes : universes_decl;
  ind_variance : Variance.t list option }

  val ind_finite : mutual_inductive_body -> recursivity_kind

  val ind_npars : mutual_inductive_body -> nat

  val ind_params : mutual_inductive_body -> context

  val ind_bodies : mutual_inductive_body -> one_inductive_body list

  val ind_universes : mutual_inductive_body -> universes_decl

  val ind_variance : mutual_inductive_body -> Variance.t list option

  type constant_body = PCUICEnvironment.constant_body = { cst_type : 
                                                          term;
                                                          cst_body : 
                                                          term option;
                                                          cst_universes : 
                                                          universes_decl;
                                                          cst_relevance : 
                                                          relevance }

  val cst_type : constant_body -> term

  val cst_body : constant_body -> term option

  val cst_universes : constant_body -> universes_decl

  val cst_relevance : constant_body -> relevance

  val map_constant_body : (term -> term) -> constant_body -> constant_body

  type global_decl = PCUICEnvironment.global_decl =
  | ConstantDecl of constant_body
  | InductiveDecl of mutual_inductive_body

  val global_decl_rect :
    (constant_body -> 'a1) -> (mutual_inductive_body -> 'a1) -> global_decl
    -> 'a1

  val global_decl_rec :
    (constant_body -> 'a1) -> (mutual_inductive_body -> 'a1) -> global_decl
    -> 'a1

  val coq_NoConfusionPackage_global_decl : global_decl coq_NoConfusionPackage

  type global_declarations = (kername * global_decl) list

  type global_env = PCUICEnvironment.global_env = { universes : ContextSet.t;
                                                    declarations : global_declarations;
                                                    retroknowledge : 
                                                    Environment.Retroknowledge.t }

  val universes : global_env -> ContextSet.t

  val declarations : global_env -> global_declarations

  val retroknowledge : global_env -> Environment.Retroknowledge.t

  val empty_global_env : global_env

  val add_global_decl : global_env -> (kername * global_decl) -> global_env

  val set_declarations : global_env -> global_declarations -> global_env

  val lookup_global : global_declarations -> kername -> global_decl option

  val lookup_env : global_env -> kername -> global_decl option

  val lookup_globals : global_declarations -> kername -> global_decl list

  val lookup_envs : global_env -> kername -> global_decl list

  type extends = (__, kername -> (global_decl list, __) sigT, __) and3

  type extends_decls = (__, kername -> (global_decl list, __) sigT, __) and3

  type extends_strictly_on_decls =
    (__, ((kername * global_decl) list, __) sigT, __) and3

  type strictly_extends_decls =
    (__, ((kername * global_decl) list, __) sigT, __) and3

  val strictly_extends_decls_extends_part_globals :
    (kername * global_decl) list -> (kername * global_decl) list ->
    ((kername * global_decl) list, __) sigT -> kername -> (global_decl list,
    __) sigT

  val strictly_extends_decls_extends_part :
    global_env -> global_env -> ((kername * global_decl) list, __) sigT ->
    kername -> (global_decl list, __) sigT

  val strictly_extends_decls_extends_decls :
    global_env -> global_env -> strictly_extends_decls -> extends_decls

  val strictly_extends_decls_extends_strictly_on_decls :
    global_env -> global_env -> strictly_extends_decls ->
    extends_strictly_on_decls

  val extends_decls_extends :
    global_env -> global_env -> extends_decls -> extends

  val extends_strictly_on_decls_extends :
    global_env -> global_env -> extends_strictly_on_decls -> extends

  val strictly_extends_decls_extends_decls_subrel :
    (global_env, strictly_extends_decls, extends_decls) subrelation

  val strictly_extends_decls_extends_strictly_on_decls_subrel :
    (global_env, strictly_extends_decls, extends_strictly_on_decls)
    subrelation

  val extends_decls_extends_subrel :
    (global_env, extends_decls, extends) subrelation

  val extends_strictly_on_decls_extends_subrel :
    (global_env, extends_strictly_on_decls, extends) subrelation

  val strictly_extends_decls_extends_subrel :
    (global_env, strictly_extends_decls, extends) subrelation

  val strictly_extends_decls_refl :
    (global_env, strictly_extends_decls) coq_Reflexive

  val extends_decls_refl : (global_env, extends_decls) coq_Reflexive

  val extends_strictly_on_decls_refl :
    (global_env, extends_strictly_on_decls) coq_Reflexive

  val extends_refl : (global_env, extends) coq_Reflexive

  val extends_decls_part_globals_refl :
    global_declarations -> kername -> (global_decl list, __) sigT

  val extends_decls_part_refl :
    global_env -> kername -> (global_decl list, __) sigT

  val strictly_extends_decls_part_globals_refl :
    global_declarations -> ((kername * global_decl) list, __) sigT

  val strictly_extends_decls_part_refl :
    global_env -> ((kername * global_decl) list, __) sigT

  val extends_decls_part_globals_trans :
    global_declarations -> global_declarations -> global_declarations ->
    (kername -> (global_decl list, __) sigT) -> (kername -> (global_decl
    list, __) sigT) -> kername -> (global_decl list, __) sigT

  val extends_decls_part_trans :
    global_env -> global_env -> global_env -> (kername -> (global_decl list,
    __) sigT) -> (kername -> (global_decl list, __) sigT) -> kername ->
    (global_decl list, __) sigT

  val strictly_extends_decls_part_globals_trans :
    global_declarations -> global_declarations -> global_declarations ->
    ((kername * global_decl) list, __) sigT -> ((kername * global_decl) list,
    __) sigT -> ((kername * global_decl) list, __) sigT

  val strictly_extends_decls_part_trans :
    global_env -> global_env -> global_env -> ((kername * global_decl) list,
    __) sigT -> ((kername * global_decl) list, __) sigT ->
    ((kername * global_decl) list, __) sigT

  val strictly_extends_decls_trans :
    (global_env, strictly_extends_decls) coq_Transitive

  val extends_decls_trans : (global_env, extends_decls) coq_Transitive

  val extends_strictly_on_decls_trans :
    (global_env, extends_strictly_on_decls) coq_Transitive

  val extends_trans : (global_env, extends) coq_Transitive

  val declared_kername_set : global_declarations -> KernameSet.t

  val merge_globals :
    global_declarations -> global_declarations -> global_declarations

  val merge_global_envs : global_env -> global_env -> global_env

  val strictly_extends_decls_l_merge_globals :
    global_declarations -> global_declarations -> ((kername * global_decl)
    list, __) sigT

  val extends_l_merge_globals :
    global_declarations -> global_declarations -> kername -> (global_decl
    list, __) sigT

  val extends_strictly_on_decls_l_merge :
    global_env -> global_env -> extends_strictly_on_decls

  val extends_l_merge : global_env -> global_env -> extends

  val extends_r_merge_globals :
    global_declarations -> global_declarations -> kername -> (global_decl
    list, __) sigT

  val extends_r_merge : global_env -> global_env -> extends

  val primitive_constant : global_env -> prim_tag -> kername option

  val tImpl : term -> term -> term

  val array_uctx : name list * ConstraintSet.t

  type global_env_ext = global_env * universes_decl

  val fst_ctx : global_env_ext -> global_env

  val empty_ext : global_env -> global_env_ext

  type program = global_env * term

  val mkLambda_or_LetIn : term BasicAst.context_decl -> term -> term

  val it_mkLambda_or_LetIn : context -> term -> term

  val mkProd_or_LetIn : term BasicAst.context_decl -> term -> term

  val it_mkProd_or_LetIn : context -> term -> term

  val reln : term list -> nat -> term BasicAst.context_decl list -> term list

  val to_extended_list_k : term BasicAst.context_decl list -> nat -> term list

  val to_extended_list : term BasicAst.context_decl list -> term list

  val reln_alt : nat -> context -> term list

  val arities_context :
    one_inductive_body list -> term BasicAst.context_decl list

  val map_mutual_inductive_body :
    (nat -> term -> term) -> mutual_inductive_body -> mutual_inductive_body

  val projs : inductive -> nat -> nat -> term list

  type 'p coq_All_decls = 'p PCUICEnvironment.coq_All_decls =
  | Coq_on_vass of aname * term * term * 'p
  | Coq_on_vdef of aname * term * term * term * term * 'p * 'p

  val coq_All_decls_rect :
    (aname -> term -> term -> 'a1 -> 'a2) -> (aname -> term -> term -> term
    -> term -> 'a1 -> 'a1 -> 'a2) -> term BasicAst.context_decl -> term
    BasicAst.context_decl -> 'a1 coq_All_decls -> 'a2

  val coq_All_decls_rec :
    (aname -> term -> term -> 'a1 -> 'a2) -> (aname -> term -> term -> term
    -> term -> 'a1 -> 'a1 -> 'a2) -> term BasicAst.context_decl -> term
    BasicAst.context_decl -> 'a1 coq_All_decls -> 'a2

  type 'p coq_All_decls_sig = 'p coq_All_decls

  val coq_All_decls_sig_pack :
    term BasicAst.context_decl -> term BasicAst.context_decl -> 'a1
    coq_All_decls -> (term BasicAst.context_decl * term
    BasicAst.context_decl) * 'a1 coq_All_decls

  val coq_All_decls_Signature :
    term BasicAst.context_decl -> term BasicAst.context_decl -> ('a1
    coq_All_decls, term BasicAst.context_decl * term BasicAst.context_decl,
    'a1 coq_All_decls_sig) coq_Signature

  val coq_NoConfusionPackage_All_decls :
    ((term BasicAst.context_decl * term BasicAst.context_decl) * 'a1
    coq_All_decls) coq_NoConfusionPackage

  type 'p coq_All_decls_alpha = 'p PCUICEnvironment.coq_All_decls_alpha =
  | Coq_on_vass_alpha of name binder_annot * name binder_annot * term * 
     term * 'p
  | Coq_on_vdef_alpha of name binder_annot * name binder_annot * term * 
     term * term * term * 'p * 'p

  val coq_All_decls_alpha_rect :
    (name binder_annot -> name binder_annot -> term -> term -> __ -> 'a1 ->
    'a2) -> (name binder_annot -> name binder_annot -> term -> term -> term
    -> term -> __ -> 'a1 -> 'a1 -> 'a2) -> term BasicAst.context_decl -> term
    BasicAst.context_decl -> 'a1 coq_All_decls_alpha -> 'a2

  val coq_All_decls_alpha_rec :
    (name binder_annot -> name binder_annot -> term -> term -> __ -> 'a1 ->
    'a2) -> (name binder_annot -> name binder_annot -> term -> term -> term
    -> term -> __ -> 'a1 -> 'a1 -> 'a2) -> term BasicAst.context_decl -> term
    BasicAst.context_decl -> 'a1 coq_All_decls_alpha -> 'a2

  type 'p coq_All_decls_alpha_sig = 'p coq_All_decls_alpha

  val coq_All_decls_alpha_sig_pack :
    term BasicAst.context_decl -> term BasicAst.context_decl -> 'a1
    coq_All_decls_alpha -> (term BasicAst.context_decl * term
    BasicAst.context_decl) * 'a1 coq_All_decls_alpha

  val coq_All_decls_alpha_Signature :
    term BasicAst.context_decl -> term BasicAst.context_decl -> ('a1
    coq_All_decls_alpha, term BasicAst.context_decl * term
    BasicAst.context_decl, 'a1 coq_All_decls_alpha_sig) coq_Signature

  val coq_NoConfusionPackage_All_decls_alpha :
    ((term BasicAst.context_decl * term BasicAst.context_decl) * 'a1
    coq_All_decls_alpha) coq_NoConfusionPackage

  val coq_All_decls_impl :
    term BasicAst.context_decl -> term BasicAst.context_decl -> 'a1
    coq_All_decls -> (term -> term -> 'a1 -> 'a2) -> 'a2 coq_All_decls

  val coq_All_decls_alpha_impl :
    term BasicAst.context_decl -> term BasicAst.context_decl -> 'a1
    coq_All_decls_alpha -> (term -> term -> 'a1 -> 'a2) -> 'a2
    coq_All_decls_alpha

  val coq_All_decls_to_alpha :
    term BasicAst.context_decl -> term BasicAst.context_decl -> 'a1
    coq_All_decls -> 'a1 coq_All_decls_alpha

  type 'p coq_All2_fold_over =
    (term BasicAst.context_decl, (term BasicAst.context_decl, term
    BasicAst.context_decl, 'p) coq_All_over) coq_All2_fold
 end

val compute_masks :
  (kername -> bitmask option) -> bool -> bool -> global_env -> (dearg_set,
  String.t) result

val make_env : PCUICEnvironment.global_env_ext -> __

val erase_term : PCUICEnvironment.global_env_ext -> term -> EAst.term
