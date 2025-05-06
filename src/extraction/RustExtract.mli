open BasicAst
open Basics
open Byte
open Classes1
open Datatypes
open EAst
open EPrimitive
open ExAst
open Extraction
open Kernames
open List0
open MCList
open MCProd
open MCString
open Nat0
open PeanoNat
open PrettyPrinterMonad
open Printing
open ResultMonad
open StringExtra
open Universes0
open Bytestring
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

module Ex :
 sig
  type box_type = ExAst.box_type =
  | TBox
  | TAny
  | TArr of box_type * box_type
  | TApp of box_type * box_type
  | TVar of nat
  | TInd of inductive
  | TConst of kername

  val box_type_rect :
    'a1 -> 'a1 -> (box_type -> 'a1 -> box_type -> 'a1 -> 'a1) -> (box_type ->
    'a1 -> box_type -> 'a1 -> 'a1) -> (nat -> 'a1) -> (inductive -> 'a1) ->
    (kername -> 'a1) -> box_type -> 'a1

  val box_type_rec :
    'a1 -> 'a1 -> (box_type -> 'a1 -> box_type -> 'a1 -> 'a1) -> (box_type ->
    'a1 -> box_type -> 'a1 -> 'a1) -> (nat -> 'a1) -> (inductive -> 'a1) ->
    (kername -> 'a1) -> box_type -> 'a1

  val decompose_arr : box_type -> box_type list * box_type

  val can_have_args : box_type -> bool

  val mkTApps : box_type -> box_type list -> box_type

  type constant_body = ExAst.constant_body = { cst_type : (name
                                                          list * box_type);
                                               cst_body : term option }

  val cst_type : constant_body -> name list * box_type

  val cst_body : constant_body -> term option

  type type_var_info = ExAst.type_var_info = { tvar_name : name;
                                               tvar_is_logical : bool;
                                               tvar_is_arity : bool;
                                               tvar_is_sort : bool }

  val tvar_name : type_var_info -> name

  val tvar_is_logical : type_var_info -> bool

  val tvar_is_arity : type_var_info -> bool

  val tvar_is_sort : type_var_info -> bool

  type one_inductive_body = ExAst.one_inductive_body = { ind_name : ident;
                                                         ind_propositional : 
                                                         bool;
                                                         ind_kelim : 
                                                         allowed_eliminations;
                                                         ind_type_vars : 
                                                         type_var_info list;
                                                         ind_ctors : 
                                                         ((ident * (name * box_type)
                                                         list) * nat) list;
                                                         ind_projs : 
                                                         (ident * box_type)
                                                         list }

  val ind_name : one_inductive_body -> ident

  val ind_propositional : one_inductive_body -> bool

  val ind_kelim : one_inductive_body -> allowed_eliminations

  val ind_type_vars : one_inductive_body -> type_var_info list

  val ind_ctors :
    one_inductive_body -> ((ident * (name * box_type) list) * nat) list

  val ind_projs : one_inductive_body -> (ident * box_type) list

  type mutual_inductive_body = ExAst.mutual_inductive_body = { ind_finite : 
                                                               recursivity_kind;
                                                               ind_npars : 
                                                               nat;
                                                               ind_bodies : 
                                                               one_inductive_body
                                                               list }

  val ind_finite : mutual_inductive_body -> recursivity_kind

  val ind_npars : mutual_inductive_body -> nat

  val ind_bodies : mutual_inductive_body -> one_inductive_body list

  type global_decl = ExAst.global_decl =
  | ConstantDecl of constant_body
  | InductiveDecl of mutual_inductive_body
  | TypeAliasDecl of (type_var_info list * box_type) option

  val global_decl_rect :
    (constant_body -> 'a1) -> (mutual_inductive_body -> 'a1) ->
    ((type_var_info list * box_type) option -> 'a1) -> global_decl -> 'a1

  val global_decl_rec :
    (constant_body -> 'a1) -> (mutual_inductive_body -> 'a1) ->
    ((type_var_info list * box_type) option -> 'a1) -> global_decl -> 'a1

  type global_env = ((kername * bool) * global_decl) list

  val lookup_env : global_env -> kername -> global_decl option

  val lookup_constant : global_env -> kername -> constant_body option

  val lookup_minductive :
    global_env -> kername -> mutual_inductive_body option

  val lookup_inductive : global_env -> inductive -> one_inductive_body option

  val lookup_constructor :
    global_env -> inductive -> nat -> ((ident * (name * box_type)
    list) * nat) option

  val lookup_constructor_full :
    global_env -> inductive -> nat ->
    ((mutual_inductive_body * one_inductive_body) * ((ident * (name * box_type)
    list) * nat)) option

  val trans_cst : constant_body -> EAst.constant_body

  val trans_ctors :
    ((ident * (name * box_type) list) * nat) list -> constructor_body list

  val trans_oib : one_inductive_body -> EAst.one_inductive_body

  val trans_mib : mutual_inductive_body -> EAst.mutual_inductive_body

  val trans_global_decl : global_decl -> EAst.global_decl

  val trans_env : global_env -> global_declarations

  val print_term : global_env -> term -> String.t
 end

val indent_size : nat

type coq_RustPrintConfig = { term_box_symbol : String.t;
                             type_box_symbol : String.t;
                             any_type_symbol : String.t;
                             print_full_names : bool }

type ind_attr_map = inductive -> String.t

val lookup_ind_decl :
  Ex.global_env -> inductive -> (Ex.one_inductive_body, String.t) result

val clean_global_ident : String.t -> String.t

val const_global_ident_of_kername : coq_RustPrintConfig -> kername -> String.t

val ty_const_global_ident_of_kername :
  coq_RustPrintConfig -> kername -> String.t

val get_ty_const_ident : remaps -> coq_RustPrintConfig -> kername -> String.t

val get_ind_ident :
  Ex.global_env -> remaps -> coq_RustPrintConfig -> inductive -> String.t
  coq_PrettyPrinter

val clean_local_ident : ident -> String.t

val print_ind :
  Ex.global_env -> remaps -> coq_RustPrintConfig -> inductive -> unit
  coq_PrettyPrinter

val print_parenthesized :
  bool -> unit coq_PrettyPrinter -> unit coq_PrettyPrinter

val print_parenthesized_with :
  String.t -> String.t -> bool -> unit coq_PrettyPrinter -> unit
  coq_PrettyPrinter

val fresh : ident -> ident list -> ident

val fresh_ident : name -> ident list -> ident coq_PrettyPrinter

val fresh_ty_arg_name : name -> ident list -> ident coq_PrettyPrinter

val print_type_aux :
  Ex.global_env -> remaps -> coq_RustPrintConfig -> ident list -> box_type ->
  unit coq_PrettyPrinter list -> unit coq_PrettyPrinter

val print_type :
  Ex.global_env -> remaps -> coq_RustPrintConfig -> ident list -> box_type ->
  unit coq_PrettyPrinter

val get_num_inline_args : Ex.global_env -> kername -> nat coq_PrettyPrinter

val print_app :
  unit coq_PrettyPrinter -> unit coq_PrettyPrinter list -> unit
  coq_PrettyPrinter

val print_constructor :
  Ex.global_env -> remaps -> coq_RustPrintConfig -> inductive -> nat -> unit
  coq_PrettyPrinter list -> unit coq_PrettyPrinter

val print_const :
  Ex.global_env -> remaps -> coq_RustPrintConfig -> kername -> unit
  coq_PrettyPrinter list -> unit coq_PrettyPrinter

val print_case :
  Ex.global_env -> remaps -> coq_RustPrintConfig -> (ident list -> term ->
  unit coq_PrettyPrinter) -> ident list -> inductive -> nat -> term -> (name
  list * term) list -> unit coq_PrettyPrinter

val print_remapped_case :
  Ex.global_env -> (ident list -> term -> unit coq_PrettyPrinter) -> ident
  list -> inductive -> term -> (name list * term) list -> String.t -> unit
  coq_PrettyPrinter

val needs_block : term -> bool

val print_term :
  Ex.global_env -> remaps -> coq_RustPrintConfig -> ident list -> term ->
  unit coq_PrettyPrinter

val print_constant :
  Ex.global_env -> remaps -> coq_RustPrintConfig -> kername -> (name
  list * box_type) -> E.term -> unit coq_PrettyPrinter

val print_ind_ctor_definition :
  Ex.global_env -> remaps -> coq_RustPrintConfig -> ident list -> ident ->
  (name * box_type) list -> unit coq_PrettyPrinter

val print_mutual_inductive_body :
  Ex.global_env -> remaps -> coq_RustPrintConfig -> ind_attr_map -> kername
  -> Ex.mutual_inductive_body -> unit coq_PrettyPrinter

val print_type_alias :
  Ex.global_env -> remaps -> coq_RustPrintConfig -> kername -> type_var_info
  list -> box_type -> unit coq_PrettyPrinter

val print_decls_aux :
  Ex.global_env -> remaps -> coq_RustPrintConfig -> ind_attr_map ->
  ((kername * bool) * Ex.global_decl) list -> unit coq_PrettyPrinter -> unit
  coq_PrettyPrinter

type coq_Preamble = { top_preamble : String.t list;
                      program_preamble : String.t list }

val print_program :
  Ex.global_env -> remaps -> coq_RustPrintConfig -> ind_attr_map ->
  coq_Preamble -> unit coq_PrettyPrinter
