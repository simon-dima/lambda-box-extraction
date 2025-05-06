open AST
open Archi
open Ascii
open AstCommon
open BasicAst
open Basics
open BinInt
open BinNat
open BinNums
open BinPos
open Binary
open Byte
open Clight
open Cop
open Ctypes
open Ctypesdefs
open Datatypes
open Errors0
open FloatOps
open Floats
open List0
open Maps
open Monad0
open Nat0
open PeanoNat
open Specif
open String0
open Uint0
open Bytestring
open CompM
open Cps
open Cps_show
open Identifiers
open Set_util
open State
open Toplevel0

val show_name : name -> String.t

val maxArgs : coq_Z

val makeArgList' : positive list -> coq_N list

val makeArgList : positive list -> coq_N list

type fun_info_env = (positive * fun_tag) M.t

val compute_fun_env' : nat -> name_env -> fun_env -> exp -> fun_env

val compute_fun_env_fundefs : nat -> name_env -> fundefs -> fun_env -> fun_env

val max_depth : exp -> nat

val max_depth_fundefs : fundefs -> nat

val compute_fun_env : name_env -> exp -> fun_env

val get_locals : exp -> positive list

val get_locals_fundefs : fundefs -> positive list

val max_allocs : exp -> nat

val max_allocs_fundefs : fundefs -> nat

type n_ind_ty_info = name * (((name * ctor_tag) * coq_N) * coq_N) list

type n_ind_env = n_ind_ty_info M.t

val update_ind_env : n_ind_env -> positive -> ctor_ty_info -> n_ind_env

val compute_ind_env : ctor_env -> n_ind_env

type ctor_rep =
| Coq_enum of coq_N
| Coq_boxed of coq_N * coq_N

val make_ctor_rep : ctor_env -> ctor_tag -> ctor_rep error

val coq_val : coq_type

val uval : coq_type

val val_typ : typ

val coq_Init_int : coq_Z -> init_data

val make_cint : coq_Z -> coq_type -> expr

val floatType : coq_type

val mkFunTyList : nat -> typelist

val mkFunTy : ident -> nat -> coq_type

val mkPrimTy : nat -> coq_type

val mkPrimTyTinfo : ident -> nat -> coq_type

val add : expr -> expr -> expr

val sub : expr -> expr -> expr

val not : expr -> expr

val c_int' : coq_Z -> coq_type -> expr

val stackframeT : ident -> coq_type

val stackframeTPtr : ident -> coq_type

val rootT : coq_Z -> coq_type

val rootTPtr : coq_type

val stack_decl : ident -> ident -> ident -> coq_Z -> (ident * coq_type) list

val init_stack :
  ident -> ident -> ident -> ident -> ident -> ident -> ident -> ident ->
  ident -> statement

val set_stack :
  ident -> ident -> ident -> ident -> ident -> coq_N -> bool -> statement

val update_stack : ident -> ident -> ident -> ident -> coq_N -> statement

val reset_stack :
  ident -> ident -> ident -> ident -> ident -> ident -> coq_N -> bool ->
  statement

val push_var : ident -> coq_N -> positive -> statement

val pop_var : ident -> coq_N -> positive -> statement

val push_live_vars_offset :
  ident -> coq_N -> positive list -> statement * coq_N

val pop_live_vars_offset : ident -> coq_N -> positive list -> statement

val push_live_vars : ident -> positive list -> statement * coq_N

val pop_live_vars : ident -> positive list -> statement

val makeTagZ : ctor_env -> ctor_tag -> coq_Z error

val makeTag : ctor_env -> ctor_tag -> expr error

val mkFunVar : ident -> nat -> ident -> coq_N list -> expr

val makeVar : ident -> nat -> positive -> fun_env -> fun_info_env -> expr

val assignConstructorS' :
  ident -> nat -> fun_env -> fun_info_env -> positive -> nat -> positive list
  -> statement

val assignConstructorS :
  ident -> ident -> nat -> ctor_env -> n_ind_env -> fun_env -> fun_info_env
  -> positive -> ctor_tag -> positive list -> statement error

val isPtr : positive -> positive -> expr

val mkCallVars :
  ident -> nat -> fun_env -> fun_info_env -> nat -> positive list -> expr
  list error

val mkCall :
  ident -> ident -> nat -> positive option -> fun_env -> fun_info_env -> expr
  -> nat -> positive list -> statement error

val mkPrimCall :
  ident -> nat -> positive -> positive -> nat -> fun_env -> fun_info_env ->
  positive list -> statement error

val mkPrimCallTinfo :
  ident -> ident -> nat -> positive -> positive -> nat -> fun_env ->
  fun_info_env -> positive list -> statement error

val asgnFunVars' : ident -> positive list -> coq_N list -> statement error

val asgnFunVars :
  ident -> nat -> positive list -> coq_N list -> statement error

val asgnAppVars'' :
  ident -> ident -> nat -> positive list -> coq_N list -> fun_env ->
  fun_info_env -> String.t -> statement error

val asgnAppVars' :
  ident -> ident -> nat -> positive list -> coq_N list -> fun_env ->
  fun_info_env -> String.t -> statement error

val get_ind : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 -> nat error

val remove_AppVars :
  positive list -> positive list -> coq_N list -> coq_N list -> (positive
  list * coq_N list) error

val asgnAppVars_fast' :
  ident -> ident -> nat -> positive list -> positive list -> coq_N list ->
  coq_N list -> fun_env -> fun_info_env -> String.t -> statement error

val asgnAppVars :
  ident -> ident -> ident -> nat -> positive list -> coq_N list -> fun_env ->
  fun_info_env -> String.t -> statement error

val asgnAppVars_fast :
  ident -> ident -> ident -> nat -> positive list -> positive list -> coq_N
  list -> coq_N list -> fun_env -> fun_info_env -> String.t -> statement error

val set_nalloc : ident -> ident -> ident -> expr -> statement

val make_GC_call :
  ident -> ident -> ident -> ident -> ident -> ident -> ident -> ident ->
  ident -> ident -> ident -> ident -> nat -> positive list -> coq_N ->
  statement * coq_N

val make_case_switch :
  ident -> positive -> labeled_statements -> labeled_statements -> statement

val to_int64 : Uint63.t -> Integers.Int64.int

val float64_to_model : Float64.t -> float64_model

val model_to_ff : float64_model -> full_float

val to_float :
  ident -> ident -> ident -> ident -> ident -> ident -> ident -> String.t ->
  ident -> ident -> ident -> ident -> ident -> ident -> ident -> nat ->
  prim_env -> ident -> ident -> ident -> ident -> ident -> ident -> ident ->
  Float64.t -> float

val compile_float :
  ident -> ctor_env -> n_ind_env -> fun_env -> fun_info_env -> positive ->
  float -> statement

val compile_primitive :
  ident -> ident -> ident -> ident -> ident -> ident -> ident -> String.t ->
  ident -> ident -> ident -> ident -> ident -> ident -> ident -> nat ->
  prim_env -> ident -> ident -> ident -> ident -> ident -> ident -> ident ->
  ctor_env -> n_ind_env -> fun_env -> fun_info_env -> positive -> primitive
  -> statement

val translate_body :
  ident -> ident -> ident -> ident -> ident -> ident -> ident -> String.t ->
  ident -> ident -> ident -> ident -> ident -> ident -> ident -> nat ->
  prim_env -> ident -> ident -> ident -> ident -> ident -> ident -> ident ->
  bool -> positive list -> coq_FVSet -> coq_N list -> name_env -> exp ->
  fun_env -> ctor_env -> n_ind_env -> fun_info_env -> coq_N ->
  (statement * coq_N) error

val mkFun :
  ident -> ident -> ident -> ident -> ident -> ident -> ident -> nat -> ident
  -> ident -> ident -> coq_Z -> positive list -> positive list -> statement
  -> coq_function

val translate_fundefs :
  ident -> ident -> ident -> ident -> ident -> ident -> ident -> String.t ->
  ident -> ident -> ident -> ident -> ident -> ident -> ident -> nat ->
  prim_env -> ident -> ident -> ident -> ident -> ident -> ident -> ident ->
  bool -> fundefs -> fun_env -> ctor_env -> n_ind_env -> fun_info_env ->
  name_env -> (positive * (Clight.fundef, coq_type) globdef) list error

val make_extern_decl :
  name_env -> (positive * (Clight.fundef, coq_type) globdef) -> bool ->
  (positive * (Clight.fundef, coq_type) globdef) option

val make_extern_decls :
  name_env -> (positive * (Clight.fundef, coq_type) globdef) list -> bool ->
  (positive * (Clight.fundef, coq_type) globdef) list

val body_external_decl :
  ident -> String.t -> ident -> ident -> positive * (Clight.fundef, coq_type)
  globdef

val translate_program :
  ident -> ident -> ident -> ident -> ident -> ident -> ident -> String.t ->
  ident -> ident -> ident -> ident -> ident -> ident -> ident -> nat ->
  prim_env -> ident -> ident -> ident -> ident -> ident -> ident -> ident ->
  bool -> exp -> fun_env -> ctor_env -> n_ind_env -> fun_info_env -> name_env
  -> (positive * (Clight.fundef, coq_type) globdef) list error

type 'a nState = (positive, 'a) compM'

val getName : positive nState

val make_ind_array : coq_N list -> init_data list

val update_name_env_fun_info : positive -> positive -> name_env -> name_env

val make_fundef_info :
  fundefs -> fun_env -> name_env -> (((positive * (Clight.fundef, coq_type)
  globdef) list * fun_info_env) * name_env) nState

val add_bodyinfo :
  ident -> exp -> fun_env -> name_env -> fun_info_env ->
  (positive * (Clight.fundef, coq_type) globdef) list ->
  (((positive * (Clight.fundef, coq_type) globdef)
  list * (positive * positive) M.tree) * name M.tree) nState

val make_funinfo :
  ident -> exp -> fun_env -> name_env -> (((positive * (Clight.fundef,
  coq_type) globdef) list * fun_info_env) * name_env) nState

val global_defs : exp -> (positive * (Clight.fundef, coq_type) globdef) list

val make_defs :
  ident -> ident -> ident -> ident -> ident -> ident -> ident -> String.t ->
  ident -> ident -> ident -> ident -> ident -> ident -> ident -> nat ->
  prim_env -> ident -> ident -> ident -> ident -> ident -> ident -> ident ->
  bool -> exp -> fun_env -> ctor_env -> n_ind_env -> name_env ->
  (name_env * (positive * (Clight.fundef, coq_type) globdef) list) nState

val composites : composite_definition list

val mk_prog_opt :
  ident -> (ident * (Clight.fundef, coq_type) globdef) list -> ident -> bool
  -> Clight.program error

val wrap_in_fun : exp -> exp

val inf_vars :
  ident -> ident -> ident -> ident -> ident -> ident -> ident -> String.t ->
  ident -> ident -> ident -> ident -> ident -> ident -> ident -> ident ->
  ident -> ident -> ident -> ident -> ident -> ident -> (ident * name) list

val add_inf_vars :
  ident -> ident -> ident -> ident -> ident -> ident -> ident -> String.t ->
  ident -> ident -> ident -> ident -> ident -> ident -> ident -> ident ->
  ident -> ident -> ident -> ident -> ident -> ident -> name_env -> name_env

val ensure_unique : name_env -> name_env

val make_tinfoIdent : positive

val make_tinfo_rec : ident -> positive * (Clight.fundef, coq_type) globdef

val compile :
  ident -> ident -> ident -> ident -> ident -> ident -> ident -> String.t ->
  ident -> ident -> ident -> ident -> ident -> ident -> ident -> nat ->
  prim_env -> ident -> ident -> ident -> ident -> ident -> ident -> ident ->
  bool -> exp -> ctor_env -> name_env ->
  ((name_env * Clight.program) * Clight.program) error * String.t

val empty_program : ident -> Clight.program

val stripOption : ident -> Clight.program option -> Clight.program
