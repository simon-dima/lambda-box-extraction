open AST
open Archi
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
open MonadState
open Nat0
open OptionMonad
open PeanoNat
open Specif
open StateMonad
open Uint0
open Bytestring
open Cps
open Cps_show
open ExceptionMonad
open Identifiers
open Toplevel0

val maxArgs : coq_Z

val makeArgList' : positive list -> coq_N list

val makeArgList : positive list -> coq_N list

type fun_info_env = (positive * fun_tag) M.t

val compute_fun_env' : nat -> fun_env -> exp -> fun_env

val compute_fun_env_fundefs : nat -> fundefs -> fun_env -> fun_env

val max_depth : exp -> nat

val max_depth_fundefs : fundefs -> nat

val compute_fun_env : exp -> fun_env

val get_allocs : exp -> positive list

val get_allocs_fundefs : fundefs -> positive list

val max_allocs : exp -> nat

val max_allocs_fundefs : fundefs -> nat

type n_ind_ty_info = name * ctor_ty_info list

type n_ind_env = n_ind_ty_info M.t

val update_ind_env : n_ind_env -> positive -> ctor_ty_info -> n_ind_env

val compute_ind_env : ctor_env -> n_ind_env

type ctor_rep =
| Coq_enum of coq_N
| Coq_boxed of coq_N * coq_N

val make_ctor_rep : ctor_env -> ctor_tag -> ctor_rep option

val threadStructInf : ident -> coq_type

val threadInf : ident -> coq_type

val uintTy : coq_type

val ulongTy : coq_type

val coq_val : coq_type

val uval : coq_type

val val_typ : typ

val coq_Init_int : coq_Z -> init_data

val make_cint : coq_Z -> coq_type -> expr

val gcTy : ident -> coq_type

val isptrTy : coq_type

val valPtr : coq_type

val boolTy : coq_type

val mkFunTyList : nat -> typelist

val mkFunTy : ident -> nat -> coq_type

val mkPrimTy : nat -> coq_type

val mkPrimTyTinfo : ident -> nat -> coq_type

val allocPtr : ident -> expr

val limitPtr : ident -> expr

val args : ident -> expr

val gc : ident -> ident -> expr

val ptr : ident -> expr

val tinf : ident -> ident -> expr

val tinfd : ident -> ident -> expr

val add : expr -> expr -> expr

val sub : expr -> expr -> expr

val not : expr -> expr

val c_int : coq_Z -> coq_type -> expr

val reserve_body :
  ident -> ident -> ident -> ident -> ident -> positive -> coq_Z -> statement

val makeTagZ : ctor_env -> ctor_tag -> coq_Z option

val makeTag : ctor_env -> ctor_tag -> expr option

val mkFunVar : ident -> nat -> ident -> coq_N list -> expr

val makeVar : ident -> nat -> positive -> fun_env -> fun_info_env -> expr

val assignConstructorS' :
  ident -> nat -> fun_env -> fun_info_env -> positive -> nat -> positive list
  -> statement

val assignConstructorS :
  ident -> ident -> nat -> ctor_env -> n_ind_env -> fun_env -> fun_info_env
  -> positive -> ctor_tag -> positive list -> statement option

val isPtr : ident -> positive -> positive -> statement

val mkCallVars :
  ident -> nat -> fun_env -> fun_info_env -> nat -> positive list -> expr
  list option

val mkCall :
  ident -> ident -> nat -> fun_env -> fun_info_env -> expr -> nat -> positive
  list -> statement option

val mkPrimCall :
  ident -> nat -> positive -> positive -> nat -> fun_env -> fun_info_env ->
  positive list -> statement option

val mkPrimCallTinfo :
  ident -> ident -> nat -> positive -> positive -> nat -> fun_env ->
  fun_info_env -> positive list -> statement option

val asgnFunVars' : ident -> positive list -> coq_N list -> statement option

val asgnFunVars :
  ident -> nat -> positive list -> coq_N list -> statement option

val asgnAppVars'' :
  ident -> ident -> nat -> positive list -> coq_N list -> fun_env ->
  fun_info_env -> statement option

val asgnAppVars' :
  ident -> ident -> nat -> positive list -> coq_N list -> fun_env ->
  fun_info_env -> statement option

val asgnAppVars :
  ident -> ident -> ident -> nat -> positive list -> coq_N list -> fun_env ->
  fun_info_env -> statement option

val reserve :
  ident -> ident -> ident -> ident -> ident -> ident -> nat -> positive ->
  coq_Z -> positive list -> coq_N list -> fun_env -> fun_info_env ->
  statement option

val make_case_switch :
  ident -> ident -> positive -> labeled_statements -> labeled_statements ->
  statement

val to_int64 : Uint63.t -> Integers.Int64.int

val float64_to_model : Float64.t -> float64_model

val model_to_ff : float64_model -> full_float

val to_float :
  ident -> ident -> ident -> ident -> ident -> ident -> String.t -> ident ->
  ident -> ident -> ident -> ident -> ident -> nat -> prim_env -> Float64.t
  -> float

val compile_float :
  ident -> ctor_env -> n_ind_env -> fun_env -> fun_info_env -> positive ->
  float -> statement

val compile_primitive :
  ident -> ident -> ident -> ident -> ident -> ident -> String.t -> ident ->
  ident -> ident -> ident -> ident -> ident -> nat -> prim_env -> ctor_env ->
  n_ind_env -> fun_env -> fun_info_env -> positive -> primitive -> statement

val translate_body :
  ident -> ident -> ident -> ident -> ident -> ident -> String.t -> ident ->
  ident -> ident -> ident -> ident -> ident -> nat -> prim_env -> exp ->
  fun_env -> ctor_env -> n_ind_env -> fun_info_env -> statement option

val mkFun :
  ident -> ident -> ident -> ident -> ident -> ident -> nat -> positive list
  -> positive list -> statement -> coq_function

val translate_fundefs :
  ident -> ident -> ident -> ident -> ident -> ident -> String.t -> ident ->
  ident -> ident -> ident -> ident -> ident -> nat -> prim_env -> fundefs ->
  fun_env -> ctor_env -> n_ind_env -> fun_info_env ->
  (positive * (Clight.fundef, coq_type) globdef) list option

val make_extern_decl :
  name_env -> (positive * (Clight.fundef, coq_type) globdef) -> bool ->
  (positive * (Clight.fundef, coq_type) globdef) option

val make_extern_decls :
  name_env -> (positive * (Clight.fundef, coq_type) globdef) list -> bool ->
  (positive * (Clight.fundef, coq_type) globdef) list

val body_external_decl :
  ident -> String.t -> ident -> ident -> positive * (Clight.fundef, coq_type)
  globdef

val translate_funs :
  ident -> ident -> ident -> ident -> ident -> ident -> String.t -> ident ->
  ident -> ident -> ident -> ident -> ident -> nat -> prim_env -> exp ->
  fun_env -> ctor_env -> n_ind_env -> fun_info_env ->
  (positive * (Clight.fundef, coq_type) globdef) list option

type 't nState = (positive, 't) state

val getName : positive nState

val make_ind_array : coq_N list -> init_data list

val update_name_env_fun_info : positive -> positive -> name_env -> name_env

val make_fundef_info :
  fundefs -> fun_env -> name_env -> (((positive * (Clight.fundef, coq_type)
  globdef) list * fun_info_env) * name_env) option nState

val add_bodyinfo :
  ident -> exp -> fun_env -> name_env -> fun_info_env ->
  (positive * (Clight.fundef, coq_type) globdef) list ->
  (((positive * (Clight.fundef, coq_type) globdef)
  list * (positive * positive) M.tree) * name M.tree) option nState

val make_funinfo :
  ident -> exp -> fun_env -> name_env -> (((positive * (Clight.fundef,
  coq_type) globdef) list * fun_info_env) * name_env) option nState

val global_defs : exp -> (positive * (Clight.fundef, coq_type) globdef) list

val make_defs :
  ident -> ident -> ident -> ident -> ident -> ident -> String.t -> ident ->
  ident -> ident -> ident -> ident -> ident -> nat -> prim_env -> exp ->
  fun_env -> ctor_env -> n_ind_env -> name M.t -> (name
  M.t * (positive * (Clight.fundef, coq_type) globdef) list) coq_exception
  nState

val composites : composite_definition list

val mk_prog_opt :
  ident -> (ident * (Clight.fundef, coq_type) globdef) list -> ident -> bool
  -> Clight.program option

val wrap_in_fun : exp -> exp

val add_inf_vars :
  ident -> ident -> ident -> ident -> ident -> ident -> String.t -> ident ->
  ident -> ident -> ident -> ident -> ident -> name_env -> name_env

val ensure_unique : name M.t -> name M.t

val make_tinfoIdent : positive

val exportIdent : positive

val make_tinfo_rec : ident -> positive * (Clight.fundef, coq_type) globdef

val export_rec : ident -> positive * (Clight.fundef, coq_type) globdef

val make_empty_header :
  ctor_env -> n_ind_env -> exp -> name_env ->
  (name_env * (ident * (Clight.fundef, coq_type) globdef) list) option nState

val compile :
  ident -> ident -> ident -> ident -> ident -> ident -> String.t -> ident ->
  ident -> ident -> ident -> ident -> ident -> nat -> prim_env -> exp ->
  ctor_env -> name M.t -> ((name M.t * Clight.program
  option) * Clight.program option) coq_exception
