open AstCommon
open BinInt
open BinNat
open BinNums
open Byte
open Datatypes
open Kernames
open LambdaANF_to_Wasm_primitives
open LambdaANF_to_Wasm_restrictions
open List0
open MCString
open Monad0
open Nat0
open PeanoNat
open Specif
open Uint0
open Bytestring
open CompM
open Cps
open Cps_show
open Datatypes0
open List_extra
open Numerics
open Operations
open Toplevel0

val max_mem_pages : coq_N

val main_function_name : String.t

val main_function_idx : funcidx

val num_custom_funs : nat

val glob_mem_ptr : globalidx

val glob_cap : globalidx

val glob_result : globalidx

val glob_out_of_mem : globalidx

val glob_tmp1 : globalidx

val glob_tmp2 : globalidx

val glob_tmp3 : globalidx

val glob_tmp4 : globalidx

type localvar_env = localidx M.tree

type fname_env = funcidx M.tree

type wasm_function = { fidx : funcidx; export_name : String.t;
                       coq_type : coq_N; locals : value_type list;
                       body : basic_instruction list }

val nat_to_i32 : nat -> Wasm_int.Int32.int

val nat_to_i64 : nat -> Wasm_int.Int64.int

val nat_to_value : nat -> value_num

val nat_to_value64 : nat -> value_num

val coq_Z_to_value : coq_Z -> value_num

val coq_N_to_value : coq_N -> value_num

val translate_var : name_env -> localvar_env -> var -> String.t -> u32 error

val is_function_var : fname_env -> var -> bool

val instr_local_var_read :
  name_env -> localvar_env -> fname_env -> var -> basic_instruction error

val get_ctor_arity : ctor_env -> ctor_tag -> nat error

val get_ctor_size : ctor_env -> ctor_tag -> coq_N error

val pass_function_args :
  name_env -> localvar_env -> fname_env -> var list -> basic_instruction list
  error

val translate_call :
  name_env -> localvar_env -> fname_env -> var -> var list -> bool ->
  basic_instruction list error

val store_constructor_args :
  name_env -> localvar_env -> fname_env -> var list -> nat ->
  basic_instruction list error

val store_constructor :
  name_env -> ctor_env -> localvar_env -> fname_env -> ctor_tag -> var list
  -> basic_instruction list error

val create_case_nested_if_chain :
  bool -> localidx -> (coq_N * basic_instruction list) list ->
  basic_instruction list

val translate_primitive_value : primitive -> Wasm_int.Int64.int error

val translate_primitive_operation :
  name_env -> localvar_env -> (((kername * String.t) * bool) * nat) -> var
  list -> basic_instruction list error

val grow_memory_if_necessary : basic_instruction list

val call_grow_mem_if_necessary :
  coq_N -> coq_N -> basic_instruction list * coq_N

val translate_body :
  name_env -> ctor_env -> localvar_env -> fname_env -> prim_env -> exp ->
  coq_N -> basic_instruction list error

val collect_local_variables : exp -> var list

val create_var_mapping : u32 -> var list -> u32 M.tree -> u32 M.tree

val create_local_variable_mapping : var list -> localvar_env

val translate_function :
  name_env -> ctor_env -> fname_env -> prim_env -> var -> var list -> exp ->
  wasm_function error

val translate_functions :
  name_env -> ctor_env -> fname_env -> prim_env -> fundefs -> wasm_function
  list error

val sanitize_function_name : String.t -> nat -> String.t

val unique_export_names : wasm_function list -> wasm_function list

val collect_function_vars : exp -> var list

val create_fname_mapping : exp -> fname_env

val list_function_types : nat -> function_type list

val table_element_mapping : nat -> nat -> module_element list

val coq_LambdaANF_to_Wasm :
  name_env -> ctor_env -> prim_env -> exp ->
  ((coq_module * fname_env) * localvar_env) error
