open BinNatDef
open BinNums
open Byte
open Datatypes
open List0
open Bytes0
open Datatypes0
open Datatypes_properties
open Eqtype
open Leb128
open List_extra
open Numerics
open Seq
open Ssrnat

val binary_of_number_type : number_type -> Byte.byte

val binary_of_vector_type : vector_type -> Byte.byte

val binary_of_reference_type : reference_type -> Byte.byte

val binary_of_value_type : value_type -> Byte.byte

val binary_of_u32 : coq_N -> Byte.byte list

val binary_of_u32_nat : nat -> Byte.byte list

val binary_of_idx : coq_N -> Byte.byte list

val binary_of_typeidx : typeidx -> Byte.byte list

val binary_of_funcidx : funcidx -> Byte.byte list

val binary_of_tableidx : tableidx -> Byte.byte list

val binary_of_memidx : memidx -> Byte.byte list

val binary_of_globalidx : globalidx -> Byte.byte list

val binary_of_vec : ('a1 -> Byte.byte list) -> 'a1 list -> Byte.byte list

val binary_of_memarg : memarg -> Byte.byte list

val binary_of_i32 : Equality.sort -> Byte.byte list

val binary_of_i64 : Equality.sort -> Byte.byte list

val binary_of_f32 : Equality.sort -> Byte.byte list

val binary_of_f64 : Equality.sort -> Byte.byte list

val binary_of_block_type : block_type -> Byte.byte list

val binary_of_value_types : value_type list -> Byte.byte list

val binary_of_result_type : value_type list -> Byte.byte list

val dummy : Byte.byte list

val binary_of_valvec : value_vec -> Byte.byte list

val binary_of_unop_vec : unop_vec -> Byte.byte list

val binary_of_binop_vec : binop_vec -> Byte.byte list

val binary_of_ternop_vec : ternop_vec -> Byte.byte list

val binary_of_test_vec : test_vec -> Byte.byte list

val binary_of_shift_vec : shift_vec -> Byte.byte list

val binary_of_splat_vec : shape_vec -> Byte.byte list

val binary_of_extract_vec :
  shape_vec -> sx option -> laneidx -> Byte.byte list

val binary_of_replace_vec : shape_vec -> laneidx -> Byte.byte list

val binary_of_load_vec : load_vec_arg -> memarg -> Byte.byte list

val binary_of_load_vec_lane : width_vec -> memarg -> laneidx -> Byte.byte list

val binary_of_store_vec_lane :
  width_vec -> memarg -> laneidx -> Byte.byte list

val binary_of_be : basic_instruction -> Byte.byte list

val binary_of_expr : basic_instruction list -> Byte.byte list

val magic : Byte.byte list

val version : Byte.byte list

val with_length : Byte.byte list -> Byte.byte list

val binary_of_functype : function_type -> Byte.byte list

val binary_of_typesec : function_type list -> Byte.byte list

val binary_of_name : name -> Byte.byte list

val binary_of_limits : limits -> Byte.byte list

val binary_of_table_type : table_type -> Byte.byte list

val binary_of_mutability : mutability -> Byte.byte list

val binary_of_global_type : global_type -> Byte.byte list

val binary_of_memory_type : memory_type -> Byte.byte list

val binary_of_import_desc : module_import_desc -> Byte.byte list

val binary_of_module_import : module_import -> Byte.byte list

val binary_of_importsec : module_import list -> Byte.byte list

val binary_of_funcsec : module_func list -> Byte.byte list

val binary_of_module_table : module_table -> Byte.byte list

val binary_of_tablesec : module_table list -> Byte.byte list

val binary_of_module_mem : module_mem -> Byte.byte list

val binary_of_memsec : module_mem list -> Byte.byte list

val binary_of_module_global : module_global -> Byte.byte list

val binary_of_globalsec : module_global list -> Byte.byte list

val binary_of_export_desc : module_export_desc -> Byte.byte list

val binary_of_module_export : module_export -> Byte.byte list

val binary_of_exportssec : module_export list -> Byte.byte list

val binary_of_module_start : module_start -> Byte.byte list

val binary_of_startsec : module_start -> Byte.byte list

val to_ref_func : basic_instruction list -> funcidx option

val elem_of_wasm_1_0 : module_element -> Byte.byte list option

val binary_of_module_elem : module_element -> Byte.byte list

val binary_of_elemsec : module_element list -> Byte.byte list

val binary_of_local : (nat * value_type) -> Byte.byte list

val bunch_locals_aux :
  value_type -> nat -> (nat * value_type) list -> value_type list ->
  (nat * value_type) list

val bunch_locals : value_type list -> (nat * value_type) list

val binary_of_code_func : value_type list -> expr -> Byte.byte list

val binary_of_code : module_func -> Byte.byte list

val binary_of_codesec : module_func list -> Byte.byte list

val binary_of_data : module_data -> Byte.byte list

val binary_of_datasec : module_data list -> Byte.byte list

val only_if_non_nil :
  ('a1 list -> Byte.byte list) -> 'a1 list -> Byte.byte list

val only_if_non_none : ('a1 -> Byte.byte list) -> 'a1 option -> Byte.byte list

val binary_of_module : coq_module -> Byte.byte list
