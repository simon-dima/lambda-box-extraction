open BinNums
open Byte
open Datatypes
open Memdata
open Bytes0
open Eqtype
open Numerics

type u32 = coq_N

type u8 = coq_N

type typeidx = u32

type funcidx = u32

type tableidx = u32

type memidx = u32

type globalidx = u32

type elemidx = u32

type dataidx = u32

type localidx = u32

type labelidx = u32

type value_num =
| VAL_int32 of Equality.sort
| VAL_int64 of Equality.sort
| VAL_float32 of Equality.sort
| VAL_float64 of Equality.sort

type value_vec =
  unit
  (* singleton inductive, whose constructor was VAL_vec128 *)

type name = Byte.byte list

type number_type =
| T_i32
| T_i64
| T_f32
| T_f64

type vector_type =
| T_v128

type reference_type =
| T_funcref
| T_externref

type value_type =
| T_num of number_type
| T_vec of vector_type
| T_ref of reference_type
| T_bot

type result_type = value_type list

type function_type =
| Tf of result_type * result_type

type limits = { lim_min : u32; lim_max : u32 option }

type memory_type = limits

type table_type = { tt_limits : limits; tt_elem_type : reference_type }

type mutability =
| MUT_const
| MUT_var

type global_type = { tg_mut : mutability; tg_t : value_type }

type block_type =
| BT_id of typeidx
| BT_valtype of value_type option

val serialise_f32 : Equality.sort -> bytes

val serialise_f64 : Equality.sort -> bytes

type sx =
| SX_S
| SX_U

type unop_i =
| UOI_clz
| UOI_ctz
| UOI_popcnt

type unop_f =
| UOF_abs
| UOF_neg
| UOF_sqrt
| UOF_ceil
| UOF_floor
| UOF_trunc
| UOF_nearest

type unop =
| Unop_i of unop_i
| Unop_f of unop_f
| Unop_extend of coq_N

type binop_i =
| BOI_add
| BOI_sub
| BOI_mul
| BOI_div of sx
| BOI_rem of sx
| BOI_and
| BOI_or
| BOI_xor
| BOI_shl
| BOI_shr of sx
| BOI_rotl
| BOI_rotr

type binop_f =
| BOF_add
| BOF_sub
| BOF_mul
| BOF_div
| BOF_min
| BOF_max
| BOF_copysign

type binop =
| Binop_i of binop_i
| Binop_f of binop_f

type testop =
| TO_eqz

type relop_i =
| ROI_eq
| ROI_ne
| ROI_lt of sx
| ROI_gt of sx
| ROI_le of sx
| ROI_ge of sx

type relop_f =
| ROF_eq
| ROF_ne
| ROF_lt
| ROF_gt
| ROF_le
| ROF_ge

type relop =
| Relop_i of relop_i
| Relop_f of relop_f

type cvtop =
| CVO_wrap
| CVO_extend
| CVO_trunc
| CVO_trunc_sat
| CVO_convert
| CVO_demote
| CVO_promote
| CVO_reinterpret

type packed_type =
| Tp_i8
| Tp_i16
| Tp_i32

type shape_vec_i =
| SVI_8_16
| SVI_16_8
| SVI_32_4
| SVI_64_2

type shape_vec_f =
| SVF_32_4
| SVF_64_2

type shape_vec =
| SV_ishape of shape_vec_i
| SV_fshape of shape_vec_f

type unop_vec =
| VUO_not

type binop_vec =
| VBO_and

type ternop_vec =
| VTO_bitselect

type test_vec =
| VT_any_true

type shift_vec =
| VSH_any_true

type laneidx = u8

type packed_type_vec =
| Tptv_8_8
| Tptv_16_4
| Tptv_32_2

type zero_type_vec =
| Tztv_32
| Tztv_64

type width_vec =
| Twv_8
| Twv_16
| Twv_32
| Twv_64

type load_vec_arg =
| LVA_packed of packed_type_vec * sx
| LVA_zero of zero_type_vec
| LVA_splat of width_vec

type memarg = { memarg_offset : u32; memarg_align : u32 }

type basic_instruction =
| BI_const_num of value_num
| BI_unop of number_type * unop
| BI_binop of number_type * binop
| BI_testop of number_type * testop
| BI_relop of number_type * relop
| BI_cvtop of number_type * cvtop * number_type * sx option
| BI_const_vec of value_vec
| BI_unop_vec of unop_vec
| BI_binop_vec of binop_vec
| BI_ternop_vec of ternop_vec
| BI_test_vec of test_vec
| BI_shift_vec of shift_vec
| BI_splat_vec of shape_vec
| BI_extract_vec of shape_vec * sx option * laneidx
| BI_replace_vec of shape_vec * laneidx
| BI_ref_null of reference_type
| BI_ref_is_null
| BI_ref_func of funcidx
| BI_drop
| BI_select of value_type list option
| BI_local_get of localidx
| BI_local_set of localidx
| BI_local_tee of localidx
| BI_global_get of globalidx
| BI_global_set of globalidx
| BI_table_get of tableidx
| BI_table_set of tableidx
| BI_table_size of tableidx
| BI_table_grow of tableidx
| BI_table_fill of tableidx
| BI_table_copy of tableidx * tableidx
| BI_table_init of tableidx * elemidx
| BI_elem_drop of elemidx
| BI_load of number_type * (packed_type * sx) option * memarg
| BI_load_vec of load_vec_arg * memarg
| BI_load_vec_lane of width_vec * memarg * laneidx
| BI_store of number_type * packed_type option * memarg
| BI_store_vec_lane of width_vec * memarg * laneidx
| BI_memory_size
| BI_memory_grow
| BI_memory_fill
| BI_memory_copy
| BI_memory_init of dataidx
| BI_data_drop of dataidx
| BI_nop
| BI_unreachable
| BI_block of block_type * basic_instruction list
| BI_loop of block_type * basic_instruction list
| BI_if of block_type * basic_instruction list * basic_instruction list
| BI_br of labelidx
| BI_br_if of labelidx
| BI_br_table of labelidx list * labelidx
| BI_return
| BI_call of funcidx
| BI_call_indirect of tableidx * typeidx
| BI_return_call of funcidx
| BI_return_call_indirect of tableidx * typeidx

type expr = basic_instruction list

type module_import_desc =
| MID_func of typeidx
| MID_table of table_type
| MID_mem of memory_type
| MID_global of global_type

type module_import = { imp_module : name; imp_name : name;
                       imp_desc : module_import_desc }

type module_func = { modfunc_type : typeidx;
                     modfunc_locals : value_type list; modfunc_body : 
                     expr }

type module_table =
  table_type
  (* singleton inductive, whose constructor was Build_module_table *)

type module_mem =
  memory_type
  (* singleton inductive, whose constructor was Build_module_mem *)

type module_global = { modglob_type : global_type; modglob_init : expr }

type module_elemmode =
| ME_passive
| ME_active of tableidx * expr
| ME_declarative

type module_element = { modelem_type : reference_type;
                        modelem_init : expr list;
                        modelem_mode : module_elemmode }

type module_datamode =
| MD_passive
| MD_active of memidx * expr

type module_data = { moddata_init : byte list; moddata_mode : module_datamode }

type module_start =
  funcidx
  (* singleton inductive, whose constructor was Build_module_start *)

type module_export_desc =
| MED_func of funcidx
| MED_table of tableidx
| MED_mem of memidx
| MED_global of globalidx

type module_export = { modexp_name : name; modexp_desc : module_export_desc }

type coq_module = { mod_types : function_type list;
                    mod_funcs : module_func list;
                    mod_tables : module_table list;
                    mod_mems : module_mem list;
                    mod_globals : module_global list;
                    mod_elems : module_element list;
                    mod_datas : module_data list;
                    mod_start : module_start option;
                    mod_imports : module_import list;
                    mod_exports : module_export list }
