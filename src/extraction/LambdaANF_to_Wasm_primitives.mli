open BinInt
open BinNat
open BinNums
open Byte
open Datatypes
open Kernames
open Bytestring
open CompM
open Datatypes0
open Numerics

val coq_Z_to_i64val_co : coq_Z -> value_num

val maxuint63 : coq_Z

val true_ord : coq_N

val false_ord : coq_N

val coq_Eq_ord : coq_N

val coq_Lt_ord : coq_N

val coq_Gt_ord : coq_N

val coq_C0_ord : coq_N

val coq_C1_ord : coq_N

val pair_ord : coq_N

val primInt63ModPath : modpath

type primop =
| PrimInt63add
| PrimInt63sub
| PrimInt63mul
| PrimInt63div
| PrimInt63mod
| PrimInt63lsl
| PrimInt63lsr
| PrimInt63land
| PrimInt63lor
| PrimInt63lxor
| PrimInt63eqb
| PrimInt63ltb
| PrimInt63leb
| PrimInt63compare
| PrimInt63addc
| PrimInt63addcarryc
| PrimInt63subc
| PrimInt63subcarryc
| PrimInt63mulc
| PrimInt63head0
| PrimInt63tail0
| PrimInt63diveucl
| PrimInt63diveucl_21
| PrimInt63addmuldiv

val primop_map : primop KernameMap.t

val load_local_i64 : localidx -> basic_instruction list

val increment_glob_mem_ptr : globalidx -> coq_N -> basic_instruction list

val bitmask_instrs : basic_instruction list

val apply_binop_and_store_i64 :
  globalidx -> binop_i -> localidx -> localidx -> bool -> basic_instruction
  list

val make_carry : globalidx -> coq_N -> globalidx -> basic_instruction list

val apply_add_carry_operation :
  globalidx -> globalidx -> localidx -> localidx -> bool -> basic_instruction
  list

val apply_sub_carry_operation :
  globalidx -> globalidx -> localidx -> localidx -> bool -> basic_instruction
  list

val make_product : globalidx -> coq_N -> coq_N -> basic_instruction list

val make_boolean_valued_comparison :
  localidx -> localidx -> relop_i -> basic_instruction list

val compare_instrs : localidx -> localidx -> basic_instruction list

val div_instrs : globalidx -> localidx -> localidx -> basic_instruction list

val mod_instrs : globalidx -> localidx -> localidx -> basic_instruction list

val shift_instrs :
  globalidx -> localidx -> localidx -> binop_i -> bool -> basic_instruction
  list

val low32 : basic_instruction list

val high32 : basic_instruction list

val mulc_instrs :
  globalidx -> globalidx -> globalidx -> globalidx -> globalidx -> localidx
  -> localidx -> basic_instruction list

val diveucl_instrs :
  globalidx -> globalidx -> globalidx -> localidx -> localidx ->
  basic_instruction list

val translate_primitive_binary_op :
  globalidx -> globalidx -> globalidx -> globalidx -> globalidx -> primop ->
  localidx -> localidx -> basic_instruction list error

val head0_instrs : globalidx -> localidx -> basic_instruction list

val tail0_instrs : globalidx -> localidx -> basic_instruction list

val translate_primitive_unary_op :
  globalidx -> primop -> localidx -> basic_instruction list error

val diveucl_21_loop_body :
  globalidx -> globalidx -> globalidx -> globalidx -> basic_instruction list

val diveucl_21_loop :
  globalidx -> globalidx -> globalidx -> globalidx -> globalidx -> coq_Z ->
  basic_instruction list

val diveucl_21_instrs :
  globalidx -> globalidx -> globalidx -> globalidx -> globalidx -> globalidx
  -> localidx -> localidx -> localidx -> basic_instruction list

val addmuldiv_instrs :
  globalidx -> localidx -> localidx -> localidx -> basic_instruction list

val translate_primitive_ternary_op :
  globalidx -> globalidx -> globalidx -> globalidx -> globalidx -> globalidx
  -> primop -> localidx -> localidx -> localidx -> basic_instruction list
  error
