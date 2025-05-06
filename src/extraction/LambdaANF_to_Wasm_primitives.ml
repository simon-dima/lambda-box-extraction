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

(** val coq_Z_to_i64val_co : coq_Z -> value_num **)

let coq_Z_to_i64val_co z =
  VAL_int64 (Obj.magic Wasm_int.Int64.repr z)

(** val maxuint63 : coq_Z **)

let maxuint63 =
  Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    Coq_xH))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val true_ord : coq_N **)

let true_ord =
  Npos Coq_xH

(** val false_ord : coq_N **)

let false_ord =
  N0

(** val coq_Eq_ord : coq_N **)

let coq_Eq_ord =
  N0

(** val coq_Lt_ord : coq_N **)

let coq_Lt_ord =
  Npos Coq_xH

(** val coq_Gt_ord : coq_N **)

let coq_Gt_ord =
  Npos (Coq_xO Coq_xH)

(** val coq_C0_ord : coq_N **)

let coq_C0_ord =
  N0

(** val coq_C1_ord : coq_N **)

let coq_C1_ord =
  Npos Coq_xH

(** val pair_ord : coq_N **)

let pair_ord =
  N0

(** val primInt63ModPath : modpath **)

let primInt63ModPath =
  MPfile ((String.String (Coq_x50, (String.String (Coq_x72, (String.String
    (Coq_x69, (String.String (Coq_x6d, (String.String (Coq_x49,
    (String.String (Coq_x6e, (String.String (Coq_x74, (String.String
    (Coq_x36, (String.String (Coq_x33,
    String.EmptyString)))))))))))))))))) :: ((String.String (Coq_x49,
    (String.String (Coq_x6e, (String.String (Coq_x74, (String.String
    (Coq_x36, (String.String (Coq_x33,
    String.EmptyString)))))))))) :: ((String.String (Coq_x43, (String.String
    (Coq_x79, (String.String (Coq_x63, (String.String (Coq_x6c,
    (String.String (Coq_x69, (String.String (Coq_x63,
    String.EmptyString)))))))))))) :: ((String.String (Coq_x4e,
    (String.String (Coq_x75, (String.String (Coq_x6d, (String.String
    (Coq_x62, (String.String (Coq_x65, (String.String (Coq_x72,
    (String.String (Coq_x73,
    String.EmptyString)))))))))))))) :: ((String.String (Coq_x43,
    (String.String (Coq_x6f, (String.String (Coq_x71,
    String.EmptyString)))))) :: [])))))

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

(** val primop_map : primop KernameMap.t **)

let primop_map =
  KernameMap.add (primInt63ModPath, (String.String (Coq_x61, (String.String
    (Coq_x64, (String.String (Coq_x64, String.EmptyString))))))) PrimInt63add
    (KernameMap.add (primInt63ModPath, (String.String (Coq_x73,
      (String.String (Coq_x75, (String.String (Coq_x62,
      String.EmptyString))))))) PrimInt63sub
      (KernameMap.add (primInt63ModPath, (String.String (Coq_x6d,
        (String.String (Coq_x75, (String.String (Coq_x6c,
        String.EmptyString))))))) PrimInt63mul
        (KernameMap.add (primInt63ModPath, (String.String (Coq_x64,
          (String.String (Coq_x69, (String.String (Coq_x76,
          String.EmptyString))))))) PrimInt63div
          (KernameMap.add (primInt63ModPath, (String.String (Coq_x6d,
            (String.String (Coq_x6f, (String.String (Coq_x64,
            String.EmptyString))))))) PrimInt63mod
            (KernameMap.add (primInt63ModPath, (String.String (Coq_x6c,
              (String.String (Coq_x73, (String.String (Coq_x6c,
              String.EmptyString))))))) PrimInt63lsl
              (KernameMap.add (primInt63ModPath, (String.String (Coq_x6c,
                (String.String (Coq_x73, (String.String (Coq_x72,
                String.EmptyString))))))) PrimInt63lsr
                (KernameMap.add (primInt63ModPath, (String.String (Coq_x6c,
                  (String.String (Coq_x61, (String.String (Coq_x6e,
                  (String.String (Coq_x64, String.EmptyString)))))))))
                  PrimInt63land
                  (KernameMap.add (primInt63ModPath, (String.String (Coq_x6c,
                    (String.String (Coq_x6f, (String.String (Coq_x72,
                    String.EmptyString))))))) PrimInt63lor
                    (KernameMap.add (primInt63ModPath, (String.String
                      (Coq_x6c, (String.String (Coq_x78, (String.String
                      (Coq_x6f, (String.String (Coq_x72,
                      String.EmptyString))))))))) PrimInt63lxor
                      (KernameMap.add (primInt63ModPath, (String.String
                        (Coq_x65, (String.String (Coq_x71, (String.String
                        (Coq_x62, String.EmptyString))))))) PrimInt63eqb
                        (KernameMap.add (primInt63ModPath, (String.String
                          (Coq_x6c, (String.String (Coq_x74, (String.String
                          (Coq_x62, String.EmptyString))))))) PrimInt63ltb
                          (KernameMap.add (primInt63ModPath, (String.String
                            (Coq_x6c, (String.String (Coq_x65, (String.String
                            (Coq_x62, String.EmptyString))))))) PrimInt63leb
                            (KernameMap.add (primInt63ModPath, (String.String
                              (Coq_x63, (String.String (Coq_x6f,
                              (String.String (Coq_x6d, (String.String
                              (Coq_x70, (String.String (Coq_x61,
                              (String.String (Coq_x72, (String.String
                              (Coq_x65, String.EmptyString)))))))))))))))
                              PrimInt63compare
                              (KernameMap.add (primInt63ModPath,
                                (String.String (Coq_x61, (String.String
                                (Coq_x64, (String.String (Coq_x64,
                                (String.String (Coq_x63,
                                String.EmptyString))))))))) PrimInt63addc
                                (KernameMap.add (primInt63ModPath,
                                  (String.String (Coq_x61, (String.String
                                  (Coq_x64, (String.String (Coq_x64,
                                  (String.String (Coq_x63, (String.String
                                  (Coq_x61, (String.String (Coq_x72,
                                  (String.String (Coq_x72, (String.String
                                  (Coq_x79, (String.String (Coq_x63,
                                  String.EmptyString)))))))))))))))))))
                                  PrimInt63addcarryc
                                  (KernameMap.add (primInt63ModPath,
                                    (String.String (Coq_x73, (String.String
                                    (Coq_x75, (String.String (Coq_x62,
                                    (String.String (Coq_x63,
                                    String.EmptyString))))))))) PrimInt63subc
                                    (KernameMap.add (primInt63ModPath,
                                      (String.String (Coq_x73, (String.String
                                      (Coq_x75, (String.String (Coq_x62,
                                      (String.String (Coq_x63, (String.String
                                      (Coq_x61, (String.String (Coq_x72,
                                      (String.String (Coq_x72, (String.String
                                      (Coq_x79, (String.String (Coq_x63,
                                      String.EmptyString)))))))))))))))))))
                                      PrimInt63subcarryc
                                      (KernameMap.add (primInt63ModPath,
                                        (String.String (Coq_x6d,
                                        (String.String (Coq_x75,
                                        (String.String (Coq_x6c,
                                        (String.String (Coq_x63,
                                        String.EmptyString)))))))))
                                        PrimInt63mulc
                                        (KernameMap.add (primInt63ModPath,
                                          (String.String (Coq_x68,
                                          (String.String (Coq_x65,
                                          (String.String (Coq_x61,
                                          (String.String (Coq_x64,
                                          (String.String (Coq_x30,
                                          String.EmptyString)))))))))))
                                          PrimInt63head0
                                          (KernameMap.add (primInt63ModPath,
                                            (String.String (Coq_x74,
                                            (String.String (Coq_x61,
                                            (String.String (Coq_x69,
                                            (String.String (Coq_x6c,
                                            (String.String (Coq_x30,
                                            String.EmptyString)))))))))))
                                            PrimInt63tail0
                                            (KernameMap.add
                                              (primInt63ModPath,
                                              (String.String (Coq_x64,
                                              (String.String (Coq_x69,
                                              (String.String (Coq_x76,
                                              (String.String (Coq_x65,
                                              (String.String (Coq_x75,
                                              (String.String (Coq_x63,
                                              (String.String (Coq_x6c,
                                              String.EmptyString)))))))))))))))
                                              PrimInt63diveucl
                                              (KernameMap.add
                                                (primInt63ModPath,
                                                (String.String (Coq_x64,
                                                (String.String (Coq_x69,
                                                (String.String (Coq_x76,
                                                (String.String (Coq_x65,
                                                (String.String (Coq_x75,
                                                (String.String (Coq_x63,
                                                (String.String (Coq_x6c,
                                                (String.String (Coq_x5f,
                                                (String.String (Coq_x32,
                                                (String.String (Coq_x31,
                                                String.EmptyString)))))))))))))))))))))
                                                PrimInt63diveucl_21
                                                (KernameMap.add
                                                  (primInt63ModPath,
                                                  (String.String (Coq_x61,
                                                  (String.String (Coq_x64,
                                                  (String.String (Coq_x64,
                                                  (String.String (Coq_x6d,
                                                  (String.String (Coq_x75,
                                                  (String.String (Coq_x6c,
                                                  (String.String (Coq_x64,
                                                  (String.String (Coq_x69,
                                                  (String.String (Coq_x76,
                                                  String.EmptyString)))))))))))))))))))
                                                  PrimInt63addmuldiv
                                                  KernameMap.empty)))))))))))))))))))))))

(** val load_local_i64 : localidx -> basic_instruction list **)

let load_local_i64 i =
  (BI_local_get i) :: ((BI_load (T_i64, None, { memarg_offset = N0;
    memarg_align = (Npos (Coq_xO Coq_xH)) })) :: [])

(** val increment_glob_mem_ptr :
    globalidx -> coq_N -> basic_instruction list **)

let increment_glob_mem_ptr glob_mem_ptr i =
  (BI_global_get glob_mem_ptr) :: ((BI_const_num (VAL_int32
    (Obj.magic Wasm_int.Int32.repr (Z.of_N i)))) :: ((BI_binop (T_i32,
    (Binop_i BOI_add))) :: ((BI_global_set glob_mem_ptr) :: [])))

(** val bitmask_instrs : basic_instruction list **)

let bitmask_instrs =
  (BI_const_num (coq_Z_to_i64val_co maxuint63)) :: ((BI_binop (T_i64,
    (Binop_i BOI_and))) :: [])

(** val apply_binop_and_store_i64 :
    globalidx -> binop_i -> localidx -> localidx -> bool -> basic_instruction
    list **)

let apply_binop_and_store_i64 glob_mem_ptr op x y apply_bitmask =
  (BI_global_get
    glob_mem_ptr) :: (app (load_local_i64 x)
                       (app (load_local_i64 y)
                         (app ((BI_binop (T_i64, (Binop_i op))) :: [])
                           (app
                             (if apply_bitmask then bitmask_instrs else [])
                             (app ((BI_store (T_i64, None, { memarg_offset =
                               N0; memarg_align = (Npos (Coq_xO
                               Coq_xH)) })) :: ((BI_global_get
                               glob_mem_ptr) :: []))
                               (increment_glob_mem_ptr glob_mem_ptr (Npos
                                 (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))))

(** val make_carry :
    globalidx -> coq_N -> globalidx -> basic_instruction list **)

let make_carry glob_mem_ptr ord gidx =
  app ((BI_global_get glob_mem_ptr) :: ((BI_global_get gidx) :: ((BI_store
    (T_i64, None, { memarg_offset = N0; memarg_align = (Npos (Coq_xO
    Coq_xH)) })) :: ((BI_global_get glob_mem_ptr) :: ((BI_const_num
    (VAL_int32 (Obj.magic Wasm_int.Int32.repr (Z.of_N ord)))) :: ((BI_store
    (T_i32, None, { memarg_offset = (Npos (Coq_xO (Coq_xO (Coq_xO Coq_xH))));
    memarg_align = (Npos (Coq_xO Coq_xH)) })) :: ((BI_global_get
    glob_mem_ptr) :: ((BI_global_get glob_mem_ptr) :: ((BI_store (T_i32,
    None, { memarg_offset = (Npos (Coq_xO (Coq_xO (Coq_xI Coq_xH))));
    memarg_align = (Npos (Coq_xO Coq_xH)) })) :: ((BI_global_get
    glob_mem_ptr) :: ((BI_const_num (VAL_int32
    (Obj.magic Wasm_int.Int32.repr
      (Z.of_nat (S (S (S (S (S (S (S (S O)))))))))))) :: ((BI_binop (T_i32,
    (Binop_i BOI_add))) :: []))))))))))))
    (increment_glob_mem_ptr glob_mem_ptr (Npos (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO Coq_xH))))))

(** val apply_add_carry_operation :
    globalidx -> globalidx -> localidx -> localidx -> bool ->
    basic_instruction list **)

let apply_add_carry_operation glob_mem_ptr glob_tmp1 x y addone =
  app (load_local_i64 x)
    (app (load_local_i64 y)
      (app ((BI_binop (T_i64, (Binop_i BOI_add))) :: [])
        (app
          (if addone
           then (BI_const_num
                  (coq_Z_to_i64val_co (Zpos Coq_xH))) :: ((BI_binop (T_i64,
                  (Binop_i BOI_add))) :: [])
           else [])
          (app bitmask_instrs
            (app ((BI_global_set glob_tmp1) :: ((BI_global_get
              glob_tmp1) :: []))
              (app (load_local_i64 x) ((BI_relop (T_i64, (Relop_i
                (let x0 = SX_U in if addone then ROI_le x0 else ROI_lt x0)))) :: ((BI_if
                ((BT_valtype (Some (T_num T_i32))),
                (make_carry glob_mem_ptr coq_C1_ord glob_tmp1),
                (make_carry glob_mem_ptr coq_C0_ord glob_tmp1))) :: []))))))))

(** val apply_sub_carry_operation :
    globalidx -> globalidx -> localidx -> localidx -> bool ->
    basic_instruction list **)

let apply_sub_carry_operation glob_mem_ptr glob_tmp1 x y subone =
  app (load_local_i64 x)
    (app (load_local_i64 y)
      (app ((BI_binop (T_i64, (Binop_i BOI_sub))) :: [])
        (app
          (if subone
           then (BI_const_num
                  (coq_Z_to_i64val_co (Zpos Coq_xH))) :: ((BI_binop (T_i64,
                  (Binop_i BOI_sub))) :: [])
           else [])
          (app bitmask_instrs
            (app ((BI_global_set glob_tmp1) :: [])
              (app (load_local_i64 y)
                (app (load_local_i64 x) ((BI_relop (T_i64, (Relop_i
                  (let x0 = SX_U in if subone then ROI_lt x0 else ROI_le x0)))) :: ((BI_if
                  ((BT_valtype (Some (T_num T_i32))),
                  (make_carry glob_mem_ptr coq_C0_ord glob_tmp1),
                  (make_carry glob_mem_ptr coq_C1_ord glob_tmp1))) :: [])))))))))

(** val make_product :
    globalidx -> coq_N -> coq_N -> basic_instruction list **)

let make_product glob_mem_ptr gidx1 gidx2 =
  app ((BI_global_get glob_mem_ptr) :: ((BI_global_get gidx1) :: ((BI_store
    (T_i64, None, { memarg_offset = N0; memarg_align = (Npos (Coq_xO
    Coq_xH)) })) :: ((BI_global_get glob_mem_ptr) :: ((BI_global_get
    gidx2) :: ((BI_store (T_i64, None, { memarg_offset = (Npos (Coq_xO
    (Coq_xO (Coq_xO Coq_xH)))); memarg_align = (Npos (Coq_xO
    Coq_xH)) })) :: ((BI_global_get glob_mem_ptr) :: ((BI_const_num
    (VAL_int32
    (Obj.magic Wasm_int.Int32.repr (Z.of_N pair_ord)))) :: ((BI_store (T_i32,
    None, { memarg_offset = (Npos (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    Coq_xH))))); memarg_align = (Npos (Coq_xO Coq_xH)) })) :: ((BI_global_get
    glob_mem_ptr) :: ((BI_global_get glob_mem_ptr) :: ((BI_store (T_i32,
    None, { memarg_offset = (Npos (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))); memarg_align = (Npos (Coq_xO Coq_xH)) })) :: ((BI_global_get
    glob_mem_ptr) :: ((BI_global_get glob_mem_ptr) :: ((BI_const_num
    (VAL_int32
    (Obj.magic Wasm_int.Int32.repr
      (Z.of_nat (S (S (S (S (S (S (S (S O)))))))))))) :: ((BI_binop (T_i32,
    (Binop_i BOI_add))) :: ((BI_store (T_i32, None, { memarg_offset = (Npos
    (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))); memarg_align = (Npos (Coq_xO
    Coq_xH)) })) :: ((BI_global_get glob_mem_ptr) :: ((BI_const_num
    (VAL_int32
    (Obj.magic Wasm_int.Int32.repr
      (Z.of_nat (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        O)))))))))))))))))))) :: ((BI_binop (T_i32, (Binop_i
    BOI_add))) :: []))))))))))))))))))))
    (increment_glob_mem_ptr glob_mem_ptr (Npos (Coq_xO (Coq_xO (Coq_xI
      (Coq_xI Coq_xH))))))

(** val make_boolean_valued_comparison :
    localidx -> localidx -> relop_i -> basic_instruction list **)

let make_boolean_valued_comparison x y relop =
  app (load_local_i64 x)
    (app (load_local_i64 y) ((BI_relop (T_i64, (Relop_i relop))) :: ((BI_if
      ((BT_valtype (Some (T_num T_i32))), ((BI_const_num (VAL_int32
      (Obj.magic Wasm_int.Int32.repr
        (Z.of_N (N.add (N.mul (Npos (Coq_xO Coq_xH)) true_ord) (Npos Coq_xH)))))) :: []),
      ((BI_const_num (VAL_int32
      (Obj.magic Wasm_int.Int32.repr
        (Z.of_N
          (N.add (N.mul (Npos (Coq_xO Coq_xH)) false_ord) (Npos Coq_xH)))))) :: []))) :: [])))

(** val compare_instrs : localidx -> localidx -> basic_instruction list **)

let compare_instrs x y =
  (BI_local_get x) :: ((BI_load (T_i64, None, { memarg_offset = N0;
    memarg_align = (Npos (Coq_xO Coq_xH)) })) :: ((BI_local_get
    y) :: ((BI_load (T_i64, None, { memarg_offset = N0; memarg_align = (Npos
    (Coq_xO Coq_xH)) })) :: ((BI_relop (T_i64, (Relop_i (ROI_lt
    SX_U)))) :: ((BI_if ((BT_valtype (Some (T_num T_i32))), ((BI_const_num
    (VAL_int32
    (Obj.magic Wasm_int.Int32.repr
      (Z.of_N (N.add (N.mul (Npos (Coq_xO Coq_xH)) coq_Lt_ord) (Npos Coq_xH)))))) :: []),
    (app (load_local_i64 x)
      (app (load_local_i64 y) ((BI_relop (T_i64, (Relop_i
        ROI_eq))) :: ((BI_if ((BT_valtype (Some (T_num T_i32))),
        ((BI_const_num (VAL_int32
        (Obj.magic Wasm_int.Int32.repr
          (Z.of_N
            (N.add (N.mul (Npos (Coq_xO Coq_xH)) coq_Eq_ord) (Npos Coq_xH)))))) :: []),
        ((BI_const_num (VAL_int32
        (Obj.magic Wasm_int.Int32.repr
          (Z.of_N
            (N.add (N.mul (Npos (Coq_xO Coq_xH)) coq_Gt_ord) (Npos Coq_xH)))))) :: []))) :: [])))))) :: [])))))

(** val div_instrs :
    globalidx -> localidx -> localidx -> basic_instruction list **)

let div_instrs glob_mem_ptr x y =
  (BI_global_get
    glob_mem_ptr) :: (app (load_local_i64 y)
                       (app ((BI_testop (T_i64, TO_eqz)) :: ((BI_if
                         ((BT_valtype (Some (T_num T_i64))), ((BI_const_num
                         (coq_Z_to_i64val_co Z0)) :: []),
                         (app (load_local_i64 x)
                           (app (load_local_i64 y) ((BI_binop (T_i64,
                             (Binop_i (BOI_div SX_U)))) :: []))))) :: ((BI_store
                         (T_i64, None, { memarg_offset = N0; memarg_align =
                         (Npos (Coq_xO Coq_xH)) })) :: ((BI_global_get
                         glob_mem_ptr) :: []))))
                         (increment_glob_mem_ptr glob_mem_ptr (Npos (Coq_xO
                           (Coq_xO (Coq_xO Coq_xH)))))))

(** val mod_instrs :
    globalidx -> localidx -> localidx -> basic_instruction list **)

let mod_instrs glob_mem_ptr x y =
  (BI_global_get
    glob_mem_ptr) :: (app (load_local_i64 y)
                       (app ((BI_testop (T_i64, TO_eqz)) :: ((BI_if
                         ((BT_valtype (Some (T_num T_i64))),
                         (load_local_i64 x),
                         (app (load_local_i64 x)
                           (app (load_local_i64 y) ((BI_binop (T_i64,
                             (Binop_i (BOI_rem SX_U)))) :: []))))) :: ((BI_store
                         (T_i64, None, { memarg_offset = N0; memarg_align =
                         (Npos (Coq_xO Coq_xH)) })) :: ((BI_global_get
                         glob_mem_ptr) :: []))))
                         (increment_glob_mem_ptr glob_mem_ptr (Npos (Coq_xO
                           (Coq_xO (Coq_xO Coq_xH)))))))

(** val shift_instrs :
    globalidx -> localidx -> localidx -> binop_i -> bool -> basic_instruction
    list **)

let shift_instrs glob_mem_ptr x y shiftop mask =
  (BI_global_get
    glob_mem_ptr) :: (app (load_local_i64 y)
                       (app ((BI_const_num
                         (coq_Z_to_i64val_co (Zpos (Coq_xI (Coq_xI (Coq_xI
                           (Coq_xI (Coq_xI Coq_xH)))))))) :: ((BI_relop
                         (T_i64, (Relop_i (ROI_lt SX_U)))) :: ((BI_if
                         ((BT_valtype (Some (T_num T_i64))),
                         (app (load_local_i64 x)
                           (app (load_local_i64 y) ((BI_binop (T_i64,
                             (Binop_i
                             shiftop))) :: (if mask
                                            then bitmask_instrs
                                            else [])))), ((BI_const_num
                         (coq_Z_to_i64val_co Z0)) :: []))) :: ((BI_store
                         (T_i64, None, { memarg_offset = N0; memarg_align =
                         (Npos (Coq_xO Coq_xH)) })) :: ((BI_global_get
                         glob_mem_ptr) :: [])))))
                         (increment_glob_mem_ptr glob_mem_ptr (Npos (Coq_xO
                           (Coq_xO (Coq_xO Coq_xH)))))))

(** val low32 : basic_instruction list **)

let low32 =
  (BI_const_num
    (coq_Z_to_i64val_co (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
      (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
      (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
      (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
      Coq_xH)))))))))))))))))))))))))))))))))) :: ((BI_binop (T_i64, (Binop_i
    BOI_and))) :: [])

(** val high32 : basic_instruction list **)

let high32 =
  (BI_const_num
    (coq_Z_to_i64val_co (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      Coq_xH)))))))) :: ((BI_binop (T_i64, (Binop_i (BOI_shr SX_U)))) :: [])

(** val mulc_instrs :
    globalidx -> globalidx -> globalidx -> globalidx -> globalidx -> localidx
    -> localidx -> basic_instruction list **)

let mulc_instrs glob_mem_ptr glob_tmp1 glob_tmp2 glob_tmp3 glob_tmp4 x y =
  app (load_local_i64 x)
    (app low32
      (app (load_local_i64 y)
        (app low32
          (app ((BI_binop (T_i64, (Binop_i BOI_mul))) :: ((BI_global_set
            glob_tmp1) :: []))
            (app (load_local_i64 x)
              (app high32
                (app (load_local_i64 y)
                  (app low32
                    (app ((BI_binop (T_i64, (Binop_i
                      BOI_mul))) :: ((BI_global_set glob_tmp2) :: []))
                      (app (load_local_i64 x)
                        (app low32
                          (app (load_local_i64 y)
                            (app high32
                              (app ((BI_binop (T_i64, (Binop_i
                                BOI_mul))) :: ((BI_global_set
                                glob_tmp3) :: []))
                                (app (load_local_i64 x)
                                  (app high32
                                    (app (load_local_i64 y)
                                      (app high32
                                        (app ((BI_binop (T_i64, (Binop_i
                                          BOI_mul))) :: ((BI_global_set
                                          glob_tmp4) :: []))
                                          (app ((BI_global_get
                                            glob_tmp1) :: [])
                                            (app high32
                                              (app ((BI_global_get
                                                glob_tmp2) :: [])
                                                (app low32
                                                  (app ((BI_binop (T_i64,
                                                    (Binop_i
                                                    BOI_add))) :: ((BI_global_get
                                                    glob_tmp3) :: ((BI_binop
                                                    (T_i64, (Binop_i
                                                    BOI_add))) :: ((BI_global_set
                                                    glob_tmp3) :: []))))
                                                    (app ((BI_global_get
                                                      glob_tmp2) :: [])
                                                      (app high32
                                                        (app ((BI_global_get
                                                          glob_tmp3) :: [])
                                                          (app high32
                                                            (app ((BI_binop
                                                              (T_i64,
                                                              (Binop_i
                                                              BOI_add))) :: [])
                                                              (app
                                                                ((BI_global_get
                                                                glob_tmp4) :: ((BI_binop
                                                                (T_i64,
                                                                (Binop_i
                                                                BOI_add))) :: ((BI_global_set
                                                                glob_tmp2) :: [])))
                                                                (app
                                                                  ((BI_global_get
                                                                  glob_tmp3) :: ((BI_const_num
                                                                  (coq_Z_to_i64val_co
                                                                    (Zpos
                                                                    (Coq_xO
                                                                    (Coq_xO
                                                                    (Coq_xO
                                                                    (Coq_xO
                                                                    (Coq_xO
                                                                    Coq_xH)))))))) :: ((BI_binop
                                                                  (T_i64,
                                                                  (Binop_i
                                                                  BOI_shl))) :: [])))
                                                                  (app
                                                                    ((BI_global_get
                                                                    glob_tmp1) :: [])
                                                                    (app
                                                                    low32
                                                                    (app
                                                                    ((BI_binop
                                                                    (T_i64,
                                                                    (Binop_i
                                                                    BOI_or))) :: ((BI_global_set
                                                                    glob_tmp1) :: []))
                                                                    (app
                                                                    ((BI_global_get
                                                                    glob_tmp2) :: ((BI_const_num
                                                                    (coq_Z_to_i64val_co
                                                                    (Zpos
                                                                    Coq_xH))) :: ((BI_binop
                                                                    (T_i64,
                                                                    (Binop_i
                                                                    BOI_shl))) :: ((BI_global_get
                                                                    glob_tmp1) :: ((BI_const_num
                                                                    (coq_Z_to_i64val_co
                                                                    (Zpos
                                                                    (Coq_xI
                                                                    (Coq_xI
                                                                    (Coq_xI
                                                                    (Coq_xI
                                                                    (Coq_xI
                                                                    Coq_xH)))))))) :: ((BI_binop
                                                                    (T_i64,
                                                                    (Binop_i
                                                                    (BOI_shr
                                                                    SX_U)))) :: ((BI_binop
                                                                    (T_i64,
                                                                    (Binop_i
                                                                    BOI_add))) :: ((BI_global_set
                                                                    glob_tmp2) :: []))))))))
                                                                    (app
                                                                    ((BI_global_get
                                                                    glob_tmp1) :: ((BI_const_num
                                                                    (coq_Z_to_i64val_co
                                                                    maxuint63)) :: ((BI_binop
                                                                    (T_i64,
                                                                    (Binop_i
                                                                    BOI_and))) :: ((BI_global_set
                                                                    glob_tmp1) :: []))))
                                                                    (make_product
                                                                    glob_mem_ptr
                                                                    glob_tmp2
                                                                    glob_tmp1)))))))))))))))))))))))))))))))))))))

(** val diveucl_instrs :
    globalidx -> globalidx -> globalidx -> localidx -> localidx ->
    basic_instruction list **)

let diveucl_instrs glob_mem_ptr glob_tmp1 glob_tmp2 x y =
  app ((BI_local_get x) :: ((BI_load (T_i64, None, { memarg_offset = N0;
    memarg_align = (Npos (Coq_xO Coq_xH)) })) :: ((BI_testop (T_i64,
    TO_eqz)) :: ((BI_if ((BT_valtype None), ((BI_const_num (VAL_int64
    (Obj.magic Wasm_int.Int64.repr Z0))) :: ((BI_global_set
    glob_tmp1) :: ((BI_const_num (coq_Z_to_i64val_co Z0)) :: ((BI_global_set
    glob_tmp2) :: [])))), ((BI_local_get y) :: ((BI_load (T_i64, None,
    { memarg_offset = N0; memarg_align = (Npos (Coq_xO
    Coq_xH)) })) :: ((BI_testop (T_i64, TO_eqz)) :: ((BI_if ((BT_valtype
    None), ((BI_const_num (VAL_int64
    (Obj.magic Wasm_int.Int64.repr Z0))) :: ((BI_global_set
    glob_tmp1) :: ((BI_local_get x) :: ((BI_load (T_i64, None,
    { memarg_offset = N0; memarg_align = (Npos (Coq_xO
    Coq_xH)) })) :: ((BI_global_set glob_tmp2) :: []))))),
    (app (load_local_i64 x)
      (app (load_local_i64 y)
        (app ((BI_binop (T_i64, (Binop_i (BOI_div SX_U)))) :: ((BI_global_set
          glob_tmp1) :: []))
          (app (load_local_i64 x)
            (app (load_local_i64 y) ((BI_binop (T_i64, (Binop_i (BOI_rem
              SX_U)))) :: ((BI_global_set glob_tmp2) :: []))))))))) :: [])))))) :: []))))
    (make_product glob_mem_ptr glob_tmp1 glob_tmp2)

(** val translate_primitive_binary_op :
    globalidx -> globalidx -> globalidx -> globalidx -> globalidx -> primop
    -> localidx -> localidx -> basic_instruction list error **)

let translate_primitive_binary_op glob_mem_ptr glob_tmp1 glob_tmp2 glob_tmp3 glob_tmp4 op x y =
  match op with
  | PrimInt63add ->
    Ret (apply_binop_and_store_i64 glob_mem_ptr BOI_add x y true)
  | PrimInt63sub ->
    Ret (apply_binop_and_store_i64 glob_mem_ptr BOI_sub x y true)
  | PrimInt63mul ->
    Ret (apply_binop_and_store_i64 glob_mem_ptr BOI_mul x y true)
  | PrimInt63div -> Ret (div_instrs glob_mem_ptr x y)
  | PrimInt63mod -> Ret (mod_instrs glob_mem_ptr x y)
  | PrimInt63lsl -> Ret (shift_instrs glob_mem_ptr x y BOI_shl true)
  | PrimInt63lsr -> Ret (shift_instrs glob_mem_ptr x y (BOI_shr SX_U) false)
  | PrimInt63land ->
    Ret (apply_binop_and_store_i64 glob_mem_ptr BOI_and x y false)
  | PrimInt63lor ->
    Ret (apply_binop_and_store_i64 glob_mem_ptr BOI_or x y false)
  | PrimInt63lxor ->
    Ret (apply_binop_and_store_i64 glob_mem_ptr BOI_xor x y false)
  | PrimInt63eqb -> Ret (make_boolean_valued_comparison x y ROI_eq)
  | PrimInt63ltb -> Ret (make_boolean_valued_comparison x y (ROI_lt SX_U))
  | PrimInt63leb -> Ret (make_boolean_valued_comparison x y (ROI_le SX_U))
  | PrimInt63compare -> Ret (compare_instrs x y)
  | PrimInt63addc ->
    Ret (apply_add_carry_operation glob_mem_ptr glob_tmp1 x y false)
  | PrimInt63addcarryc ->
    Ret (apply_add_carry_operation glob_mem_ptr glob_tmp1 x y true)
  | PrimInt63subc ->
    Ret (apply_sub_carry_operation glob_mem_ptr glob_tmp1 x y false)
  | PrimInt63subcarryc ->
    Ret (apply_sub_carry_operation glob_mem_ptr glob_tmp1 x y true)
  | PrimInt63mulc ->
    Ret (mulc_instrs glob_mem_ptr glob_tmp1 glob_tmp2 glob_tmp3 glob_tmp4 x y)
  | PrimInt63diveucl ->
    Ret (diveucl_instrs glob_mem_ptr glob_tmp1 glob_tmp2 x y)
  | _ ->
    Err (String.String (Coq_x55, (String.String (Coq_x6e, (String.String
      (Coq_x6b, (String.String (Coq_x6e, (String.String (Coq_x6f,
      (String.String (Coq_x77, (String.String (Coq_x6e, (String.String
      (Coq_x20, (String.String (Coq_x70, (String.String (Coq_x72,
      (String.String (Coq_x69, (String.String (Coq_x6d, (String.String
      (Coq_x69, (String.String (Coq_x74, (String.String (Coq_x69,
      (String.String (Coq_x76, (String.String (Coq_x65, (String.String
      (Coq_x20, (String.String (Coq_x62, (String.String (Coq_x69,
      (String.String (Coq_x6e, (String.String (Coq_x61, (String.String
      (Coq_x72, (String.String (Coq_x79, (String.String (Coq_x20,
      (String.String (Coq_x6f, (String.String (Coq_x70, (String.String
      (Coq_x65, (String.String (Coq_x72, (String.String (Coq_x61,
      (String.String (Coq_x74, (String.String (Coq_x6f, (String.String
      (Coq_x72,
      String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val head0_instrs : globalidx -> localidx -> basic_instruction list **)

let head0_instrs glob_mem_ptr x =
  (BI_global_get
    glob_mem_ptr) :: (app (load_local_i64 x)
                       (app ((BI_unop (T_i64, (Unop_i
                         UOI_clz))) :: ((BI_const_num
                         (coq_Z_to_i64val_co (Zpos Coq_xH))) :: ((BI_binop
                         (T_i64, (Binop_i BOI_sub))) :: ((BI_store (T_i64,
                         None, { memarg_offset = N0; memarg_align = (Npos
                         (Coq_xO Coq_xH)) })) :: ((BI_global_get
                         glob_mem_ptr) :: [])))))
                         (increment_glob_mem_ptr glob_mem_ptr (Npos (Coq_xO
                           (Coq_xO (Coq_xO Coq_xH)))))))

(** val tail0_instrs : globalidx -> localidx -> basic_instruction list **)

let tail0_instrs glob_mem_ptr x =
  (BI_global_get
    glob_mem_ptr) :: (app (load_local_i64 x)
                       (app ((BI_testop (T_i64, TO_eqz)) :: ((BI_if
                         ((BT_valtype (Some (T_num T_i64))), ((BI_const_num
                         (coq_Z_to_i64val_co (Zpos (Coq_xI (Coq_xI (Coq_xI
                           (Coq_xI (Coq_xI Coq_xH)))))))) :: []),
                         (app (load_local_i64 x) ((BI_unop (T_i64, (Unop_i
                           UOI_ctz))) :: [])))) :: ((BI_store (T_i64, None,
                         { memarg_offset = N0; memarg_align = (Npos (Coq_xO
                         Coq_xH)) })) :: ((BI_global_get
                         glob_mem_ptr) :: []))))
                         (increment_glob_mem_ptr glob_mem_ptr (Npos (Coq_xO
                           (Coq_xO (Coq_xO Coq_xH)))))))

(** val translate_primitive_unary_op :
    globalidx -> primop -> localidx -> basic_instruction list error **)

let translate_primitive_unary_op glob_mem_ptr op x =
  match op with
  | PrimInt63head0 -> Ret (head0_instrs glob_mem_ptr x)
  | PrimInt63tail0 -> Ret (tail0_instrs glob_mem_ptr x)
  | _ ->
    Err (String.String (Coq_x55, (String.String (Coq_x6e, (String.String
      (Coq_x6b, (String.String (Coq_x6e, (String.String (Coq_x6f,
      (String.String (Coq_x77, (String.String (Coq_x6e, (String.String
      (Coq_x20, (String.String (Coq_x70, (String.String (Coq_x72,
      (String.String (Coq_x69, (String.String (Coq_x6d, (String.String
      (Coq_x69, (String.String (Coq_x74, (String.String (Coq_x69,
      (String.String (Coq_x76, (String.String (Coq_x65, (String.String
      (Coq_x20, (String.String (Coq_x75, (String.String (Coq_x6e,
      (String.String (Coq_x61, (String.String (Coq_x72, (String.String
      (Coq_x79, (String.String (Coq_x20, (String.String (Coq_x6f,
      (String.String (Coq_x70, (String.String (Coq_x65, (String.String
      (Coq_x72, (String.String (Coq_x61, (String.String (Coq_x74,
      (String.String (Coq_x6f, (String.String (Coq_x72,
      String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val diveucl_21_loop_body :
    globalidx -> globalidx -> globalidx -> globalidx -> basic_instruction list **)

let diveucl_21_loop_body glob_xh glob_xl glob_y glob_q =
  (BI_global_get glob_xl) :: ((BI_const_num
    (coq_Z_to_i64val_co (Zpos Coq_xH))) :: ((BI_binop (T_i64, (Binop_i
    BOI_shl))) :: ((BI_global_set glob_xl) :: ((BI_global_get
    glob_xh) :: ((BI_const_num
    (coq_Z_to_i64val_co (Zpos Coq_xH))) :: ((BI_binop (T_i64, (Binop_i
    BOI_shl))) :: ((BI_global_get glob_xl) :: ((BI_const_num
    (coq_Z_to_i64val_co (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
      Coq_xH)))))))) :: ((BI_binop (T_i64, (Binop_i (BOI_shr
    SX_U)))) :: ((BI_binop (T_i64, (Binop_i BOI_or))) :: ((BI_global_set
    glob_xh) :: ((BI_global_get glob_q) :: ((BI_const_num
    (coq_Z_to_i64val_co (Zpos Coq_xH))) :: ((BI_binop (T_i64, (Binop_i
    BOI_shl))) :: ((BI_global_set glob_q) :: ((BI_global_get
    glob_xh) :: ((BI_global_get glob_y) :: ((BI_relop (T_i64, (Relop_i
    (ROI_ge SX_U)))) :: ((BI_if ((BT_valtype None),
    (app ((BI_global_get glob_q) :: ((BI_const_num
      (coq_Z_to_i64val_co (Zpos Coq_xH))) :: ((BI_binop (T_i64, (Binop_i
      BOI_or))) :: ((BI_global_set glob_q) :: [])))) ((BI_global_get
      glob_xh) :: ((BI_global_get glob_y) :: ((BI_binop (T_i64, (Binop_i
      BOI_sub))) :: ((BI_global_set glob_xh) :: []))))),
    [])) :: [])))))))))))))))))))

(** val diveucl_21_loop :
    globalidx -> globalidx -> globalidx -> globalidx -> globalidx -> coq_Z ->
    basic_instruction list **)

let diveucl_21_loop loop_counter glob_xh glob_xl glob_y glob_q iterations =
  (BI_global_get loop_counter) :: ((BI_const_num (VAL_int32
    (Obj.magic Wasm_int.Int32.repr iterations))) :: ((BI_relop (T_i32,
    (Relop_i (ROI_lt SX_U)))) :: ((BI_if ((BT_valtype None),
    (app (diveucl_21_loop_body glob_xh glob_xl glob_y glob_q) ((BI_global_get
      loop_counter) :: ((BI_const_num (VAL_int32
      (Obj.magic Wasm_int.Int32.repr (Zpos Coq_xH)))) :: ((BI_binop (T_i32,
      (Binop_i BOI_add))) :: ((BI_global_set loop_counter) :: ((BI_br (Npos
      Coq_xH)) :: [])))))), [])) :: [])))

(** val diveucl_21_instrs :
    globalidx -> globalidx -> globalidx -> globalidx -> globalidx ->
    globalidx -> localidx -> localidx -> localidx -> basic_instruction list **)

let diveucl_21_instrs glob_mem_ptr glob_tmp1 glob_tmp2 glob_tmp3 glob_tmp4 loop_counter xh xl y =
  app (load_local_i64 y)
    (app (load_local_i64 xh) ((BI_relop (T_i64, (Relop_i (ROI_le
      SX_U)))) :: ((BI_if ((BT_valtype (Some (T_num T_i32))),
      (app ((BI_const_num (coq_Z_to_i64val_co Z0)) :: ((BI_global_set
        glob_tmp1) :: [])) (make_product glob_mem_ptr glob_tmp1 glob_tmp1)),
      (app (load_local_i64 xh)
        (app ((BI_global_set glob_tmp1) :: [])
          (app (load_local_i64 xl)
            (app ((BI_global_set glob_tmp2) :: [])
              (app (load_local_i64 y)
                (app ((BI_global_set glob_tmp3) :: [])
                  (app ((BI_const_num (VAL_int64
                    (Obj.magic Wasm_int.Int64.repr Z0))) :: ((BI_global_set
                    glob_tmp4) :: ((BI_const_num (VAL_int32
                    (Obj.magic Wasm_int.Int32.repr Z0))) :: ((BI_global_set
                    loop_counter) :: ((BI_loop ((BT_valtype None),
                    (diveucl_21_loop loop_counter glob_tmp1 glob_tmp2
                      glob_tmp3 glob_tmp4 (Zpos (Coq_xI (Coq_xI (Coq_xI
                      (Coq_xI (Coq_xI Coq_xH))))))))) :: [])))))
                    (make_product glob_mem_ptr glob_tmp4 glob_tmp1)))))))))) :: [])))

(** val addmuldiv_instrs :
    globalidx -> localidx -> localidx -> localidx -> basic_instruction list **)

let addmuldiv_instrs glob_mem_ptr p x y =
  (BI_global_get
    glob_mem_ptr) :: (app (load_local_i64 p)
                       (app ((BI_const_num
                         (coq_Z_to_i64val_co (Zpos (Coq_xI (Coq_xI (Coq_xI
                           (Coq_xI (Coq_xI Coq_xH)))))))) :: ((BI_relop
                         (T_i64, (Relop_i (ROI_gt SX_U)))) :: ((BI_if
                         ((BT_valtype (Some (T_num T_i64))), ((BI_const_num
                         (coq_Z_to_i64val_co Z0)) :: []),
                         (app (load_local_i64 x)
                           (app (load_local_i64 p)
                             (app ((BI_binop (T_i64, (Binop_i
                               BOI_shl))) :: [])
                               (app (load_local_i64 y)
                                 (app ((BI_const_num
                                   (coq_Z_to_i64val_co (Zpos (Coq_xI (Coq_xI
                                     (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))))) :: [])
                                   (app (load_local_i64 p) ((BI_binop (T_i64,
                                     (Binop_i BOI_sub))) :: ((BI_binop
                                     (T_i64, (Binop_i (BOI_shr
                                     SX_U)))) :: ((BI_binop (T_i64, (Binop_i
                                     BOI_or))) :: ((BI_const_num
                                     (coq_Z_to_i64val_co maxuint63)) :: ((BI_binop
                                     (T_i64, (Binop_i BOI_and))) :: []))))))))))))) :: ((BI_store
                         (T_i64, None, { memarg_offset = N0; memarg_align =
                         (Npos (Coq_xO Coq_xH)) })) :: ((BI_global_get
                         glob_mem_ptr) :: [])))))
                         (increment_glob_mem_ptr glob_mem_ptr (Npos (Coq_xO
                           (Coq_xO (Coq_xO Coq_xH)))))))

(** val translate_primitive_ternary_op :
    globalidx -> globalidx -> globalidx -> globalidx -> globalidx ->
    globalidx -> primop -> localidx -> localidx -> localidx ->
    basic_instruction list error **)

let translate_primitive_ternary_op glob_mem_ptr glob_tmp1 glob_tmp2 glob_tmp3 glob_tmp4 loop_counter op x y z =
  match op with
  | PrimInt63diveucl_21 ->
    Ret
      (diveucl_21_instrs glob_mem_ptr glob_tmp1 glob_tmp2 glob_tmp3 glob_tmp4
        loop_counter x y z)
  | PrimInt63addmuldiv -> Ret (addmuldiv_instrs glob_mem_ptr x y z)
  | _ ->
    Err (String.String (Coq_x55, (String.String (Coq_x6e, (String.String
      (Coq_x6b, (String.String (Coq_x6e, (String.String (Coq_x6f,
      (String.String (Coq_x77, (String.String (Coq_x6e, (String.String
      (Coq_x20, (String.String (Coq_x70, (String.String (Coq_x72,
      (String.String (Coq_x69, (String.String (Coq_x6d, (String.String
      (Coq_x69, (String.String (Coq_x74, (String.String (Coq_x69,
      (String.String (Coq_x76, (String.String (Coq_x65, (String.String
      (Coq_x20, (String.String (Coq_x74, (String.String (Coq_x65,
      (String.String (Coq_x72, (String.String (Coq_x6e, (String.String
      (Coq_x61, (String.String (Coq_x72, (String.String (Coq_x79,
      (String.String (Coq_x20, (String.String (Coq_x6f, (String.String
      (Coq_x70, (String.String (Coq_x65, (String.String (Coq_x72,
      (String.String (Coq_x61, (String.String (Coq_x74, (String.String
      (Coq_x6f, (String.String (Coq_x72,
      String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
