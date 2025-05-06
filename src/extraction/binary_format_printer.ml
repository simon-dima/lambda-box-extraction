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

(** val binary_of_number_type : number_type -> Byte.byte **)

let binary_of_number_type = function
| T_i32 -> Coq_x7f
| T_i64 -> Coq_x7e
| T_f32 -> Coq_x7d
| T_f64 -> Coq_x7c

(** val binary_of_vector_type : vector_type -> Byte.byte **)

let binary_of_vector_type _ =
  Coq_x7b

(** val binary_of_reference_type : reference_type -> Byte.byte **)

let binary_of_reference_type = function
| T_funcref -> Coq_x70
| T_externref -> Coq_x6f

(** val binary_of_value_type : value_type -> Byte.byte **)

let binary_of_value_type = function
| T_num t' -> binary_of_number_type t'
| T_vec t' -> binary_of_vector_type t'
| T_ref t' -> binary_of_reference_type t'
| T_bot -> Coq_x00

(** val binary_of_u32 : coq_N -> Byte.byte list **)

let binary_of_u32 =
  encode_unsigned

(** val binary_of_u32_nat : nat -> Byte.byte list **)

let binary_of_u32_nat n =
  encode_unsigned (N.of_nat n)

(** val binary_of_idx : coq_N -> Byte.byte list **)

let binary_of_idx =
  binary_of_u32

(** val binary_of_typeidx : typeidx -> Byte.byte list **)

let binary_of_typeidx =
  binary_of_idx

(** val binary_of_funcidx : funcidx -> Byte.byte list **)

let binary_of_funcidx =
  binary_of_idx

(** val binary_of_tableidx : tableidx -> Byte.byte list **)

let binary_of_tableidx =
  binary_of_idx

(** val binary_of_memidx : memidx -> Byte.byte list **)

let binary_of_memidx =
  binary_of_idx

(** val binary_of_globalidx : globalidx -> Byte.byte list **)

let binary_of_globalidx =
  binary_of_idx

(** val binary_of_vec :
    ('a1 -> Byte.byte list) -> 'a1 list -> Byte.byte list **)

let binary_of_vec f es =
  cat (binary_of_u32_nat (length es)) (concat (List0.map f es))

(** val binary_of_memarg : memarg -> Byte.byte list **)

let binary_of_memarg marg =
  cat (binary_of_u32_nat (nat_of_bin marg.memarg_align))
    (binary_of_u32_nat (nat_of_bin marg.memarg_offset))

(** val binary_of_i32 : Equality.sort -> Byte.byte list **)

let binary_of_i32 x =
  encode_signed (Wasm_int.Int32.signed (Obj.magic x))

(** val binary_of_i64 : Equality.sort -> Byte.byte list **)

let binary_of_i64 x =
  encode_signed (Wasm_int.Int64.signed (Obj.magic x))

(** val binary_of_f32 : Equality.sort -> Byte.byte list **)

let binary_of_f32 x =
  List0.map byte_of_compcert_byte (serialise_f32 x)

(** val binary_of_f64 : Equality.sort -> Byte.byte list **)

let binary_of_f64 x =
  List0.map byte_of_compcert_byte (serialise_f64 x)

(** val binary_of_block_type : block_type -> Byte.byte list **)

let binary_of_block_type = function
| BT_id x -> binary_of_idx x
| BT_valtype o ->
  (match o with
   | Some t -> (binary_of_value_type t) :: []
   | None -> Coq_x40 :: [])

(** val binary_of_value_types : value_type list -> Byte.byte list **)

let binary_of_value_types bt =
  binary_of_vec (fun v -> (binary_of_value_type v) :: []) bt

(** val binary_of_result_type : value_type list -> Byte.byte list **)

let binary_of_result_type rt =
  binary_of_vec (fun v -> (binary_of_value_type v) :: []) rt

(** val dummy : Byte.byte list **)

let dummy =
  Coq_x00 :: (Coq_x00 :: (Coq_x00 :: []))

(** val binary_of_valvec : value_vec -> Byte.byte list **)

let binary_of_valvec _ =
  Coq_xfd :: (Coq_x0c :: (repeat Coq_x00 (S (S (S (S (S (S (S (S (S (S (S (S
                           (S (S (S (S O))))))))))))))))))

(** val binary_of_unop_vec : unop_vec -> Byte.byte list **)

let binary_of_unop_vec _ =
  Coq_xfd :: (Coq_x0c :: (repeat Coq_x00 (S (S (S (S (S (S (S (S (S (S (S (S
                           (S (S (S (S O))))))))))))))))))

(** val binary_of_binop_vec : binop_vec -> Byte.byte list **)

let binary_of_binop_vec _ =
  Coq_xfd :: (Coq_x0c :: (repeat Coq_x00 (S (S (S (S (S (S (S (S (S (S (S (S
                           (S (S (S (S O))))))))))))))))))

(** val binary_of_ternop_vec : ternop_vec -> Byte.byte list **)

let binary_of_ternop_vec _ =
  Coq_xfd :: (Coq_x0c :: (repeat Coq_x00 (S (S (S (S (S (S (S (S (S (S (S (S
                           (S (S (S (S O))))))))))))))))))

(** val binary_of_test_vec : test_vec -> Byte.byte list **)

let binary_of_test_vec _ =
  Coq_xfd :: (Coq_x0c :: (repeat Coq_x00 (S (S (S (S (S (S (S (S (S (S (S (S
                           (S (S (S (S O))))))))))))))))))

(** val binary_of_shift_vec : shift_vec -> Byte.byte list **)

let binary_of_shift_vec _ =
  Coq_xfd :: (Coq_x0c :: (repeat Coq_x00 (S (S (S (S (S (S (S (S (S (S (S (S
                           (S (S (S (S O))))))))))))))))))

(** val binary_of_splat_vec : shape_vec -> Byte.byte list **)

let binary_of_splat_vec _ =
  Coq_xfd :: (Coq_x0c :: (repeat Coq_x00 (S (S (S (S (S (S (S (S (S (S (S (S
                           (S (S (S (S O))))))))))))))))))

(** val binary_of_extract_vec :
    shape_vec -> sx option -> laneidx -> Byte.byte list **)

let binary_of_extract_vec _ _ _ =
  Coq_xfd :: (Coq_x0c :: (repeat Coq_x00 (S (S (S (S (S (S (S (S (S (S (S (S
                           (S (S (S (S O))))))))))))))))))

(** val binary_of_replace_vec : shape_vec -> laneidx -> Byte.byte list **)

let binary_of_replace_vec _ _ =
  Coq_xfd :: (Coq_x0c :: (repeat Coq_x00 (S (S (S (S (S (S (S (S (S (S (S (S
                           (S (S (S (S O))))))))))))))))))

(** val binary_of_load_vec : load_vec_arg -> memarg -> Byte.byte list **)

let binary_of_load_vec _ _ =
  Coq_xfd :: (Coq_x0c :: (repeat Coq_x00 (S (S (S (S (S (S (S (S (S (S (S (S
                           (S (S (S (S O))))))))))))))))))

(** val binary_of_load_vec_lane :
    width_vec -> memarg -> laneidx -> Byte.byte list **)

let binary_of_load_vec_lane _ _ _ =
  Coq_xfd :: (Coq_x0c :: (repeat Coq_x00 (S (S (S (S (S (S (S (S (S (S (S (S
                           (S (S (S (S O))))))))))))))))))

(** val binary_of_store_vec_lane :
    width_vec -> memarg -> laneidx -> Byte.byte list **)

let binary_of_store_vec_lane _ _ _ =
  Coq_xfd :: (Coq_x0c :: (repeat Coq_x00 (S (S (S (S (S (S (S (S (S (S (S (S
                           (S (S (S (S O))))))))))))))))))

(** val binary_of_be : basic_instruction -> Byte.byte list **)

let rec binary_of_be be =
  let binary_of_instrs = fun bes -> concat (List0.map binary_of_be bes) in
  (match be with
   | BI_const_num v ->
     (match v with
      | VAL_int32 x -> Coq_x41 :: (binary_of_i32 x)
      | VAL_int64 x -> Coq_x42 :: (binary_of_i64 x)
      | VAL_float32 x -> Coq_x43 :: (binary_of_f32 x)
      | VAL_float64 x -> Coq_x44 :: (binary_of_f64 x))
   | BI_unop (n, u) ->
     (match n with
      | T_i32 ->
        (match u with
         | Unop_i u0 ->
           (match u0 with
            | UOI_clz -> Coq_x67 :: []
            | UOI_ctz -> Coq_x68 :: []
            | UOI_popcnt -> Coq_x69 :: [])
         | Unop_f _ -> dummy
         | Unop_extend n0 ->
           (match n0 with
            | N0 -> dummy
            | Npos p ->
              (match p with
               | Coq_xO p0 ->
                 (match p0 with
                  | Coq_xO p1 ->
                    (match p1 with
                     | Coq_xO p2 ->
                       (match p2 with
                        | Coq_xI _ -> dummy
                        | Coq_xO p3 ->
                          (match p3 with
                           | Coq_xH -> Coq_xc1 :: []
                           | _ -> dummy)
                        | Coq_xH -> Coq_xc0 :: [])
                     | _ -> dummy)
                  | _ -> dummy)
               | _ -> dummy)))
      | T_i64 ->
        (match u with
         | Unop_i u0 ->
           (match u0 with
            | UOI_clz -> Coq_x79 :: []
            | UOI_ctz -> Coq_x7a :: []
            | UOI_popcnt -> Coq_x7b :: [])
         | Unop_f _ -> dummy
         | Unop_extend n0 ->
           (match n0 with
            | N0 -> dummy
            | Npos p ->
              (match p with
               | Coq_xO p0 ->
                 (match p0 with
                  | Coq_xO p1 ->
                    (match p1 with
                     | Coq_xO p2 ->
                       (match p2 with
                        | Coq_xI _ -> dummy
                        | Coq_xO p3 ->
                          (match p3 with
                           | Coq_xI _ -> dummy
                           | Coq_xO p4 ->
                             (match p4 with
                              | Coq_xH -> Coq_xc4 :: []
                              | _ -> dummy)
                           | Coq_xH -> Coq_xc3 :: [])
                        | Coq_xH -> Coq_xc2 :: [])
                     | _ -> dummy)
                  | _ -> dummy)
               | _ -> dummy)))
      | T_f32 ->
        (match u with
         | Unop_f u0 ->
           (match u0 with
            | UOF_abs -> Coq_x8b :: []
            | UOF_neg -> Coq_x8c :: []
            | UOF_sqrt -> Coq_x91 :: []
            | UOF_ceil -> Coq_x8d :: []
            | UOF_floor -> Coq_x8e :: []
            | UOF_trunc -> Coq_x8f :: []
            | UOF_nearest -> Coq_x90 :: [])
         | _ -> dummy)
      | T_f64 ->
        (match u with
         | Unop_f u0 ->
           (match u0 with
            | UOF_abs -> Coq_x99 :: []
            | UOF_neg -> Coq_x9a :: []
            | UOF_sqrt -> Coq_x9f :: []
            | UOF_ceil -> Coq_x9b :: []
            | UOF_floor -> Coq_x9c :: []
            | UOF_trunc -> Coq_x9d :: []
            | UOF_nearest -> Coq_x9e :: [])
         | _ -> dummy))
   | BI_binop (n, b) ->
     (match n with
      | T_i32 ->
        (match b with
         | Binop_i b0 ->
           (match b0 with
            | BOI_add -> Coq_x6a :: []
            | BOI_sub -> Coq_x6b :: []
            | BOI_mul -> Coq_x6c :: []
            | BOI_div s ->
              (match s with
               | SX_S -> Coq_x6d :: []
               | SX_U -> Coq_x6e :: [])
            | BOI_rem s ->
              (match s with
               | SX_S -> Coq_x6f :: []
               | SX_U -> Coq_x70 :: [])
            | BOI_and -> Coq_x71 :: []
            | BOI_or -> Coq_x72 :: []
            | BOI_xor -> Coq_x73 :: []
            | BOI_shl -> Coq_x74 :: []
            | BOI_shr s ->
              (match s with
               | SX_S -> Coq_x75 :: []
               | SX_U -> Coq_x76 :: [])
            | BOI_rotl -> Coq_x77 :: []
            | BOI_rotr -> Coq_x78 :: [])
         | Binop_f _ -> dummy)
      | T_i64 ->
        (match b with
         | Binop_i b0 ->
           (match b0 with
            | BOI_add -> Coq_x7c :: []
            | BOI_sub -> Coq_x7d :: []
            | BOI_mul -> Coq_x7e :: []
            | BOI_div s ->
              (match s with
               | SX_S -> Coq_x7f :: []
               | SX_U -> Coq_x80 :: [])
            | BOI_rem s ->
              (match s with
               | SX_S -> Coq_x81 :: []
               | SX_U -> Coq_x82 :: [])
            | BOI_and -> Coq_x83 :: []
            | BOI_or -> Coq_x84 :: []
            | BOI_xor -> Coq_x85 :: []
            | BOI_shl -> Coq_x86 :: []
            | BOI_shr s ->
              (match s with
               | SX_S -> Coq_x87 :: []
               | SX_U -> Coq_x88 :: [])
            | BOI_rotl -> Coq_x89 :: []
            | BOI_rotr -> Coq_x8a :: [])
         | Binop_f _ -> dummy)
      | T_f32 ->
        (match b with
         | Binop_i _ -> dummy
         | Binop_f b0 ->
           (match b0 with
            | BOF_add -> Coq_x92 :: []
            | BOF_sub -> Coq_x93 :: []
            | BOF_mul -> Coq_x94 :: []
            | BOF_div -> Coq_x95 :: []
            | BOF_min -> Coq_x96 :: []
            | BOF_max -> Coq_x97 :: []
            | BOF_copysign -> Coq_x98 :: []))
      | T_f64 ->
        (match b with
         | Binop_i _ -> dummy
         | Binop_f b0 ->
           (match b0 with
            | BOF_add -> Coq_xa0 :: []
            | BOF_sub -> Coq_xa1 :: []
            | BOF_mul -> Coq_xa2 :: []
            | BOF_div -> Coq_xa3 :: []
            | BOF_min -> Coq_xa4 :: []
            | BOF_max -> Coq_xa5 :: []
            | BOF_copysign -> Coq_xa6 :: [])))
   | BI_testop (n, _) ->
     (match n with
      | T_i32 -> Coq_x45 :: []
      | T_i64 -> Coq_x50 :: []
      | _ -> dummy)
   | BI_relop (n, r) ->
     (match n with
      | T_i32 ->
        (match r with
         | Relop_i r0 ->
           (match r0 with
            | ROI_eq -> Coq_x46 :: []
            | ROI_ne -> Coq_x47 :: []
            | ROI_lt s ->
              (match s with
               | SX_S -> Coq_x48 :: []
               | SX_U -> Coq_x49 :: [])
            | ROI_gt s ->
              (match s with
               | SX_S -> Coq_x4a :: []
               | SX_U -> Coq_x4b :: [])
            | ROI_le s ->
              (match s with
               | SX_S -> Coq_x4c :: []
               | SX_U -> Coq_x4d :: [])
            | ROI_ge s ->
              (match s with
               | SX_S -> Coq_x4e :: []
               | SX_U -> Coq_x4f :: []))
         | Relop_f _ -> dummy)
      | T_i64 ->
        (match r with
         | Relop_i r0 ->
           (match r0 with
            | ROI_eq -> Coq_x51 :: []
            | ROI_ne -> Coq_x52 :: []
            | ROI_lt s ->
              (match s with
               | SX_S -> Coq_x53 :: []
               | SX_U -> Coq_x54 :: [])
            | ROI_gt s ->
              (match s with
               | SX_S -> Coq_x55 :: []
               | SX_U -> Coq_x56 :: [])
            | ROI_le s ->
              (match s with
               | SX_S -> Coq_x57 :: []
               | SX_U -> Coq_x58 :: [])
            | ROI_ge s ->
              (match s with
               | SX_S -> Coq_x59 :: []
               | SX_U -> Coq_x5a :: []))
         | Relop_f _ -> dummy)
      | T_f32 ->
        (match r with
         | Relop_i _ -> dummy
         | Relop_f r0 ->
           (match r0 with
            | ROF_eq -> Coq_x5b :: []
            | ROF_ne -> Coq_x5c :: []
            | ROF_lt -> Coq_x5d :: []
            | ROF_gt -> Coq_x5e :: []
            | ROF_le -> Coq_x5f :: []
            | ROF_ge -> Coq_x60 :: []))
      | T_f64 ->
        (match r with
         | Relop_i _ -> dummy
         | Relop_f r0 ->
           (match r0 with
            | ROF_eq -> Coq_x61 :: []
            | ROF_ne -> Coq_x62 :: []
            | ROF_lt -> Coq_x63 :: []
            | ROF_gt -> Coq_x64 :: []
            | ROF_le -> Coq_x65 :: []
            | ROF_ge -> Coq_x66 :: [])))
   | BI_cvtop (n, c, n0, o) ->
     (match n with
      | T_i32 ->
        (match c with
         | CVO_wrap ->
           (match n0 with
            | T_i64 -> (match o with
                        | Some _ -> dummy
                        | None -> Coq_xa7 :: [])
            | _ -> dummy)
         | CVO_trunc ->
           (match n0 with
            | T_f32 ->
              (match o with
               | Some s ->
                 (match s with
                  | SX_S -> Coq_xa8 :: []
                  | SX_U -> Coq_xa9 :: [])
               | None -> dummy)
            | T_f64 ->
              (match o with
               | Some s ->
                 (match s with
                  | SX_S -> Coq_xaa :: []
                  | SX_U -> Coq_xab :: [])
               | None -> dummy)
            | _ -> dummy)
         | CVO_trunc_sat ->
           (match n0 with
            | T_f32 ->
              (match o with
               | Some s ->
                 (match s with
                  | SX_S -> Coq_xfc :: (Coq_x00 :: [])
                  | SX_U -> Coq_xfc :: (Coq_x01 :: []))
               | None -> dummy)
            | T_f64 ->
              (match o with
               | Some s ->
                 (match s with
                  | SX_S -> Coq_xfc :: (Coq_x02 :: [])
                  | SX_U -> Coq_xfc :: (Coq_x03 :: []))
               | None -> dummy)
            | _ -> dummy)
         | CVO_reinterpret ->
           (match n0 with
            | T_f32 -> (match o with
                        | Some _ -> dummy
                        | None -> Coq_xbc :: [])
            | _ -> dummy)
         | _ -> dummy)
      | T_i64 ->
        (match c with
         | CVO_extend ->
           (match n0 with
            | T_i32 ->
              (match o with
               | Some s ->
                 (match s with
                  | SX_S -> Coq_xac :: []
                  | SX_U -> Coq_xad :: [])
               | None -> dummy)
            | _ -> dummy)
         | CVO_trunc ->
           (match n0 with
            | T_f32 ->
              (match o with
               | Some s ->
                 (match s with
                  | SX_S -> Coq_xae :: []
                  | SX_U -> Coq_xaf :: [])
               | None -> dummy)
            | T_f64 ->
              (match o with
               | Some s ->
                 (match s with
                  | SX_S -> Coq_xb0 :: []
                  | SX_U -> Coq_xb1 :: [])
               | None -> dummy)
            | _ -> dummy)
         | CVO_trunc_sat ->
           (match n0 with
            | T_f32 ->
              (match o with
               | Some s ->
                 (match s with
                  | SX_S -> Coq_xfc :: (Coq_x04 :: [])
                  | SX_U -> Coq_xfc :: (Coq_x05 :: []))
               | None -> dummy)
            | T_f64 ->
              (match o with
               | Some s ->
                 (match s with
                  | SX_S -> Coq_xfc :: (Coq_x06 :: [])
                  | SX_U -> Coq_xfc :: (Coq_x07 :: []))
               | None -> dummy)
            | _ -> dummy)
         | CVO_reinterpret ->
           (match n0 with
            | T_f64 -> (match o with
                        | Some _ -> dummy
                        | None -> Coq_xbd :: [])
            | _ -> dummy)
         | _ -> dummy)
      | T_f32 ->
        (match c with
         | CVO_convert ->
           (match n0 with
            | T_i32 ->
              (match o with
               | Some s ->
                 (match s with
                  | SX_S -> Coq_xb2 :: []
                  | SX_U -> Coq_xb3 :: [])
               | None -> dummy)
            | T_i64 ->
              (match o with
               | Some s ->
                 (match s with
                  | SX_S -> Coq_xb4 :: []
                  | SX_U -> Coq_xb5 :: [])
               | None -> dummy)
            | _ -> dummy)
         | CVO_demote ->
           (match n0 with
            | T_f64 -> (match o with
                        | Some _ -> dummy
                        | None -> Coq_xb6 :: [])
            | _ -> dummy)
         | CVO_reinterpret ->
           (match n0 with
            | T_i32 -> (match o with
                        | Some _ -> dummy
                        | None -> Coq_xbe :: [])
            | _ -> dummy)
         | _ -> dummy)
      | T_f64 ->
        (match c with
         | CVO_convert ->
           (match n0 with
            | T_i32 ->
              (match o with
               | Some s ->
                 (match s with
                  | SX_S -> Coq_xb7 :: []
                  | SX_U -> Coq_xb8 :: [])
               | None -> dummy)
            | T_i64 ->
              (match o with
               | Some s ->
                 (match s with
                  | SX_S -> Coq_xb9 :: []
                  | SX_U -> Coq_xba :: [])
               | None -> dummy)
            | _ -> dummy)
         | CVO_promote ->
           (match n0 with
            | T_f32 -> (match o with
                        | Some _ -> dummy
                        | None -> Coq_xbb :: [])
            | _ -> dummy)
         | CVO_reinterpret ->
           (match n0 with
            | T_i64 -> (match o with
                        | Some _ -> dummy
                        | None -> Coq_xbf :: [])
            | _ -> dummy)
         | _ -> dummy))
   | BI_const_vec v -> binary_of_valvec v
   | BI_unop_vec op -> binary_of_unop_vec op
   | BI_binop_vec op -> binary_of_binop_vec op
   | BI_ternop_vec op -> binary_of_ternop_vec op
   | BI_test_vec op -> binary_of_test_vec op
   | BI_shift_vec op -> binary_of_shift_vec op
   | BI_splat_vec sh -> binary_of_splat_vec sh
   | BI_extract_vec (sh, s, lanex) -> binary_of_extract_vec sh s lanex
   | BI_replace_vec (sh, lanex) -> binary_of_replace_vec sh lanex
   | BI_ref_null t -> Coq_xd0 :: ((binary_of_reference_type t) :: [])
   | BI_ref_is_null -> Coq_xd1 :: []
   | BI_ref_func x -> Coq_xd2 :: (binary_of_idx x)
   | BI_drop -> Coq_x1a :: []
   | BI_select o ->
     (match o with
      | Some ts -> Coq_x1c :: (binary_of_value_types ts)
      | None -> Coq_x1b :: [])
   | BI_local_get x -> Coq_x20 :: (binary_of_idx x)
   | BI_local_set x -> Coq_x21 :: (binary_of_idx x)
   | BI_local_tee x -> Coq_x22 :: (binary_of_idx x)
   | BI_global_get x -> Coq_x23 :: (binary_of_idx x)
   | BI_global_set x -> Coq_x24 :: (binary_of_idx x)
   | BI_table_get x -> Coq_x25 :: (binary_of_idx x)
   | BI_table_set x -> Coq_x26 :: (binary_of_idx x)
   | BI_table_size x -> Coq_xfc :: (Coq_x10 :: (binary_of_idx x))
   | BI_table_grow x -> Coq_xfc :: (Coq_x0f :: (binary_of_idx x))
   | BI_table_fill x -> Coq_xfc :: (Coq_x11 :: (binary_of_idx x))
   | BI_table_copy (x, y) ->
     Coq_xfc :: (Coq_x0e :: (cat (binary_of_idx x) (binary_of_idx y)))
   | BI_table_init (x, y) ->
     Coq_xfc :: (Coq_x0c :: (cat (binary_of_idx y) (binary_of_idx x)))
   | BI_elem_drop x -> Coq_xfc :: (Coq_x0d :: (binary_of_idx x))
   | BI_load (n, o, marg) ->
     (match n with
      | T_i32 ->
        (match o with
         | Some p ->
           let (p0, s) = p in
           (match p0 with
            | Tp_i8 ->
              (match s with
               | SX_S -> Coq_x2c :: (binary_of_memarg marg)
               | SX_U -> Coq_x2d :: (binary_of_memarg marg))
            | Tp_i16 ->
              (match s with
               | SX_S -> Coq_x2e :: (binary_of_memarg marg)
               | SX_U -> Coq_x2f :: (binary_of_memarg marg))
            | Tp_i32 -> dummy)
         | None -> Coq_x28 :: (binary_of_memarg marg))
      | T_i64 ->
        (match o with
         | Some p ->
           let (p0, s) = p in
           (match p0 with
            | Tp_i8 ->
              (match s with
               | SX_S -> Coq_x30 :: (binary_of_memarg marg)
               | SX_U -> Coq_x31 :: (binary_of_memarg marg))
            | Tp_i16 ->
              (match s with
               | SX_S -> Coq_x32 :: (binary_of_memarg marg)
               | SX_U -> Coq_x33 :: (binary_of_memarg marg))
            | Tp_i32 ->
              (match s with
               | SX_S -> Coq_x34 :: (binary_of_memarg marg)
               | SX_U -> Coq_x35 :: (binary_of_memarg marg)))
         | None -> Coq_x29 :: (binary_of_memarg marg))
      | T_f32 ->
        (match o with
         | Some _ -> dummy
         | None -> Coq_x2a :: (binary_of_memarg marg))
      | T_f64 ->
        (match o with
         | Some _ -> dummy
         | None -> Coq_x2b :: (binary_of_memarg marg)))
   | BI_load_vec (lvarg, marg) -> binary_of_load_vec lvarg marg
   | BI_load_vec_lane (width, marg, lanex) ->
     binary_of_load_vec_lane width marg lanex
   | BI_store (n, o, marg) ->
     (match n with
      | T_i32 ->
        (match o with
         | Some p ->
           (match p with
            | Tp_i8 -> Coq_x3a :: (binary_of_memarg marg)
            | Tp_i16 -> Coq_x3b :: (binary_of_memarg marg)
            | Tp_i32 -> dummy)
         | None -> Coq_x36 :: (binary_of_memarg marg))
      | T_i64 ->
        (match o with
         | Some p ->
           (match p with
            | Tp_i8 -> Coq_x3c :: (binary_of_memarg marg)
            | Tp_i16 -> Coq_x3d :: (binary_of_memarg marg)
            | Tp_i32 -> Coq_x3e :: (binary_of_memarg marg))
         | None -> Coq_x37 :: (binary_of_memarg marg))
      | T_f32 ->
        (match o with
         | Some _ -> dummy
         | None -> Coq_x38 :: (binary_of_memarg marg))
      | T_f64 ->
        (match o with
         | Some _ -> dummy
         | None -> Coq_x39 :: (binary_of_memarg marg)))
   | BI_store_vec_lane (width, marg, lanex) ->
     binary_of_store_vec_lane width marg lanex
   | BI_memory_size -> Coq_x3f :: (Coq_x00 :: [])
   | BI_memory_grow -> Coq_x40 :: (Coq_x00 :: [])
   | BI_memory_fill -> Coq_xfc :: (Coq_x0b :: (Coq_x00 :: []))
   | BI_memory_copy -> Coq_xfc :: (Coq_x0a :: (Coq_x00 :: (Coq_x00 :: [])))
   | BI_memory_init x ->
     Coq_xfc :: (Coq_x08 :: (cat (binary_of_idx x) (Coq_x00 :: [])))
   | BI_data_drop x -> Coq_xfc :: (Coq_x09 :: (binary_of_idx x))
   | BI_nop -> Coq_x01 :: []
   | BI_unreachable -> Coq_x00 :: []
   | BI_block (bt, ins) ->
     Coq_x02 :: (cat (binary_of_block_type bt)
                  (cat (binary_of_instrs ins) (Coq_x0b :: [])))
   | BI_loop (bt, ins) ->
     Coq_x03 :: (cat (binary_of_block_type bt)
                  (cat (binary_of_instrs ins) (Coq_x0b :: [])))
   | BI_if (bt, ins1, ins2) ->
     (match ins2 with
      | [] ->
        Coq_x04 :: (cat (binary_of_block_type bt)
                     (cat (binary_of_instrs ins1) (Coq_x0b :: [])))
      | _ :: _ ->
        Coq_x04 :: (cat (binary_of_block_type bt)
                     (cat (binary_of_instrs ins1)
                       (Coq_x05 :: (cat []
                                     (cat (binary_of_instrs ins2)
                                       (Coq_x0b :: [])))))))
   | BI_br l -> Coq_x0c :: (binary_of_idx l)
   | BI_br_if l -> Coq_x0d :: (binary_of_idx l)
   | BI_br_table (ls, l_N) ->
     Coq_x0e :: (cat (binary_of_vec binary_of_idx ls) (binary_of_idx l_N))
   | BI_return -> Coq_x0f :: []
   | BI_call x -> Coq_x10 :: (binary_of_idx x)
   | BI_call_indirect (x, y) ->
     Coq_x11 :: (cat (binary_of_idx y) (binary_of_idx x))
   | BI_return_call x -> Coq_x12 :: (binary_of_idx x)
   | BI_return_call_indirect (x, y) ->
     Coq_x13 :: (cat (binary_of_idx y) (binary_of_idx x)))

(** val binary_of_expr : basic_instruction list -> Byte.byte list **)

let binary_of_expr bes =
  cat (concat (List0.map binary_of_be bes)) (Coq_x0b :: [])

(** val magic : Byte.byte list **)

let magic =
  Coq_x00 :: (Coq_x61 :: (Coq_x73 :: (Coq_x6d :: [])))

(** val version : Byte.byte list **)

let version =
  Coq_x01 :: (Coq_x00 :: (Coq_x00 :: (Coq_x00 :: [])))

(** val with_length : Byte.byte list -> Byte.byte list **)

let with_length bs =
  cat (encode_unsigned (bin_of_nat (length bs))) bs

(** val binary_of_functype : function_type -> Byte.byte list **)

let binary_of_functype = function
| Tf (rt1, rt2) ->
  Coq_x60 :: (cat (binary_of_result_type rt1) (binary_of_result_type rt2))

(** val binary_of_typesec : function_type list -> Byte.byte list **)

let binary_of_typesec ts =
  Coq_x01 :: (with_length (binary_of_vec binary_of_functype ts))

(** val binary_of_name : name -> Byte.byte list **)

let binary_of_name n =
  binary_of_vec (fun n0 -> n0 :: []) n

(** val binary_of_limits : limits -> Byte.byte list **)

let binary_of_limits l =
  match l.lim_max with
  | Some max ->
    Coq_x01 :: (cat (encode_unsigned (bin_of_nat (nat_of_bin l.lim_min)))
                 (encode_unsigned (bin_of_nat (nat_of_bin max))))
  | None -> Coq_x00 :: (encode_unsigned (bin_of_nat (nat_of_bin l.lim_min)))

(** val binary_of_table_type : table_type -> Byte.byte list **)

let binary_of_table_type t_ty =
  (binary_of_reference_type t_ty.tt_elem_type) :: (binary_of_limits
                                                    t_ty.tt_limits)

(** val binary_of_mutability : mutability -> Byte.byte list **)

let binary_of_mutability = function
| MUT_const -> Coq_x00 :: []
| MUT_var -> Coq_x01 :: []

(** val binary_of_global_type : global_type -> Byte.byte list **)

let binary_of_global_type g_ty =
  cat ((binary_of_value_type g_ty.tg_t) :: [])
    (binary_of_mutability g_ty.tg_mut)

(** val binary_of_memory_type : memory_type -> Byte.byte list **)

let binary_of_memory_type =
  binary_of_limits

(** val binary_of_import_desc : module_import_desc -> Byte.byte list **)

let binary_of_import_desc = function
| MID_func tidx -> Coq_x00 :: (binary_of_typeidx tidx)
| MID_table t_ty -> Coq_x01 :: (binary_of_table_type t_ty)
| MID_mem m_ty -> Coq_x02 :: (binary_of_memory_type m_ty)
| MID_global g_ty -> Coq_x03 :: (binary_of_global_type g_ty)

(** val binary_of_module_import : module_import -> Byte.byte list **)

let binary_of_module_import imp =
  cat (binary_of_name imp.imp_module)
    (cat (binary_of_name imp.imp_name) (binary_of_import_desc imp.imp_desc))

(** val binary_of_importsec : module_import list -> Byte.byte list **)

let binary_of_importsec imps =
  Coq_x02 :: (with_length (binary_of_vec binary_of_module_import imps))

(** val binary_of_funcsec : module_func list -> Byte.byte list **)

let binary_of_funcsec fs =
  Coq_x03 :: (with_length
               (binary_of_vec binary_of_typeidx
                 (List0.map (fun f -> f.modfunc_type) fs)))

(** val binary_of_module_table : module_table -> Byte.byte list **)

let binary_of_module_table =
  binary_of_table_type

(** val binary_of_tablesec : module_table list -> Byte.byte list **)

let binary_of_tablesec ts =
  Coq_x04 :: (with_length (binary_of_vec binary_of_module_table ts))

(** val binary_of_module_mem : module_mem -> Byte.byte list **)

let binary_of_module_mem =
  binary_of_memory_type

(** val binary_of_memsec : module_mem list -> Byte.byte list **)

let binary_of_memsec ms =
  Coq_x05 :: (with_length (binary_of_vec binary_of_module_mem ms))

(** val binary_of_module_global : module_global -> Byte.byte list **)

let binary_of_module_global g =
  cat (binary_of_global_type g.modglob_type) (binary_of_expr g.modglob_init)

(** val binary_of_globalsec : module_global list -> Byte.byte list **)

let binary_of_globalsec gs =
  Coq_x06 :: (with_length (binary_of_vec binary_of_module_global gs))

(** val binary_of_export_desc : module_export_desc -> Byte.byte list **)

let binary_of_export_desc = function
| MED_func n -> Coq_x00 :: (binary_of_funcidx n)
| MED_table n -> Coq_x01 :: (binary_of_tableidx n)
| MED_mem n -> Coq_x02 :: (binary_of_memidx n)
| MED_global n -> Coq_x03 :: (binary_of_globalidx n)

(** val binary_of_module_export : module_export -> Byte.byte list **)

let binary_of_module_export e =
  cat (binary_of_name e.modexp_name) (binary_of_export_desc e.modexp_desc)

(** val binary_of_exportssec : module_export list -> Byte.byte list **)

let binary_of_exportssec es =
  Coq_x07 :: (with_length (binary_of_vec binary_of_module_export es))

(** val binary_of_module_start : module_start -> Byte.byte list **)

let binary_of_module_start =
  binary_of_funcidx

(** val binary_of_startsec : module_start -> Byte.byte list **)

let binary_of_startsec s =
  Coq_x08 :: (with_length (binary_of_module_start s))

(** val to_ref_func : basic_instruction list -> funcidx option **)

let to_ref_func = function
| [] -> None
| b :: l ->
  (match b with
   | BI_ref_func x -> (match l with
                       | [] -> Some x
                       | _ :: _ -> None)
   | _ -> None)

(** val elem_of_wasm_1_0 : module_element -> Byte.byte list option **)

let elem_of_wasm_1_0 e =
  match e.modelem_mode with
  | ME_active (t, offset) ->
    (match t with
     | N0 ->
       if eq_op reference_type_eqType (Obj.magic e.modelem_type)
            (Obj.magic T_funcref)
       then (match those (map to_ref_func e.modelem_init) with
             | Some ys ->
               Some
                 (Coq_x00 :: (cat (binary_of_expr offset)
                               (binary_of_vec binary_of_funcidx ys)))
             | None -> None)
       else None
     | Npos _ -> None)
  | _ -> None

(** val binary_of_module_elem : module_element -> Byte.byte list **)

let binary_of_module_elem e =
  match e.modelem_mode with
  | ME_passive ->
    Coq_x05 :: ((binary_of_reference_type e.modelem_type) :: (binary_of_vec
                                                               binary_of_expr
                                                               e.modelem_init))
  | ME_active (x, offset) ->
    (match elem_of_wasm_1_0 e with
     | Some bs -> bs
     | None ->
       Coq_x06 :: (cat (binary_of_tableidx x)
                    (cat (binary_of_expr offset)
                      ((binary_of_reference_type e.modelem_type) :: (binary_of_vec
                                                                    binary_of_expr
                                                                    e.modelem_init)))))
  | ME_declarative ->
    Coq_x07 :: ((binary_of_reference_type e.modelem_type) :: (binary_of_vec
                                                               binary_of_expr
                                                               e.modelem_init))

(** val binary_of_elemsec : module_element list -> Byte.byte list **)

let binary_of_elemsec es =
  Coq_x09 :: (with_length (binary_of_vec binary_of_module_elem es))

(** val binary_of_local : (nat * value_type) -> Byte.byte list **)

let binary_of_local = function
| (n, t) ->
  cat (encode_unsigned (bin_of_nat n)) ((binary_of_value_type t) :: [])

(** val bunch_locals_aux :
    value_type -> nat -> (nat * value_type) list -> value_type list ->
    (nat * value_type) list **)

let rec bunch_locals_aux cur_ty cur_count acc = function
| [] -> rev ((cur_count, cur_ty) :: acc)
| ty :: tys' ->
  if eq_op value_type_eqType (Obj.magic cur_ty) (Obj.magic ty)
  then bunch_locals_aux cur_ty (addn cur_count (S O)) acc tys'
  else bunch_locals_aux ty (S O) ((cur_count, cur_ty) :: acc) tys'

(** val bunch_locals : value_type list -> (nat * value_type) list **)

let bunch_locals = function
| [] -> []
| ty :: tys' -> bunch_locals_aux ty (S O) [] tys'

(** val binary_of_code_func : value_type list -> expr -> Byte.byte list **)

let binary_of_code_func tlocs e =
  cat (binary_of_vec binary_of_local (bunch_locals tlocs)) (binary_of_expr e)

(** val binary_of_code : module_func -> Byte.byte list **)

let binary_of_code mf =
  let func_bin = binary_of_code_func mf.modfunc_locals mf.modfunc_body in
  let func_len = length func_bin in
  cat (encode_unsigned (bin_of_nat func_len)) func_bin

(** val binary_of_codesec : module_func list -> Byte.byte list **)

let binary_of_codesec fs =
  Coq_x0a :: (with_length (binary_of_vec binary_of_code fs))

(** val binary_of_data : module_data -> Byte.byte list **)

let binary_of_data d =
  match d.moddata_mode with
  | MD_passive ->
    Coq_x01 :: (binary_of_vec (fun x -> (byte_of_compcert_byte x) :: [])
                 d.moddata_init)
  | MD_active (x, offset) ->
    Coq_x02 :: (cat (binary_of_memidx x)
                 (cat (binary_of_expr offset)
                   (binary_of_vec (fun x0 ->
                     (byte_of_compcert_byte x0) :: []) d.moddata_init)))

(** val binary_of_datasec : module_data list -> Byte.byte list **)

let binary_of_datasec ds =
  Coq_x0b :: (with_length (binary_of_vec binary_of_data ds))

(** val only_if_non_nil :
    ('a1 list -> Byte.byte list) -> 'a1 list -> Byte.byte list **)

let only_if_non_nil f xs = match xs with
| [] -> []
| _ :: _ -> f xs

(** val only_if_non_none :
    ('a1 -> Byte.byte list) -> 'a1 option -> Byte.byte list **)

let only_if_non_none f = function
| Some x -> f x
| None -> []

(** val binary_of_module : coq_module -> Byte.byte list **)

let binary_of_module m =
  cat magic
    (cat version
      (cat (only_if_non_nil binary_of_typesec m.mod_types)
        (cat (only_if_non_nil binary_of_importsec m.mod_imports)
          (cat (only_if_non_nil binary_of_funcsec m.mod_funcs)
            (cat (only_if_non_nil binary_of_tablesec m.mod_tables)
              (cat (only_if_non_nil binary_of_memsec m.mod_mems)
                (cat (only_if_non_nil binary_of_globalsec m.mod_globals)
                  (cat (only_if_non_nil binary_of_exportssec m.mod_exports)
                    (cat (only_if_non_none binary_of_startsec m.mod_start)
                      (cat (only_if_non_nil binary_of_elemsec m.mod_elems)
                        (cat (only_if_non_nil binary_of_codesec m.mod_funcs)
                          (only_if_non_nil binary_of_datasec m.mod_datas))))))))))))
