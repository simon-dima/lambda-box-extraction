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

(** val max_mem_pages : coq_N **)

let max_mem_pages =
  Npos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI Coq_xH))))))))))))))

(** val main_function_name : String.t **)

let main_function_name =
  String.String (Coq_x6d, (String.String (Coq_x61, (String.String (Coq_x69,
    (String.String (Coq_x6e, (String.String (Coq_x5f, (String.String
    (Coq_x66, (String.String (Coq_x75, (String.String (Coq_x6e,
    (String.String (Coq_x63, (String.String (Coq_x74, (String.String
    (Coq_x69, (String.String (Coq_x6f, (String.String (Coq_x6e,
    String.EmptyString)))))))))))))))))))))))))

(** val main_function_idx : funcidx **)

let main_function_idx =
  N0

(** val num_custom_funs : nat **)

let num_custom_funs =
  S O

(** val glob_mem_ptr : globalidx **)

let glob_mem_ptr =
  N0

(** val glob_cap : globalidx **)

let glob_cap =
  Npos Coq_xH

(** val glob_result : globalidx **)

let glob_result =
  Npos (Coq_xO Coq_xH)

(** val glob_out_of_mem : globalidx **)

let glob_out_of_mem =
  Npos (Coq_xI Coq_xH)

(** val glob_tmp1 : globalidx **)

let glob_tmp1 =
  Npos (Coq_xO (Coq_xO Coq_xH))

(** val glob_tmp2 : globalidx **)

let glob_tmp2 =
  Npos (Coq_xI (Coq_xO Coq_xH))

(** val glob_tmp3 : globalidx **)

let glob_tmp3 =
  Npos (Coq_xO (Coq_xI Coq_xH))

(** val glob_tmp4 : globalidx **)

let glob_tmp4 =
  Npos (Coq_xI (Coq_xI Coq_xH))

type localvar_env = localidx M.tree

type fname_env = funcidx M.tree

type wasm_function = { fidx : funcidx; export_name : String.t;
                       coq_type : coq_N; locals : value_type list;
                       body : basic_instruction list }

(** val nat_to_i32 : nat -> Wasm_int.Int32.int **)

let nat_to_i32 n =
  Wasm_int.Int32.repr (Z.of_nat n)

(** val nat_to_i64 : nat -> Wasm_int.Int64.int **)

let nat_to_i64 n =
  Wasm_int.Int64.repr (Z.of_nat n)

(** val nat_to_value : nat -> value_num **)

let nat_to_value n =
  VAL_int32 (Obj.magic nat_to_i32 n)

(** val nat_to_value64 : nat -> value_num **)

let nat_to_value64 n =
  VAL_int64 (Obj.magic nat_to_i64 n)

(** val coq_Z_to_value : coq_Z -> value_num **)

let coq_Z_to_value z =
  VAL_int32 (Obj.magic Wasm_int.Int32.repr z)

(** val coq_N_to_value : coq_N -> value_num **)

let coq_N_to_value n =
  coq_Z_to_value (Z.of_N n)

(** val translate_var :
    name_env -> localvar_env -> var -> String.t -> u32 error **)

let translate_var nenv lenv v err =
  match M.get v lenv with
  | Some n -> Ret n
  | None ->
    Err
      (String.append (String.String (Coq_x65, (String.String (Coq_x78,
        (String.String (Coq_x70, (String.String (Coq_x65, (String.String
        (Coq_x63, (String.String (Coq_x74, (String.String (Coq_x65,
        (String.String (Coq_x64, (String.String (Coq_x20, (String.String
        (Coq_x74, (String.String (Coq_x6f, (String.String (Coq_x20,
        (String.String (Coq_x66, (String.String (Coq_x69, (String.String
        (Coq_x6e, (String.String (Coq_x64, (String.String (Coq_x20,
        (String.String (Coq_x69, (String.String (Coq_x64, (String.String
        (Coq_x20, (String.String (Coq_x66, (String.String (Coq_x6f,
        (String.String (Coq_x72, (String.String (Coq_x20, (String.String
        (Coq_x76, (String.String (Coq_x61, (String.String (Coq_x72,
        (String.String (Coq_x69, (String.String (Coq_x61, (String.String
        (Coq_x62, (String.String (Coq_x6c, (String.String (Coq_x65,
        (String.String (Coq_x20,
        String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
        (String.append (show_tree (show_var nenv v))
          (String.append (String.String (Coq_x20, (String.String (Coq_x69,
            (String.String (Coq_x6e, (String.String (Coq_x20, (String.String
            (Coq_x76, (String.String (Coq_x61, (String.String (Coq_x72,
            (String.String (Coq_x2f, (String.String (Coq_x66, (String.String
            (Coq_x76, (String.String (Coq_x61, (String.String (Coq_x72,
            (String.String (Coq_x20, (String.String (Coq_x6d, (String.String
            (Coq_x61, (String.String (Coq_x70, (String.String (Coq_x70,
            (String.String (Coq_x69, (String.String (Coq_x6e, (String.String
            (Coq_x67, (String.String (Coq_x3a, (String.String (Coq_x20,
            String.EmptyString))))))))))))))))))))))))))))))))))))))))))))
            err)))

(** val is_function_var : fname_env -> var -> bool **)

let is_function_var fenv v =
  match M.get v fenv with
  | Some _ -> true
  | None -> false

(** val instr_local_var_read :
    name_env -> localvar_env -> fname_env -> var -> basic_instruction error **)

let instr_local_var_read nenv lenv fenv v =
  if is_function_var fenv v
  then bind (Obj.magic coq_MonadError)
         (Obj.magic translate_var nenv fenv v (String.String (Coq_x74,
           (String.String (Coq_x72, (String.String (Coq_x61, (String.String
           (Coq_x6e, (String.String (Coq_x73, (String.String (Coq_x6c,
           (String.String (Coq_x61, (String.String (Coq_x74, (String.String
           (Coq_x65, (String.String (Coq_x20, (String.String (Coq_x6c,
           (String.String (Coq_x6f, (String.String (Coq_x63, (String.String
           (Coq_x61, (String.String (Coq_x6c, (String.String (Coq_x20,
           (String.String (Coq_x76, (String.String (Coq_x61, (String.String
           (Coq_x72, (String.String (Coq_x20, (String.String (Coq_x72,
           (String.String (Coq_x65, (String.String (Coq_x61, (String.String
           (Coq_x64, (String.String (Coq_x3a, (String.String (Coq_x20,
           (String.String (Coq_x6f, (String.String (Coq_x62, (String.String
           (Coq_x74, (String.String (Coq_x61, (String.String (Coq_x69,
           (String.String (Coq_x6e, (String.String (Coq_x69, (String.String
           (Coq_x6e, (String.String (Coq_x67, (String.String (Coq_x20,
           (String.String (Coq_x66, (String.String (Coq_x75, (String.String
           (Coq_x6e, (String.String (Coq_x63, (String.String (Coq_x74,
           (String.String (Coq_x69, (String.String (Coq_x6f, (String.String
           (Coq_x6e, (String.String (Coq_x20, (String.String (Coq_x69,
           (String.String (Coq_x64, (String.String (Coq_x78,
           String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
         (fun fidx0 -> Ret (BI_const_num (coq_N_to_value fidx0)))
  else bind (Obj.magic coq_MonadError)
         (Obj.magic translate_var nenv lenv v (String.String (Coq_x69,
           (String.String (Coq_x6e, (String.String (Coq_x73, (String.String
           (Coq_x74, (String.String (Coq_x72, (String.String (Coq_x5f,
           (String.String (Coq_x6c, (String.String (Coq_x6f, (String.String
           (Coq_x63, (String.String (Coq_x61, (String.String (Coq_x6c,
           (String.String (Coq_x5f, (String.String (Coq_x76, (String.String
           (Coq_x61, (String.String (Coq_x72, (String.String (Coq_x5f,
           (String.String (Coq_x72, (String.String (Coq_x65, (String.String
           (Coq_x61, (String.String (Coq_x64, (String.String (Coq_x3a,
           (String.String (Coq_x20, (String.String (Coq_x6e, (String.String
           (Coq_x6f, (String.String (Coq_x72, (String.String (Coq_x6d,
           (String.String (Coq_x61, (String.String (Coq_x6c, (String.String
           (Coq_x20, (String.String (Coq_x76, (String.String (Coq_x61,
           (String.String (Coq_x72,
           String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
         (fun var0 -> Ret (BI_local_get var0))

(** val get_ctor_arity : ctor_env -> ctor_tag -> nat error **)

let get_ctor_arity cenv t0 =
  match M.get t0 cenv with
  | Some c ->
    let { ctor_name = _; ctor_ind_name = _; ctor_ind_tag = _; ctor_arity = n;
      ctor_ordinal = _ } = c
    in
    Ret (N.to_nat n)
  | None ->
    Err (String.String (Coq_x66, (String.String (Coq_x6f, (String.String
      (Coq_x75, (String.String (Coq_x6e, (String.String (Coq_x64,
      (String.String (Coq_x20, (String.String (Coq_x63, (String.String
      (Coq_x6f, (String.String (Coq_x6e, (String.String (Coq_x73,
      (String.String (Coq_x74, (String.String (Coq_x72, (String.String
      (Coq_x75, (String.String (Coq_x63, (String.String (Coq_x74,
      (String.String (Coq_x6f, (String.String (Coq_x72, (String.String
      (Coq_x20, (String.String (Coq_x77, (String.String (Coq_x69,
      (String.String (Coq_x74, (String.String (Coq_x68, (String.String
      (Coq_x6f, (String.String (Coq_x75, (String.String (Coq_x74,
      (String.String (Coq_x20, (String.String (Coq_x63, (String.String
      (Coq_x74, (String.String (Coq_x6f, (String.String (Coq_x72,
      (String.String (Coq_x5f, (String.String (Coq_x61, (String.String
      (Coq_x72, (String.String (Coq_x69, (String.String (Coq_x74,
      (String.String (Coq_x79, (String.String (Coq_x20, (String.String
      (Coq_x73, (String.String (Coq_x65, (String.String (Coq_x74,
      String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val get_ctor_size : ctor_env -> ctor_tag -> coq_N error **)

let get_ctor_size cenv t0 =
  bind (Obj.magic coq_MonadError) (Obj.magic get_ctor_arity cenv t0)
    (fun arity -> Ret
    (if Nat.eqb arity O
     then N0
     else N.of_nat
            (add (mul (S (S (S (S O)))) (add arity (S O))) (S (S (S (S (S (S
              (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
              O)))))))))))))))))))))))))))

(** val pass_function_args :
    name_env -> localvar_env -> fname_env -> var list -> basic_instruction
    list error **)

let rec pass_function_args nenv lenv fenv = function
| [] -> Ret []
| a0 :: args' ->
  bind (Obj.magic coq_MonadError)
    (Obj.magic instr_local_var_read nenv lenv fenv a0) (fun a0' ->
    bind (Obj.magic coq_MonadError) (pass_function_args nenv lenv fenv args')
      (fun args'' -> Ret (a0' :: args'')))

(** val translate_call :
    name_env -> localvar_env -> fname_env -> var -> var list -> bool ->
    basic_instruction list error **)

let translate_call nenv lenv fenv f args tailcall =
  bind (Obj.magic coq_MonadError) (pass_function_args nenv lenv fenv args)
    (fun instr_pass_params ->
    bind (Obj.magic coq_MonadError)
      (Obj.magic instr_local_var_read nenv lenv fenv f) (fun instr_fidx ->
      let call = fun num_args ->
        if tailcall
        then BI_return_call_indirect (N0, (N.of_nat num_args))
        else BI_call_indirect (N0, (N.of_nat num_args))
      in
      Ret
      (app instr_pass_params
        (app (instr_fidx :: []) ((call (length args)) :: [])))))

(** val store_constructor_args :
    name_env -> localvar_env -> fname_env -> var list -> nat ->
    basic_instruction list error **)

let rec store_constructor_args nenv lenv fenv args current =
  match args with
  | [] -> Ret []
  | y :: ys ->
    bind (Obj.magic coq_MonadError)
      (Obj.magic instr_local_var_read nenv lenv fenv y) (fun read_y ->
      bind (Obj.magic coq_MonadError)
        (store_constructor_args nenv lenv fenv ys (add (S O) current))
        (fun remaining -> Ret
        (app ((BI_global_get glob_cap) :: ((BI_const_num
          (nat_to_value (mul (S (S (S (S O)))) (add (S O) current)))) :: ((BI_binop
          (T_i32, (Binop_i BOI_add))) :: (read_y :: ((BI_store (T_i32, None,
          { memarg_offset = N0; memarg_align = (Npos (Coq_xO
          Coq_xH)) })) :: ((BI_global_get glob_mem_ptr) :: ((BI_const_num
          (nat_to_value (S (S (S (S O)))))) :: ((BI_binop (T_i32, (Binop_i
          BOI_add))) :: ((BI_global_set glob_mem_ptr) :: [])))))))))
          remaining)))

(** val store_constructor :
    name_env -> ctor_env -> localvar_env -> fname_env -> ctor_tag -> var list
    -> basic_instruction list error **)

let store_constructor nenv cenv lenv fenv c ys =
  bind (Obj.magic coq_MonadError) (Obj.magic get_ctor_ord cenv c) (fun ord ->
    bind (Obj.magic coq_MonadError)
      (store_constructor_args nenv lenv fenv ys O) (fun store_constr_args ->
      Ret
      (app ((BI_global_get glob_mem_ptr) :: ((BI_global_set
        glob_cap) :: ((BI_global_get glob_cap) :: ((BI_const_num
        (nat_to_value (N.to_nat ord))) :: ((BI_store (T_i32, None,
        { memarg_offset = N0; memarg_align = (Npos (Coq_xO
        Coq_xH)) })) :: ((BI_global_get glob_mem_ptr) :: ((BI_const_num
        (nat_to_value (S (S (S (S O)))))) :: ((BI_binop (T_i32, (Binop_i
        BOI_add))) :: ((BI_global_set glob_mem_ptr) :: [])))))))))
        store_constr_args)))

(** val create_case_nested_if_chain :
    bool -> localidx -> (coq_N * basic_instruction list) list ->
    basic_instruction list **)

let rec create_case_nested_if_chain boxed v = function
| [] -> BI_unreachable :: []
| p :: tl ->
  let (ord, instrs) = p in
  (BI_local_get
  v) :: (app
          (if boxed
           then (BI_load (T_i32, None, { memarg_offset = N0; memarg_align =
                  (Npos (Coq_xO Coq_xH)) })) :: ((BI_const_num
                  (nat_to_value (N.to_nat ord))) :: [])
           else (BI_const_num
                  (nat_to_value
                    (N.to_nat
                      (N.add (N.mul (Npos (Coq_xO Coq_xH)) ord) (Npos Coq_xH))))) :: [])
          ((BI_relop (T_i32, (Relop_i ROI_eq))) :: ((BI_if ((BT_valtype
          None), instrs, (create_case_nested_if_chain boxed v tl))) :: [])))

(** val translate_primitive_value : primitive -> Wasm_int.Int64.int error **)

let translate_primitive_value p =
  let i = projT2 p in
  (match projT1 p with
   | Coq_primInt -> Ret (Wasm_int.Int64.repr (to_Z (Obj.magic i)))
   | Coq_primFloat ->
     Err (String.String (Coq_x45, (String.String (Coq_x78, (String.String
       (Coq_x74, (String.String (Coq_x72, (String.String (Coq_x61,
       (String.String (Coq_x63, (String.String (Coq_x74, (String.String
       (Coq_x69, (String.String (Coq_x6f, (String.String (Coq_x6e,
       (String.String (Coq_x20, (String.String (Coq_x6f, (String.String
       (Coq_x66, (String.String (Coq_x20, (String.String (Coq_x66,
       (String.String (Coq_x6c, (String.String (Coq_x6f, (String.String
       (Coq_x61, (String.String (Coq_x74, (String.String (Coq_x73,
       (String.String (Coq_x20, (String.String (Coq_x74, (String.String
       (Coq_x6f, (String.String (Coq_x20, (String.String (Coq_x57,
       (String.String (Coq_x61, (String.String (Coq_x73, (String.String
       (Coq_x6d, (String.String (Coq_x20, (String.String (Coq_x6e,
       (String.String (Coq_x6f, (String.String (Coq_x74, (String.String
       (Coq_x20, (String.String (Coq_x79, (String.String (Coq_x65,
       (String.String (Coq_x74, (String.String (Coq_x20, (String.String
       (Coq_x73, (String.String (Coq_x75, (String.String (Coq_x70,
       (String.String (Coq_x70, (String.String (Coq_x6f, (String.String
       (Coq_x72, (String.String (Coq_x74, (String.String (Coq_x65,
       (String.String (Coq_x64,
       String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val translate_primitive_operation :
    name_env -> localvar_env -> (((kername * String.t) * bool) * nat) -> var
    list -> basic_instruction list error **)

let translate_primitive_operation nenv lenv p args =
  let (p0, _) = p in
  let (p1, _) = p0 in
  let (op_name, _) = p1 in
  (match KernameMap.find op_name primop_map with
   | Some op ->
     (match args with
      | [] ->
        Err (String.String (Coq_x4f, (String.String (Coq_x6e, (String.String
          (Coq_x6c, (String.String (Coq_x79, (String.String (Coq_x20,
          (String.String (Coq_x70, (String.String (Coq_x72, (String.String
          (Coq_x69, (String.String (Coq_x6d, (String.String (Coq_x69,
          (String.String (Coq_x74, (String.String (Coq_x69, (String.String
          (Coq_x76, (String.String (Coq_x65, (String.String (Coq_x20,
          (String.String (Coq_x6f, (String.String (Coq_x70, (String.String
          (Coq_x65, (String.String (Coq_x72, (String.String (Coq_x61,
          (String.String (Coq_x74, (String.String (Coq_x69, (String.String
          (Coq_x6f, (String.String (Coq_x6e, (String.String (Coq_x73,
          (String.String (Coq_x20, (String.String (Coq_x77, (String.String
          (Coq_x69, (String.String (Coq_x74, (String.String (Coq_x68,
          (String.String (Coq_x20, (String.String (Coq_x31, (String.String
          (Coq_x2c, (String.String (Coq_x20, (String.String (Coq_x32,
          (String.String (Coq_x20, (String.String (Coq_x6f, (String.String
          (Coq_x72, (String.String (Coq_x20, (String.String (Coq_x33,
          (String.String (Coq_x20, (String.String (Coq_x61, (String.String
          (Coq_x72, (String.String (Coq_x67, (String.String (Coq_x75,
          (String.String (Coq_x6d, (String.String (Coq_x65, (String.String
          (Coq_x6e, (String.String (Coq_x74, (String.String (Coq_x73,
          (String.String (Coq_x20, (String.String (Coq_x61, (String.String
          (Coq_x72, (String.String (Coq_x65, (String.String (Coq_x20,
          (String.String (Coq_x73, (String.String (Coq_x75, (String.String
          (Coq_x70, (String.String (Coq_x70, (String.String (Coq_x6f,
          (String.String (Coq_x72, (String.String (Coq_x74, (String.String
          (Coq_x65, (String.String (Coq_x64,
          String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      | x :: l ->
        (match l with
         | [] ->
           bind (Obj.magic coq_MonadError)
             (Obj.magic translate_var nenv lenv x (String.String (Coq_x74,
               (String.String (Coq_x72, (String.String (Coq_x61,
               (String.String (Coq_x6e, (String.String (Coq_x73,
               (String.String (Coq_x6c, (String.String (Coq_x61,
               (String.String (Coq_x74, (String.String (Coq_x65,
               (String.String (Coq_x20, (String.String (Coq_x70,
               (String.String (Coq_x72, (String.String (Coq_x69,
               (String.String (Coq_x6d, (String.String (Coq_x69,
               (String.String (Coq_x74, (String.String (Coq_x69,
               (String.String (Coq_x76, (String.String (Coq_x65,
               (String.String (Coq_x20, (String.String (Coq_x75,
               (String.String (Coq_x6e, (String.String (Coq_x6f,
               (String.String (Coq_x70, (String.String (Coq_x20,
               (String.String (Coq_x6f, (String.String (Coq_x70,
               (String.String (Coq_x65, (String.String (Coq_x72,
               (String.String (Coq_x61, (String.String (Coq_x6e,
               (String.String (Coq_x64,
               String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
             (fun x_var -> translate_primitive_unary_op glob_mem_ptr op x_var)
         | y :: l0 ->
           (match l0 with
            | [] ->
              bind (Obj.magic coq_MonadError)
                (Obj.magic translate_var nenv lenv x (String.String (Coq_x74,
                  (String.String (Coq_x72, (String.String (Coq_x61,
                  (String.String (Coq_x6e, (String.String (Coq_x73,
                  (String.String (Coq_x6c, (String.String (Coq_x61,
                  (String.String (Coq_x74, (String.String (Coq_x65,
                  (String.String (Coq_x20, (String.String (Coq_x70,
                  (String.String (Coq_x72, (String.String (Coq_x69,
                  (String.String (Coq_x6d, (String.String (Coq_x69,
                  (String.String (Coq_x74, (String.String (Coq_x69,
                  (String.String (Coq_x76, (String.String (Coq_x65,
                  (String.String (Coq_x20, (String.String (Coq_x62,
                  (String.String (Coq_x69, (String.String (Coq_x6e,
                  (String.String (Coq_x61, (String.String (Coq_x72,
                  (String.String (Coq_x79, (String.String (Coq_x20,
                  (String.String (Coq_x6f, (String.String (Coq_x70,
                  (String.String (Coq_x65, (String.String (Coq_x72,
                  (String.String (Coq_x61, (String.String (Coq_x74,
                  (String.String (Coq_x6f, (String.String (Coq_x72,
                  (String.String (Coq_x20, (String.String (Coq_x31,
                  (String.String (Coq_x73, (String.String (Coq_x74,
                  (String.String (Coq_x20, (String.String (Coq_x6f,
                  (String.String (Coq_x70, (String.String (Coq_x65,
                  (String.String (Coq_x72, (String.String (Coq_x61,
                  (String.String (Coq_x6e, (String.String (Coq_x64,
                  String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                (fun x_var ->
                bind (Obj.magic coq_MonadError)
                  (Obj.magic translate_var nenv lenv y (String.String
                    (Coq_x74, (String.String (Coq_x72, (String.String
                    (Coq_x61, (String.String (Coq_x6e, (String.String
                    (Coq_x73, (String.String (Coq_x6c, (String.String
                    (Coq_x61, (String.String (Coq_x74, (String.String
                    (Coq_x65, (String.String (Coq_x20, (String.String
                    (Coq_x70, (String.String (Coq_x72, (String.String
                    (Coq_x69, (String.String (Coq_x6d, (String.String
                    (Coq_x69, (String.String (Coq_x74, (String.String
                    (Coq_x69, (String.String (Coq_x76, (String.String
                    (Coq_x65, (String.String (Coq_x20, (String.String
                    (Coq_x62, (String.String (Coq_x69, (String.String
                    (Coq_x6e, (String.String (Coq_x61, (String.String
                    (Coq_x72, (String.String (Coq_x79, (String.String
                    (Coq_x20, (String.String (Coq_x6f, (String.String
                    (Coq_x70, (String.String (Coq_x65, (String.String
                    (Coq_x72, (String.String (Coq_x61, (String.String
                    (Coq_x74, (String.String (Coq_x6f, (String.String
                    (Coq_x72, (String.String (Coq_x20, (String.String
                    (Coq_x32, (String.String (Coq_x6e, (String.String
                    (Coq_x64, (String.String (Coq_x20, (String.String
                    (Coq_x6f, (String.String (Coq_x70, (String.String
                    (Coq_x65, (String.String (Coq_x72, (String.String
                    (Coq_x61, (String.String (Coq_x6e, (String.String
                    (Coq_x64,
                    String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                  (fun y_var ->
                  translate_primitive_binary_op glob_mem_ptr glob_tmp1
                    glob_tmp2 glob_tmp3 glob_tmp4 op x_var y_var))
            | z :: l1 ->
              (match l1 with
               | [] ->
                 bind (Obj.magic coq_MonadError)
                   (Obj.magic translate_var nenv lenv x (String.String
                     (Coq_x74, (String.String (Coq_x72, (String.String
                     (Coq_x61, (String.String (Coq_x6e, (String.String
                     (Coq_x73, (String.String (Coq_x6c, (String.String
                     (Coq_x61, (String.String (Coq_x74, (String.String
                     (Coq_x65, (String.String (Coq_x20, (String.String
                     (Coq_x70, (String.String (Coq_x72, (String.String
                     (Coq_x69, (String.String (Coq_x6d, (String.String
                     (Coq_x69, (String.String (Coq_x74, (String.String
                     (Coq_x69, (String.String (Coq_x76, (String.String
                     (Coq_x65, (String.String (Coq_x20, (String.String
                     (Coq_x74, (String.String (Coq_x65, (String.String
                     (Coq_x72, (String.String (Coq_x6e, (String.String
                     (Coq_x61, (String.String (Coq_x72, (String.String
                     (Coq_x79, (String.String (Coq_x20, (String.String
                     (Coq_x6f, (String.String (Coq_x70, (String.String
                     (Coq_x65, (String.String (Coq_x72, (String.String
                     (Coq_x61, (String.String (Coq_x74, (String.String
                     (Coq_x6f, (String.String (Coq_x72, (String.String
                     (Coq_x20, (String.String (Coq_x31, (String.String
                     (Coq_x73, (String.String (Coq_x74, (String.String
                     (Coq_x20, (String.String (Coq_x6f, (String.String
                     (Coq_x70, (String.String (Coq_x65, (String.String
                     (Coq_x72, (String.String (Coq_x61, (String.String
                     (Coq_x6e, (String.String (Coq_x64,
                     String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                   (fun x_var ->
                   bind (Obj.magic coq_MonadError)
                     (Obj.magic translate_var nenv lenv y (String.String
                       (Coq_x74, (String.String (Coq_x72, (String.String
                       (Coq_x61, (String.String (Coq_x6e, (String.String
                       (Coq_x73, (String.String (Coq_x6c, (String.String
                       (Coq_x61, (String.String (Coq_x74, (String.String
                       (Coq_x65, (String.String (Coq_x20, (String.String
                       (Coq_x70, (String.String (Coq_x72, (String.String
                       (Coq_x69, (String.String (Coq_x6d, (String.String
                       (Coq_x69, (String.String (Coq_x74, (String.String
                       (Coq_x69, (String.String (Coq_x76, (String.String
                       (Coq_x65, (String.String (Coq_x20, (String.String
                       (Coq_x74, (String.String (Coq_x65, (String.String
                       (Coq_x72, (String.String (Coq_x6e, (String.String
                       (Coq_x61, (String.String (Coq_x72, (String.String
                       (Coq_x79, (String.String (Coq_x20, (String.String
                       (Coq_x6f, (String.String (Coq_x70, (String.String
                       (Coq_x65, (String.String (Coq_x72, (String.String
                       (Coq_x61, (String.String (Coq_x74, (String.String
                       (Coq_x6f, (String.String (Coq_x72, (String.String
                       (Coq_x20, (String.String (Coq_x32, (String.String
                       (Coq_x6e, (String.String (Coq_x64, (String.String
                       (Coq_x20, (String.String (Coq_x6f, (String.String
                       (Coq_x70, (String.String (Coq_x65, (String.String
                       (Coq_x72, (String.String (Coq_x61, (String.String
                       (Coq_x6e, (String.String (Coq_x64,
                       String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                     (fun y_var ->
                     bind (Obj.magic coq_MonadError)
                       (Obj.magic translate_var nenv lenv z (String.String
                         (Coq_x74, (String.String (Coq_x72, (String.String
                         (Coq_x61, (String.String (Coq_x6e, (String.String
                         (Coq_x73, (String.String (Coq_x6c, (String.String
                         (Coq_x61, (String.String (Coq_x74, (String.String
                         (Coq_x65, (String.String (Coq_x20, (String.String
                         (Coq_x70, (String.String (Coq_x72, (String.String
                         (Coq_x69, (String.String (Coq_x6d, (String.String
                         (Coq_x69, (String.String (Coq_x74, (String.String
                         (Coq_x69, (String.String (Coq_x76, (String.String
                         (Coq_x65, (String.String (Coq_x20, (String.String
                         (Coq_x74, (String.String (Coq_x65, (String.String
                         (Coq_x72, (String.String (Coq_x6e, (String.String
                         (Coq_x61, (String.String (Coq_x72, (String.String
                         (Coq_x79, (String.String (Coq_x20, (String.String
                         (Coq_x6f, (String.String (Coq_x70, (String.String
                         (Coq_x65, (String.String (Coq_x72, (String.String
                         (Coq_x61, (String.String (Coq_x74, (String.String
                         (Coq_x6f, (String.String (Coq_x72, (String.String
                         (Coq_x20, (String.String (Coq_x33, (String.String
                         (Coq_x72, (String.String (Coq_x64, (String.String
                         (Coq_x20, (String.String (Coq_x6f, (String.String
                         (Coq_x70, (String.String (Coq_x65, (String.String
                         (Coq_x72, (String.String (Coq_x61, (String.String
                         (Coq_x6e, (String.String (Coq_x64,
                         String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                       (fun z_var ->
                       translate_primitive_ternary_op glob_mem_ptr glob_tmp1
                         glob_tmp2 glob_tmp3 glob_tmp4 glob_cap op x_var
                         y_var z_var)))
               | _ :: _ ->
                 Err (String.String (Coq_x4f, (String.String (Coq_x6e,
                   (String.String (Coq_x6c, (String.String (Coq_x79,
                   (String.String (Coq_x20, (String.String (Coq_x70,
                   (String.String (Coq_x72, (String.String (Coq_x69,
                   (String.String (Coq_x6d, (String.String (Coq_x69,
                   (String.String (Coq_x74, (String.String (Coq_x69,
                   (String.String (Coq_x76, (String.String (Coq_x65,
                   (String.String (Coq_x20, (String.String (Coq_x6f,
                   (String.String (Coq_x70, (String.String (Coq_x65,
                   (String.String (Coq_x72, (String.String (Coq_x61,
                   (String.String (Coq_x74, (String.String (Coq_x69,
                   (String.String (Coq_x6f, (String.String (Coq_x6e,
                   (String.String (Coq_x73, (String.String (Coq_x20,
                   (String.String (Coq_x77, (String.String (Coq_x69,
                   (String.String (Coq_x74, (String.String (Coq_x68,
                   (String.String (Coq_x20, (String.String (Coq_x31,
                   (String.String (Coq_x2c, (String.String (Coq_x20,
                   (String.String (Coq_x32, (String.String (Coq_x20,
                   (String.String (Coq_x6f, (String.String (Coq_x72,
                   (String.String (Coq_x20, (String.String (Coq_x33,
                   (String.String (Coq_x20, (String.String (Coq_x61,
                   (String.String (Coq_x72, (String.String (Coq_x67,
                   (String.String (Coq_x75, (String.String (Coq_x6d,
                   (String.String (Coq_x65, (String.String (Coq_x6e,
                   (String.String (Coq_x74, (String.String (Coq_x73,
                   (String.String (Coq_x20, (String.String (Coq_x61,
                   (String.String (Coq_x72, (String.String (Coq_x65,
                   (String.String (Coq_x20, (String.String (Coq_x73,
                   (String.String (Coq_x75, (String.String (Coq_x70,
                   (String.String (Coq_x70, (String.String (Coq_x6f,
                   (String.String (Coq_x72, (String.String (Coq_x74,
                   (String.String (Coq_x65, (String.String (Coq_x64,
                   String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
   | None ->
     Err
       (String.append (String.String (Coq_x55, (String.String (Coq_x6e,
         (String.String (Coq_x73, (String.String (Coq_x75, (String.String
         (Coq_x70, (String.String (Coq_x70, (String.String (Coq_x6f,
         (String.String (Coq_x72, (String.String (Coq_x74, (String.String
         (Coq_x65, (String.String (Coq_x64, (String.String (Coq_x20,
         (String.String (Coq_x70, (String.String (Coq_x72, (String.String
         (Coq_x69, (String.String (Coq_x6d, (String.String (Coq_x69,
         (String.String (Coq_x74, (String.String (Coq_x69, (String.String
         (Coq_x76, (String.String (Coq_x65, (String.String (Coq_x20,
         (String.String (Coq_x6f, (String.String (Coq_x70, (String.String
         (Coq_x65, (String.String (Coq_x72, (String.String (Coq_x61,
         (String.String (Coq_x74, (String.String (Coq_x6f, (String.String
         (Coq_x72, (String.String (Coq_x3a, (String.String (Coq_x20,
         String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
         (string_of_kername op_name)))

(** val grow_memory_if_necessary : basic_instruction list **)

let grow_memory_if_necessary =
  (BI_global_get glob_mem_ptr) :: ((BI_const_num
    (coq_N_to_value page_size)) :: ((BI_binop (T_i32, (Binop_i
    BOI_add))) :: ((BI_const_num
    (coq_Z_to_value
      (Z.pow (Zpos (Coq_xO Coq_xH)) (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO
        Coq_xH)))))))) :: ((BI_binop (T_i32, (Binop_i (BOI_div
    SX_S)))) :: (BI_memory_size :: ((BI_relop (T_i32, (Relop_i (ROI_ge
    SX_S)))) :: ((BI_if ((BT_valtype None), ((BI_const_num
    (nat_to_value (S O))) :: (BI_memory_grow :: ((BI_const_num
    (coq_Z_to_value (Zneg Coq_xH))) :: ((BI_relop (T_i32, (Relop_i
    ROI_eq))) :: ((BI_if ((BT_valtype None), ((BI_const_num
    (nat_to_value (S O))) :: ((BI_global_set
    glob_out_of_mem) :: (BI_return :: []))), [])) :: []))))),
    [])) :: [])))))))

(** val call_grow_mem_if_necessary :
    coq_N -> coq_N -> basic_instruction list * coq_N **)

let call_grow_mem_if_necessary mem required_bytes =
  if N.leb required_bytes mem
  then ([], (N.sub mem required_bytes))
  else (grow_memory_if_necessary,
         (N.sub (Npos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
           (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
           (Coq_xO Coq_xH))))))))))))))))) required_bytes))

(** val translate_body :
    name_env -> ctor_env -> localvar_env -> fname_env -> prim_env -> exp ->
    coq_N -> basic_instruction list error **)

let rec translate_body nenv cenv lenv fenv penv e mem =
  match e with
  | Econstr (x, tg, ys, e') ->
    bind (Obj.magic coq_MonadError)
      (Obj.magic translate_var nenv lenv x (String.String (Coq_x74,
        (String.String (Coq_x72, (String.String (Coq_x61, (String.String
        (Coq_x6e, (String.String (Coq_x73, (String.String (Coq_x6c,
        (String.String (Coq_x61, (String.String (Coq_x74, (String.String
        (Coq_x65, (String.String (Coq_x5f, (String.String (Coq_x62,
        (String.String (Coq_x6f, (String.String (Coq_x64, (String.String
        (Coq_x79, (String.String (Coq_x20, (String.String (Coq_x63,
        (String.String (Coq_x6f, (String.String (Coq_x6e, (String.String
        (Coq_x73, (String.String (Coq_x74, (String.String (Coq_x72,
        String.EmptyString)))))))))))))))))))))))))))))))))))))))))))
      (fun x_var ->
      match ys with
      | [] ->
        bind (Obj.magic coq_MonadError) (Obj.magic get_ctor_ord cenv tg)
          (fun ord ->
          bind (Obj.magic coq_MonadError)
            (translate_body nenv cenv lenv fenv penv e' mem)
            (fun following_instr -> Ret
            (app ((BI_const_num
              (nat_to_value
                (N.to_nat
                  (N.add (N.mul (Npos (Coq_xO Coq_xH)) ord) (Npos Coq_xH))))) :: ((BI_local_set
              x_var) :: [])) following_instr)))
      | _ :: _ ->
        bind (Obj.magic coq_MonadError)
          (store_constructor nenv cenv lenv fenv tg ys) (fun store_constr ->
          bind (Obj.magic coq_MonadError) (Obj.magic get_ctor_size cenv tg)
            (fun constr_size ->
            let p = call_grow_mem_if_necessary mem constr_size in
            let grow_mem_instr = fst p in
            let mem' = snd p in
            bind (Obj.magic coq_MonadError)
              (translate_body nenv cenv lenv fenv penv e' mem')
              (fun following_instr -> Ret
              (app grow_mem_instr
                (app store_constr
                  (app ((BI_global_get glob_cap) :: ((BI_local_set
                    x_var) :: [])) following_instr)))))))
  | Ecase (x, arms) ->
    let translate_case_branch_expressions =
      let rec translate_case_branch_expressions = function
      | [] -> Ret ([], [])
      | p :: tl ->
        let (t0, e0) = p in
        bind (Obj.magic coq_MonadError)
          (Obj.magic translate_body nenv cenv lenv fenv penv e0 mem)
          (fun instrs ->
          bind (Obj.magic coq_MonadError)
            (translate_case_branch_expressions tl) (fun x0 ->
            let (arms_boxed, arms_unboxed) = x0 in
            bind (Obj.magic coq_MonadError) (Obj.magic get_ctor_ord cenv t0)
              (fun ord ->
              bind (Obj.magic coq_MonadError)
                (Obj.magic get_ctor_arity cenv t0) (fun arity ->
                if Nat.eqb arity O
                then Ret (arms_boxed, ((ord, instrs) :: arms_unboxed))
                else Ret (((ord, instrs) :: arms_boxed), arms_unboxed)))))
      in translate_case_branch_expressions
    in
    bind (Obj.magic coq_MonadError)
      (Obj.magic translate_var nenv lenv x (String.String (Coq_x74,
        (String.String (Coq_x72, (String.String (Coq_x61, (String.String
        (Coq_x6e, (String.String (Coq_x73, (String.String (Coq_x6c,
        (String.String (Coq_x61, (String.String (Coq_x74, (String.String
        (Coq_x65, (String.String (Coq_x5f, (String.String (Coq_x62,
        (String.String (Coq_x6f, (String.String (Coq_x64, (String.String
        (Coq_x79, (String.String (Coq_x20, (String.String (Coq_x63,
        (String.String (Coq_x61, (String.String (Coq_x73, (String.String
        (Coq_x65, String.EmptyString)))))))))))))))))))))))))))))))))))))))
      (fun x_var ->
      bind (Obj.magic coq_MonadError)
        (Obj.magic translate_case_branch_expressions arms) (fun x0 ->
        let (arms_boxed, arms_unboxed) = x0 in
        Ret ((BI_local_get x_var) :: ((BI_const_num
        (nat_to_value (S O))) :: ((BI_binop (T_i32, (Binop_i
        BOI_and))) :: ((BI_testop (T_i32, TO_eqz)) :: ((BI_if ((BT_valtype
        None), (create_case_nested_if_chain true x_var arms_boxed),
        (create_case_nested_if_chain false x_var arms_unboxed))) :: [])))))))
  | Eproj (x, _, n, y, e') ->
    bind (Obj.magic coq_MonadError)
      (translate_body nenv cenv lenv fenv penv e' mem)
      (fun following_instr ->
      bind (Obj.magic coq_MonadError)
        (Obj.magic translate_var nenv lenv y (String.String (Coq_x74,
          (String.String (Coq_x72, (String.String (Coq_x61, (String.String
          (Coq_x6e, (String.String (Coq_x73, (String.String (Coq_x6c,
          (String.String (Coq_x61, (String.String (Coq_x74, (String.String
          (Coq_x65, (String.String (Coq_x5f, (String.String (Coq_x62,
          (String.String (Coq_x6f, (String.String (Coq_x64, (String.String
          (Coq_x79, (String.String (Coq_x20, (String.String (Coq_x70,
          (String.String (Coq_x72, (String.String (Coq_x6f, (String.String
          (Coq_x6a, (String.String (Coq_x20, (String.String (Coq_x79,
          String.EmptyString)))))))))))))))))))))))))))))))))))))))))))
        (fun y_var ->
        bind (Obj.magic coq_MonadError)
          (Obj.magic translate_var nenv lenv x (String.String (Coq_x74,
            (String.String (Coq_x72, (String.String (Coq_x61, (String.String
            (Coq_x6e, (String.String (Coq_x73, (String.String (Coq_x6c,
            (String.String (Coq_x61, (String.String (Coq_x74, (String.String
            (Coq_x65, (String.String (Coq_x5f, (String.String (Coq_x62,
            (String.String (Coq_x6f, (String.String (Coq_x64, (String.String
            (Coq_x79, (String.String (Coq_x20, (String.String (Coq_x70,
            (String.String (Coq_x72, (String.String (Coq_x6f, (String.String
            (Coq_x6a, (String.String (Coq_x20, (String.String (Coq_x78,
            String.EmptyString)))))))))))))))))))))))))))))))))))))))))))
          (fun x_var -> Ret
          (app ((BI_local_get y_var) :: ((BI_const_num
            (nat_to_value (mul (add (N.to_nat n) (S O)) (S (S (S (S O))))))) :: ((BI_binop
            (T_i32, (Binop_i BOI_add))) :: ((BI_load (T_i32, None,
            { memarg_offset = N0; memarg_align = (Npos (Coq_xO
            Coq_xH)) })) :: ((BI_local_set x_var) :: []))))) following_instr))))
  | Eletapp (x, f, _, ys, e') ->
    bind (Obj.magic coq_MonadError)
      (Obj.magic translate_var nenv lenv x (String.String (Coq_x74,
        (String.String (Coq_x72, (String.String (Coq_x61, (String.String
        (Coq_x6e, (String.String (Coq_x73, (String.String (Coq_x6c,
        (String.String (Coq_x61, (String.String (Coq_x74, (String.String
        (Coq_x65, (String.String (Coq_x5f, (String.String (Coq_x62,
        (String.String (Coq_x6f, (String.String (Coq_x64, (String.String
        (Coq_x79, (String.String (Coq_x20, (String.String (Coq_x70,
        (String.String (Coq_x72, (String.String (Coq_x6f, (String.String
        (Coq_x6a, (String.String (Coq_x20, (String.String (Coq_x78,
        String.EmptyString)))))))))))))))))))))))))))))))))))))))))))
      (fun x_var ->
      bind (Obj.magic coq_MonadError)
        (translate_body nenv cenv lenv fenv penv e' N0)
        (fun following_instr ->
        bind (Obj.magic coq_MonadError)
          (translate_call nenv lenv fenv f ys false) (fun instr_call -> Ret
          (app instr_call
            (app ((BI_global_get glob_out_of_mem) :: ((BI_if ((BT_valtype
              None), (BI_return :: []), [])) :: ((BI_global_get
              glob_result) :: ((BI_local_set x_var) :: [])))) following_instr)))))
  | Efun (_, _) ->
    Err (String.String (Coq_x75, (String.String (Coq_x6e, (String.String
      (Coq_x65, (String.String (Coq_x78, (String.String (Coq_x70,
      (String.String (Coq_x65, (String.String (Coq_x63, (String.String
      (Coq_x74, (String.String (Coq_x65, (String.String (Coq_x64,
      (String.String (Coq_x20, (String.String (Coq_x6e, (String.String
      (Coq_x65, (String.String (Coq_x73, (String.String (Coq_x74,
      (String.String (Coq_x65, (String.String (Coq_x64, (String.String
      (Coq_x20, (String.String (Coq_x66, (String.String (Coq_x75,
      (String.String (Coq_x6e, (String.String (Coq_x63, (String.String
      (Coq_x74, (String.String (Coq_x69, (String.String (Coq_x6f,
      (String.String (Coq_x6e, (String.String (Coq_x20, (String.String
      (Coq_x64, (String.String (Coq_x65, (String.String (Coq_x66,
      (String.String (Coq_x69, (String.String (Coq_x6e, (String.String
      (Coq_x69, (String.String (Coq_x74, (String.String (Coq_x69,
      (String.String (Coq_x6f, (String.String (Coq_x6e,
      String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | Eapp (f, _, ys) -> translate_call nenv lenv fenv f ys true
  | Eprim_val (x, p, e') ->
    bind (Obj.magic coq_MonadError)
      (Obj.magic translate_var nenv lenv x (String.String (Coq_x74,
        (String.String (Coq_x72, (String.String (Coq_x61, (String.String
        (Coq_x6e, (String.String (Coq_x73, (String.String (Coq_x6c,
        (String.String (Coq_x61, (String.String (Coq_x74, (String.String
        (Coq_x65, (String.String (Coq_x5f, (String.String (Coq_x62,
        (String.String (Coq_x6f, (String.String (Coq_x64, (String.String
        (Coq_x79, (String.String (Coq_x20, (String.String (Coq_x70,
        (String.String (Coq_x72, (String.String (Coq_x69, (String.String
        (Coq_x6d, (String.String (Coq_x20, (String.String (Coq_x76,
        (String.String (Coq_x61, (String.String (Coq_x6c,
        String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))
      (fun x_var ->
      bind (Obj.magic coq_MonadError) (Obj.magic translate_primitive_value p)
        (fun val0 ->
        let instrs = (BI_global_get glob_mem_ptr) :: ((BI_const_num
          (VAL_int64 val0)) :: ((BI_store (T_i64, None, { memarg_offset = N0;
          memarg_align = (Npos (Coq_xO Coq_xH)) })) :: ((BI_global_get
          glob_mem_ptr) :: ((BI_local_set x_var) :: ((BI_global_get
          glob_mem_ptr) :: ((BI_const_num
          (nat_to_value (S (S (S (S (S (S (S (S O)))))))))) :: ((BI_binop
          (T_i32, (Binop_i BOI_add))) :: ((BI_global_set
          glob_mem_ptr) :: []))))))))
        in
        let p0 =
          call_grow_mem_if_necessary mem (Npos (Coq_xO (Coq_xO (Coq_xO
            (Coq_xO (Coq_xO Coq_xH))))))
        in
        let grow_instr = fst p0 in
        let mem' = snd p0 in
        bind (Obj.magic coq_MonadError)
          (translate_body nenv cenv lenv fenv penv e' mem')
          (fun following_instr -> Ret
          (app grow_instr (app instrs following_instr)))))
  | Eprim (x, p, ys, e') ->
    (match M.get p penv with
     | Some p' ->
       bind (Obj.magic coq_MonadError)
         (Obj.magic translate_var nenv lenv x (String.String (Coq_x74,
           (String.String (Coq_x72, (String.String (Coq_x61, (String.String
           (Coq_x6e, (String.String (Coq_x73, (String.String (Coq_x6c,
           (String.String (Coq_x61, (String.String (Coq_x74, (String.String
           (Coq_x65, (String.String (Coq_x5f, (String.String (Coq_x65,
           (String.String (Coq_x78, (String.String (Coq_x70, (String.String
           (Coq_x20, (String.String (Coq_x70, (String.String (Coq_x72,
           (String.String (Coq_x69, (String.String (Coq_x6d, (String.String
           (Coq_x20, (String.String (Coq_x6f, (String.String (Coq_x70,
           String.EmptyString)))))))))))))))))))))))))))))))))))))))))))
         (fun x_var ->
         bind (Obj.magic coq_MonadError)
           (translate_primitive_operation nenv lenv p' ys)
           (fun prim_op_instrs ->
           let p0 =
             call_grow_mem_if_necessary mem (Npos (Coq_xO (Coq_xO (Coq_xI
               (Coq_xO (Coq_xI Coq_xH))))))
           in
           let grow_instr = fst p0 in
           let mem' = snd p0 in
           bind (Obj.magic coq_MonadError)
             (translate_body nenv cenv lenv fenv penv e' mem')
             (fun following_instr -> Ret
             (app grow_instr
               (app prim_op_instrs
                 (app ((BI_local_set x_var) :: []) following_instr))))))
     | None ->
       Err (String.String (Coq_x50, (String.String (Coq_x72, (String.String
         (Coq_x69, (String.String (Coq_x6d, (String.String (Coq_x69,
         (String.String (Coq_x74, (String.String (Coq_x69, (String.String
         (Coq_x76, (String.String (Coq_x65, (String.String (Coq_x20,
         (String.String (Coq_x6f, (String.String (Coq_x70, (String.String
         (Coq_x65, (String.String (Coq_x72, (String.String (Coq_x61,
         (String.String (Coq_x74, (String.String (Coq_x69, (String.String
         (Coq_x6f, (String.String (Coq_x6e, (String.String (Coq_x20,
         (String.String (Coq_x6e, (String.String (Coq_x6f, (String.String
         (Coq_x74, (String.String (Coq_x20, (String.String (Coq_x66,
         (String.String (Coq_x6f, (String.String (Coq_x75, (String.String
         (Coq_x6e, (String.String (Coq_x64, (String.String (Coq_x20,
         (String.String (Coq_x69, (String.String (Coq_x6e, (String.String
         (Coq_x20, (String.String (Coq_x70, (String.String (Coq_x72,
         (String.String (Coq_x69, (String.String (Coq_x6d, (String.String
         (Coq_x5f, (String.String (Coq_x65, (String.String (Coq_x6e,
         (String.String (Coq_x76,
         String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | Ehalt x ->
    bind (Obj.magic coq_MonadError)
      (Obj.magic translate_var nenv lenv x (String.String (Coq_x74,
        (String.String (Coq_x72, (String.String (Coq_x61, (String.String
        (Coq_x6e, (String.String (Coq_x73, (String.String (Coq_x6c,
        (String.String (Coq_x61, (String.String (Coq_x74, (String.String
        (Coq_x65, (String.String (Coq_x5f, (String.String (Coq_x62,
        (String.String (Coq_x6f, (String.String (Coq_x64, (String.String
        (Coq_x79, (String.String (Coq_x20, (String.String (Coq_x68,
        (String.String (Coq_x61, (String.String (Coq_x6c, (String.String
        (Coq_x74, String.EmptyString)))))))))))))))))))))))))))))))))))))))
      (fun x_var -> Ret ((BI_local_get x_var) :: ((BI_global_set
      glob_result) :: (BI_return :: []))))

(** val collect_local_variables : exp -> var list **)

let rec collect_local_variables = function
| Econstr (x, _, _, e') -> x :: (collect_local_variables e')
| Ecase (_, arms) -> flat_map (fun a -> collect_local_variables (snd a)) arms
| Eproj (x, _, _, _, e') -> x :: (collect_local_variables e')
| Eletapp (x, _, _, _, e') -> x :: (collect_local_variables e')
| Efun (_, e') -> collect_local_variables e'
| Eprim_val (x, _, e') -> x :: (collect_local_variables e')
| Eprim (x, _, _, e') -> x :: (collect_local_variables e')
| _ -> []

(** val create_var_mapping : u32 -> var list -> u32 M.tree -> u32 M.tree **)

let rec create_var_mapping start_id vars env =
  match vars with
  | [] -> env
  | v :: l' ->
    let mapping = create_var_mapping (N.succ start_id) l' env in
    M.set v start_id mapping

(** val create_local_variable_mapping : var list -> localvar_env **)

let create_local_variable_mapping vars =
  create_var_mapping N0 vars M.empty

(** val translate_function :
    name_env -> ctor_env -> fname_env -> prim_env -> var -> var list -> exp
    -> wasm_function error **)

let translate_function nenv cenv fenv penv f args body0 =
  let locals0 = collect_local_variables body0 in
  let lenv = create_local_variable_mapping (app args locals0) in
  bind (Obj.magic coq_MonadError)
    (Obj.magic translate_var nenv fenv f (String.String (Coq_x74,
      (String.String (Coq_x72, (String.String (Coq_x61, (String.String
      (Coq_x6e, (String.String (Coq_x73, (String.String (Coq_x6c,
      (String.String (Coq_x61, (String.String (Coq_x74, (String.String
      (Coq_x65, (String.String (Coq_x20, (String.String (Coq_x66,
      (String.String (Coq_x75, (String.String (Coq_x6e, (String.String
      (Coq_x63, (String.String (Coq_x74, (String.String (Coq_x69,
      (String.String (Coq_x6f, (String.String (Coq_x6e,
      String.EmptyString))))))))))))))))))))))))))))))))))))) (fun fn_idx ->
    bind (Obj.magic coq_MonadError)
      (Obj.magic translate_body nenv cenv lenv fenv penv body0 N0)
      (fun body_res -> Ret { fidx = fn_idx; export_name =
      (show_tree (show_var nenv f)); coq_type = (N.of_nat (length args));
      locals = (map (fun _ -> T_num T_i32) locals0); body = body_res }))

(** val translate_functions :
    name_env -> ctor_env -> fname_env -> prim_env -> fundefs -> wasm_function
    list error **)

let rec translate_functions nenv cenv fenv penv = function
| Fcons (x, _, xs, e, fds') ->
  bind (Obj.magic coq_MonadError)
    (Obj.magic translate_function nenv cenv fenv penv x xs e) (fun fn ->
    bind (Obj.magic coq_MonadError)
      (translate_functions nenv cenv fenv penv fds') (fun following -> Ret
      (fn :: following)))
| Fnil -> Ret []

(** val sanitize_function_name : String.t -> nat -> String.t **)

let sanitize_function_name s prefix_id =
  let prefix_bytes =
    app (String.print (string_of_nat prefix_id)) (Coq_x5f :: (Coq_x5f :: []))
  in
  let s_bytes = String.print s in
  let s_bytes' =
    map (fun b -> match b with
                  | Coq_x2e -> Coq_x5f
                  | _ -> b) s_bytes
  in
  let bytes'' = Coq_x5f :: (app prefix_bytes s_bytes') in String.parse bytes''

(** val unique_export_names : wasm_function list -> wasm_function list **)

let unique_export_names fns =
  mapi (fun i fn -> { fidx = fn.fidx; export_name =
    (sanitize_function_name fn.export_name i); coq_type = fn.coq_type;
    locals = fn.locals; body = fn.body }) fns

(** val collect_function_vars : exp -> var list **)

let collect_function_vars = function
| Efun (fds, _) ->
  let rec iter = function
  | Fcons (x, _, _, _, fds') -> x :: (iter fds')
  | Fnil -> []
  in iter fds
| _ -> []

(** val create_fname_mapping : exp -> fname_env **)

let create_fname_mapping e =
  let fun_vars = collect_function_vars e in
  create_var_mapping (N.of_nat num_custom_funs) fun_vars M.empty

(** val list_function_types : nat -> function_type list **)

let rec list_function_types = function
| O -> (Tf ([], [])) :: []
| S n' ->
  (Tf ([],
    [])) :: (map (fun t0 ->
              let Tf (args, rt) = t0 in Tf (((T_num T_i32) :: args), rt))
              (list_function_types n'))

(** val table_element_mapping : nat -> nat -> module_element list **)

let rec table_element_mapping len startidx =
  match len with
  | O -> []
  | S len' ->
    { modelem_type = T_funcref; modelem_init = (((BI_ref_func
      (N.of_nat startidx)) :: []) :: []); modelem_mode = (ME_active (N0,
      ((BI_const_num
      (nat_to_value startidx)) :: []))) } :: (table_element_mapping len' (S
                                               startidx))

(** val coq_LambdaANF_to_Wasm :
    name_env -> ctor_env -> prim_env -> exp ->
    ((coq_module * fname_env) * localvar_env) error **)

let coq_LambdaANF_to_Wasm nenv cenv penv e =
  bind (Obj.magic coq_MonadError) (Obj.magic check_restrictions cenv e)
    (fun _ ->
    let fname_mapping = create_fname_mapping e in
    bind (Obj.magic coq_MonadError)
      (match e with
       | Efun (fds, _) ->
         Obj.magic translate_functions nenv cenv fname_mapping penv fds
       | _ ->
         Err (String.String (Coq_x75, (String.String (Coq_x6e, (String.String
           (Coq_x72, (String.String (Coq_x65, (String.String (Coq_x61,
           (String.String (Coq_x63, (String.String (Coq_x68, (String.String
           (Coq_x61, (String.String (Coq_x62, (String.String (Coq_x6c,
           (String.String (Coq_x65, String.EmptyString)))))))))))))))))))))))
      (fun fns ->
      bind (Obj.magic coq_MonadError)
        (match e with
         | Efun (_, exp0) -> Ret (Obj.magic exp0)
         | _ ->
           Err (String.String (Coq_x75, (String.String (Coq_x6e,
             (String.String (Coq_x72, (String.String (Coq_x65, (String.String
             (Coq_x61, (String.String (Coq_x63, (String.String (Coq_x68,
             (String.String (Coq_x61, (String.String (Coq_x62, (String.String
             (Coq_x6c, (String.String (Coq_x65,
             String.EmptyString))))))))))))))))))))))) (fun main_expr ->
        let main_vars = collect_local_variables main_expr in
        let main_lenv = create_local_variable_mapping main_vars in
        bind (Obj.magic coq_MonadError)
          (Obj.magic translate_body nenv cenv main_lenv fname_mapping penv
            main_expr N0) (fun main_instr ->
          let main_function = { fidx = main_function_idx; export_name =
            main_function_name; coq_type = N0; locals =
            (map (fun _ -> T_num T_i32) main_vars); body = main_instr }
          in
          let functions = app (main_function :: []) (unique_export_names fns)
          in
          let exports =
            app
              (map (fun f -> { modexp_name = (String.print f.export_name);
                modexp_desc = (MED_func f.fidx) }) functions)
              ({ modexp_name =
              (String.print (String.String (Coq_x6f, (String.String (Coq_x75,
                (String.String (Coq_x74, (String.String (Coq_x5f,
                (String.String (Coq_x6f, (String.String (Coq_x66,
                (String.String (Coq_x5f, (String.String (Coq_x6d,
                (String.String (Coq_x65, (String.String (Coq_x6d,
                String.EmptyString))))))))))))))))))))); modexp_desc =
              (MED_global glob_out_of_mem) } :: ({ modexp_name =
              (String.print (String.String (Coq_x6d, (String.String (Coq_x65,
                (String.String (Coq_x6d, (String.String (Coq_x5f,
                (String.String (Coq_x70, (String.String (Coq_x74,
                (String.String (Coq_x72, String.EmptyString)))))))))))))));
              modexp_desc = (MED_global glob_mem_ptr) } :: ({ modexp_name =
              (String.print (String.String (Coq_x72, (String.String (Coq_x65,
                (String.String (Coq_x73, (String.String (Coq_x75,
                (String.String (Coq_x6c, (String.String (Coq_x74,
                String.EmptyString))))))))))))); modexp_desc = (MED_global
              glob_result) } :: ({ modexp_name =
              (String.print (String.String (Coq_x6d, (String.String (Coq_x65,
                (String.String (Coq_x6d, (String.String (Coq_x6f,
                (String.String (Coq_x72, (String.String (Coq_x79,
                String.EmptyString))))))))))))); modexp_desc = (MED_mem
              N0) } :: []))))
          in
          let elements =
            table_element_mapping (add (length fns) num_custom_funs) O
          in
          let functions_final =
            map (fun f -> { modfunc_type = f.coq_type; modfunc_locals =
              f.locals; modfunc_body = f.body }) functions
          in
          let ftys = list_function_types (Z.to_nat max_function_args) in
          let module0 = { mod_types = ftys; mod_funcs = functions_final;
            mod_tables = ({ tt_limits = { lim_min =
            (N.of_nat (add (length fns) num_custom_funs)); lim_max = None };
            tt_elem_type = T_funcref } :: []); mod_mems = ({ lim_min = (Npos
            Coq_xH); lim_max = (Some max_mem_pages) } :: []); mod_globals =
            ({ modglob_type = { tg_mut = MUT_var; tg_t = (T_num T_i32) };
            modglob_init = ((BI_const_num
            (nat_to_value O)) :: []) } :: ({ modglob_type = { tg_mut =
            MUT_var; tg_t = (T_num T_i32) }; modglob_init = ((BI_const_num
            (nat_to_value O)) :: []) } :: ({ modglob_type = { tg_mut =
            MUT_var; tg_t = (T_num T_i32) }; modglob_init = ((BI_const_num
            (nat_to_value O)) :: []) } :: ({ modglob_type = { tg_mut =
            MUT_var; tg_t = (T_num T_i32) }; modglob_init = ((BI_const_num
            (nat_to_value O)) :: []) } :: ({ modglob_type = { tg_mut =
            MUT_var; tg_t = (T_num T_i64) }; modglob_init = ((BI_const_num
            (nat_to_value64 O)) :: []) } :: ({ modglob_type = { tg_mut =
            MUT_var; tg_t = (T_num T_i64) }; modglob_init = ((BI_const_num
            (nat_to_value64 O)) :: []) } :: ({ modglob_type = { tg_mut =
            MUT_var; tg_t = (T_num T_i64) }; modglob_init = ((BI_const_num
            (nat_to_value64 O)) :: []) } :: ({ modglob_type = { tg_mut =
            MUT_var; tg_t = (T_num T_i64) }; modglob_init = ((BI_const_num
            (nat_to_value64 O)) :: []) } :: [])))))))); mod_elems = elements;
            mod_datas = []; mod_start = None; mod_imports = []; mod_exports =
            exports }
          in
          Ret ((module0, fname_mapping), main_lenv)))))
