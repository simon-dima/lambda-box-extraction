open BinInt
open BinNums
open Byte
open Datatypes
open List0
open MCString
open Monad0
open Bytestring
open CompM
open Cps
open Cps_util
open Numerics

(** val max_function_args : coq_Z **)

let max_function_args =
  Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))

(** val max_num_functions : coq_Z **)

let max_num_functions =
  Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI
    (Coq_xI (Coq_xI Coq_xH)))))))))))))))))))

(** val max_constr_args : coq_Z **)

let max_constr_args =
  Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO Coq_xH))))))))))

(** val coq_assert : bool -> String.t -> unit error **)

let coq_assert b err =
  if b then Ret () else Err err

(** val get_ctor_ord : ctor_env -> ctor_tag -> coq_N error **)

let get_ctor_ord cenv t0 =
  match M.get t0 cenv with
  | Some c -> Ret c.ctor_ordinal
  | None ->
    Err
      (String.append (String.String (Coq_x43, (String.String (Coq_x6f,
        (String.String (Coq_x6e, (String.String (Coq_x73, (String.String
        (Coq_x74, (String.String (Coq_x72, (String.String (Coq_x75,
        (String.String (Coq_x63, (String.String (Coq_x74, (String.String
        (Coq_x6f, (String.String (Coq_x72, (String.String (Coq_x20,
        (String.String (Coq_x77, (String.String (Coq_x69, (String.String
        (Coq_x74, (String.String (Coq_x68, (String.String (Coq_x20,
        (String.String (Coq_x74, (String.String (Coq_x61, (String.String
        (Coq_x67, (String.String (Coq_x20,
        String.EmptyString))))))))))))))))))))))))))))))))))))))))))
        (String.append (string_of_positive t0) (String.String (Coq_x20,
          (String.String (Coq_x69, (String.String (Coq_x6e, (String.String
          (Coq_x20, (String.String (Coq_x63, (String.String (Coq_x6f,
          (String.String (Coq_x6e, (String.String (Coq_x73, (String.String
          (Coq_x74, (String.String (Coq_x72, (String.String (Coq_x75,
          (String.String (Coq_x63, (String.String (Coq_x74, (String.String
          (Coq_x6f, (String.String (Coq_x72, (String.String (Coq_x20,
          (String.String (Coq_x65, (String.String (Coq_x78, (String.String
          (Coq_x70, (String.String (Coq_x72, (String.String (Coq_x65,
          (String.String (Coq_x73, (String.String (Coq_x73, (String.String
          (Coq_x69, (String.String (Coq_x6f, (String.String (Coq_x6e,
          (String.String (Coq_x20, (String.String (Coq_x6e, (String.String
          (Coq_x6f, (String.String (Coq_x74, (String.String (Coq_x20,
          (String.String (Coq_x66, (String.String (Coq_x6f, (String.String
          (Coq_x75, (String.String (Coq_x6e, (String.String (Coq_x64,
          (String.String (Coq_x20, (String.String (Coq_x69, (String.String
          (Coq_x6e, (String.String (Coq_x20, (String.String (Coq_x63,
          (String.String (Coq_x6f, (String.String (Coq_x6e, (String.String
          (Coq_x73, (String.String (Coq_x74, (String.String (Coq_x72,
          (String.String (Coq_x75, (String.String (Coq_x63, (String.String
          (Coq_x74, (String.String (Coq_x6f, (String.String (Coq_x72,
          (String.String (Coq_x20, (String.String (Coq_x65, (String.String
          (Coq_x6e, (String.String (Coq_x76, (String.String (Coq_x69,
          (String.String (Coq_x72, (String.String (Coq_x6f, (String.String
          (Coq_x6e, (String.String (Coq_x6d, (String.String (Coq_x65,
          (String.String (Coq_x6e, (String.String (Coq_x74,
          String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val check_restrictions : ctor_env -> exp -> unit error **)

let rec check_restrictions cenv = function
| Econstr (_, t0, ys, e') ->
  bind (Obj.magic coq_MonadError) (Obj.magic get_ctor_ord cenv t0)
    (fun ord ->
    bind (Obj.magic coq_MonadError)
      (coq_assert (Z.ltb (Z.of_N ord) Wasm_int.Int32.half_modulus)
        (String.String (Coq_x43, (String.String (Coq_x6f, (String.String
        (Coq_x6e, (String.String (Coq_x73, (String.String (Coq_x74,
        (String.String (Coq_x72, (String.String (Coq_x75, (String.String
        (Coq_x63, (String.String (Coq_x74, (String.String (Coq_x6f,
        (String.String (Coq_x72, (String.String (Coq_x20, (String.String
        (Coq_x6f, (String.String (Coq_x72, (String.String (Coq_x64,
        (String.String (Coq_x69, (String.String (Coq_x6e, (String.String
        (Coq_x61, (String.String (Coq_x6c, (String.String (Coq_x20,
        (String.String (Coq_x74, (String.String (Coq_x6f, (String.String
        (Coq_x6f, (String.String (Coq_x20, (String.String (Coq_x6c,
        (String.String (Coq_x61, (String.String (Coq_x72, (String.String
        (Coq_x67, (String.String (Coq_x65,
        String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      (fun _ ->
      bind (Obj.magic coq_MonadError)
        (coq_assert (Z.leb (Z.of_nat (length ys)) max_constr_args)
          (String.String (Coq_x66, (String.String (Coq_x6f, (String.String
          (Coq_x75, (String.String (Coq_x6e, (String.String (Coq_x64,
          (String.String (Coq_x20, (String.String (Coq_x63, (String.String
          (Coq_x6f, (String.String (Coq_x6e, (String.String (Coq_x73,
          (String.String (Coq_x74, (String.String (Coq_x72, (String.String
          (Coq_x75, (String.String (Coq_x63, (String.String (Coq_x74,
          (String.String (Coq_x6f, (String.String (Coq_x72, (String.String
          (Coq_x20, (String.String (Coq_x77, (String.String (Coq_x69,
          (String.String (Coq_x74, (String.String (Coq_x68, (String.String
          (Coq_x20, (String.String (Coq_x74, (String.String (Coq_x6f,
          (String.String (Coq_x6f, (String.String (Coq_x20, (String.String
          (Coq_x6d, (String.String (Coq_x61, (String.String (Coq_x6e,
          (String.String (Coq_x79, (String.String (Coq_x20, (String.String
          (Coq_x61, (String.String (Coq_x72, (String.String (Coq_x67,
          (String.String (Coq_x73, (String.String (Coq_x2c, (String.String
          (Coq_x20, (String.String (Coq_x63, (String.String (Coq_x68,
          (String.String (Coq_x65, (String.String (Coq_x63, (String.String
          (Coq_x6b, (String.String (Coq_x20, (String.String (Coq_x6d,
          (String.String (Coq_x61, (String.String (Coq_x78, (String.String
          (Coq_x5f, (String.String (Coq_x63, (String.String (Coq_x6f,
          (String.String (Coq_x6e, (String.String (Coq_x73, (String.String
          (Coq_x74, (String.String (Coq_x72, (String.String (Coq_x5f,
          (String.String (Coq_x61, (String.String (Coq_x72, (String.String
          (Coq_x67, (String.String (Coq_x73,
          String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
        (fun _ -> check_restrictions cenv e')))
| Ecase (_, ms) ->
  bind (Obj.magic coq_MonadError)
    (sequence (Obj.magic coq_MonadError)
      (map (fun pat ->
        let (t0, e') = pat in
        bind (Obj.magic coq_MonadError) (Obj.magic get_ctor_ord cenv t0)
          (fun ord ->
          bind (Obj.magic coq_MonadError)
            (coq_assert (Z.ltb (Z.of_N ord) Wasm_int.Int32.half_modulus)
              (String.String (Coq_x43, (String.String (Coq_x6f,
              (String.String (Coq_x6e, (String.String (Coq_x73,
              (String.String (Coq_x74, (String.String (Coq_x72,
              (String.String (Coq_x75, (String.String (Coq_x63,
              (String.String (Coq_x74, (String.String (Coq_x6f,
              (String.String (Coq_x72, (String.String (Coq_x20,
              (String.String (Coq_x6f, (String.String (Coq_x72,
              (String.String (Coq_x64, (String.String (Coq_x69,
              (String.String (Coq_x6e, (String.String (Coq_x61,
              (String.String (Coq_x6c, (String.String (Coq_x20,
              (String.String (Coq_x74, (String.String (Coq_x6f,
              (String.String (Coq_x6f, (String.String (Coq_x20,
              (String.String (Coq_x6c, (String.String (Coq_x61,
              (String.String (Coq_x72, (String.String (Coq_x67,
              (String.String (Coq_x65,
              String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
            (fun _ -> check_restrictions cenv e'))) ms)) (fun _ -> Ret ())
| Eproj (_, _, _, _, e') -> check_restrictions cenv e'
| Eletapp (_, _, _, ys, e') ->
  bind (Obj.magic coq_MonadError)
    (coq_assert (Z.leb (Z.of_nat (length ys)) max_function_args)
      (String.String (Coq_x66, (String.String (Coq_x6f, (String.String
      (Coq_x75, (String.String (Coq_x6e, (String.String (Coq_x64,
      (String.String (Coq_x20, (String.String (Coq_x66, (String.String
      (Coq_x75, (String.String (Coq_x6e, (String.String (Coq_x63,
      (String.String (Coq_x74, (String.String (Coq_x69, (String.String
      (Coq_x6f, (String.String (Coq_x6e, (String.String (Coq_x20,
      (String.String (Coq_x61, (String.String (Coq_x70, (String.String
      (Coq_x70, (String.String (Coq_x6c, (String.String (Coq_x69,
      (String.String (Coq_x63, (String.String (Coq_x61, (String.String
      (Coq_x74, (String.String (Coq_x69, (String.String (Coq_x6f,
      (String.String (Coq_x6e, (String.String (Coq_x20, (String.String
      (Coq_x77, (String.String (Coq_x69, (String.String (Coq_x74,
      (String.String (Coq_x68, (String.String (Coq_x20, (String.String
      (Coq_x74, (String.String (Coq_x6f, (String.String (Coq_x6f,
      (String.String (Coq_x20, (String.String (Coq_x6d, (String.String
      (Coq_x61, (String.String (Coq_x6e, (String.String (Coq_x79,
      (String.String (Coq_x20, (String.String (Coq_x66, (String.String
      (Coq_x75, (String.String (Coq_x6e, (String.String (Coq_x63,
      (String.String (Coq_x74, (String.String (Coq_x69, (String.String
      (Coq_x6f, (String.String (Coq_x6e, (String.String (Coq_x20,
      (String.String (Coq_x70, (String.String (Coq_x61, (String.String
      (Coq_x72, (String.String (Coq_x61, (String.String (Coq_x6d,
      (String.String (Coq_x73, (String.String (Coq_x2c, (String.String
      (Coq_x20, (String.String (Coq_x63, (String.String (Coq_x68,
      (String.String (Coq_x65, (String.String (Coq_x63, (String.String
      (Coq_x6b, (String.String (Coq_x20, (String.String (Coq_x6d,
      (String.String (Coq_x61, (String.String (Coq_x78, (String.String
      (Coq_x5f, (String.String (Coq_x66, (String.String (Coq_x75,
      (String.String (Coq_x6e, (String.String (Coq_x63, (String.String
      (Coq_x74, (String.String (Coq_x69, (String.String (Coq_x6f,
      (String.String (Coq_x6e, (String.String (Coq_x5f, (String.String
      (Coq_x61, (String.String (Coq_x72, (String.String (Coq_x67,
      (String.String (Coq_x73,
      String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    (fun _ -> check_restrictions cenv e')
| Efun (fds, e') ->
  bind (Obj.magic coq_MonadError)
    (coq_assert (Z.leb (Z.of_nat (numOf_fundefs fds)) max_num_functions)
      (String.String (Coq_x74, (String.String (Coq_x6f, (String.String
      (Coq_x6f, (String.String (Coq_x20, (String.String (Coq_x6d,
      (String.String (Coq_x61, (String.String (Coq_x6e, (String.String
      (Coq_x79, (String.String (Coq_x20, (String.String (Coq_x66,
      (String.String (Coq_x75, (String.String (Coq_x6e, (String.String
      (Coq_x63, (String.String (Coq_x74, (String.String (Coq_x69,
      (String.String (Coq_x6f, (String.String (Coq_x6e, (String.String
      (Coq_x73, (String.String (Coq_x2c, (String.String (Coq_x20,
      (String.String (Coq_x63, (String.String (Coq_x68, (String.String
      (Coq_x65, (String.String (Coq_x63, (String.String (Coq_x6b,
      (String.String (Coq_x20, (String.String (Coq_x6d, (String.String
      (Coq_x61, (String.String (Coq_x78, (String.String (Coq_x5f,
      (String.String (Coq_x6e, (String.String (Coq_x75, (String.String
      (Coq_x6d, (String.String (Coq_x5f, (String.String (Coq_x66,
      (String.String (Coq_x75, (String.String (Coq_x6e, (String.String
      (Coq_x63, (String.String (Coq_x74, (String.String (Coq_x69,
      (String.String (Coq_x6f, (String.String (Coq_x6e, (String.String
      (Coq_x73,
      String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    (fun _ ->
    bind (Obj.magic coq_MonadError)
      (let rec iter = function
       | Fcons (_, _, ys, e'0, fds') ->
         bind (Obj.magic coq_MonadError)
           (coq_assert (Z.leb (Z.of_nat (length ys)) max_function_args)
             (String.String (Coq_x66, (String.String (Coq_x6f, (String.String
             (Coq_x75, (String.String (Coq_x6e, (String.String (Coq_x64,
             (String.String (Coq_x20, (String.String (Coq_x66, (String.String
             (Coq_x75, (String.String (Coq_x6e, (String.String (Coq_x64,
             (String.String (Coq_x65, (String.String (Coq_x66, (String.String
             (Coq_x20, (String.String (Coq_x77, (String.String (Coq_x69,
             (String.String (Coq_x74, (String.String (Coq_x68, (String.String
             (Coq_x20, (String.String (Coq_x74, (String.String (Coq_x6f,
             (String.String (Coq_x6f, (String.String (Coq_x20, (String.String
             (Coq_x6d, (String.String (Coq_x61, (String.String (Coq_x6e,
             (String.String (Coq_x79, (String.String (Coq_x20, (String.String
             (Coq_x66, (String.String (Coq_x75, (String.String (Coq_x6e,
             (String.String (Coq_x63, (String.String (Coq_x74, (String.String
             (Coq_x69, (String.String (Coq_x6f, (String.String (Coq_x6e,
             (String.String (Coq_x20, (String.String (Coq_x61, (String.String
             (Coq_x72, (String.String (Coq_x67, (String.String (Coq_x73,
             (String.String (Coq_x2c, (String.String (Coq_x20, (String.String
             (Coq_x63, (String.String (Coq_x68, (String.String (Coq_x65,
             (String.String (Coq_x63, (String.String (Coq_x6b, (String.String
             (Coq_x20, (String.String (Coq_x6d, (String.String (Coq_x61,
             (String.String (Coq_x78, (String.String (Coq_x5f, (String.String
             (Coq_x66, (String.String (Coq_x75, (String.String (Coq_x6e,
             (String.String (Coq_x63, (String.String (Coq_x74, (String.String
             (Coq_x69, (String.String (Coq_x6f, (String.String (Coq_x6e,
             (String.String (Coq_x5f, (String.String (Coq_x61, (String.String
             (Coq_x72, (String.String (Coq_x67, (String.String (Coq_x73,
             String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
           (fun _ ->
           bind (Obj.magic coq_MonadError) (iter fds') (fun _ ->
             check_restrictions cenv e'0))
       | Fnil -> Ret ()
       in iter fds) (fun _ -> check_restrictions cenv e'))
| Eapp (_, _, ys) ->
  coq_assert (Z.leb (Z.of_nat (length ys)) max_function_args) (String.String
    (Coq_x66, (String.String (Coq_x6f, (String.String (Coq_x75,
    (String.String (Coq_x6e, (String.String (Coq_x64, (String.String
    (Coq_x20, (String.String (Coq_x66, (String.String (Coq_x75,
    (String.String (Coq_x6e, (String.String (Coq_x63, (String.String
    (Coq_x74, (String.String (Coq_x69, (String.String (Coq_x6f,
    (String.String (Coq_x6e, (String.String (Coq_x20, (String.String
    (Coq_x61, (String.String (Coq_x70, (String.String (Coq_x70,
    (String.String (Coq_x6c, (String.String (Coq_x69, (String.String
    (Coq_x63, (String.String (Coq_x61, (String.String (Coq_x74,
    (String.String (Coq_x69, (String.String (Coq_x6f, (String.String
    (Coq_x6e, (String.String (Coq_x20, (String.String (Coq_x77,
    (String.String (Coq_x69, (String.String (Coq_x74, (String.String
    (Coq_x68, (String.String (Coq_x20, (String.String (Coq_x74,
    (String.String (Coq_x6f, (String.String (Coq_x6f, (String.String
    (Coq_x20, (String.String (Coq_x6d, (String.String (Coq_x61,
    (String.String (Coq_x6e, (String.String (Coq_x79, (String.String
    (Coq_x20, (String.String (Coq_x66, (String.String (Coq_x75,
    (String.String (Coq_x6e, (String.String (Coq_x63, (String.String
    (Coq_x74, (String.String (Coq_x69, (String.String (Coq_x6f,
    (String.String (Coq_x6e, (String.String (Coq_x20, (String.String
    (Coq_x70, (String.String (Coq_x61, (String.String (Coq_x72,
    (String.String (Coq_x61, (String.String (Coq_x6d, (String.String
    (Coq_x73, (String.String (Coq_x2c, (String.String (Coq_x20,
    (String.String (Coq_x63, (String.String (Coq_x68, (String.String
    (Coq_x65, (String.String (Coq_x63, (String.String (Coq_x6b,
    (String.String (Coq_x20, (String.String (Coq_x6d, (String.String
    (Coq_x61, (String.String (Coq_x78, (String.String (Coq_x5f,
    (String.String (Coq_x66, (String.String (Coq_x75, (String.String
    (Coq_x6e, (String.String (Coq_x63, (String.String (Coq_x74,
    (String.String (Coq_x69, (String.String (Coq_x6f, (String.String
    (Coq_x6e, (String.String (Coq_x5f, (String.String (Coq_x61,
    (String.String (Coq_x72, (String.String (Coq_x67, (String.String
    (Coq_x73,
    String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
| Eprim_val (_, _, e') -> check_restrictions cenv e'
| Eprim (_, _, _, e') -> check_restrictions cenv e'
| Ehalt _ -> Ret ()
