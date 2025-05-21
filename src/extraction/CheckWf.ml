open Byte
open Datatypes
open EAst
open EGlobalEnv
open EPrimitive
open EWellformed
open ExAst
open Kernames
open List0
open MCString
open Nat0
open PeanoNat
open Primitive
open ReflectEq
open ResultMonad
open Bytestring
open Monad_utils

(** val metacoq_erasure_eflags : coq_EEnvFlags **)

let metacoq_erasure_eflags =
  { has_axioms = true; has_cstr_params = true; term_switches = { has_tBox =
    true; has_tRel = true; has_tVar = true; has_tEvar = false; has_tLambda =
    true; has_tLetIn = true; has_tApp = true; has_tConst = true;
    has_tConstruct = true; has_tCase = true; has_tProj = true; has_tFix =
    true; has_tCoFix = true; has_tPrim = all_primitive_flags;
    has_tLazy_Force = true }; cstr_as_blocks = false }

(** val agda_typed_eflags : coq_EEnvFlags **)

let agda_typed_eflags =
  { has_axioms = true; has_cstr_params = false; term_switches = { has_tBox =
    true; has_tRel = true; has_tVar = true; has_tEvar = true; has_tLambda =
    true; has_tLetIn = true; has_tApp = true; has_tConst = true;
    has_tConstruct = true; has_tCase = true; has_tProj = false; has_tFix =
    true; has_tCoFix = true; has_tPrim = all_primitive_flags;
    has_tLazy_Force = true }; cstr_as_blocks = false }

(** val coq_assert : bool -> (unit -> String.t) -> (unit, String.t) result **)

let coq_assert b s =
  if b then Ok () else Err (s ())

(** val assert_some :
    'a1 option -> (unit -> String.t) -> (unit, String.t) result **)

let assert_some b s =
  match b with
  | Some _ -> Ok ()
  | None -> Err (s ())

(** val result_forall :
    ('a1 -> (unit, String.t) result) -> 'a1 list -> (unit, String.t) result **)

let result_forall f l =
  fold_left (fun a t0 -> bind (Obj.magic coq_Monad_result) a (fun _ -> f t0))
    l (Ok ())

(** val wf_fix_gen_ :
    (nat -> term -> (unit, String.t) result) -> nat -> term def list -> nat
    -> (unit, String.t) result **)

let wf_fix_gen_ wf k mfix idx =
  let k' = add (length mfix) k in
  bind (Obj.magic coq_Monad_result)
    (coq_assert (Nat.ltb idx (length mfix)) (fun _ -> String.String (Coq_x46,
      (String.String (Coq_x69, (String.String (Coq_x78, (String.String
      (Coq_x70, (String.String (Coq_x6f, (String.String (Coq_x69,
      (String.String (Coq_x6e, (String.String (Coq_x74, (String.String
      (Coq_x20, (String.String (Coq_x69, (String.String (Coq_x6e,
      (String.String (Coq_x64, (String.String (Coq_x65, (String.String
      (Coq_x78, (String.String (Coq_x20, (String.String (Coq_x6f,
      (String.String (Coq_x75, (String.String (Coq_x74, (String.String
      (Coq_x20, (String.String (Coq_x6f, (String.String (Coq_x66,
      (String.String (Coq_x20, (String.String (Coq_x62, (String.String
      (Coq_x6f, (String.String (Coq_x75, (String.String (Coq_x6e,
      (String.String (Coq_x64, (String.String (Coq_x73,
      String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    (fun _ -> result_forall (fun d -> wf k' d.dbody) mfix)

(** val bool_of_result : ('a1, 'a2) result -> bool **)

let bool_of_result = function
| Ok _ -> true
| Err _ -> false

(** val has_prim_ :
    coq_EPrimitiveFlags -> term prim_val -> (unit, String.t) result **)

let has_prim_ epfl p =
  match prim_val_tag p with
  | Coq_primInt ->
    coq_assert epfl.has_primint (fun _ -> String.String (Coq_x50,
      (String.String (Coq_x72, (String.String (Coq_x6f, (String.String
      (Coq_x67, (String.String (Coq_x72, (String.String (Coq_x61,
      (String.String (Coq_x6d, (String.String (Coq_x20, (String.String
      (Coq_x63, (String.String (Coq_x6f, (String.String (Coq_x6e,
      (String.String (Coq_x74, (String.String (Coq_x61, (String.String
      (Coq_x69, (String.String (Coq_x6e, (String.String (Coq_x73,
      (String.String (Coq_x20, (String.String (Coq_x70, (String.String
      (Coq_x72, (String.String (Coq_x69, (String.String (Coq_x6d,
      (String.String (Coq_x69, (String.String (Coq_x74, (String.String
      (Coq_x69, (String.String (Coq_x76, (String.String (Coq_x65,
      (String.String (Coq_x20, (String.String (Coq_x69, (String.String
      (Coq_x6e, (String.String (Coq_x74, (String.String (Coq_x65,
      (String.String (Coq_x67, (String.String (Coq_x65, (String.String
      (Coq_x72, (String.String (Coq_x73,
      String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | Coq_primFloat ->
    coq_assert epfl.has_primfloat (fun _ -> String.String (Coq_x50,
      (String.String (Coq_x72, (String.String (Coq_x6f, (String.String
      (Coq_x67, (String.String (Coq_x72, (String.String (Coq_x61,
      (String.String (Coq_x6d, (String.String (Coq_x20, (String.String
      (Coq_x63, (String.String (Coq_x6f, (String.String (Coq_x6e,
      (String.String (Coq_x74, (String.String (Coq_x61, (String.String
      (Coq_x69, (String.String (Coq_x6e, (String.String (Coq_x73,
      (String.String (Coq_x20, (String.String (Coq_x70, (String.String
      (Coq_x72, (String.String (Coq_x69, (String.String (Coq_x6d,
      (String.String (Coq_x69, (String.String (Coq_x74, (String.String
      (Coq_x69, (String.String (Coq_x76, (String.String (Coq_x65,
      (String.String (Coq_x20, (String.String (Coq_x66, (String.String
      (Coq_x6c, (String.String (Coq_x6f, (String.String (Coq_x61,
      (String.String (Coq_x74, (String.String (Coq_x73,
      String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | Coq_primArray ->
    coq_assert epfl.has_primarray (fun _ -> String.String (Coq_x50,
      (String.String (Coq_x72, (String.String (Coq_x6f, (String.String
      (Coq_x67, (String.String (Coq_x72, (String.String (Coq_x61,
      (String.String (Coq_x6d, (String.String (Coq_x20, (String.String
      (Coq_x63, (String.String (Coq_x6f, (String.String (Coq_x6e,
      (String.String (Coq_x74, (String.String (Coq_x61, (String.String
      (Coq_x69, (String.String (Coq_x6e, (String.String (Coq_x73,
      (String.String (Coq_x20, (String.String (Coq_x70, (String.String
      (Coq_x72, (String.String (Coq_x69, (String.String (Coq_x6d,
      (String.String (Coq_x69, (String.String (Coq_x74, (String.String
      (Coq_x69, (String.String (Coq_x76, (String.String (Coq_x65,
      (String.String (Coq_x20, (String.String (Coq_x61, (String.String
      (Coq_x72, (String.String (Coq_x72, (String.String (Coq_x61,
      (String.String (Coq_x79, (String.String (Coq_x73,
      String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val wellformed :
    coq_EEnvFlags -> global_declarations -> nat -> term -> (unit, String.t)
    result **)

let rec wellformed efl _UU03a3_ k = function
| Coq_tBox ->
  coq_assert efl.term_switches.has_tBox (fun _ -> String.String (Coq_x50,
    (String.String (Coq_x72, (String.String (Coq_x6f, (String.String
    (Coq_x67, (String.String (Coq_x72, (String.String (Coq_x61,
    (String.String (Coq_x6d, (String.String (Coq_x20, (String.String
    (Coq_x63, (String.String (Coq_x6f, (String.String (Coq_x6e,
    (String.String (Coq_x74, (String.String (Coq_x61, (String.String
    (Coq_x69, (String.String (Coq_x6e, (String.String (Coq_x73,
    (String.String (Coq_x20, (String.String (Coq_x74, (String.String
    (Coq_x42, (String.String (Coq_x6f, (String.String (Coq_x78,
    String.EmptyString))))))))))))))))))))))))))))))))))))))))))
| Coq_tRel i ->
  bind (Obj.magic coq_Monad_result)
    (coq_assert efl.term_switches.has_tRel (fun _ -> String.String (Coq_x50,
      (String.String (Coq_x72, (String.String (Coq_x6f, (String.String
      (Coq_x67, (String.String (Coq_x72, (String.String (Coq_x61,
      (String.String (Coq_x6d, (String.String (Coq_x20, (String.String
      (Coq_x63, (String.String (Coq_x6f, (String.String (Coq_x6e,
      (String.String (Coq_x74, (String.String (Coq_x61, (String.String
      (Coq_x69, (String.String (Coq_x6e, (String.String (Coq_x73,
      (String.String (Coq_x20, (String.String (Coq_x74, (String.String
      (Coq_x52, (String.String (Coq_x65, (String.String (Coq_x6c,
      String.EmptyString))))))))))))))))))))))))))))))))))))))))))) (fun _ ->
    coq_assert (Nat.ltb i k) (fun _ ->
      String.append (String.String (Coq_x50, (String.String (Coq_x72,
        (String.String (Coq_x6f, (String.String (Coq_x67, (String.String
        (Coq_x72, (String.String (Coq_x61, (String.String (Coq_x6d,
        (String.String (Coq_x20, (String.String (Coq_x6e, (String.String
        (Coq_x6f, (String.String (Coq_x74, (String.String (Coq_x20,
        (String.String (Coq_x63, (String.String (Coq_x6c, (String.String
        (Coq_x6f, (String.String (Coq_x73, (String.String (Coq_x65,
        (String.String (Coq_x64, (String.String (Coq_x2c, (String.String
        (Coq_x20, (String.String (Coq_x69, (String.String (Coq_x6e,
        (String.String (Coq_x76, (String.String (Coq_x61, (String.String
        (Coq_x6c, (String.String (Coq_x69, (String.String (Coq_x64,
        (String.String (Coq_x20, (String.String (Coq_x74, (String.String
        (Coq_x52, (String.String (Coq_x65, (String.String (Coq_x6c,
        (String.String (Coq_x20,
        String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
        (string_of_nat i)))
| Coq_tVar _ ->
  coq_assert efl.term_switches.has_tVar (fun _ -> String.String (Coq_x50,
    (String.String (Coq_x72, (String.String (Coq_x6f, (String.String
    (Coq_x67, (String.String (Coq_x72, (String.String (Coq_x61,
    (String.String (Coq_x6d, (String.String (Coq_x20, (String.String
    (Coq_x63, (String.String (Coq_x6f, (String.String (Coq_x6e,
    (String.String (Coq_x74, (String.String (Coq_x61, (String.String
    (Coq_x69, (String.String (Coq_x6e, (String.String (Coq_x73,
    (String.String (Coq_x20, (String.String (Coq_x74, (String.String
    (Coq_x56, (String.String (Coq_x61, (String.String (Coq_x72,
    String.EmptyString))))))))))))))))))))))))))))))))))))))))))
| Coq_tEvar (_, args) ->
  bind (Obj.magic coq_Monad_result)
    (coq_assert efl.term_switches.has_tEvar (fun _ -> String.String (Coq_x50,
      (String.String (Coq_x72, (String.String (Coq_x6f, (String.String
      (Coq_x67, (String.String (Coq_x72, (String.String (Coq_x61,
      (String.String (Coq_x6d, (String.String (Coq_x20, (String.String
      (Coq_x63, (String.String (Coq_x6f, (String.String (Coq_x6e,
      (String.String (Coq_x74, (String.String (Coq_x61, (String.String
      (Coq_x69, (String.String (Coq_x6e, (String.String (Coq_x73,
      (String.String (Coq_x20, (String.String (Coq_x74, (String.String
      (Coq_x45, (String.String (Coq_x76, (String.String (Coq_x61,
      (String.String (Coq_x72,
      String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))
    (fun _ -> result_forall (wellformed efl _UU03a3_ k) args)
| Coq_tLambda (_, m) ->
  bind (Obj.magic coq_Monad_result)
    (coq_assert efl.term_switches.has_tLambda (fun _ -> String.String
      (Coq_x50, (String.String (Coq_x72, (String.String (Coq_x6f,
      (String.String (Coq_x67, (String.String (Coq_x72, (String.String
      (Coq_x61, (String.String (Coq_x6d, (String.String (Coq_x20,
      (String.String (Coq_x63, (String.String (Coq_x6f, (String.String
      (Coq_x6e, (String.String (Coq_x74, (String.String (Coq_x61,
      (String.String (Coq_x69, (String.String (Coq_x6e, (String.String
      (Coq_x73, (String.String (Coq_x20, (String.String (Coq_x74,
      (String.String (Coq_x4c, (String.String (Coq_x61, (String.String
      (Coq_x6d, (String.String (Coq_x62, (String.String (Coq_x64,
      (String.String (Coq_x61,
      String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))
    (fun _ -> wellformed efl _UU03a3_ (S k) m)
| Coq_tLetIn (_, b, b') ->
  bind (Obj.magic coq_Monad_result)
    (coq_assert efl.term_switches.has_tLetIn (fun _ -> String.String
      (Coq_x50, (String.String (Coq_x72, (String.String (Coq_x6f,
      (String.String (Coq_x67, (String.String (Coq_x72, (String.String
      (Coq_x61, (String.String (Coq_x6d, (String.String (Coq_x20,
      (String.String (Coq_x63, (String.String (Coq_x6f, (String.String
      (Coq_x6e, (String.String (Coq_x74, (String.String (Coq_x61,
      (String.String (Coq_x69, (String.String (Coq_x6e, (String.String
      (Coq_x73, (String.String (Coq_x20, (String.String (Coq_x74,
      (String.String (Coq_x4c, (String.String (Coq_x65, (String.String
      (Coq_x74, (String.String (Coq_x49, (String.String (Coq_x6e,
      String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))
    (fun _ ->
    bind (Obj.magic coq_Monad_result) (wellformed efl _UU03a3_ k b) (fun _ ->
      wellformed efl _UU03a3_ (S k) b'))
| Coq_tApp (u, v) ->
  bind (Obj.magic coq_Monad_result)
    (coq_assert efl.term_switches.has_tApp (fun _ -> String.String (Coq_x50,
      (String.String (Coq_x72, (String.String (Coq_x6f, (String.String
      (Coq_x67, (String.String (Coq_x72, (String.String (Coq_x61,
      (String.String (Coq_x6d, (String.String (Coq_x20, (String.String
      (Coq_x63, (String.String (Coq_x6f, (String.String (Coq_x6e,
      (String.String (Coq_x74, (String.String (Coq_x61, (String.String
      (Coq_x69, (String.String (Coq_x6e, (String.String (Coq_x73,
      (String.String (Coq_x20, (String.String (Coq_x74, (String.String
      (Coq_x41, (String.String (Coq_x70, (String.String (Coq_x70,
      String.EmptyString))))))))))))))))))))))))))))))))))))))))))) (fun _ ->
    bind (Obj.magic coq_Monad_result) (wellformed efl _UU03a3_ k u) (fun _ ->
      wellformed efl _UU03a3_ k v))
| Coq_tConst kn ->
  bind (Obj.magic coq_Monad_result)
    (coq_assert efl.term_switches.has_tConst (fun _ -> String.String
      (Coq_x50, (String.String (Coq_x72, (String.String (Coq_x6f,
      (String.String (Coq_x67, (String.String (Coq_x72, (String.String
      (Coq_x61, (String.String (Coq_x6d, (String.String (Coq_x20,
      (String.String (Coq_x63, (String.String (Coq_x6f, (String.String
      (Coq_x6e, (String.String (Coq_x74, (String.String (Coq_x61,
      (String.String (Coq_x69, (String.String (Coq_x6e, (String.String
      (Coq_x73, (String.String (Coq_x20, (String.String (Coq_x74,
      (String.String (Coq_x43, (String.String (Coq_x6f, (String.String
      (Coq_x6e, (String.String (Coq_x73, (String.String (Coq_x74,
      String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))
    (fun _ ->
    match EGlobalEnv.lookup_constant _UU03a3_ kn with
    | Some d ->
      coq_assert ((||) efl.has_axioms (isSome d)) (fun _ ->
        String.append (String.String (Coq_x49, (String.String (Coq_x6e,
          (String.String (Coq_x76, (String.String (Coq_x61, (String.String
          (Coq_x6c, (String.String (Coq_x69, (String.String (Coq_x64,
          (String.String (Coq_x20, (String.String (Coq_x61, (String.String
          (Coq_x78, (String.String (Coq_x69, (String.String (Coq_x6f,
          (String.String (Coq_x6d, (String.String (Coq_x20,
          String.EmptyString))))))))))))))))))))))))))))
          (string_of_kername kn))
    | None ->
      Err
        (String.append (String.String (Coq_x43, (String.String (Coq_x6f,
          (String.String (Coq_x6e, (String.String (Coq_x73, (String.String
          (Coq_x74, (String.String (Coq_x61, (String.String (Coq_x6e,
          (String.String (Coq_x74, (String.String (Coq_x20, (String.String
          (Coq_x6e, (String.String (Coq_x6f, (String.String (Coq_x74,
          (String.String (Coq_x20, (String.String (Coq_x66, (String.String
          (Coq_x6f, (String.String (Coq_x75, (String.String (Coq_x6e,
          (String.String (Coq_x64, (String.String (Coq_x20, (String.String
          (Coq_x69, (String.String (Coq_x6e, (String.String (Coq_x20,
          (String.String (Coq_x65, (String.String (Coq_x6e, (String.String
          (Coq_x76, (String.String (Coq_x69, (String.String (Coq_x72,
          (String.String (Coq_x6f, (String.String (Coq_x6e, (String.String
          (Coq_x6d, (String.String (Coq_x65, (String.String (Coq_x6e,
          (String.String (Coq_x74, (String.String (Coq_x20,
          String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
          (string_of_kername kn)))
| Coq_tConstruct (ind, c, block_args) ->
  bind (Obj.magic coq_Monad_result)
    (coq_assert efl.term_switches.has_tConstruct (fun _ -> String.String
      (Coq_x50, (String.String (Coq_x72, (String.String (Coq_x6f,
      (String.String (Coq_x67, (String.String (Coq_x72, (String.String
      (Coq_x61, (String.String (Coq_x6d, (String.String (Coq_x20,
      (String.String (Coq_x63, (String.String (Coq_x6f, (String.String
      (Coq_x6e, (String.String (Coq_x74, (String.String (Coq_x61,
      (String.String (Coq_x69, (String.String (Coq_x6e, (String.String
      (Coq_x73, (String.String (Coq_x20, (String.String (Coq_x74,
      (String.String (Coq_x43, (String.String (Coq_x6f, (String.String
      (Coq_x6e, (String.String (Coq_x73, (String.String (Coq_x74,
      (String.String (Coq_x72, (String.String (Coq_x75, (String.String
      (Coq_x63, (String.String (Coq_x74,
      String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))
    (fun _ ->
    bind (Obj.magic coq_Monad_result)
      (assert_some (EGlobalEnv.lookup_constructor _UU03a3_ ind c) (fun _ ->
        String.append (String.String (Coq_x43, (String.String (Coq_x6f,
          (String.String (Coq_x6e, (String.String (Coq_x73, (String.String
          (Coq_x74, (String.String (Coq_x72, (String.String (Coq_x75,
          (String.String (Coq_x63, (String.String (Coq_x74, (String.String
          (Coq_x6f, (String.String (Coq_x72, (String.String (Coq_x20,
          String.EmptyString))))))))))))))))))))))))
          (String.append (string_of_inductive ind)
            (String.append (String.String (Coq_x3a, String.EmptyString))
              (String.append (string_of_nat c) (String.String (Coq_x20,
                (String.String (Coq_x6e, (String.String (Coq_x6f,
                (String.String (Coq_x74, (String.String (Coq_x20,
                (String.String (Coq_x66, (String.String (Coq_x6f,
                (String.String (Coq_x75, (String.String (Coq_x6e,
                (String.String (Coq_x64,
                String.EmptyString))))))))))))))))))))))))) (fun _ ->
      if efl.cstr_as_blocks
      then bind (Obj.magic coq_Monad_result)
             (match lookup_constructor_pars_args _UU03a3_ ind c with
              | Some p0 ->
                let (p, a) = p0 in
                coq_assert (reflect_nat (add p a) (length block_args))
                  (fun _ ->
                  String.append (String.String (Coq_x43, (String.String
                    (Coq_x6f, (String.String (Coq_x6e, (String.String
                    (Coq_x73, (String.String (Coq_x74, (String.String
                    (Coq_x72, (String.String (Coq_x75, (String.String
                    (Coq_x63, (String.String (Coq_x74, (String.String
                    (Coq_x6f, (String.String (Coq_x72, (String.String
                    (Coq_x20, String.EmptyString))))))))))))))))))))))))
                    (String.append (string_of_inductive ind)
                      (String.append (String.String (Coq_x3a,
                        String.EmptyString))
                        (String.append (string_of_nat c) (String.String
                          (Coq_x20, (String.String (Coq_x6e, (String.String
                          (Coq_x6f, (String.String (Coq_x74, (String.String
                          (Coq_x20, (String.String (Coq_x66, (String.String
                          (Coq_x75, (String.String (Coq_x6c, (String.String
                          (Coq_x6c, (String.String (Coq_x79, (String.String
                          (Coq_x20, (String.String (Coq_x61, (String.String
                          (Coq_x70, (String.String (Coq_x70, (String.String
                          (Coq_x6c, (String.String (Coq_x69, (String.String
                          (Coq_x65, (String.String (Coq_x64,
                          String.EmptyString))))))))))))))))))))))))))))))))))))))))
              | None -> Ok ()) (fun _ ->
             result_forall (wellformed efl _UU03a3_ k) block_args)
      else coq_assert (is_nil block_args) (fun _ -> String.String (Coq_x43,
             (String.String (Coq_x6f, (String.String (Coq_x6e, (String.String
             (Coq_x73, (String.String (Coq_x74, (String.String (Coq_x72,
             (String.String (Coq_x75, (String.String (Coq_x63, (String.String
             (Coq_x74, (String.String (Coq_x6f, (String.String (Coq_x72,
             (String.String (Coq_x20, (String.String (Coq_x61, (String.String
             (Coq_x72, (String.String (Coq_x67, (String.String (Coq_x73,
             (String.String (Coq_x20, (String.String (Coq_x6e, (String.String
             (Coq_x6f, (String.String (Coq_x6e, (String.String (Coq_x2d,
             (String.String (Coq_x65, (String.String (Coq_x6d, (String.String
             (Coq_x70, (String.String (Coq_x74, (String.String (Coq_x79,
             String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))
| Coq_tCase (ind, c, brs) ->
  bind (Obj.magic coq_Monad_result)
    (coq_assert efl.term_switches.has_tCase (fun _ -> String.String (Coq_x50,
      (String.String (Coq_x72, (String.String (Coq_x6f, (String.String
      (Coq_x67, (String.String (Coq_x72, (String.String (Coq_x61,
      (String.String (Coq_x6d, (String.String (Coq_x20, (String.String
      (Coq_x63, (String.String (Coq_x6f, (String.String (Coq_x6e,
      (String.String (Coq_x74, (String.String (Coq_x61, (String.String
      (Coq_x69, (String.String (Coq_x6e, (String.String (Coq_x73,
      (String.String (Coq_x20, (String.String (Coq_x74, (String.String
      (Coq_x43, (String.String (Coq_x61, (String.String (Coq_x73,
      (String.String (Coq_x65,
      String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))
    (fun _ ->
    let brs' =
      result_forall (fun br ->
        wellformed efl _UU03a3_ (add (length (fst br)) k) (snd br)) brs
    in
    bind (Obj.magic coq_Monad_result)
      (coq_assert (wf_brs _UU03a3_ (fst ind) (length brs)) (fun _ ->
        String.String (Coq_x43, (String.String (Coq_x61, (String.String
        (Coq_x73, (String.String (Coq_x65, (String.String (Coq_x20,
        (String.String (Coq_x6e, (String.String (Coq_x6f, (String.String
        (Coq_x74, (String.String (Coq_x20, (String.String (Coq_x65,
        (String.String (Coq_x78, (String.String (Coq_x68, (String.String
        (Coq_x61, (String.String (Coq_x75, (String.String (Coq_x73,
        (String.String (Coq_x74, (String.String (Coq_x69, (String.String
        (Coq_x76, (String.String (Coq_x65,
        String.EmptyString))))))))))))))))))))))))))))))))))))))) (fun _ ->
      bind (Obj.magic coq_Monad_result) (wellformed efl _UU03a3_ k c)
        (fun _ -> brs')))
| Coq_tProj (p, c) ->
  bind (Obj.magic coq_Monad_result)
    (coq_assert efl.term_switches.has_tProj (fun _ -> String.String (Coq_x50,
      (String.String (Coq_x72, (String.String (Coq_x6f, (String.String
      (Coq_x67, (String.String (Coq_x72, (String.String (Coq_x61,
      (String.String (Coq_x6d, (String.String (Coq_x20, (String.String
      (Coq_x63, (String.String (Coq_x6f, (String.String (Coq_x6e,
      (String.String (Coq_x74, (String.String (Coq_x61, (String.String
      (Coq_x69, (String.String (Coq_x6e, (String.String (Coq_x73,
      (String.String (Coq_x20, (String.String (Coq_x74, (String.String
      (Coq_x50, (String.String (Coq_x72, (String.String (Coq_x6f,
      (String.String (Coq_x6a,
      String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))
    (fun _ ->
    bind (Obj.magic coq_Monad_result)
      (assert_some (lookup_projection _UU03a3_ p) (fun _ ->
        String.append (String.String (Coq_x50, (String.String (Coq_x72,
          (String.String (Coq_x6f, (String.String (Coq_x6a, (String.String
          (Coq_x65, (String.String (Coq_x63, (String.String (Coq_x74,
          (String.String (Coq_x69, (String.String (Coq_x6f, (String.String
          (Coq_x6e, (String.String (Coq_x20,
          String.EmptyString))))))))))))))))))))))
          (String.append (string_of_inductive p.proj_ind)
            (String.append (String.String (Coq_x3a, String.EmptyString))
              (String.append (string_of_nat p.proj_npars)
                (String.append (String.String (Coq_x2c, String.EmptyString))
                  (String.append (string_of_nat p.proj_arg) (String.String
                    (Coq_x20, (String.String (Coq_x6e, (String.String
                    (Coq_x6f, (String.String (Coq_x74, (String.String
                    (Coq_x20, (String.String (Coq_x66, (String.String
                    (Coq_x6f, (String.String (Coq_x75, (String.String
                    (Coq_x6e, (String.String (Coq_x64,
                    String.EmptyString))))))))))))))))))))))))))) (fun _ ->
      wellformed efl _UU03a3_ k c))
| Coq_tFix (mfix, idx) ->
  bind (Obj.magic coq_Monad_result)
    (coq_assert efl.term_switches.has_tFix (fun _ -> String.String (Coq_x50,
      (String.String (Coq_x72, (String.String (Coq_x6f, (String.String
      (Coq_x67, (String.String (Coq_x72, (String.String (Coq_x61,
      (String.String (Coq_x6d, (String.String (Coq_x20, (String.String
      (Coq_x63, (String.String (Coq_x6f, (String.String (Coq_x6e,
      (String.String (Coq_x74, (String.String (Coq_x61, (String.String
      (Coq_x69, (String.String (Coq_x6e, (String.String (Coq_x73,
      (String.String (Coq_x20, (String.String (Coq_x74, (String.String
      (Coq_x46, (String.String (Coq_x69, (String.String (Coq_x78,
      String.EmptyString))))))))))))))))))))))))))))))))))))))))))) (fun _ ->
    bind (Obj.magic coq_Monad_result)
      (result_forall (fun t1 ->
        coq_assert (let f0 = fun d -> d.dbody in isLambda (f0 t1)) (fun _ ->
          String.String (Coq_x46, (String.String (Coq_x69, (String.String
          (Coq_x78, (String.String (Coq_x70, (String.String (Coq_x6f,
          (String.String (Coq_x69, (String.String (Coq_x6e, (String.String
          (Coq_x74, (String.String (Coq_x20, (String.String (Coq_x62,
          (String.String (Coq_x6f, (String.String (Coq_x64, (String.String
          (Coq_x79, (String.String (Coq_x20, (String.String (Coq_x69,
          (String.String (Coq_x73, (String.String (Coq_x20, (String.String
          (Coq_x6e, (String.String (Coq_x6f, (String.String (Coq_x74,
          (String.String (Coq_x20, (String.String (Coq_x61, (String.String
          (Coq_x20, (String.String (Coq_x6c, (String.String (Coq_x61,
          (String.String (Coq_x6d, (String.String (Coq_x62, (String.String
          (Coq_x64, (String.String (Coq_x61,
          String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
        mfix) (fun _ -> wf_fix_gen_ (wellformed efl _UU03a3_) k mfix idx))
| Coq_tCoFix (mfix, idx) ->
  bind (Obj.magic coq_Monad_result)
    (coq_assert efl.term_switches.has_tCoFix (fun _ -> String.String
      (Coq_x50, (String.String (Coq_x72, (String.String (Coq_x6f,
      (String.String (Coq_x67, (String.String (Coq_x72, (String.String
      (Coq_x61, (String.String (Coq_x6d, (String.String (Coq_x20,
      (String.String (Coq_x63, (String.String (Coq_x6f, (String.String
      (Coq_x6e, (String.String (Coq_x74, (String.String (Coq_x61,
      (String.String (Coq_x69, (String.String (Coq_x6e, (String.String
      (Coq_x73, (String.String (Coq_x20, (String.String (Coq_x74,
      (String.String (Coq_x43, (String.String (Coq_x6f, (String.String
      (Coq_x46, (String.String (Coq_x69, (String.String (Coq_x78,
      String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))
    (fun _ -> wf_fix_gen_ (wellformed efl _UU03a3_) k mfix idx)
| Coq_tPrim p ->
  bind (Obj.magic coq_Monad_result) (has_prim_ efl.term_switches.has_tPrim p)
    (fun _ ->
    coq_assert
      (test_prim (fun t1 -> bool_of_result (wellformed efl _UU03a3_ k t1)) p)
      (fun _ -> String.String (Coq_x49, (String.String (Coq_x6e,
      (String.String (Coq_x76, (String.String (Coq_x61, (String.String
      (Coq_x6c, (String.String (Coq_x69, (String.String (Coq_x64,
      (String.String (Coq_x20, (String.String (Coq_x61, (String.String
      (Coq_x72, (String.String (Coq_x72, (String.String (Coq_x61,
      (String.String (Coq_x79, (String.String (Coq_x20, (String.String
      (Coq_x70, (String.String (Coq_x72, (String.String (Coq_x69,
      (String.String (Coq_x6d, (String.String (Coq_x69, (String.String
      (Coq_x74, (String.String (Coq_x69, (String.String (Coq_x76,
      (String.String (Coq_x65,
      String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))
| Coq_tLazy t1 ->
  bind (Obj.magic coq_Monad_result)
    (coq_assert efl.term_switches.has_tLazy_Force (fun _ -> String.String
      (Coq_x50, (String.String (Coq_x72, (String.String (Coq_x6f,
      (String.String (Coq_x67, (String.String (Coq_x72, (String.String
      (Coq_x61, (String.String (Coq_x6d, (String.String (Coq_x20,
      (String.String (Coq_x63, (String.String (Coq_x6f, (String.String
      (Coq_x6e, (String.String (Coq_x74, (String.String (Coq_x61,
      (String.String (Coq_x69, (String.String (Coq_x6e, (String.String
      (Coq_x73, (String.String (Coq_x20, (String.String (Coq_x6c,
      (String.String (Coq_x61, (String.String (Coq_x7a, (String.String
      (Coq_x79, (String.String (Coq_x2f, (String.String (Coq_x66,
      (String.String (Coq_x6f, (String.String (Coq_x72, (String.String
      (Coq_x63, (String.String (Coq_x65,
      String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))
    (fun _ -> wellformed efl _UU03a3_ k t1)
| Coq_tForce t1 ->
  bind (Obj.magic coq_Monad_result)
    (coq_assert efl.term_switches.has_tLazy_Force (fun _ -> String.String
      (Coq_x50, (String.String (Coq_x72, (String.String (Coq_x6f,
      (String.String (Coq_x67, (String.String (Coq_x72, (String.String
      (Coq_x61, (String.String (Coq_x6d, (String.String (Coq_x20,
      (String.String (Coq_x63, (String.String (Coq_x6f, (String.String
      (Coq_x6e, (String.String (Coq_x74, (String.String (Coq_x61,
      (String.String (Coq_x69, (String.String (Coq_x6e, (String.String
      (Coq_x73, (String.String (Coq_x20, (String.String (Coq_x6c,
      (String.String (Coq_x61, (String.String (Coq_x7a, (String.String
      (Coq_x79, (String.String (Coq_x2f, (String.String (Coq_x66,
      (String.String (Coq_x6f, (String.String (Coq_x72, (String.String
      (Coq_x63, (String.String (Coq_x65,
      String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))
    (fun _ -> wellformed efl _UU03a3_ k t1)

(** val wf_projections :
    EAst.one_inductive_body -> (unit, String.t) result **)

let wf_projections idecl =
  match idecl.EAst.ind_projs with
  | [] -> Ok ()
  | _ :: _ ->
    (match idecl.EAst.ind_ctors with
     | [] ->
       Err (String.String (Coq_x49, (String.String (Coq_x6e, (String.String
         (Coq_x76, (String.String (Coq_x61, (String.String (Coq_x6c,
         (String.String (Coq_x69, (String.String (Coq_x64, (String.String
         (Coq_x20, (String.String (Coq_x70, (String.String (Coq_x72,
         (String.String (Coq_x6f, (String.String (Coq_x6a, (String.String
         (Coq_x65, (String.String (Coq_x63, (String.String (Coq_x74,
         (String.String (Coq_x69, (String.String (Coq_x6f, (String.String
         (Coq_x6e, String.EmptyString))))))))))))))))))))))))))))))))))))
     | cstr :: l ->
       (match l with
        | [] ->
          coq_assert
            (reflect_nat (length idecl.EAst.ind_projs) cstr.cstr_nargs)
            (fun _ -> String.String (Coq_x4e, (String.String (Coq_x75,
            (String.String (Coq_x6d, (String.String (Coq_x62, (String.String
            (Coq_x65, (String.String (Coq_x72, (String.String (Coq_x20,
            (String.String (Coq_x6f, (String.String (Coq_x66, (String.String
            (Coq_x20, (String.String (Coq_x70, (String.String (Coq_x72,
            (String.String (Coq_x69, (String.String (Coq_x6d, (String.String
            (Coq_x69, (String.String (Coq_x74, (String.String (Coq_x69,
            (String.String (Coq_x76, (String.String (Coq_x65, (String.String
            (Coq_x20, (String.String (Coq_x70, (String.String (Coq_x72,
            (String.String (Coq_x6f, (String.String (Coq_x6a, (String.String
            (Coq_x65, (String.String (Coq_x63, (String.String (Coq_x74,
            (String.String (Coq_x69, (String.String (Coq_x6f, (String.String
            (Coq_x6e, (String.String (Coq_x73, (String.String (Coq_x20,
            (String.String (Coq_x64, (String.String (Coq_x6f, (String.String
            (Coq_x65, (String.String (Coq_x73, (String.String (Coq_x6e,
            (String.String (Coq_x27, (String.String (Coq_x74, (String.String
            (Coq_x20, (String.String (Coq_x6d, (String.String (Coq_x61,
            (String.String (Coq_x74, (String.String (Coq_x63, (String.String
            (Coq_x68, (String.String (Coq_x20, (String.String (Coq_x63,
            (String.String (Coq_x6f, (String.String (Coq_x6e, (String.String
            (Coq_x73, (String.String (Coq_x74, (String.String (Coq_x72,
            (String.String (Coq_x75, (String.String (Coq_x63, (String.String
            (Coq_x74, (String.String (Coq_x6f, (String.String (Coq_x72,
            (String.String (Coq_x20, (String.String (Coq_x61, (String.String
            (Coq_x72, (String.String (Coq_x67, (String.String (Coq_x73,
            String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
        | _ :: _ ->
          Err (String.String (Coq_x49, (String.String (Coq_x6e,
            (String.String (Coq_x76, (String.String (Coq_x61, (String.String
            (Coq_x6c, (String.String (Coq_x69, (String.String (Coq_x64,
            (String.String (Coq_x20, (String.String (Coq_x70, (String.String
            (Coq_x72, (String.String (Coq_x6f, (String.String (Coq_x6a,
            (String.String (Coq_x65, (String.String (Coq_x63, (String.String
            (Coq_x74, (String.String (Coq_x69, (String.String (Coq_x6f,
            (String.String (Coq_x6e,
            String.EmptyString))))))))))))))))))))))))))))))))))))))

(** val wf_inductive : EAst.one_inductive_body -> (unit, String.t) result **)

let wf_inductive =
  wf_projections

(** val wf_minductive :
    coq_EEnvFlags -> EAst.mutual_inductive_body -> (unit, String.t) result **)

let wf_minductive efl mdecl =
  bind (Obj.magic coq_Monad_result)
    (coq_assert
      ((||) efl.has_cstr_params (reflect_nat mdecl.EAst.ind_npars O))
      (fun _ -> String.String (Coq_x48, (String.String (Coq_x61,
      (String.String (Coq_x73, (String.String (Coq_x20, (String.String
      (Coq_x63, (String.String (Coq_x6f, (String.String (Coq_x6e,
      (String.String (Coq_x73, (String.String (Coq_x74, (String.String
      (Coq_x72, (String.String (Coq_x75, (String.String (Coq_x63,
      (String.String (Coq_x74, (String.String (Coq_x6f, (String.String
      (Coq_x72, (String.String (Coq_x20, (String.String (Coq_x70,
      (String.String (Coq_x61, (String.String (Coq_x72, (String.String
      (Coq_x61, (String.String (Coq_x6d, (String.String (Coq_x73,
      String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))
    (fun _ -> result_forall wf_inductive mdecl.EAst.ind_bodies)

(** val wf_global_decl :
    coq_EEnvFlags -> global_declarations -> EAst.global_decl -> (unit,
    String.t) result **)

let wf_global_decl efl _UU03a3_ = function
| EAst.ConstantDecl cb ->
  (match cb with
   | Some cb0 -> wellformed efl _UU03a3_ O cb0
   | None ->
     coq_assert efl.has_axioms (fun _ -> String.String (Coq_x50,
       (String.String (Coq_x72, (String.String (Coq_x6f, (String.String
       (Coq_x67, (String.String (Coq_x72, (String.String (Coq_x61,
       (String.String (Coq_x6d, (String.String (Coq_x20, (String.String
       (Coq_x63, (String.String (Coq_x6f, (String.String (Coq_x6e,
       (String.String (Coq_x74, (String.String (Coq_x61, (String.String
       (Coq_x69, (String.String (Coq_x6e, (String.String (Coq_x73,
       (String.String (Coq_x20, (String.String (Coq_x61, (String.String
       (Coq_x78, (String.String (Coq_x69, (String.String (Coq_x6f,
       (String.String (Coq_x6d, (String.String (Coq_x73,
       String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))
| EAst.InductiveDecl idecl -> wf_minductive efl idecl

(** val check_fresh_global :
    kername -> global_declarations -> (unit, String.t) result **)

let rec check_fresh_global k = function
| [] -> Ok ()
| p :: ds ->
  bind (Obj.magic coq_Monad_result)
    (coq_assert (negb (Kername.reflect_kername (fst p) k)) (fun _ ->
      String.append (String.String (Coq_x44, (String.String (Coq_x75,
        (String.String (Coq_x70, (String.String (Coq_x6c, (String.String
        (Coq_x69, (String.String (Coq_x63, (String.String (Coq_x61,
        (String.String (Coq_x74, (String.String (Coq_x65, (String.String
        (Coq_x20, (String.String (Coq_x64, (String.String (Coq_x65,
        (String.String (Coq_x66, (String.String (Coq_x69, (String.String
        (Coq_x6e, (String.String (Coq_x69, (String.String (Coq_x74,
        (String.String (Coq_x69, (String.String (Coq_x6f, (String.String
        (Coq_x6e, (String.String (Coq_x20,
        String.EmptyString))))))))))))))))))))))))))))))))))))))))))
        (string_of_kername (fst p)))) (fun _ -> check_fresh_global k ds)

(** val check_wf_glob :
    coq_EEnvFlags -> global_declarations -> (unit, String.t) result **)

let rec check_wf_glob efl = function
| [] -> Ok ()
| p :: ds ->
  bind (Obj.magic coq_Monad_result) (check_wf_glob efl ds) (fun _ ->
    bind (Obj.magic coq_Monad_result) (check_fresh_global (fst p) ds)
      (fun _ ->
      map_error (fun e ->
        String.append (String.String (Coq_x45, (String.String (Coq_x72,
          (String.String (Coq_x72, (String.String (Coq_x6f, (String.String
          (Coq_x72, (String.String (Coq_x20, (String.String (Coq_x77,
          (String.String (Coq_x68, (String.String (Coq_x69, (String.String
          (Coq_x6c, (String.String (Coq_x65, (String.String (Coq_x20,
          (String.String (Coq_x63, (String.String (Coq_x68, (String.String
          (Coq_x65, (String.String (Coq_x63, (String.String (Coq_x6b,
          (String.String (Coq_x69, (String.String (Coq_x6e, (String.String
          (Coq_x67, (String.String (Coq_x20,
          String.EmptyString))))))))))))))))))))))))))))))))))))))))))
          (String.append (string_of_kername (fst p))
            (String.append (String.String (Coq_x3a, (String.String (Coq_x20,
              String.EmptyString)))) e))) (wf_global_decl efl ds (snd p))))

(** val check_wf_program :
    coq_EEnvFlags -> program -> (unit, String.t) result **)

let check_wf_program efl p =
  bind (Obj.magic coq_Monad_result) (check_wf_glob efl (fst p)) (fun _ ->
    wellformed efl (fst p) O (snd p))

module CheckWfExAst =
 struct
  (** val check_wf_typed_program :
      coq_EEnvFlags -> global_env -> (unit, String.t) result **)

  let check_wf_typed_program efl p =
    check_wf_glob efl (trans_env p)
 end
