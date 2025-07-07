open BinInt
open BinNums
open Byte
open Compile1
open Datatypes
open EAst
open EGlobalEnv
open EImplementBox
open EPrimitive
open EProgram
open EWcbvEvalNamed
open EWellformed
open Erasure0
open Kernames
open List0
open Malfunction
open PeanoNat
open SemanticsSpec
open Uint0
open Bytestring

type malfunction_pipeline_config = { erasure_config : erasure_configuration;
                                     reorder_cstrs : inductives_mapping;
                                     prims : primitives }

(** val array_length : Uint63.t **)

let array_length =
  (Uint63.of_int (4194303))

(** val ignore : 'a1 -> 'a2 -> 'a2 **)

let ignore _ y =
  y

(** val bool_good_error : bool -> String.t -> bool **)

let bool_good_error a s =
  if a then true else let r = (fun s -> ()) s in ignore r false

(** val array_length_Z : coq_Z **)

let array_length_Z =
  to_Z array_length

(** val wellformed_fast :
    coq_EEnvFlags -> global_declarations -> term -> bool **)

let rec wellformed_fast efl _UU03a3_ = function
| Coq_tBox -> efl.term_switches.has_tBox
| Coq_tRel _ -> efl.term_switches.has_tRel
| Coq_tVar _ -> efl.term_switches.has_tVar
| Coq_tEvar (_, args) ->
  (&&) efl.term_switches.has_tEvar
    (forallb (wellformed_fast efl _UU03a3_) args)
| Coq_tLambda (_, m) ->
  (&&) efl.term_switches.has_tLambda (wellformed_fast efl _UU03a3_ m)
| Coq_tLetIn (_, b, b') ->
  (&&) ((&&) efl.term_switches.has_tLetIn (wellformed_fast efl _UU03a3_ b))
    (wellformed_fast efl _UU03a3_ b')
| Coq_tApp (u, v) ->
  (&&) ((&&) efl.term_switches.has_tApp (wellformed_fast efl _UU03a3_ u))
    (wellformed_fast efl _UU03a3_ v)
| Coq_tConst _ -> efl.term_switches.has_tConst
| Coq_tConstruct (_, _, _) -> efl.term_switches.has_tConstruct
| Coq_tCase (ind, c, brs) ->
  (&&) efl.term_switches.has_tCase
    (let brs' = forallb (fun br -> wellformed_fast efl _UU03a3_ (snd br)) brs
     in
     (&&)
       ((&&) (isSome (lookup_inductive _UU03a3_ (fst ind)))
         (wellformed_fast efl _UU03a3_ c)) brs')
| Coq_tProj (p, c) ->
  (&&)
    ((&&) efl.term_switches.has_tProj (isSome (lookup_projection _UU03a3_ p)))
    (wellformed_fast efl _UU03a3_ c)
| Coq_tFix (mfix, idx) ->
  (&&)
    ((&&) efl.term_switches.has_tFix
      (forallb (let f0 = fun d -> d.dbody in fun x -> isLambda (f0 x)) mfix))
    (wf_fix_gen (fun _ -> wellformed_fast efl _UU03a3_) O mfix idx)
| Coq_tCoFix (mfix, idx) ->
  (&&) efl.term_switches.has_tCoFix
    (wf_fix_gen (fun _ -> wellformed_fast efl _UU03a3_) O mfix idx)
| Coq_tPrim p ->
  (&&) (has_prim efl.term_switches.has_tPrim p)
    (test_prim (wellformed_fast efl _UU03a3_) p)
| Coq_tLazy t1 ->
  (&&) efl.term_switches.has_tLazy_Force (wellformed_fast efl _UU03a3_ t1)
| Coq_tForce t1 ->
  (&&) efl.term_switches.has_tLazy_Force (wellformed_fast efl _UU03a3_ t1)

(** val check_good_for_extraction_rec :
    coq_EEnvFlags -> (kername * global_decl) list -> bool **)

let rec check_good_for_extraction_rec fl = function
| [] -> true
| p :: _UU03a3_0 ->
  let (kn, g) = p in
  (match g with
   | ConstantDecl d ->
     (match d with
      | Some b ->
        if wellformed_fast fl _UU03a3_0 b
        then check_good_for_extraction_rec fl _UU03a3_0
        else ignore
               ((fun s -> ()) (String.String (Coq_x57, (String.String
                 (Coq_x61, (String.String (Coq_x72, (String.String (Coq_x6e,
                 (String.String (Coq_x69, (String.String (Coq_x6e,
                 (String.String (Coq_x67, (String.String (Coq_x3a,
                 (String.String (Coq_x20, (String.String (Coq_x65,
                 (String.String (Coq_x6e, (String.String (Coq_x76,
                 (String.String (Coq_x69, (String.String (Coq_x72,
                 (String.String (Coq_x6f, (String.String (Coq_x6e,
                 (String.String (Coq_x6d, (String.String (Coq_x65,
                 (String.String (Coq_x6e, (String.String (Coq_x74,
                 (String.String (Coq_x20, (String.String (Coq_x63,
                 (String.String (Coq_x6f, (String.String (Coq_x6e,
                 (String.String (Coq_x74, (String.String (Coq_x61,
                 (String.String (Coq_x69, (String.String (Coq_x6e,
                 (String.String (Coq_x73, (String.String (Coq_x20,
                 (String.String (Coq_x63, (String.String (Coq_x6f,
                 (String.String (Coq_x6e, (String.String (Coq_x73,
                 (String.String (Coq_x74, (String.String (Coq_x72,
                 (String.String (Coq_x75, (String.String (Coq_x63,
                 (String.String (Coq_x74, (String.String (Coq_x6f,
                 (String.String (Coq_x72, (String.String (Coq_x73,
                 (String.String (Coq_x20, (String.String (Coq_x66,
                 (String.String (Coq_x6f, (String.String (Coq_x72,
                 (String.String (Coq_x20, (String.String (Coq_x77,
                 (String.String (Coq_x68, (String.String (Coq_x69,
                 (String.String (Coq_x63, (String.String (Coq_x68,
                 (String.String (Coq_x20, (String.String (Coq_x65,
                 (String.String (Coq_x78, (String.String (Coq_x74,
                 (String.String (Coq_x72, (String.String (Coq_x61,
                 (String.String (Coq_x63, (String.String (Coq_x74,
                 (String.String (Coq_x69, (String.String (Coq_x6f,
                 (String.String (Coq_x6e, (String.String (Coq_x20,
                 (String.String (Coq_x69, (String.String (Coq_x73,
                 (String.String (Coq_x20, (String.String (Coq_x6e,
                 (String.String (Coq_x6f, (String.String (Coq_x74,
                 (String.String (Coq_x20, (String.String (Coq_x76,
                 (String.String (Coq_x65, (String.String (Coq_x72,
                 (String.String (Coq_x69, (String.String (Coq_x66,
                 (String.String (Coq_x69, (String.String (Coq_x65,
                 (String.String (Coq_x64,
                 String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
               (check_good_for_extraction_rec fl _UU03a3_0)
      | None ->
        ignore
          ((fun s -> ())
            (String.append (String.String (Coq_x57, (String.String (Coq_x61,
              (String.String (Coq_x72, (String.String (Coq_x6e,
              (String.String (Coq_x69, (String.String (Coq_x6e,
              (String.String (Coq_x67, (String.String (Coq_x3a,
              (String.String (Coq_x20, (String.String (Coq_x65,
              (String.String (Coq_x6e, (String.String (Coq_x76,
              (String.String (Coq_x69, (String.String (Coq_x72,
              (String.String (Coq_x6f, (String.String (Coq_x6e,
              (String.String (Coq_x6d, (String.String (Coq_x65,
              (String.String (Coq_x6e, (String.String (Coq_x74,
              (String.String (Coq_x20, (String.String (Coq_x63,
              (String.String (Coq_x6f, (String.String (Coq_x6e,
              (String.String (Coq_x74, (String.String (Coq_x61,
              (String.String (Coq_x69, (String.String (Coq_x6e,
              (String.String (Coq_x73, (String.String (Coq_x20,
              (String.String (Coq_x61, (String.String (Coq_x78,
              (String.String (Coq_x69, (String.String (Coq_x6f,
              (String.String (Coq_x6d, (String.String (Coq_x20,
              String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
              (string_of_kername kn))) false)
   | InductiveDecl mind ->
     (&&)
       ((&&)
         ((&&)
           ((&&)
             (bool_good_error
               (forallb (fun ob ->
                 let args = map (fun c -> c.cstr_nargs) ob.ind_ctors in
                 Nat.ltb (blocks_until (length args) args) (S (S (S (S (S (S
                   (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                   (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                   (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                   (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                   (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                   (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                   (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                   (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                   (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                   (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                   (S (S (S (S
                   O)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                 mind.ind_bodies)
               (String.append (String.String (Coq_x49, (String.String
                 (Coq_x6e, (String.String (Coq_x64, (String.String (Coq_x75,
                 (String.String (Coq_x63, (String.String (Coq_x74,
                 (String.String (Coq_x69, (String.String (Coq_x76,
                 (String.String (Coq_x65, (String.String (Coq_x20,
                 String.EmptyString))))))))))))))))))))
                 (String.append (string_of_kername kn) (String.String
                   (Coq_x68, (String.String (Coq_x61, (String.String
                   (Coq_x73, (String.String (Coq_x20, (String.String
                   (Coq_x74, (String.String (Coq_x6f, (String.String
                   (Coq_x6f, (String.String (Coq_x20, (String.String
                   (Coq_x6d, (String.String (Coq_x61, (String.String
                   (Coq_x6e, (String.String (Coq_x79, (String.String
                   (Coq_x20, (String.String (Coq_x6e, (String.String
                   (Coq_x6f, (String.String (Coq_x6e, (String.String
                   (Coq_x2d, (String.String (Coq_x63, (String.String
                   (Coq_x6f, (String.String (Coq_x6e, (String.String
                   (Coq_x73, (String.String (Coq_x74, (String.String
                   (Coq_x61, (String.String (Coq_x6e, (String.String
                   (Coq_x74, (String.String (Coq_x20, (String.String
                   (Coq_x63, (String.String (Coq_x6f, (String.String
                   (Coq_x6e, (String.String (Coq_x73, (String.String
                   (Coq_x74, (String.String (Coq_x72, (String.String
                   (Coq_x75, (String.String (Coq_x63, (String.String
                   (Coq_x74, (String.String (Coq_x6f, (String.String
                   (Coq_x72, (String.String (Coq_x73, (String.String
                   (Coq_x2c, (String.String (Coq_x20, (String.String
                   (Coq_x6d, (String.String (Coq_x61, (String.String
                   (Coq_x78, (String.String (Coq_x69, (String.String
                   (Coq_x6d, (String.String (Coq_x75, (String.String
                   (Coq_x6d, (String.String (Coq_x20, (String.String
                   (Coq_x32, (String.String (Coq_x30, (String.String
                   (Coq_x30, (String.String (Coq_x20, (String.String
                   (Coq_x61, (String.String (Coq_x6c, (String.String
                   (Coq_x6c, (String.String (Coq_x6f, (String.String
                   (Coq_x77, (String.String (Coq_x65, (String.String
                   (Coq_x64,
                   String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
             (bool_good_error
               (forallb (fun ob ->
                 Z.ltb (Z.of_nat (length ob.ind_ctors)) Int63.wB)
                 mind.ind_bodies) (String.String (Coq_x69, (String.String
               (Coq_x6e, (String.String (Coq_x64, (String.String (Coq_x75,
               (String.String (Coq_x63, (String.String (Coq_x74,
               (String.String (Coq_x69, (String.String (Coq_x76,
               (String.String (Coq_x65, (String.String (Coq_x20,
               (String.String (Coq_x77, (String.String (Coq_x69,
               (String.String (Coq_x74, (String.String (Coq_x68,
               (String.String (Coq_x20, (String.String (Coq_x74,
               (String.String (Coq_x6f, (String.String (Coq_x6f,
               (String.String (Coq_x20, (String.String (Coq_x6d,
               (String.String (Coq_x61, (String.String (Coq_x6e,
               (String.String (Coq_x79, (String.String (Coq_x20,
               (String.String (Coq_x63, (String.String (Coq_x6f,
               (String.String (Coq_x6e, (String.String (Coq_x73,
               (String.String (Coq_x74, (String.String (Coq_x72,
               (String.String (Coq_x75, (String.String (Coq_x63,
               (String.String (Coq_x74, (String.String (Coq_x6f,
               (String.String (Coq_x72, (String.String (Coq_x73,
               String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
           (bool_good_error
             (forallb (fun ob ->
               forallb (fun b ->
                 Z.ltb (Z.of_nat b.cstr_nargs) array_length_Z) ob.ind_ctors)
               mind.ind_bodies) (String.String (Coq_x69, (String.String
             (Coq_x6e, (String.String (Coq_x64, (String.String (Coq_x75,
             (String.String (Coq_x63, (String.String (Coq_x74, (String.String
             (Coq_x69, (String.String (Coq_x76, (String.String (Coq_x65,
             (String.String (Coq_x20, (String.String (Coq_x77, (String.String
             (Coq_x69, (String.String (Coq_x74, (String.String (Coq_x68,
             (String.String (Coq_x20, (String.String (Coq_x74, (String.String
             (Coq_x6f, (String.String (Coq_x6f, (String.String (Coq_x20,
             (String.String (Coq_x6d, (String.String (Coq_x61, (String.String
             (Coq_x6e, (String.String (Coq_x79, (String.String (Coq_x20,
             (String.String (Coq_x63, (String.String (Coq_x6f, (String.String
             (Coq_x6e, (String.String (Coq_x73, (String.String (Coq_x74,
             (String.String (Coq_x72, (String.String (Coq_x75, (String.String
             (Coq_x63, (String.String (Coq_x74, (String.String (Coq_x6f,
             (String.String (Coq_x72, (String.String (Coq_x20, (String.String
             (Coq_x61, (String.String (Coq_x72, (String.String (Coq_x67,
             (String.String (Coq_x75, (String.String (Coq_x6d, (String.String
             (Coq_x65, (String.String (Coq_x6e, (String.String (Coq_x74,
             (String.String (Coq_x73,
             String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
         (bool_good_error (wf_minductive fl mind) (String.String (Coq_x65,
           (String.String (Coq_x6e, (String.String (Coq_x76, (String.String
           (Coq_x69, (String.String (Coq_x72, (String.String (Coq_x6f,
           (String.String (Coq_x6e, (String.String (Coq_x6d, (String.String
           (Coq_x65, (String.String (Coq_x6e, (String.String (Coq_x74,
           (String.String (Coq_x20, (String.String (Coq_x63, (String.String
           (Coq_x6f, (String.String (Coq_x6e, (String.String (Coq_x74,
           (String.String (Coq_x61, (String.String (Coq_x69, (String.String
           (Coq_x6e, (String.String (Coq_x73, (String.String (Coq_x20,
           (String.String (Coq_x6e, (String.String (Coq_x6f, (String.String
           (Coq_x6e, (String.String (Coq_x2d, (String.String (Coq_x65,
           (String.String (Coq_x78, (String.String (Coq_x74, (String.String
           (Coq_x72, (String.String (Coq_x61, (String.String (Coq_x63,
           (String.String (Coq_x74, (String.String (Coq_x61, (String.String
           (Coq_x62, (String.String (Coq_x6c, (String.String (Coq_x65,
           (String.String (Coq_x20, (String.String (Coq_x69, (String.String
           (Coq_x6e, (String.String (Coq_x64, (String.String (Coq_x75,
           (String.String (Coq_x63, (String.String (Coq_x74, (String.String
           (Coq_x69, (String.String (Coq_x76, (String.String (Coq_x65,
           String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
       (check_good_for_extraction_rec fl _UU03a3_0))

(** val check_good_for_extraction :
    coq_EEnvFlags -> ((kername * global_decl) list, term)
    Transform.Transform.program -> bool **)

let check_good_for_extraction fl p =
  if wellformed_fast fl (fst p) (snd p)
  then check_good_for_extraction_rec fl (fst p)
  else ignore
         ((fun s -> ()) (String.String (Coq_x57, (String.String (Coq_x61,
           (String.String (Coq_x72, (String.String (Coq_x6e, (String.String
           (Coq_x69, (String.String (Coq_x6e, (String.String (Coq_x67,
           (String.String (Coq_x3a, (String.String (Coq_x20, (String.String
           (Coq_x74, (String.String (Coq_x65, (String.String (Coq_x72,
           (String.String (Coq_x6d, (String.String (Coq_x20, (String.String
           (Coq_x63, (String.String (Coq_x6f, (String.String (Coq_x6e,
           (String.String (Coq_x74, (String.String (Coq_x61, (String.String
           (Coq_x69, (String.String (Coq_x6e, (String.String (Coq_x73,
           (String.String (Coq_x20, (String.String (Coq_x63, (String.String
           (Coq_x6f, (String.String (Coq_x6e, (String.String (Coq_x73,
           (String.String (Coq_x74, (String.String (Coq_x72, (String.String
           (Coq_x75, (String.String (Coq_x63, (String.String (Coq_x74,
           (String.String (Coq_x6f, (String.String (Coq_x72, (String.String
           (Coq_x73, (String.String (Coq_x20, (String.String (Coq_x66,
           (String.String (Coq_x6f, (String.String (Coq_x72, (String.String
           (Coq_x20, (String.String (Coq_x77, (String.String (Coq_x68,
           (String.String (Coq_x69, (String.String (Coq_x63, (String.String
           (Coq_x68, (String.String (Coq_x20, (String.String (Coq_x65,
           (String.String (Coq_x78, (String.String (Coq_x74, (String.String
           (Coq_x72, (String.String (Coq_x61, (String.String (Coq_x63,
           (String.String (Coq_x74, (String.String (Coq_x69, (String.String
           (Coq_x6f, (String.String (Coq_x6e, (String.String (Coq_x20,
           (String.String (Coq_x69, (String.String (Coq_x73, (String.String
           (Coq_x20, (String.String (Coq_x6e, (String.String (Coq_x6f,
           (String.String (Coq_x74, (String.String (Coq_x20, (String.String
           (Coq_x76, (String.String (Coq_x65, (String.String (Coq_x72,
           (String.String (Coq_x69, (String.String (Coq_x66, (String.String
           (Coq_x69, (String.String (Coq_x65, (String.String (Coq_x64,
           String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
         (check_good_for_extraction_rec fl (fst p))

(** val extraction_term_flags_mlf : coq_ETermFlags **)

let extraction_term_flags_mlf =
  { has_tBox = true; has_tRel = true; has_tVar = false; has_tEvar = false;
    has_tLambda = true; has_tLetIn = true; has_tApp = true; has_tConst =
    true; has_tConstruct = true; has_tCase = true; has_tProj = false;
    has_tFix = true; has_tCoFix = false; has_tPrim = { has_primint = true;
    has_primfloat = true; has_primarray = false }; has_tLazy_Force = false }

(** val extraction_env_flags_mlf : coq_EEnvFlags **)

let extraction_env_flags_mlf =
  { has_axioms = false; has_cstr_params = false; term_switches =
    extraction_term_flags_mlf; cstr_as_blocks = true }

(** val enforce_extraction_conditions :
    coq_Pointer -> coq_Pointer -> coq_Heap -> (global_declarations,
    global_declarations, term, term, term, term) Transform.Transform.t **)

let enforce_extraction_conditions _ _ _ =
  { Transform.Transform.name = (String.String (Coq_x45, (String.String
    (Coq_x6e, (String.String (Coq_x66, (String.String (Coq_x6f,
    (String.String (Coq_x72, (String.String (Coq_x63, (String.String
    (Coq_x65, (String.String (Coq_x20, (String.String (Coq_x74,
    (String.String (Coq_x68, (String.String (Coq_x65, (String.String
    (Coq_x20, (String.String (Coq_x74, (String.String (Coq_x65,
    (String.String (Coq_x72, (String.String (Coq_x6d, (String.String
    (Coq_x20, (String.String (Coq_x69, (String.String (Coq_x73,
    (String.String (Coq_x20, (String.String (Coq_x65, (String.String
    (Coq_x78, (String.String (Coq_x74, (String.String (Coq_x72,
    (String.String (Coq_x61, (String.String (Coq_x63, (String.String
    (Coq_x74, (String.String (Coq_x61, (String.String (Coq_x62,
    (String.String (Coq_x6c, (String.String (Coq_x65,
    String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
    Transform.Transform.transform = (fun p _ ->
    let r = check_good_for_extraction extraction_env_flags_mlf p in ignore r p) }

(** val implement_box_transformation :
    (global_declarations, global_declarations, term, term, term, term)
    Transform.Transform.t **)

let implement_box_transformation =
  { Transform.Transform.name = (String.String (Coq_x69, (String.String
    (Coq_x6d, (String.String (Coq_x70, (String.String (Coq_x6c,
    (String.String (Coq_x65, (String.String (Coq_x6d, (String.String
    (Coq_x65, (String.String (Coq_x6e, (String.String (Coq_x74,
    (String.String (Coq_x69, (String.String (Coq_x6e, (String.String
    (Coq_x67, (String.String (Coq_x20, (String.String (Coq_x62,
    (String.String (Coq_x6f, (String.String (Coq_x78,
    String.EmptyString))))))))))))))))))))))))))))))));
    Transform.Transform.transform = (fun p _ -> implement_box_program p) }

(** val name_annotation :
    (global_declarations, (kername * global_decl) list, term, term, term,
    EWcbvEvalNamed.value) Transform.Transform.t **)

let name_annotation =
  { Transform.Transform.name = (String.String (Coq_x61, (String.String
    (Coq_x6e, (String.String (Coq_x6e, (String.String (Coq_x6f,
    (String.String (Coq_x74, (String.String (Coq_x61, (String.String
    (Coq_x74, (String.String (Coq_x65, (String.String (Coq_x20,
    (String.String (Coq_x6e, (String.String (Coq_x61, (String.String
    (Coq_x6d, (String.String (Coq_x65, (String.String (Coq_x73,
    String.EmptyString))))))))))))))))))))))))))));
    Transform.Transform.transform = (fun p _ -> ((annotate_env [] (fst p)),
    (annotate [] (snd p)))) }

(** val compile_to_malfunction :
    coq_Pointer -> coq_Heap -> ((kername * global_decl) list, (Ident.t * t
    option) list, term, t, EWcbvEvalNamed.value, value) Transform.Transform.t **)

let compile_to_malfunction _ _ =
  { Transform.Transform.name = (String.String (Coq_x63, (String.String
    (Coq_x6f, (String.String (Coq_x6d, (String.String (Coq_x70,
    (String.String (Coq_x69, (String.String (Coq_x6c, (String.String
    (Coq_x65, (String.String (Coq_x20, (String.String (Coq_x74,
    (String.String (Coq_x6f, (String.String (Coq_x20, (String.String
    (Coq_x4d, (String.String (Coq_x61, (String.String (Coq_x6c,
    (String.String (Coq_x66, (String.String (Coq_x75, (String.String
    (Coq_x6e, (String.String (Coq_x63, (String.String (Coq_x74,
    (String.String (Coq_x69, (String.String (Coq_x6f, (String.String
    (Coq_x6e, String.EmptyString))))))))))))))))))))))))))))))))))))))))))));
    Transform.Transform.transform = (fun p _ -> compile_program p) }

(** val post_verified_named_erasure_pipeline :
    coq_Pointer -> coq_Heap -> (global_declarations, global_declarations,
    term, term, term, EWcbvEvalNamed.value) Transform.Transform.t **)

let post_verified_named_erasure_pipeline h h0 =
  Transform.Transform.compose
    (Transform.Transform.compose (enforce_extraction_conditions h h h0)
      implement_box_transformation) name_annotation
