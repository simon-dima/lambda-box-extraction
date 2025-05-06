open BasicAst
open Byte
open Datatypes
open EAst
open EGlobalEnv
open EPrimitive
open EProgram
open Kernames
open List0
open MCList
open Malfunction
open Nat0
open Specif
open Bytestring
open Utils_array

(** val coq_Mapply_ : (t * t list) -> t **)

let coq_Mapply_ = function
| (e, l) -> (match l with
             | [] -> e
             | _ :: _ -> Mapply (e, l))

(** val coq_Mlambda_ : (Ident.t list * t) -> t **)

let coq_Mlambda_ = function
| (e, l) -> (match e with
             | [] -> l
             | _ :: _ -> Mlambda (e, l))

(** val blocks_until : nat -> nat list -> nat **)

let blocks_until i num_args =
  length
    (filter (fun x -> match x with
                      | O -> false
                      | S _ -> true) (firstn i num_args))

(** val nonblocks_until : nat -> nat list -> nat **)

let nonblocks_until i num_args =
  length
    (filter (fun x -> match x with
                      | O -> true
                      | S _ -> false) (firstn i num_args))

(** val coq_Mcase : ((nat list * t) * (Ident.t list * t) list) -> t **)

let coq_Mcase = function
| (p, brs) ->
  let (num_args, discr) = p in
  Mswitch (discr,
  (mapi (fun i pat0 ->
    let (nms, b) = pat0 in
    ((match nth_error num_args i with
      | Some n ->
        (match n with
         | O ->
           (Intrange ((int_of_nat (nonblocks_until i num_args)),
             (int_of_nat (nonblocks_until i num_args)))) :: []
         | S _ -> (Tag (int_of_nat (blocks_until i num_args))) :: [])
      | None -> (Tag (int_of_nat (blocks_until i num_args))) :: []),
    (coq_Mapply_ ((coq_Mlambda_ (nms, b)),
      (mapi (fun i0 _ -> Mfield ((int_of_nat i0), discr)) nms))))) brs))

(** val lookup_record_projs :
    global_declarations -> inductive -> ident list option **)

let lookup_record_projs e ind =
  match lookup_inductive e ind with
  | Some p -> let (_, idecl) = p in Some (map (fun p0 -> p0) idecl.ind_projs)
  | None -> None

(** val lookup_constructor_args :
    global_declarations -> inductive -> nat list option **)

let lookup_constructor_args e ind =
  match lookup_inductive e ind with
  | Some p ->
    let (_, idecl) = p in Some (map (fun c -> c.cstr_nargs) idecl.ind_ctors)
  | None -> None

(** val coq_Mapply_u : t -> t -> t **)

let coq_Mapply_u t0 a =
  match t0 with
  | Mapply p -> let (fn, args) = p in Mapply (fn, (app args (a :: [])))
  | _ -> Mapply (t0, (a :: []))

(** val num_of_nat : nat -> t **)

let num_of_nat n =
  Mnum (Coq_numconst_Int (int_of_nat n))

(** val compile_array : t list -> t -> t **)

let compile_array values default =
  let init = Mvecnew ((Array, (num_of_nat (length values))), default) in
  fold_left_i (fun v idx arr -> Mvecset (((Array, arr), (num_of_nat idx)),
    v)) values init

(** val is_wf_rec_body : t -> bool **)

let rec is_wf_rec_body = function
| Mlambda _ -> true
| Mlet p -> let (_, t1) = p in is_wf_rec_body t1
| Mlazy _ -> true
| _ -> false

(** val force_lambda : t -> t **)

let force_lambda t0 =
  if is_wf_rec_body t0
  then t0
  else Mlambda (((String.String (Coq_x5f, (String.String (Coq_x5f,
         (String.String (Coq_x65, (String.String (Coq_x78, (String.String
         (Coq_x70, (String.String (Coq_x61, (String.String (Coq_x6e,
         (String.String (Coq_x64, (String.String (Coq_x65, (String.String
         (Coq_x64, String.EmptyString)))))))))))))))))))) :: []),
         (coq_Mapply_u t0 (Mvar (String.String (Coq_x5f, (String.String
           (Coq_x5f, (String.String (Coq_x65, (String.String (Coq_x78,
           (String.String (Coq_x70, (String.String (Coq_x61, (String.String
           (Coq_x6e, (String.String (Coq_x64, (String.String (Coq_x65,
           (String.String (Coq_x64, String.EmptyString)))))))))))))))))))))))

(** val compile : global_declarations -> term -> t **)

let rec compile _UU03a3_ = function
| Coq_tBox ->
  Mstring (String.String (Coq_x65, (String.String (Coq_x72, (String.String
    (Coq_x72, (String.String (Coq_x6f, (String.String (Coq_x72,
    (String.String (Coq_x3a, (String.String (Coq_x20, (String.String
    (Coq_x74, (String.String (Coq_x42, (String.String (Coq_x6f,
    (String.String (Coq_x78, (String.String (Coq_x20, (String.String
    (Coq_x68, (String.String (Coq_x61, (String.String (Coq_x73,
    (String.String (Coq_x20, (String.String (Coq_x62, (String.String
    (Coq_x65, (String.String (Coq_x65, (String.String (Coq_x6e,
    (String.String (Coq_x20, (String.String (Coq_x74, (String.String
    (Coq_x72, (String.String (Coq_x61, (String.String (Coq_x6e,
    (String.String (Coq_x73, (String.String (Coq_x6c, (String.String
    (Coq_x61, (String.String (Coq_x74, (String.String (Coq_x65,
    (String.String (Coq_x64, (String.String (Coq_x20, (String.String
    (Coq_x61, (String.String (Coq_x77, (String.String (Coq_x61,
    (String.String (Coq_x79,
    String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
| Coq_tRel _ ->
  Mstring (String.String (Coq_x65, (String.String (Coq_x72, (String.String
    (Coq_x72, (String.String (Coq_x6f, (String.String (Coq_x72,
    (String.String (Coq_x3a, (String.String (Coq_x20, (String.String
    (Coq_x74, (String.String (Coq_x52, (String.String (Coq_x65,
    (String.String (Coq_x6c, (String.String (Coq_x20, (String.String
    (Coq_x68, (String.String (Coq_x61, (String.String (Coq_x73,
    (String.String (Coq_x20, (String.String (Coq_x62, (String.String
    (Coq_x65, (String.String (Coq_x65, (String.String (Coq_x6e,
    (String.String (Coq_x20, (String.String (Coq_x74, (String.String
    (Coq_x72, (String.String (Coq_x61, (String.String (Coq_x6e,
    (String.String (Coq_x73, (String.String (Coq_x6c, (String.String
    (Coq_x61, (String.String (Coq_x74, (String.String (Coq_x65,
    (String.String (Coq_x64, (String.String (Coq_x20, (String.String
    (Coq_x61, (String.String (Coq_x77, (String.String (Coq_x61,
    (String.String (Coq_x79,
    String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
| Coq_tVar i -> Mvar i
| Coq_tEvar (_, _) ->
  Mstring (String.String (Coq_x65, (String.String (Coq_x72, (String.String
    (Coq_x72, (String.String (Coq_x6f, (String.String (Coq_x72,
    (String.String (Coq_x3a, (String.String (Coq_x20, (String.String
    (Coq_x74, (String.String (Coq_x45, (String.String (Coq_x76,
    (String.String (Coq_x61, (String.String (Coq_x72, (String.String
    (Coq_x20, (String.String (Coq_x6e, (String.String (Coq_x6f,
    (String.String (Coq_x74, (String.String (Coq_x20, (String.String
    (Coq_x73, (String.String (Coq_x75, (String.String (Coq_x70,
    (String.String (Coq_x70, (String.String (Coq_x6f, (String.String
    (Coq_x72, (String.String (Coq_x74, (String.String (Coq_x65,
    (String.String (Coq_x64,
    String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))
| Coq_tLambda (na, t0) ->
  Mlambda (((string_of_name na) :: []), (compile _UU03a3_ t0))
| Coq_tLetIn (na, b, t0) ->
  Mlet (((Named ((string_of_name na), (compile _UU03a3_ b))) :: []),
    (compile _UU03a3_ t0))
| Coq_tApp (u, v) -> coq_Mapply_u (compile _UU03a3_ u) (compile _UU03a3_ v)
| Coq_tConst k -> Mglobal (string_of_kername k)
| Coq_tConstruct (ind, n, args) ->
  (match args with
   | [] ->
     (match lookup_constructor_args _UU03a3_ ind with
      | Some num_args ->
        Mnum (Coq_numconst_Int (int_of_nat (nonblocks_until n num_args)))
      | None ->
        Mstring (String.String (Coq_x65, (String.String (Coq_x72,
          (String.String (Coq_x72, (String.String (Coq_x6f, (String.String
          (Coq_x72, (String.String (Coq_x3a, (String.String (Coq_x20,
          (String.String (Coq_x69, (String.String (Coq_x6e, (String.String
          (Coq_x64, (String.String (Coq_x75, (String.String (Coq_x63,
          (String.String (Coq_x74, (String.String (Coq_x69, (String.String
          (Coq_x76, (String.String (Coq_x65, (String.String (Coq_x20,
          (String.String (Coq_x6e, (String.String (Coq_x6f, (String.String
          (Coq_x74, (String.String (Coq_x20, (String.String (Coq_x66,
          (String.String (Coq_x6f, (String.String (Coq_x75, (String.String
          (Coq_x6e, (String.String (Coq_x64,
          String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))
   | t0 :: l ->
     (match lookup_constructor_args _UU03a3_ ind with
      | Some num_args ->
        Mblock ((int_of_nat (blocks_until n num_args)),
          (map_InP (t0 :: l) (fun x0 _ -> compile _UU03a3_ x0)))
      | None ->
        Mstring (String.String (Coq_x65, (String.String (Coq_x72,
          (String.String (Coq_x72, (String.String (Coq_x6f, (String.String
          (Coq_x72, (String.String (Coq_x3a, (String.String (Coq_x20,
          (String.String (Coq_x69, (String.String (Coq_x6e, (String.String
          (Coq_x64, (String.String (Coq_x75, (String.String (Coq_x63,
          (String.String (Coq_x74, (String.String (Coq_x69, (String.String
          (Coq_x76, (String.String (Coq_x65, (String.String (Coq_x20,
          (String.String (Coq_x6e, (String.String (Coq_x6f, (String.String
          (Coq_x74, (String.String (Coq_x20, (String.String (Coq_x66,
          (String.String (Coq_x6f, (String.String (Coq_x75, (String.String
          (Coq_x6e, (String.String (Coq_x64,
          String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))
| Coq_tCase (indn, c, brs) ->
  (match brs with
   | [] ->
     Mlambda (((String.String (Coq_x65, (String.String (Coq_x6d,
       (String.String (Coq_x70, (String.String (Coq_x74, (String.String
       (Coq_x79, (String.String (Coq_x5f, (String.String (Coq_x6d,
       (String.String (Coq_x61, (String.String (Coq_x74, (String.String
       (Coq_x63, (String.String (Coq_x68,
       String.EmptyString)))))))))))))))))))))) :: []), (Mvar (String.String
       (Coq_x65, (String.String (Coq_x6d, (String.String (Coq_x70,
       (String.String (Coq_x74, (String.String (Coq_x79, (String.String
       (Coq_x5f, (String.String (Coq_x6d, (String.String (Coq_x61,
       (String.String (Coq_x74, (String.String (Coq_x63, (String.String
       (Coq_x68, String.EmptyString))))))))))))))))))))))))
   | p :: l ->
     (match lookup_constructor_args _UU03a3_ (fst indn) with
      | Some num_args ->
        coq_Mcase ((num_args, (compile _UU03a3_ c)),
          (map_InP (p :: l) (fun br _ -> ((rev_map string_of_name (fst br)),
            (compile _UU03a3_ (snd br))))))
      | None ->
        Mstring (String.String (Coq_x65, (String.String (Coq_x72,
          (String.String (Coq_x72, (String.String (Coq_x6f, (String.String
          (Coq_x72, (String.String (Coq_x3a, (String.String (Coq_x20,
          (String.String (Coq_x69, (String.String (Coq_x6e, (String.String
          (Coq_x64, (String.String (Coq_x75, (String.String (Coq_x63,
          (String.String (Coq_x74, (String.String (Coq_x69, (String.String
          (Coq_x76, (String.String (Coq_x65, (String.String (Coq_x20,
          (String.String (Coq_x6e, (String.String (Coq_x6f, (String.String
          (Coq_x74, (String.String (Coq_x20, (String.String (Coq_x66,
          (String.String (Coq_x6f, (String.String (Coq_x75, (String.String
          (Coq_x6e, (String.String (Coq_x64,
          String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))
| Coq_tProj (p, c) ->
  let { proj_ind = proj_ind0; proj_npars = _; proj_arg = proj_arg0 } = p in
  (match lookup_record_projs _UU03a3_ proj_ind0 with
   | Some l ->
     let len = length l in
     Mfield ((int_of_nat (sub (sub len (S O)) proj_arg0)),
     (compile _UU03a3_ c))
   | None ->
     Mstring (String.String (Coq_x69, (String.String (Coq_x6e, (String.String
       (Coq_x64, (String.String (Coq_x75, (String.String (Coq_x63,
       (String.String (Coq_x74, (String.String (Coq_x69, (String.String
       (Coq_x76, (String.String (Coq_x65, (String.String (Coq_x20,
       (String.String (Coq_x6e, (String.String (Coq_x6f, (String.String
       (Coq_x74, (String.String (Coq_x20, (String.String (Coq_x66,
       (String.String (Coq_x6f, (String.String (Coq_x75, (String.String
       (Coq_x6e, (String.String (Coq_x64,
       String.EmptyString)))))))))))))))))))))))))))))))))))))))
| Coq_tFix (mfix, idx) ->
  let bodies =
    map_InP mfix (fun d _ -> ((string_of_name d.dname),
      (force_lambda (compile _UU03a3_ d.dbody))))
  in
  Mlet (((Recursive bodies) :: []), (Mvar
  (fst (nth idx bodies (String.EmptyString, (Mstring String.EmptyString))))))
| Coq_tCoFix (_, _) ->
  Mstring (String.String (Coq_x65, (String.String (Coq_x72, (String.String
    (Coq_x72, (String.String (Coq_x6f, (String.String (Coq_x72,
    (String.String (Coq_x3a, (String.String (Coq_x20, (String.String
    (Coq_x74, (String.String (Coq_x43, (String.String (Coq_x6f,
    (String.String (Coq_x66, (String.String (Coq_x69, (String.String
    (Coq_x78, (String.String (Coq_x20, (String.String (Coq_x6e,
    (String.String (Coq_x6f, (String.String (Coq_x74, (String.String
    (Coq_x20, (String.String (Coq_x73, (String.String (Coq_x75,
    (String.String (Coq_x70, (String.String (Coq_x70, (String.String
    (Coq_x6f, (String.String (Coq_x72, (String.String (Coq_x74,
    (String.String (Coq_x65, (String.String (Coq_x64,
    String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))
| Coq_tPrim prim ->
  let Coq_existT (_, p) = prim in
  (match p with
   | Coq_primIntModel i -> Mnum (Coq_numconst_Int i)
   | Coq_primFloatModel f -> Mnum (Coq_numconst_Float64 f)
   | Coq_primArrayModel a ->
     let default = compile _UU03a3_ a.array_default in
     let values = map_InP a.array_value (fun v _ -> compile _UU03a3_ v) in
     let arr = compile_array values default in
     Mapply ((Mglobal (String.String (Coq_x50, (String.String (Coq_x41,
     (String.String (Coq_x72, (String.String (Coq_x72, (String.String
     (Coq_x61, (String.String (Coq_x79, (String.String (Coq_x2e,
     (String.String (Coq_x6f, (String.String (Coq_x66, (String.String
     (Coq_x5f, (String.String (Coq_x61, (String.String (Coq_x72,
     (String.String (Coq_x72, (String.String (Coq_x61, (String.String
     (Coq_x79, String.EmptyString))))))))))))))))))))))))))))))),
     (arr :: (default :: []))))
| Coq_tLazy t0 -> Mlazy (compile _UU03a3_ t0)
| Coq_tForce t0 -> Mforce (compile _UU03a3_ t0)

(** val compile_constant_decl :
    global_declarations -> constant_body -> t option **)

let compile_constant_decl _UU03a3_ cb =
  option_map (compile _UU03a3_) cb

(** val compile_env :
    (kername * global_decl) list -> (String.t * t option) list **)

let rec compile_env = function
| [] -> []
| y :: _UU03a3_0 ->
  let (x, d) = y in
  (match d with
   | ConstantDecl cb ->
     ((string_of_kername x),
       (compile_constant_decl _UU03a3_0 cb)) :: (compile_env _UU03a3_0)
   | InductiveDecl _ -> compile_env _UU03a3_0)

(** val compile_program : eprogram -> program **)

let compile_program p =
  ((compile_env (fst p)), (compile (fst p) (snd p)))
