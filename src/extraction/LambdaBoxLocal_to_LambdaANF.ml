open AstCommon
open BasicAst
open BinNat
open BinNums
open BinPos
open Byte
open Datatypes
open Kernames
open List0
open Monad0
open Nat0
open PeanoNat
open Bytestring
open CompM
open Cps
open Ctx
open Expression
open State

type conId_map = (dcon * ctor_tag) list

(** val conId_dec : dcon -> dcon -> bool **)

let conId_dec x y =
  let (i, n) = x in
  let (i0, n0) = y in
  let h = inductive_dec i i0 in if h then coq_NEq n n0 else false

(** val dcon_to_info : positive -> dcon -> conId_map -> positive **)

let rec dcon_to_info default_tag a = function
| [] -> default_tag
| p :: sig' ->
  let (cId, inf) = p in
  if conId_dec a cId then inf else dcon_to_info default_tag a sig'

type constr_env = conId_map

(** val dcon_to_tag : positive -> dcon -> conId_map -> positive **)

let dcon_to_tag =
  dcon_to_info

type name_env = name M.t

type ienv = (kername * itypPack) list

(** val fromN : positive -> nat -> positive list * positive **)

let rec fromN n = function
| O -> ([], n)
| S m' -> let (l, nm) = fromN (Pos.add n Coq_xH) m' in ((n :: l), nm)

(** val ctx_bind_proj : ctor_tag -> positive -> var list -> nat -> exp_ctx **)

let rec ctx_bind_proj tg r vars args =
  match vars with
  | [] -> Hole_c
  | v :: vars0 ->
    let ctx_p' = ctx_bind_proj tg r vars0 (sub args (S O)) in
    Eproj_c (v, tg, (N.of_nat (sub args (S O))), r, ctx_p')

(** val convert_cnstrs :
    String.t -> ctor_tag list -> coq_Cnstr list -> inductive -> coq_N ->
    coq_N -> coq_N -> ind_tag -> ctor_env -> conId_map -> ctor_env * conId_map **)

let rec convert_cnstrs tyname cct itC ind nCon unboxed boxed niT ce dcm =
  match cct with
  | [] -> (ce, dcm)
  | cn :: cct' ->
    (match itC with
     | [] -> (ce, dcm)
     | cst :: icT' ->
       let { coq_CnstrNm = cname; coq_CnstrArity = ccn } = cst in
       let is_unboxed = Nat.eqb ccn O in
       let info = { ctor_name = (Coq_nNamed cname); ctor_ind_name =
         (Coq_nNamed tyname); ctor_ind_tag = niT; ctor_arity =
         (N.of_nat ccn); ctor_ordinal =
         (if is_unboxed then unboxed else boxed) }
       in
       convert_cnstrs tyname cct' icT' ind (N.add nCon (Npos Coq_xH))
         (if is_unboxed then N.add unboxed (Npos Coq_xH) else unboxed)
         (if is_unboxed then boxed else N.add boxed (Npos Coq_xH)) niT
         (M.set cn info ce) (((ind, nCon), cn) :: dcm))

(** val convert_typack :
    ityp list -> kername -> nat ->
    ((((ind_env * ctor_env) * ctor_tag) * ind_tag) * conId_map) ->
    (((ind_env * ctor_env) * ctor_tag) * ind_tag) * conId_map **)

let rec convert_typack typ idBundle n ice = match ice with
| (p, dcm) ->
  let (p0, niT) = p in
  let (p1, ncT) = p0 in
  let (ie, ce) = p1 in
  (match typ with
   | [] -> ice
   | y :: typ' ->
     let { itypNm = itN; itypCnstrs = itC } = y in
     let (cct, ncT') = fromN ncT (length itC) in
     let (ce', dcm') =
       convert_cnstrs itN cct itC { inductive_mind = idBundle;
         inductive_ind = n } N0 N0 N0 niT ce dcm
     in
     let ityi =
       combine cct
         (map (fun c ->
           let { coq_CnstrNm = _; coq_CnstrArity = n0 } = c in N.of_nat n0)
           itC)
     in
     convert_typack typ' idBundle (add n (S O)) (((((M.set niT ityi ie),
       ce'), ncT'), (Pos.succ niT)), dcm'))

(** val convert_env' :
    ienv -> ((((ind_env * ctor_env) * ctor_tag) * ind_tag) * conId_map) ->
    (((ind_env * ctor_env) * ctor_tag) * ind_tag) * conId_map **)

let rec convert_env' g ice =
  match g with
  | [] -> ice
  | p :: g' ->
    let (id, ty) = p in convert_env' g' (convert_typack ty id O ice)

(** val convert_env :
    positive -> positive -> ienv ->
    (((ind_env * ctor_env) * ctor_tag) * ind_tag) * conId_map **)

let convert_env default_tag default_itag g =
  let default_ind_env = M.set default_itag ((default_tag, N0) :: []) M.empty
  in
  let info = { ctor_name = Coq_nAnon; ctor_ind_name = Coq_nAnon;
    ctor_ind_tag = default_itag; ctor_arity = N0; ctor_ordinal = N0 }
  in
  let default_ctor_env = M.set default_tag info M.empty in
  convert_env' g ((((default_ind_env, default_ctor_env),
    (Pos.succ default_tag)), (Pos.succ default_itag)), [])

type 'a cpsM = (unit, 'a) compM'

(** val get_named_str_lst : String.t list -> var list cpsM **)

let get_named_str_lst s =
  mapM (coq_MonadErrorT coq_MonadState) get_named_str s

(** val convert_prim :
    positive -> positive -> nat -> positive -> var list -> var -> Cps.exp cpsM **)

let rec convert_prim func_tag kon_tag n prim args kont =
  match n with
  | O ->
    bind (coq_MonadErrorT coq_MonadState)
      (get_named_str (String.String (Coq_x70, (String.String (Coq_x72,
        (String.String (Coq_x69, (String.String (Coq_x6d,
        String.EmptyString))))))))) (fun pr ->
      ret (coq_MonadErrorT coq_MonadState) (Eprim (pr, prim, (rev args),
        (Eapp (kont, kon_tag, (pr :: []))))))
  | S n0 ->
    bind (coq_MonadErrorT coq_MonadState)
      (get_named_str (String.String (Coq_x70, (String.String (Coq_x5f,
        (String.String (Coq_x61, (String.String (Coq_x72, (String.String
        (Coq_x67, String.EmptyString))))))))))) (fun arg ->
      bind (coq_MonadErrorT coq_MonadState)
        (get_named_str (String.String (Coq_x70, (String.String (Coq_x5f,
          (String.String (Coq_x6b, String.EmptyString))))))) (fun kont1 ->
        bind (coq_MonadErrorT coq_MonadState)
          (get_named_str (String.String (Coq_x70, (String.String (Coq_x72,
            (String.String (Coq_x69, (String.String (Coq_x6d, (String.String
            (Coq_x5f, (String.String (Coq_x77, (String.String (Coq_x72,
            (String.String (Coq_x61, (String.String (Coq_x70, (String.String
            (Coq_x70, (String.String (Coq_x65, (String.String (Coq_x72,
            String.EmptyString))))))))))))))))))))))))) (fun f ->
          bind (coq_MonadErrorT coq_MonadState)
            (convert_prim func_tag kon_tag n0 prim (arg :: args) kont1)
            (fun trm ->
            ret (coq_MonadErrorT coq_MonadState) (Efun ((Fcons (f, func_tag,
              (kont1 :: (arg :: [])), trm, Fnil)), (Eapp (kont, kon_tag,
              (f :: [])))))))))

(** val names_lst_len : name list -> nat -> name list **)

let rec names_lst_len ns m =
  match ns with
  | [] -> (match m with
           | O -> []
           | S _ -> repeat Coq_nAnon m)
  | n :: ns0 -> (match m with
                 | O -> []
                 | S m0 -> n :: (names_lst_len ns0 m0))

(** val cps_cvt :
    (((kername * String.t) * bool) * nat) M.t -> positive -> positive ->
    positive -> exp -> var list -> var -> constr_env -> Cps.exp cpsM **)

let cps_cvt prim_map func_tag kon_tag default_tag =
  let rec cps_cvt0 e vn k tgm =
    match e with
    | Var_e x ->
      (match nth_error vn (N.to_nat x) with
       | Some v ->
         ret (coq_MonadErrorT coq_MonadState) (Eapp (k, kon_tag, (v :: [])))
       | None ->
         failwith (String.String (Coq_x55, (String.String (Coq_x6e,
           (String.String (Coq_x6b, (String.String (Coq_x6e, (String.String
           (Coq_x6f, (String.String (Coq_x77, (String.String (Coq_x6e,
           (String.String (Coq_x20, (String.String (Coq_x44, (String.String
           (Coq_x65, (String.String (Coq_x42, (String.String (Coq_x72,
           (String.String (Coq_x75, (String.String (Coq_x69, (String.String
           (Coq_x6a, (String.String (Coq_x6e, (String.String (Coq_x20,
           (String.String (Coq_x69, (String.String (Coq_x6e, (String.String
           (Coq_x64, (String.String (Coq_x65, (String.String (Coq_x78,
           String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))
    | Lam_e (n, e1) ->
      bind (coq_MonadErrorT coq_MonadState)
        (get_named_str (String.String (Coq_x78, (String.String (Coq_x31,
          String.EmptyString))))) (fun x1 ->
        bind (coq_MonadErrorT coq_MonadState) (get_named n) (fun f ->
          bind (coq_MonadErrorT coq_MonadState)
            (get_named_str (String.String (Coq_x6b, (String.String (Coq_x31,
              String.EmptyString))))) (fun k1 ->
            bind (coq_MonadErrorT coq_MonadState)
              (cps_cvt0 e1 (x1 :: vn) k1 tgm) (fun e1' ->
              ret (coq_MonadErrorT coq_MonadState) (Efun ((Fcons (f,
                func_tag, (k1 :: (x1 :: [])), e1', Fnil)), (Eapp (k, kon_tag,
                (f :: [])))))))))
    | App_e (e1, e2) ->
      bind (coq_MonadErrorT coq_MonadState)
        (get_named_str (String.String (Coq_x78, (String.String (Coq_x31,
          String.EmptyString))))) (fun x1 ->
        bind (coq_MonadErrorT coq_MonadState)
          (get_named_str (String.String (Coq_x6b, (String.String (Coq_x31,
            String.EmptyString))))) (fun k1 ->
          bind (coq_MonadErrorT coq_MonadState)
            (get_named_str (String.String (Coq_x78, (String.String (Coq_x32,
              String.EmptyString))))) (fun x2 ->
            bind (coq_MonadErrorT coq_MonadState)
              (get_named_str (String.String (Coq_x6b, (String.String
                (Coq_x32, String.EmptyString))))) (fun k2 ->
              bind (coq_MonadErrorT coq_MonadState) (cps_cvt0 e1 vn k1 tgm)
                (fun e1' ->
                bind (coq_MonadErrorT coq_MonadState) (cps_cvt0 e2 vn k2 tgm)
                  (fun e2' ->
                  ret (coq_MonadErrorT coq_MonadState) (Efun ((Fcons (k1,
                    kon_tag, (x1 :: []), (Efun ((Fcons (k2, kon_tag,
                    (x2 :: []), (Eapp (x1, func_tag, (k :: (x2 :: [])))),
                    Fnil)), e2')), Fnil)), e1'))))))))
    | Con_e (dci, es) ->
      let c_tag = dcon_to_tag default_tag dci tgm in
      bind (coq_MonadErrorT coq_MonadState)
        (get_named_str (String.String (Coq_x78, (String.String (Coq_x27,
          String.EmptyString))))) (fun x' ->
        bind (coq_MonadErrorT coq_MonadState)
          (get_named_str_lst
            (map (fun _ -> String.String (Coq_x78, String.EmptyString))
              (exps_as_list es))) (fun xs ->
          bind (coq_MonadErrorT coq_MonadState)
            (get_named_str_lst
              (map (fun _ -> String.String (Coq_x6b, String.EmptyString))
                (exps_as_list es))) (fun ks ->
            cps_cvt_exps es vn (Econstr (x', c_tag, xs, (Eapp (k, kon_tag,
              (x' :: []))))) xs ks tgm)))
    | Match_e (e1, _, bl) ->
      bind (coq_MonadErrorT coq_MonadState)
        (get_named_str (String.String (Coq_x78, (String.String (Coq_x31,
          String.EmptyString))))) (fun x1 ->
        bind (coq_MonadErrorT coq_MonadState)
          (get_named_str (String.String (Coq_x6b, (String.String (Coq_x31,
            String.EmptyString))))) (fun k1 ->
          bind (coq_MonadErrorT coq_MonadState) (cps_cvt0 e1 vn k1 tgm)
            (fun e1' ->
            bind (coq_MonadErrorT coq_MonadState)
              (cps_cvt_branches bl vn k x1 tgm) (fun cbl ->
              ret (coq_MonadErrorT coq_MonadState) (Efun ((Fcons (k1,
                kon_tag, (x1 :: []), (Ecase (x1, cbl)), Fnil)), e1'))))))
    | Let_e (n, e1, e2) ->
      bind (coq_MonadErrorT coq_MonadState)
        (get_named_str (string_of_name n)) (fun x ->
        bind (coq_MonadErrorT coq_MonadState)
          (get_named_str (String.String (Coq_x6b, String.EmptyString)))
          (fun k1 ->
          bind (coq_MonadErrorT coq_MonadState) (cps_cvt0 e2 (x :: vn) k tgm)
            (fun e2' ->
            bind (coq_MonadErrorT coq_MonadState) (cps_cvt0 e1 vn k1 tgm)
              (fun e1' ->
              ret (coq_MonadErrorT coq_MonadState) (Efun ((Fcons (k1,
                kon_tag, (x :: []), e2', Fnil)), e1'))))))
    | Fix_e (fnlst, i) ->
      let names_lst = fnames fnlst in
      bind (coq_MonadErrorT coq_MonadState) (get_named_lst names_lst)
        (fun nlst ->
        bind (coq_MonadErrorT coq_MonadState)
          (cps_cvt_efnlst fnlst (app (rev nlst) vn) nlst tgm) (fun fdefs ->
          match nth_error nlst (N.to_nat i) with
          | Some i' ->
            ret (coq_MonadErrorT coq_MonadState) (Efun (fdefs, (Eapp (k,
              kon_tag, (i' :: [])))))
          | None ->
            failwith (String.String (Coq_x55, (String.String (Coq_x6e,
              (String.String (Coq_x6b, (String.String (Coq_x6e,
              (String.String (Coq_x6f, (String.String (Coq_x77,
              (String.String (Coq_x6e, (String.String (Coq_x20,
              (String.String (Coq_x66, (String.String (Coq_x75,
              (String.String (Coq_x6e, (String.String (Coq_x63,
              (String.String (Coq_x74, (String.String (Coq_x69,
              (String.String (Coq_x6f, (String.String (Coq_x6e,
              (String.String (Coq_x20, (String.String (Coq_x69,
              (String.String (Coq_x6e, (String.String (Coq_x64,
              (String.String (Coq_x65, (String.String (Coq_x78,
              String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))
    | Prf_e ->
      bind (coq_MonadErrorT coq_MonadState)
        (get_named_str (String.String (Coq_x78, String.EmptyString)))
        (fun x ->
        ret (coq_MonadErrorT coq_MonadState) (Econstr (x, default_tag, [],
          (Eapp (k, kon_tag, (x :: []))))))
    | Prim_val_e p ->
      bind (coq_MonadErrorT coq_MonadState)
        (get_named_str (String.String (Coq_x70, (String.String (Coq_x72,
          (String.String (Coq_x69, (String.String (Coq_x6d,
          String.EmptyString))))))))) (fun x ->
        ret (coq_MonadErrorT coq_MonadState) (Eprim_val (x, p, (Eapp (k,
          kon_tag, (x :: []))))))
    | Prim_e p ->
      (match M.get p prim_map with
       | Some p0 ->
         let (_, ar) = p0 in convert_prim func_tag kon_tag ar p [] k
       | None ->
         failwith (String.String (Coq_x49, (String.String (Coq_x6e,
           (String.String (Coq_x74, (String.String (Coq_x65, (String.String
           (Coq_x72, (String.String (Coq_x6e, (String.String (Coq_x61,
           (String.String (Coq_x6c, (String.String (Coq_x20, (String.String
           (Coq_x65, (String.String (Coq_x72, (String.String (Coq_x72,
           (String.String (Coq_x6f, (String.String (Coq_x72, (String.String
           (Coq_x3a, (String.String (Coq_x20, (String.String (Coq_x69,
           (String.String (Coq_x64, (String.String (Coq_x65, (String.String
           (Coq_x6e, (String.String (Coq_x74, (String.String (Coq_x69,
           (String.String (Coq_x66, (String.String (Coq_x69, (String.String
           (Coq_x65, (String.String (Coq_x72, (String.String (Coq_x20,
           (String.String (Coq_x66, (String.String (Coq_x6f, (String.String
           (Coq_x72, (String.String (Coq_x20, (String.String (Coq_x70,
           (String.String (Coq_x72, (String.String (Coq_x69, (String.String
           (Coq_x6d, (String.String (Coq_x69, (String.String (Coq_x74,
           (String.String (Coq_x69, (String.String (Coq_x76, (String.String
           (Coq_x65, (String.String (Coq_x20, (String.String (Coq_x6e,
           (String.String (Coq_x6f, (String.String (Coq_x74, (String.String
           (Coq_x20, (String.String (Coq_x66, (String.String (Coq_x6f,
           (String.String (Coq_x75, (String.String (Coq_x6e, (String.String
           (Coq_x64,
           String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  and cps_cvt_exps es vn e_app xs ks tgm =
    match es with
    | Coq_enil -> ret (coq_MonadErrorT coq_MonadState) e_app
    | Coq_econs (e, es') ->
      (match xs with
       | [] ->
         failwith (String.String (Coq_x49, (String.String (Coq_x6e,
           (String.String (Coq_x74, (String.String (Coq_x65, (String.String
           (Coq_x72, (String.String (Coq_x6e, (String.String (Coq_x61,
           (String.String (Coq_x6c, (String.String (Coq_x20, (String.String
           (Coq_x65, (String.String (Coq_x72, (String.String (Coq_x72,
           (String.String (Coq_x6f, (String.String (Coq_x72, (String.String
           (Coq_x3a, (String.String (Coq_x20, (String.String (Coq_x77,
           (String.String (Coq_x72, (String.String (Coq_x6f, (String.String
           (Coq_x6e, (String.String (Coq_x67, (String.String (Coq_x20,
           (String.String (Coq_x6e, (String.String (Coq_x75, (String.String
           (Coq_x6d, (String.String (Coq_x62, (String.String (Coq_x65,
           (String.String (Coq_x72, (String.String (Coq_x20, (String.String
           (Coq_x6f, (String.String (Coq_x66, (String.String (Coq_x20,
           (String.String (Coq_x61, (String.String (Coq_x72, (String.String
           (Coq_x67, (String.String (Coq_x75, (String.String (Coq_x6d,
           (String.String (Coq_x65, (String.String (Coq_x6e, (String.String
           (Coq_x74, (String.String (Coq_x73, (String.String (Coq_x20,
           (String.String (Coq_x69, (String.String (Coq_x6e, (String.String
           (Coq_x20, (String.String (Coq_x63, (String.String (Coq_x6f,
           (String.String (Coq_x6e, (String.String (Coq_x73, (String.String
           (Coq_x74, (String.String (Coq_x72, (String.String (Coq_x75,
           (String.String (Coq_x63, (String.String (Coq_x74, (String.String
           (Coq_x6f, (String.String (Coq_x72, (String.String (Coq_x20,
           (String.String (Coq_x63, (String.String (Coq_x6f, (String.String
           (Coq_x6e, (String.String (Coq_x74, (String.String (Coq_x69,
           (String.String (Coq_x6e, (String.String (Coq_x75, (String.String
           (Coq_x61, (String.String (Coq_x74, (String.String (Coq_x69,
           (String.String (Coq_x6f, (String.String (Coq_x6e,
           String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
       | x1 :: xs0 ->
         (match ks with
          | [] ->
            failwith (String.String (Coq_x49, (String.String (Coq_x6e,
              (String.String (Coq_x74, (String.String (Coq_x65,
              (String.String (Coq_x72, (String.String (Coq_x6e,
              (String.String (Coq_x61, (String.String (Coq_x6c,
              (String.String (Coq_x20, (String.String (Coq_x65,
              (String.String (Coq_x72, (String.String (Coq_x72,
              (String.String (Coq_x6f, (String.String (Coq_x72,
              (String.String (Coq_x3a, (String.String (Coq_x20,
              (String.String (Coq_x77, (String.String (Coq_x72,
              (String.String (Coq_x6f, (String.String (Coq_x6e,
              (String.String (Coq_x67, (String.String (Coq_x20,
              (String.String (Coq_x6e, (String.String (Coq_x75,
              (String.String (Coq_x6d, (String.String (Coq_x62,
              (String.String (Coq_x65, (String.String (Coq_x72,
              (String.String (Coq_x20, (String.String (Coq_x6f,
              (String.String (Coq_x66, (String.String (Coq_x20,
              (String.String (Coq_x61, (String.String (Coq_x72,
              (String.String (Coq_x67, (String.String (Coq_x75,
              (String.String (Coq_x6d, (String.String (Coq_x65,
              (String.String (Coq_x6e, (String.String (Coq_x74,
              (String.String (Coq_x73, (String.String (Coq_x20,
              (String.String (Coq_x69, (String.String (Coq_x6e,
              (String.String (Coq_x20, (String.String (Coq_x63,
              (String.String (Coq_x6f, (String.String (Coq_x6e,
              (String.String (Coq_x73, (String.String (Coq_x74,
              (String.String (Coq_x72, (String.String (Coq_x75,
              (String.String (Coq_x63, (String.String (Coq_x74,
              (String.String (Coq_x6f, (String.String (Coq_x72,
              (String.String (Coq_x20, (String.String (Coq_x63,
              (String.String (Coq_x6f, (String.String (Coq_x6e,
              (String.String (Coq_x74, (String.String (Coq_x69,
              (String.String (Coq_x6e, (String.String (Coq_x75,
              (String.String (Coq_x61, (String.String (Coq_x74,
              (String.String (Coq_x69, (String.String (Coq_x6f,
              (String.String (Coq_x6e,
              String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
          | k1 :: ks0 ->
            bind (coq_MonadErrorT coq_MonadState) (cps_cvt0 e vn k1 tgm)
              (fun e' ->
              bind (coq_MonadErrorT coq_MonadState)
                (cps_cvt_exps es' vn e_app xs0 ks0 tgm) (fun e_exp ->
                ret (coq_MonadErrorT coq_MonadState) (Efun ((Fcons (k1,
                  kon_tag, (x1 :: []), e_exp, Fnil)), e'))))))
  and cps_cvt_efnlst fdefs vn nlst tgm =
    match fdefs with
    | Coq_eflnil -> ret (coq_MonadErrorT coq_MonadState) Fnil
    | Coq_eflcons (_, e1, fdefs') ->
      bind (coq_MonadErrorT coq_MonadState)
        (get_named_str (String.String (Coq_x78, String.EmptyString)))
        (fun x ->
        bind (coq_MonadErrorT coq_MonadState)
          (get_named_str (String.String (Coq_x6b, String.EmptyString)))
          (fun k' ->
          let curr_var = hd Coq_xH nlst in
          (match e1 with
           | Lam_e (_, e2) ->
             bind (coq_MonadErrorT coq_MonadState)
               (cps_cvt0 e2 (x :: vn) k' tgm) (fun ce ->
               bind (coq_MonadErrorT coq_MonadState)
                 (cps_cvt_efnlst fdefs' vn (tl nlst) tgm) (fun cfdefs ->
                 ret (coq_MonadErrorT coq_MonadState) (Fcons (curr_var,
                   func_tag, (k' :: (x :: [])), ce, cfdefs))))
           | _ ->
             failwith (String.String (Coq_x62, (String.String (Coq_x6f,
               (String.String (Coq_x64, (String.String (Coq_x79,
               (String.String (Coq_x20, (String.String (Coq_x6f,
               (String.String (Coq_x66, (String.String (Coq_x20,
               (String.String (Coq_x66, (String.String (Coq_x69,
               (String.String (Coq_x78, (String.String (Coq_x20,
               (String.String (Coq_x6d, (String.String (Coq_x75,
               (String.String (Coq_x73, (String.String (Coq_x74,
               (String.String (Coq_x20, (String.String (Coq_x62,
               (String.String (Coq_x65, (String.String (Coq_x20,
               (String.String (Coq_x61, (String.String (Coq_x20,
               (String.String (Coq_x6c, (String.String (Coq_x61,
               (String.String (Coq_x6d, (String.String (Coq_x62,
               (String.String (Coq_x64, (String.String (Coq_x61,
               (String.String (Coq_x20, (String.String (Coq_x65,
               (String.String (Coq_x78, (String.String (Coq_x70,
               (String.String (Coq_x72,
               String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  and cps_cvt_branches bl vn k r tgm =
    match bl with
    | Coq_brnil_e -> ret (coq_MonadErrorT coq_MonadState) []
    | Coq_brcons_e (dc, p, e, bl') ->
      let (i, lnames) = p in
      let tg = dcon_to_tag default_tag dc tgm in
      bind (coq_MonadErrorT coq_MonadState) (cps_cvt_branches bl' vn k r tgm)
        (fun cbl ->
        bind (coq_MonadErrorT coq_MonadState)
          (get_named_lst (names_lst_len lnames (N.to_nat i))) (fun vars ->
          let ctx_p = ctx_bind_proj tg r vars (length vars) in
          bind (coq_MonadErrorT coq_MonadState)
            (cps_cvt0 e (app vars vn) k tgm) (fun ce ->
            ret (coq_MonadErrorT coq_MonadState) ((tg,
              (app_ctx_f ctx_p ce)) :: cbl))))
  in cps_cvt0

(** val convert_whole_exp :
    (((kername * String.t) * bool) * nat) M.t -> positive -> positive ->
    positive -> exp -> conId_map -> Cps.exp cpsM **)

let convert_whole_exp prim_map func_tag kon_tag default_tag e dcm =
  bind (coq_MonadErrorT coq_MonadState)
    (get_named_str (String.String (Coq_x6b, String.EmptyString))) (fun k ->
    bind (coq_MonadErrorT coq_MonadState)
      (get_named_str (String.String (Coq_x66, String.EmptyString))) (fun f ->
      bind (coq_MonadErrorT coq_MonadState)
        (get_named_str (String.String (Coq_x78, String.EmptyString)))
        (fun x ->
        bind (coq_MonadErrorT coq_MonadState)
          (cps_cvt prim_map func_tag kon_tag default_tag e [] k dcm)
          (fun e' ->
          ret (coq_MonadErrorT coq_MonadState) (Efun ((Fcons (f, kon_tag,
            (k :: []), e', (Fcons (k, kon_tag, (x :: []), (Ehalt x),
            Fnil)))), (Eapp (f, kon_tag, (k :: [])))))))))

(** val convert_top :
    (((kername * String.t) * bool) * nat) M.t -> positive -> positive ->
    positive -> positive -> positive -> (ienv * exp) -> Cps.exp
    error * comp_data **)

let convert_top prim_map func_tag kon_tag default_tag default_itag next_id ee =
  let (p, dcm) = convert_env default_tag default_itag (fst ee) in
  let (p0, itag) = p in
  let (p1, ctag) = p0 in
  let (_, cenv) = p1 in
  let ftag = Pos.add func_tag Coq_xH in
  let fenv =
    M.set func_tag ((Npos (Coq_xO Coq_xH)), (N0 :: ((Npos Coq_xH) :: [])))
      (M.set kon_tag ((Npos Coq_xH), (N0 :: [])) M.empty)
  in
  let comp_d = pack_data next_id ctag itag ftag cenv fenv M.empty M.empty []
  in
  let (res_err, p2) =
    run_compM
      (convert_whole_exp prim_map func_tag kon_tag default_tag (snd ee) dcm)
      comp_d ()
  in
  let (comp_d', _) = p2 in (res_err, comp_d')

type 'a anfM = (unit, 'a) compM'

type var_map = var M.t * coq_N

(** val get_var_name : var_map -> coq_N -> var option **)

let get_var_name vmp x =
  let (vm, p) = vmp in
  (match N.sub p x with
   | N0 -> None
   | Npos pos -> M.get pos vm)

(** val add_var_name : var_map -> var -> var M.tree * coq_N **)

let add_var_name vmp x' =
  let (vm, p) = vmp in let p' = N.succ_pos p in ((M.set p' x' vm), (Npos p'))

(** val new_var_map : var M.t * coq_N **)

let new_var_map =
  (M.empty, N0)

type anf_value =
| Anf_Var of var
| Anf_App of var * var
| Constr of ctor_tag * var list
| Proj of ctor_tag * coq_N * var
| Fun of fun_tag * var * Cps.exp
| Prim_val of primitive
| Prim of positive * var list

type anf_term = anf_value * exp_ctx

(** val anf_term_to_exp : positive -> anf_term -> Cps.exp anfM **)

let anf_term_to_exp func_tag = function
| (v, c) ->
  (match v with
   | Anf_Var x -> ret (coq_MonadErrorT coq_MonadState) (app_ctx_f c (Ehalt x))
   | Anf_App (f, x) ->
     ret (coq_MonadErrorT coq_MonadState)
       (app_ctx_f c (Eapp (f, func_tag, (x :: []))))
   | Constr (c0, xs) ->
     bind (coq_MonadErrorT coq_MonadState)
       (get_named_str (String.String (Coq_x79, String.EmptyString)))
       (fun x' ->
       ret (coq_MonadErrorT coq_MonadState)
         (app_ctx_f c (Econstr (x', c0, xs, (Ehalt x')))))
   | Proj (c0, n, y) ->
     bind (coq_MonadErrorT coq_MonadState)
       (get_named_str (String.String (Coq_x79, String.EmptyString)))
       (fun x' ->
       ret (coq_MonadErrorT coq_MonadState)
         (app_ctx_f c (Eproj (x', c0, n, y, (Ehalt x')))))
   | Fun (ft, x, e) ->
     bind (coq_MonadErrorT coq_MonadState)
       (get_named_str (String.String (Coq_x79, String.EmptyString)))
       (fun x' ->
       ret (coq_MonadErrorT coq_MonadState)
         (app_ctx_f c (Efun ((Fcons (x', ft, (x :: []), e, Fnil)), (Ehalt
           x')))))
   | Prim_val p ->
     bind (coq_MonadErrorT coq_MonadState)
       (get_named_str (String.String (Coq_x79, String.EmptyString)))
       (fun x' ->
       ret (coq_MonadErrorT coq_MonadState)
         (app_ctx_f c (Eprim_val (x', p, (Ehalt x')))))
   | Prim (pr, xs) ->
     bind (coq_MonadErrorT coq_MonadState)
       (get_named_str (String.String (Coq_x79, String.EmptyString)))
       (fun x' ->
       ret (coq_MonadErrorT coq_MonadState)
         (app_ctx_f c (Eprim (x', pr, xs, (Ehalt x'))))))

(** val anf_term_to_ctx :
    positive -> anf_term -> name -> (var * exp_ctx) anfM **)

let anf_term_to_ctx func_tag t0 n =
  let (v, c) = t0 in
  (match v with
   | Anf_Var x -> ret (coq_MonadErrorT coq_MonadState) (x, c)
   | Anf_App (f, x) ->
     bind (coq_MonadErrorT coq_MonadState) (get_named n) (fun x' ->
       ret (coq_MonadErrorT coq_MonadState) (x',
         (comp_ctx_f c (Eletapp_c (x', f, func_tag, (x :: []), Hole_c)))))
   | Constr (c0, xs) ->
     bind (coq_MonadErrorT coq_MonadState) (get_named n) (fun x' ->
       ret (coq_MonadErrorT coq_MonadState) (x',
         (comp_ctx_f c (Econstr_c (x', c0, xs, Hole_c)))))
   | Proj (c0, i, y) ->
     bind (coq_MonadErrorT coq_MonadState) (get_named n) (fun x' ->
       ret (coq_MonadErrorT coq_MonadState) (x',
         (comp_ctx_f c (Eproj_c (x', c0, i, y, Hole_c)))))
   | Fun (ft, x, e) ->
     bind (coq_MonadErrorT coq_MonadState) (get_named n) (fun x' ->
       ret (coq_MonadErrorT coq_MonadState) (x',
         (comp_ctx_f c (Efun1_c ((Fcons (x', ft, (x :: []), e, Fnil)),
           Hole_c)))))
   | Prim_val p ->
     bind (coq_MonadErrorT coq_MonadState) (get_named n) (fun x' ->
       ret (coq_MonadErrorT coq_MonadState) (x',
         (comp_ctx_f c (Eprim_val_c (x', p, Hole_c)))))
   | Prim (pr, xs) ->
     bind (coq_MonadErrorT coq_MonadState) (get_named n) (fun x' ->
       ret (coq_MonadErrorT coq_MonadState) (x',
         (comp_ctx_f c (Eprim_c (x', pr, xs, Hole_c))))))

(** val def_name : name **)

let def_name =
  Coq_nNamed (String.String (Coq_x79, String.EmptyString))

(** val proj_ctx :
    name list -> coq_N -> var -> var_map -> ctor_tag -> (exp_ctx * var_map)
    anfM **)

let rec proj_ctx names i scrut vm ct =
  match names with
  | [] -> ret (coq_MonadErrorT coq_MonadState) (Hole_c, vm)
  | n :: ns ->
    bind (coq_MonadErrorT coq_MonadState) (get_named n) (fun x ->
      let vm' = add_var_name vm x in
      bind (coq_MonadErrorT coq_MonadState)
        (proj_ctx ns (N.add i (Npos Coq_xH)) scrut vm' ct) (fun cvm ->
        let (c, vm'') = cvm in
        ret (coq_MonadErrorT coq_MonadState) ((Eproj_c (x, ct, i, scrut, c)),
          vm'')))

(** val add_fix_names : efnlst -> var_map -> (var list * var_map) anfM **)

let rec add_fix_names b vm =
  match b with
  | Coq_eflnil -> ret (coq_MonadErrorT coq_MonadState) ([], vm)
  | Coq_eflcons (n, _, b') ->
    bind (coq_MonadErrorT coq_MonadState) (get_named n) (fun f_name ->
      bind (coq_MonadErrorT coq_MonadState)
        (add_fix_names b' (add_var_name vm f_name)) (fun lvm ->
        let (fs, vm') = lvm in
        ret (coq_MonadErrorT coq_MonadState) ((f_name :: fs), vm')))

(** val convert_prim_anf :
    positive -> nat -> positive -> var list -> anf_term anfM **)

let rec convert_prim_anf func_tag n prim args =
  match n with
  | O ->
    bind (coq_MonadErrorT coq_MonadState)
      (get_named_str (String.String (Coq_x70, (String.String (Coq_x72,
        (String.String (Coq_x69, (String.String (Coq_x6d,
        String.EmptyString))))))))) (fun x ->
      ret (coq_MonadErrorT coq_MonadState) ((Anf_Var x), (Eprim_c (x, prim,
        (rev args), Hole_c))))
  | S n0 ->
    bind (coq_MonadErrorT coq_MonadState)
      (get_named_str (String.String (Coq_x70, (String.String (Coq_x5f,
        (String.String (Coq_x61, (String.String (Coq_x72, (String.String
        (Coq_x67, String.EmptyString))))))))))) (fun arg ->
      bind (coq_MonadErrorT coq_MonadState)
        (get_named_str (String.String (Coq_x70, (String.String (Coq_x72,
          (String.String (Coq_x69, (String.String (Coq_x6d, (String.String
          (Coq_x5f, (String.String (Coq_x77, (String.String (Coq_x72,
          (String.String (Coq_x61, (String.String (Coq_x70, (String.String
          (Coq_x70, (String.String (Coq_x65, (String.String (Coq_x72,
          String.EmptyString))))))))))))))))))))))))) (fun f ->
        bind (coq_MonadErrorT coq_MonadState)
          (convert_prim_anf func_tag n0 prim (arg :: args)) (fun x ->
          let (anf_val, c) = x in
          (match anf_val with
           | Anf_Var x0 ->
             ret (coq_MonadErrorT coq_MonadState) ((Anf_Var f), (Efun1_c
               ((Fcons (f, func_tag, (arg :: []), (app_ctx_f c (Ehalt x0)),
               Fnil)), Hole_c)))
           | _ ->
             failwith (String.String (Coq_x49, (String.String (Coq_x6e,
               (String.String (Coq_x74, (String.String (Coq_x65,
               (String.String (Coq_x72, (String.String (Coq_x6e,
               (String.String (Coq_x61, (String.String (Coq_x6c,
               (String.String (Coq_x20, (String.String (Coq_x65,
               (String.String (Coq_x72, (String.String (Coq_x72,
               (String.String (Coq_x6f, (String.String (Coq_x72,
               (String.String (Coq_x3a, (String.String (Coq_x20,
               (String.String (Coq_x45, (String.String (Coq_x78,
               (String.String (Coq_x70, (String.String (Coq_x65,
               (String.String (Coq_x63, (String.String (Coq_x74,
               (String.String (Coq_x65, (String.String (Coq_x64,
               (String.String (Coq_x20, (String.String (Coq_x41,
               (String.String (Coq_x6e, (String.String (Coq_x66,
               (String.String (Coq_x5f, (String.String (Coq_x56,
               (String.String (Coq_x61, (String.String (Coq_x72,
               (String.String (Coq_x20, (String.String (Coq_x62,
               (String.String (Coq_x75, (String.String (Coq_x74,
               (String.String (Coq_x20, (String.String (Coq_x66,
               (String.String (Coq_x6f, (String.String (Coq_x75,
               (String.String (Coq_x6e, (String.String (Coq_x64,
               (String.String (Coq_x20, (String.String (Coq_x73,
               (String.String (Coq_x6f, (String.String (Coq_x6d,
               (String.String (Coq_x65, (String.String (Coq_x74,
               (String.String (Coq_x68, (String.String (Coq_x69,
               (String.String (Coq_x6e, (String.String (Coq_x67,
               (String.String (Coq_x20, (String.String (Coq_x65,
               (String.String (Coq_x6c, (String.String (Coq_x73,
               (String.String (Coq_x65, (String.String (Coq_x2e,
               String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val convert_anf :
    (((kername * String.t) * bool) * nat) M.t -> positive -> positive ->
    conId_map -> exp -> var_map -> anf_term anfM **)

let convert_anf prim_map func_tag default_tag tgm =
  let rec convert_anf0 e vm =
    match e with
    | Var_e x ->
      (match get_var_name vm x with
       | Some v -> ret (coq_MonadErrorT coq_MonadState) ((Anf_Var v), Hole_c)
       | None ->
         failwith (String.String (Coq_x55, (String.String (Coq_x6e,
           (String.String (Coq_x6b, (String.String (Coq_x6e, (String.String
           (Coq_x6f, (String.String (Coq_x77, (String.String (Coq_x6e,
           (String.String (Coq_x20, (String.String (Coq_x44, (String.String
           (Coq_x65, (String.String (Coq_x42, (String.String (Coq_x72,
           (String.String (Coq_x75, (String.String (Coq_x69, (String.String
           (Coq_x6a, (String.String (Coq_x6e, (String.String (Coq_x20,
           (String.String (Coq_x69, (String.String (Coq_x6e, (String.String
           (Coq_x64, (String.String (Coq_x65, (String.String (Coq_x78,
           String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))
    | Lam_e (n, e1) ->
      bind (coq_MonadErrorT coq_MonadState) (get_named n) (fun x ->
        bind (coq_MonadErrorT coq_MonadState)
          (convert_anf0 e1 (add_var_name vm x)) (fun a1 ->
          bind (coq_MonadErrorT coq_MonadState) (anf_term_to_exp func_tag a1)
            (fun e_body ->
            ret (coq_MonadErrorT coq_MonadState) ((Fun (func_tag, x,
              e_body)), Hole_c))))
    | App_e (e1, e2) ->
      bind (coq_MonadErrorT coq_MonadState) (convert_anf0 e1 vm) (fun a1 ->
        bind (coq_MonadErrorT coq_MonadState) (convert_anf0 e2 vm) (fun a2 ->
          bind (coq_MonadErrorT coq_MonadState)
            (anf_term_to_ctx func_tag a1 def_name) (fun xc1 ->
            bind (coq_MonadErrorT coq_MonadState)
              (anf_term_to_ctx func_tag a2 def_name) (fun xc2 ->
              let (x1, c1) = xc1 in
              let (x2, c2) = xc2 in
              ret (coq_MonadErrorT coq_MonadState) ((Anf_App (x1, x2)),
                (comp_ctx_f c1 c2))))))
    | Con_e (dci, es) ->
      let c_tag = dcon_to_tag default_tag dci tgm in
      bind (coq_MonadErrorT coq_MonadState) (convert_anf_exps es vm)
        (fun ts ->
        let (ys, c) = ts in
        ret (coq_MonadErrorT coq_MonadState) ((Constr (c_tag, ys)), c))
    | Match_e (e1, _, bl) ->
      bind (coq_MonadErrorT coq_MonadState) (convert_anf0 e1 vm) (fun a ->
        bind (coq_MonadErrorT coq_MonadState)
          (get_named_str (String.String (Coq_x66, (String.String (Coq_x5f,
            (String.String (Coq_x63, (String.String (Coq_x61, (String.String
            (Coq_x73, (String.String (Coq_x65, String.EmptyString)))))))))))))
          (fun f ->
          bind (coq_MonadErrorT coq_MonadState)
            (get_named_str (String.String (Coq_x73, String.EmptyString)))
            (fun y ->
            bind (coq_MonadErrorT coq_MonadState)
              (convert_anf_branches bl y vm) (fun pats ->
              bind (coq_MonadErrorT coq_MonadState)
                (anf_term_to_ctx func_tag a def_name) (fun xc ->
                let (x, c) = xc in
                let c_fun = Efun1_c ((Fcons (f, func_tag, (y :: []), (Ecase
                  (y, pats)), Fnil)), c)
                in
                ret (coq_MonadErrorT coq_MonadState) ((Anf_App (f, x)), c_fun))))))
    | Let_e (n, e1, e2) ->
      bind (coq_MonadErrorT coq_MonadState) (convert_anf0 e1 vm) (fun a1 ->
        bind (coq_MonadErrorT coq_MonadState) (anf_term_to_ctx func_tag a1 n)
          (fun vC ->
          let (x, c1) = vC in
          bind (coq_MonadErrorT coq_MonadState)
            (convert_anf0 e2 (add_var_name vm x)) (fun a2 ->
            let (v, c2) = a2 in
            ret (coq_MonadErrorT coq_MonadState) (v, (comp_ctx_f c1 c2)))))
    | Fix_e (fnlst, i) ->
      bind (coq_MonadErrorT coq_MonadState) (add_fix_names fnlst vm)
        (fun lvm ->
        let (names, vm') = lvm in
        bind (coq_MonadErrorT coq_MonadState)
          (convert_anf_efnlst fnlst names i vm') (fun ds ->
          let (x, defs) = ds in
          ret (coq_MonadErrorT coq_MonadState) ((Anf_Var x), (Efun1_c (defs,
            Hole_c)))))
    | Prf_e ->
      ret (coq_MonadErrorT coq_MonadState) ((Constr (default_tag, [])),
        Hole_c)
    | Prim_val_e p ->
      ret (coq_MonadErrorT coq_MonadState) ((Prim_val p), Hole_c)
    | Prim_e p ->
      (match M.get p prim_map with
       | Some p0 -> let (_, ar) = p0 in convert_prim_anf func_tag ar p []
       | None ->
         failwith (String.String (Coq_x49, (String.String (Coq_x6e,
           (String.String (Coq_x74, (String.String (Coq_x65, (String.String
           (Coq_x72, (String.String (Coq_x6e, (String.String (Coq_x61,
           (String.String (Coq_x6c, (String.String (Coq_x20, (String.String
           (Coq_x65, (String.String (Coq_x72, (String.String (Coq_x72,
           (String.String (Coq_x6f, (String.String (Coq_x72, (String.String
           (Coq_x3a, (String.String (Coq_x20, (String.String (Coq_x69,
           (String.String (Coq_x64, (String.String (Coq_x65, (String.String
           (Coq_x6e, (String.String (Coq_x74, (String.String (Coq_x69,
           (String.String (Coq_x66, (String.String (Coq_x69, (String.String
           (Coq_x65, (String.String (Coq_x72, (String.String (Coq_x20,
           (String.String (Coq_x66, (String.String (Coq_x6f, (String.String
           (Coq_x72, (String.String (Coq_x20, (String.String (Coq_x70,
           (String.String (Coq_x72, (String.String (Coq_x69, (String.String
           (Coq_x6d, (String.String (Coq_x69, (String.String (Coq_x74,
           (String.String (Coq_x69, (String.String (Coq_x76, (String.String
           (Coq_x65, (String.String (Coq_x20, (String.String (Coq_x6e,
           (String.String (Coq_x6f, (String.String (Coq_x74, (String.String
           (Coq_x20, (String.String (Coq_x66, (String.String (Coq_x6f,
           (String.String (Coq_x75, (String.String (Coq_x6e, (String.String
           (Coq_x64,
           String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  and convert_anf_exps es vm =
    match es with
    | Coq_enil -> ret (coq_MonadErrorT coq_MonadState) ([], Hole_c)
    | Coq_econs (e, es') ->
      bind (coq_MonadErrorT coq_MonadState) (convert_anf_exps es' vm)
        (fun ts ->
        bind (coq_MonadErrorT coq_MonadState) (convert_anf0 e vm) (fun a ->
          bind (coq_MonadErrorT coq_MonadState)
            (anf_term_to_ctx func_tag a def_name) (fun t0 ->
            let (y, c1) = t0 in
            let (ys, c2) = ts in
            ret (coq_MonadErrorT coq_MonadState) ((y :: ys),
              (comp_ctx_f c1 c2)))))
  and convert_anf_efnlst fdefs names i vm =
    match fdefs with
    | Coq_eflnil ->
      (match names with
       | [] -> ret (coq_MonadErrorT coq_MonadState) (Coq_xH, Fnil)
       | _ :: _ ->
         failwith (String.String (Coq_x57, (String.String (Coq_x72,
           (String.String (Coq_x6f, (String.String (Coq_x6e, (String.String
           (Coq_x67, (String.String (Coq_x20, (String.String (Coq_x6e,
           (String.String (Coq_x75, (String.String (Coq_x6d, (String.String
           (Coq_x62, (String.String (Coq_x65, (String.String (Coq_x72,
           (String.String (Coq_x20, (String.String (Coq_x6f, (String.String
           (Coq_x66, (String.String (Coq_x20, (String.String (Coq_x6e,
           (String.String (Coq_x61, (String.String (Coq_x6d, (String.String
           (Coq_x65, (String.String (Coq_x73, (String.String (Coq_x20,
           (String.String (Coq_x66, (String.String (Coq_x6f, (String.String
           (Coq_x72, (String.String (Coq_x20, (String.String (Coq_x6d,
           (String.String (Coq_x75, (String.String (Coq_x74, (String.String
           (Coq_x2e, (String.String (Coq_x20, (String.String (Coq_x72,
           (String.String (Coq_x65, (String.String (Coq_x63, (String.String
           (Coq_x2e, (String.String (Coq_x20, (String.String (Coq_x66,
           (String.String (Coq_x75, (String.String (Coq_x6e, (String.String
           (Coq_x63, (String.String (Coq_x74, (String.String (Coq_x69,
           (String.String (Coq_x6f, (String.String (Coq_x6e, (String.String
           (Coq_x73,
           String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    | Coq_eflcons (n, e, fdefs') ->
      (match names with
       | [] ->
         failwith (String.String (Coq_x57, (String.String (Coq_x72,
           (String.String (Coq_x6f, (String.String (Coq_x6e, (String.String
           (Coq_x67, (String.String (Coq_x20, (String.String (Coq_x6e,
           (String.String (Coq_x75, (String.String (Coq_x6d, (String.String
           (Coq_x62, (String.String (Coq_x65, (String.String (Coq_x72,
           (String.String (Coq_x20, (String.String (Coq_x6f, (String.String
           (Coq_x66, (String.String (Coq_x20, (String.String (Coq_x6e,
           (String.String (Coq_x61, (String.String (Coq_x6d, (String.String
           (Coq_x65, (String.String (Coq_x73, (String.String (Coq_x20,
           (String.String (Coq_x66, (String.String (Coq_x6f, (String.String
           (Coq_x72, (String.String (Coq_x20, (String.String (Coq_x6d,
           (String.String (Coq_x75, (String.String (Coq_x74, (String.String
           (Coq_x2e, (String.String (Coq_x20, (String.String (Coq_x72,
           (String.String (Coq_x65, (String.String (Coq_x63, (String.String
           (Coq_x2e, (String.String (Coq_x20, (String.String (Coq_x66,
           (String.String (Coq_x75, (String.String (Coq_x6e, (String.String
           (Coq_x63, (String.String (Coq_x74, (String.String (Coq_x69,
           (String.String (Coq_x6f, (String.String (Coq_x6e, (String.String
           (Coq_x73,
           String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
       | f_name :: names0 ->
         bind (coq_MonadErrorT coq_MonadState) (convert_anf0 e vm) (fun a1 ->
           let (a, e0) = a1 in
           (match a with
            | Fun (_, arg, e_body) ->
              (match e0 with
               | Hole_c ->
                 bind (coq_MonadErrorT coq_MonadState)
                   (convert_anf_efnlst fdefs' names0 (N.sub i (Npos Coq_xH))
                     vm) (fun ds ->
                   let (fi, defs') = ds in
                   let fi' = match i with
                             | N0 -> f_name
                             | Npos _ -> fi in
                   ret (coq_MonadErrorT coq_MonadState) (fi', (Fcons (f_name,
                     func_tag, (arg :: []), e_body, defs'))))
               | _ ->
                 failwith
                   (String.append (String.String (Coq_x55, (String.String
                     (Coq_x6e, (String.String (Coq_x65, (String.String
                     (Coq_x78, (String.String (Coq_x70, (String.String
                     (Coq_x65, (String.String (Coq_x63, (String.String
                     (Coq_x74, (String.String (Coq_x65, (String.String
                     (Coq_x64, (String.String (Coq_x20, (String.String
                     (Coq_x62, (String.String (Coq_x6f, (String.String
                     (Coq_x64, (String.String (Coq_x79, (String.String
                     (Coq_x20, (String.String (Coq_x6f, (String.String
                     (Coq_x66, (String.String (Coq_x20, (String.String
                     (Coq_x66, (String.String (Coq_x69, (String.String
                     (Coq_x78, (String.String (Coq_x20, (String.String
                     (Coq_x69, (String.String (Coq_x6e, (String.String
                     (Coq_x20, (String.String (Coq_x66, (String.String
                     (Coq_x75, (String.String (Coq_x6e, (String.String
                     (Coq_x63, (String.String (Coq_x74, (String.String
                     (Coq_x69, (String.String (Coq_x6f, (String.String
                     (Coq_x6e, (String.String (Coq_x20,
                     String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                     (print_name n)))
            | _ ->
              failwith
                (String.append (String.String (Coq_x55, (String.String
                  (Coq_x6e, (String.String (Coq_x65, (String.String (Coq_x78,
                  (String.String (Coq_x70, (String.String (Coq_x65,
                  (String.String (Coq_x63, (String.String (Coq_x74,
                  (String.String (Coq_x65, (String.String (Coq_x64,
                  (String.String (Coq_x20, (String.String (Coq_x62,
                  (String.String (Coq_x6f, (String.String (Coq_x64,
                  (String.String (Coq_x79, (String.String (Coq_x20,
                  (String.String (Coq_x6f, (String.String (Coq_x66,
                  (String.String (Coq_x20, (String.String (Coq_x66,
                  (String.String (Coq_x69, (String.String (Coq_x78,
                  (String.String (Coq_x20, (String.String (Coq_x69,
                  (String.String (Coq_x6e, (String.String (Coq_x20,
                  (String.String (Coq_x66, (String.String (Coq_x75,
                  (String.String (Coq_x6e, (String.String (Coq_x63,
                  (String.String (Coq_x74, (String.String (Coq_x69,
                  (String.String (Coq_x6f, (String.String (Coq_x6e,
                  (String.String (Coq_x20,
                  String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                  (print_name n)))))
  and convert_anf_branches bl scrut vm =
    match bl with
    | Coq_brnil_e -> ret (coq_MonadErrorT coq_MonadState) []
    | Coq_brcons_e (dc, p, e, bl') ->
      let (_, lnames) = p in
      let ctag = dcon_to_tag default_tag dc tgm in
      bind (coq_MonadErrorT coq_MonadState)
        (proj_ctx lnames N0 scrut vm ctag) (fun cm ->
        let (cproj, vm') = cm in
        bind (coq_MonadErrorT coq_MonadState) (convert_anf0 e vm') (fun a ->
          bind (coq_MonadErrorT coq_MonadState) (anf_term_to_exp func_tag a)
            (fun e' ->
            bind (coq_MonadErrorT coq_MonadState)
              (convert_anf_branches bl' scrut vm) (fun pats' ->
              ret (coq_MonadErrorT coq_MonadState) ((ctag,
                (app_ctx_f cproj e')) :: pats')))))
  in convert_anf0

(** val convert_anf_exp :
    (((kername * String.t) * bool) * nat) M.t -> positive -> positive ->
    conId_map -> exp -> Cps.exp anfM **)

let convert_anf_exp prim_map func_tag default_tag dcm e =
  bind (coq_MonadErrorT coq_MonadState)
    (convert_anf prim_map func_tag default_tag dcm e new_var_map) (fun a ->
    anf_term_to_exp func_tag a)

(** val convert_top_anf :
    (((kername * String.t) * bool) * nat) M.t -> positive -> positive ->
    positive -> positive -> (ienv * exp) -> Cps.exp error * comp_data **)

let convert_top_anf prim_map func_tag default_tag default_itag next_id ee =
  let (p, dcm) = convert_env default_tag default_itag (fst ee) in
  let (p0, itag) = p in
  let (p1, ctag) = p0 in
  let (_, cenv) = p1 in
  let ftag = Pos.add func_tag Coq_xH in
  let fenv = M.set func_tag ((Npos Coq_xH), (N0 :: [])) M.empty in
  let comp_d = pack_data next_id ctag itag ftag cenv fenv M.empty M.empty []
  in
  let (res_err, p2) =
    run_compM (convert_anf_exp prim_map func_tag default_tag dcm (snd ee))
      comp_d ()
  in
  let (comp_d', _) = p2 in (res_err, comp_d')
