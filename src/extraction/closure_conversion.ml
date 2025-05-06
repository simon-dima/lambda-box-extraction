open BinNat
open BinNums
open BinPos
open Byte
open Datatypes
open List0
open Maps
open Monad0
open Bytestring
open CompM
open Cps
open Identifiers
open Set_util
open State

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

type coq_VarInfo =
| FVar of coq_N
| MRFun of var
| BoundVar

type coq_VarInfoMap = coq_VarInfo M.t

type coq_GFunMap = __ M.t

type 'a ccstate = (unit, 'a) compM'

(** val clo_env_suffix : String.t **)

let clo_env_suffix =
  String.String (Coq_x5f, (String.String (Coq_x65, (String.String (Coq_x6e,
    (String.String (Coq_x76, String.EmptyString)))))))

(** val clo_suffix : String.t **)

let clo_suffix =
  String.String (Coq_x5f, (String.String (Coq_x63, (String.String (Coq_x6c,
    (String.String (Coq_x6f, String.EmptyString)))))))

(** val code_suffix : String.t **)

let code_suffix =
  String.String (Coq_x5f, (String.String (Coq_x63, (String.String (Coq_x6f,
    (String.String (Coq_x64, (String.String (Coq_x65,
    String.EmptyString)))))))))

(** val proj_suffix : String.t **)

let proj_suffix =
  String.String (Coq_x5f, (String.String (Coq_x70, (String.String (Coq_x72,
    (String.String (Coq_x6f, (String.String (Coq_x6a,
    String.EmptyString)))))))))

(** val get_var :
    ctor_tag -> var -> coq_VarInfoMap -> coq_GFunMap -> ctor_tag -> var ->
    (var * (exp -> exp)) ccstate **)

let get_var clo_tag x map gfuns c _UU0393_ =
  match PTree.get x map with
  | Some entry ->
    (match entry with
     | FVar pos ->
       bind (coq_MonadErrorT coq_MonadState) (get_name x proj_suffix)
         (fun y ->
         ret (coq_MonadErrorT coq_MonadState) (y, (fun e -> Eproj (y, c, pos,
           _UU0393_, e))))
     | MRFun env ->
       bind (coq_MonadErrorT coq_MonadState) (get_name x clo_suffix)
         (fun y ->
         ret (coq_MonadErrorT coq_MonadState) (y, (fun e -> Econstr (y,
           clo_tag, (x :: (env :: [])), e))))
     | BoundVar -> ret (coq_MonadErrorT coq_MonadState) (x, id))
  | None ->
    (match M.get x gfuns with
     | Some _ ->
       bind (coq_MonadErrorT coq_MonadState) (make_record_ctor_tag N0)
         (fun c_env ->
         bind (coq_MonadErrorT coq_MonadState) (get_name x clo_suffix)
           (fun y ->
           bind (coq_MonadErrorT coq_MonadState)
             (get_name x (String.String (Coq_x62, (String.String (Coq_x6f,
               (String.String (Coq_x67, (String.String (Coq_x75,
               (String.String (Coq_x73, (String.String (Coq_x5f,
               (String.String (Coq_x65, (String.String (Coq_x6e,
               (String.String (Coq_x76, String.EmptyString)))))))))))))))))))
             (fun g_env ->
             ret (coq_MonadErrorT coq_MonadState) (y, (fun e -> Econstr
               (g_env, c_env, [], (Econstr (y, clo_tag, (x :: (g_env :: [])),
               e))))))))
     | None -> ret (coq_MonadErrorT coq_MonadState) (x, id))

(** val get_vars :
    ctor_tag -> var list -> coq_VarInfoMap -> coq_GFunMap -> ctor_tag -> var
    -> (var list * (exp -> exp)) ccstate **)

let rec get_vars clo_tag xs map gfuns c _UU0393_ =
  match xs with
  | [] -> ret (coq_MonadErrorT coq_MonadState) ([], id)
  | x :: xs0 ->
    bind (coq_MonadErrorT coq_MonadState)
      (get_var clo_tag x map gfuns c _UU0393_) (fun t1 ->
      let (y, f) = t1 in
      bind (coq_MonadErrorT coq_MonadState)
        (get_vars clo_tag xs0 map gfuns c _UU0393_) (fun t2 ->
        let (ys, f') = t2 in
        ret (coq_MonadErrorT coq_MonadState) ((y :: ys), (fun e -> f (f' e)))))

(** val add_params : positive list -> coq_VarInfoMap -> coq_VarInfoMap **)

let rec add_params args mapfv =
  match args with
  | [] -> mapfv
  | x :: xs -> M.set x BoundVar (add_params xs mapfv)

(** val make_env :
    ctor_tag -> var list -> coq_VarInfoMap -> coq_VarInfoMap -> ctor_tag ->
    var -> var -> coq_GFunMap -> ((ctor_tag * coq_VarInfoMap) * (exp -> exp))
    ccstate **)

let make_env clo_tag fvs _ mapfv_old c_old _UU0393__new _UU0393__old gfuns =
  let (map_new', n) =
    let rec add_fvs l n map =
      match l with
      | [] -> (map, n)
      | x :: xs ->
        add_fvs xs (N.add n (Npos Coq_xH)) (PTree.set x (FVar n) map)
    in add_fvs fvs N0 PTree.empty
  in
  bind (coq_MonadErrorT coq_MonadState)
    (get_vars clo_tag fvs mapfv_old gfuns c_old _UU0393__old) (fun t1 ->
    let (fv', g') = t1 in
    bind (coq_MonadErrorT coq_MonadState) (make_record_ctor_tag n)
      (fun c_new ->
      ret (coq_MonadErrorT coq_MonadState) ((c_new, map_new'), (fun e ->
        g' (Econstr (_UU0393__new, c_new, fv', e))))))

(** val add_closures : fundefs -> coq_VarInfoMap -> var -> coq_VarInfoMap **)

let rec add_closures defs mapfv env =
  match defs with
  | Fcons (f, _, _, _, defs') ->
    let mapfv' = add_closures defs' mapfv env in
    PTree.set f (MRFun env) mapfv'
  | Fnil -> mapfv

(** val add_closures_gfuns : fundefs -> coq_GFunMap -> bool -> coq_GFunMap **)

let rec add_closures_gfuns defs gfuns is_closed =
  match defs with
  | Fcons (f, _, _, _, defs') ->
    let gfuns' = add_closures_gfuns defs' gfuns is_closed in
    if is_closed then M.set f __ gfuns' else gfuns
  | Fnil -> gfuns

(** val exp_closure_conv :
    ctor_tag -> exp -> coq_VarInfoMap -> coq_GFunMap -> ctor_tag -> var ->
    (exp * (exp -> exp)) ccstate **)

let exp_closure_conv clo_tag =
  let rec exp_closure_conv0 e mapfv gfuns c _UU0393_ =
    match e with
    | Econstr (x, tag, ys, e') ->
      bind (coq_MonadErrorT coq_MonadState)
        (get_vars clo_tag ys mapfv gfuns c _UU0393_) (fun t1 ->
        let (ys', f) = t1 in
        bind (coq_MonadErrorT coq_MonadState)
          (exp_closure_conv0 e' (PTree.set x BoundVar mapfv) gfuns c _UU0393_)
          (fun ef ->
          ret (coq_MonadErrorT coq_MonadState) ((Econstr (x, tag, ys',
            (snd ef (fst ef)))), f)))
    | Ecase (x, pats) ->
      bind (coq_MonadErrorT coq_MonadState)
        (let rec mapM_cc = function
         | [] -> ret (coq_MonadErrorT coq_MonadState) []
         | y0 :: xs ->
           let (y, e0) = y0 in
           bind (coq_MonadErrorT coq_MonadState)
             (exp_closure_conv0 e0 mapfv gfuns c _UU0393_) (fun ef ->
             bind (coq_MonadErrorT coq_MonadState) (mapM_cc xs) (fun xs' ->
               ret (coq_MonadErrorT coq_MonadState) ((y,
                 (snd ef (fst ef))) :: xs')))
         in mapM_cc pats) (fun pats' ->
        bind (coq_MonadErrorT coq_MonadState)
          (get_var clo_tag x mapfv gfuns c _UU0393_) (fun t1 ->
          let (x', f1) = t1 in
          ret (coq_MonadErrorT coq_MonadState) ((Ecase (x', pats')), f1)))
    | Eproj (x, tag, n, y, e') ->
      bind (coq_MonadErrorT coq_MonadState)
        (get_var clo_tag y mapfv gfuns c _UU0393_) (fun t1 ->
        let (y', f) = t1 in
        bind (coq_MonadErrorT coq_MonadState)
          (exp_closure_conv0 e' (PTree.set x BoundVar mapfv) gfuns c _UU0393_)
          (fun ef ->
          ret (coq_MonadErrorT coq_MonadState) ((Eproj (x, tag, n, y',
            (snd ef (fst ef)))), f)))
    | Eletapp (x, f, ft, xs, e') ->
      bind (coq_MonadErrorT coq_MonadState)
        (get_var clo_tag f mapfv gfuns c _UU0393_) (fun t1 ->
        let (f', g1) = t1 in
        bind (coq_MonadErrorT coq_MonadState)
          (get_vars clo_tag xs mapfv gfuns c _UU0393_) (fun t2 ->
          let (xs', g2) = t2 in
          bind (coq_MonadErrorT coq_MonadState) (get_name f code_suffix)
            (fun ptr ->
            bind (coq_MonadErrorT coq_MonadState) (get_name f clo_env_suffix)
              (fun _UU0393_' ->
              bind (coq_MonadErrorT coq_MonadState)
                (exp_closure_conv0 e' (PTree.set x BoundVar mapfv) gfuns c
                  _UU0393_) (fun ef ->
                ret (coq_MonadErrorT coq_MonadState) ((Eproj (ptr, clo_tag,
                  N0, f', (Eproj (_UU0393_', clo_tag, (Npos Coq_xH), f',
                  (Eletapp (x, ptr, ft, (_UU0393_' :: xs'),
                  (snd ef (fst ef)))))))), (fun e0 -> g1 (g2 e0))))))))
    | Efun (defs, e0) ->
      let fv = fundefs_fv defs in
      let fvs =
        filter (fun x ->
          match M.get x gfuns with
          | Some _ -> false
          | None -> true) (PS.elements fv)
      in
      bind (coq_MonadErrorT coq_MonadState)
        (get_named_str (String.String (Coq_x65, (String.String (Coq_x6e,
          (String.String (Coq_x76, String.EmptyString)))))))
        (fun _UU0393_' ->
        bind (coq_MonadErrorT coq_MonadState)
          (make_env clo_tag fvs PTree.empty mapfv c _UU0393_' _UU0393_ gfuns)
          (fun t1 ->
          let (p, g1) = t1 in
          let (c', mapfv_new) = p in
          let is_closed = match fvs with
                          | [] -> true
                          | _ :: _ -> false in
          let mapfv' = add_closures defs mapfv _UU0393_' in
          let gfuns' = add_closures_gfuns defs gfuns is_closed in
          bind (coq_MonadErrorT coq_MonadState)
            (fundefs_closure_conv defs defs mapfv_new gfuns' c')
            (fun defs' ->
            bind (coq_MonadErrorT coq_MonadState)
              (exp_closure_conv0 e0 mapfv' gfuns' c _UU0393_) (fun ef ->
              ret (coq_MonadErrorT coq_MonadState) ((Efun (defs',
                (snd ef (fst ef)))), g1)))))
    | Eapp (f, ft, xs) ->
      bind (coq_MonadErrorT coq_MonadState)
        (get_var clo_tag f mapfv gfuns c _UU0393_) (fun t1 ->
        let (f', g1) = t1 in
        bind (coq_MonadErrorT coq_MonadState)
          (get_vars clo_tag xs mapfv gfuns c _UU0393_) (fun t2 ->
          let (xs', g2) = t2 in
          bind (coq_MonadErrorT coq_MonadState) (get_name f code_suffix)
            (fun ptr ->
            bind (coq_MonadErrorT coq_MonadState) (get_name f clo_env_suffix)
              (fun _UU0393_0 ->
              ret (coq_MonadErrorT coq_MonadState) ((Eproj (ptr, clo_tag, N0,
                f', (Eproj (_UU0393_0, clo_tag, (Npos Coq_xH), f', (Eapp
                (ptr, ft, (_UU0393_0 :: xs'))))))), (fun e0 -> g1 (g2 e0)))))))
    | Eprim_val (x, prim, e') ->
      bind (coq_MonadErrorT coq_MonadState)
        (exp_closure_conv0 e' (PTree.set x BoundVar mapfv) gfuns c _UU0393_)
        (fun ef ->
        ret (coq_MonadErrorT coq_MonadState) ((Eprim_val (x, prim,
          (snd ef (fst ef)))), id))
    | Eprim (x, prim, ys, e') ->
      bind (coq_MonadErrorT coq_MonadState)
        (get_vars clo_tag ys mapfv gfuns c _UU0393_) (fun t1 ->
        let (ys', f) = t1 in
        bind (coq_MonadErrorT coq_MonadState)
          (exp_closure_conv0 e' (PTree.set x BoundVar mapfv) gfuns c _UU0393_)
          (fun ef ->
          ret (coq_MonadErrorT coq_MonadState) ((Eprim (x, prim, ys',
            (snd ef (fst ef)))), f)))
    | Ehalt x ->
      bind (coq_MonadErrorT coq_MonadState)
        (get_var clo_tag x mapfv gfuns c _UU0393_) (fun t1 ->
        let (x', f) = t1 in
        ret (coq_MonadErrorT coq_MonadState) ((Ehalt x'), f))
  and fundefs_closure_conv all_defs defs mapfv gfuns c =
    match defs with
    | Fcons (f, tag, ys, e, defs') ->
      bind (coq_MonadErrorT coq_MonadState)
        (get_named_str (String.String (Coq_x65, (String.String (Coq_x6e,
          (String.String (Coq_x76, String.EmptyString))))))) (fun _UU0393_ ->
        let mapfv' = add_closures all_defs mapfv _UU0393_ in
        let mapfv'0 = add_params ys mapfv' in
        bind (coq_MonadErrorT coq_MonadState)
          (exp_closure_conv0 e mapfv'0 gfuns c _UU0393_) (fun ef ->
          bind (coq_MonadErrorT coq_MonadState)
            (fundefs_closure_conv all_defs defs' mapfv gfuns c)
            (fun defs'' ->
            ret (coq_MonadErrorT coq_MonadState) (Fcons (f, tag,
              (_UU0393_ :: ys), (snd ef (fst ef)), defs'')))))
    | Fnil -> ret (coq_MonadErrorT coq_MonadState) Fnil
  in exp_closure_conv0

(** val populate_map : coq_FVSet -> coq_VarInfoMap -> coq_VarInfoMap **)

let populate_map s map =
  PS.fold (fun x map0 -> M.set x BoundVar map0) s map

(** val get_name : comp_data -> var * comp_data **)

let get_name c =
  let { next_var = n; nect_ctor_tag = c0; next_ind_tag = i; next_fun_tag = f;
    cenv = e; fenv = fenv0; nenv = names; inline_map = imap; log = log0 } = c
  in
  let c' = { next_var = (Pos.add n Coq_xH); nect_ctor_tag = c0;
    next_ind_tag = i; next_fun_tag = f; cenv = e; fenv = fenv0; nenv = names;
    inline_map = imap; log = log0 }
  in
  (n, c')

(** val closure_conversion_top :
    ctor_tag -> ind_tag -> exp -> comp_data -> exp error * comp_data **)

let closure_conversion_top clo_tag clo_itag e c =
  let (_UU0393_, c0) = get_name c in
  let map = populate_map (exp_fv e) PTree.empty in
  let (ef'_err, p) =
    run_compM (exp_closure_conv clo_tag e map PTree.empty Coq_xH _UU0393_) c0
      ()
  in
  let (c', _) = p in
  (match ef'_err with
   | Err str -> ((Err str), c')
   | Ret p0 ->
     let (e', f') = p0 in
     let cenv' = add_closure_tag clo_tag clo_itag c'.cenv in
     let c'' = put_ctor_env cenv' c' in ((Ret (f' e')), c''))
