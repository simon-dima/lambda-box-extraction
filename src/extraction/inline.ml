open BinNums
open BinPos
open BinPosDef
open Datatypes
open List0
open Monad0
open Nat0
open PeanoNat
open Bytestring
open CompM
open Cps
open Cps_show
open Ctx
open Identifiers
open Inline_letapp
open Map_util
open Rename
open Set_util
open Shrink_cps
open State

type r_map = var Cps.M.t

type 'st coq_InlineHeuristic = { update_funDef : (fundefs -> r_map -> 'st ->
                                                 'st * 'st);
                                 update_inFun : (var -> fun_tag -> var list
                                                -> exp -> r_map -> 'st -> 'st);
                                 update_App : (var -> fun_tag -> var list ->
                                              'st -> 'st * bool);
                                 update_letApp : (var -> fun_tag -> var list
                                                 -> 'st -> ('st * 'st) * bool) }

type fun_map = ((fun_tag * var list) * exp) Cps.M.t

type inline_state = bool * name_env

type 'x inlineM = (inline_state, 'x) compM'

(** val click : unit inlineM **)

let click =
  bind (coq_MonadErrorT coq_MonadState) (get_state ()) (fun x ->
    let (_, s) = x in put_state (true, s))

(** val get_fresh_name : var -> var inlineM **)

let get_fresh_name x =
  bind (coq_MonadErrorT coq_MonadState) (get_state ()) (fun x0 ->
    let (_, nenv_old) = x0 in get_name' x String.EmptyString nenv_old)

(** val get_fresh_names : var list -> var list inlineM **)

let get_fresh_names xs =
  bind (coq_MonadErrorT coq_MonadState) (get_state ()) (fun x ->
    let (_, nenv_old) = x in get_names_lst' xs String.EmptyString nenv_old)

(** val add_fundefs : fundefs -> fun_map -> fun_map **)

let rec add_fundefs fds fm =
  match fds with
  | Fcons (f, t0, xs, e, fds0) ->
    Cps.M.set f ((t0, xs), e) (add_fundefs fds0 fm)
  | Fnil -> fm

(** val split_fuel : nat -> nat * nat **)

let split_fuel d =
  let d2 = Nat.div2 d in (d2, (add d2 (Nat.b2n (Nat.odd d))))

(** val inline_exp :
    'a1 coq_InlineHeuristic -> nat -> nat -> exp -> var M.t ->
    ((fun_tag * var list) * exp) Cps.M.tree -> 'a1 -> exp inlineM **)

let rec inline_exp iH d j =
  let rec inline_exp_aux e sig0 fm s =
    match e with
    | Econstr (x, t0, ys, e0) ->
      let ys' = apply_r_list sig0 ys in
      bind (coq_MonadErrorT coq_MonadState) (get_fresh_name x) (fun x' ->
        bind (coq_MonadErrorT coq_MonadState)
          (inline_exp_aux e0 (Cps.M.set x x' sig0) fm s) (fun e' ->
          ret (coq_MonadErrorT coq_MonadState) (Econstr (x', t0, ys', e'))))
    | Ecase (v, cl) ->
      let v' = apply_r sig0 v in
      bind (coq_MonadErrorT coq_MonadState)
        (let rec beta_list = function
         | [] -> ret (coq_MonadErrorT coq_MonadState) []
         | p :: br' ->
           let (t0, e0) = p in
           bind (coq_MonadErrorT coq_MonadState)
             (inline_exp_aux e0 sig0 fm s) (fun e' ->
             bind (coq_MonadErrorT coq_MonadState) (beta_list br')
               (fun br'' ->
               ret (coq_MonadErrorT coq_MonadState) ((t0, e') :: br'')))
         in beta_list cl) (fun cl' ->
        ret (coq_MonadErrorT coq_MonadState) (Ecase (v', cl')))
    | Eproj (x, t0, n, y, e0) ->
      let y' = apply_r sig0 y in
      bind (coq_MonadErrorT coq_MonadState) (get_fresh_name x) (fun x' ->
        bind (coq_MonadErrorT coq_MonadState)
          (inline_exp_aux e0 (Cps.M.set x x' sig0) fm s) (fun e' ->
          ret (coq_MonadErrorT coq_MonadState) (Eproj (x', t0, n, y', e'))))
    | Eletapp (x, f, t0, ys, ec) ->
      let f' = apply_r sig0 f in
      let ys' = apply_r_list sig0 ys in
      let (p, inl_dec) = iH.update_letApp f t0 ys' s in
      let (s', s'') = p in
      let p0 = ((inl_dec, (Cps.M.get f fm)), d) in
      let (p1, n0) = p0 in
      let (b, o) = p1 in
      if b
      then (match o with
            | Some p2 ->
              let (p3, e0) = p2 in
              let (_, xs) = p3 in
              (match n0 with
               | O ->
                 bind (coq_MonadErrorT coq_MonadState) (get_fresh_name x)
                   (fun x' ->
                   bind (coq_MonadErrorT coq_MonadState)
                     (inline_exp_aux ec (Cps.M.set x x' sig0) fm s')
                     (fun ec' ->
                     ret (coq_MonadErrorT coq_MonadState) (Eletapp (x', f',
                       t0, ys', ec'))))
               | S d' ->
                 (match j with
                  | O ->
                    bind (coq_MonadErrorT coq_MonadState) (get_fresh_name x)
                      (fun x' ->
                      bind (coq_MonadErrorT coq_MonadState)
                        (inline_exp_aux ec (Cps.M.set x x' sig0) fm s')
                        (fun ec' ->
                        ret (coq_MonadErrorT coq_MonadState) (Eletapp (x',
                          f', t0, ys', ec'))))
                  | S j' ->
                    if Nat.eqb (length xs) (length ys)
                    then let sig' = set_list (combine xs ys') sig0 in
                         bind (coq_MonadErrorT coq_MonadState)
                           (get_fresh_name x) (fun x' ->
                           let (j1, j2) = split_fuel j' in
                           bind (coq_MonadErrorT coq_MonadState)
                             (inline_exp iH d' j1 e0 sig' (Cps.M.remove f fm)
                               s') (fun e' ->
                             match inline_letapp e' x' with
                             | Some p4 ->
                               let (c, x'0) = p4 in
                               bind (coq_MonadErrorT coq_MonadState) click
                                 (fun _ ->
                                 bind (coq_MonadErrorT coq_MonadState)
                                   (inline_exp iH d' j2 ec
                                     (Cps.M.set x x'0 sig0) fm s'')
                                   (fun ec' ->
                                   ret (coq_MonadErrorT coq_MonadState)
                                     (app_ctx_f c ec')))
                             | None ->
                               bind (coq_MonadErrorT coq_MonadState)
                                 (get_fresh_name x) (fun x'0 ->
                                 bind (coq_MonadErrorT coq_MonadState)
                                   (inline_exp_aux ec (Cps.M.set x x'0 sig0)
                                     fm s') (fun ec' ->
                                   ret (coq_MonadErrorT coq_MonadState)
                                     (Eletapp (x'0, f', t0, ys', ec'))))))
                    else bind (coq_MonadErrorT coq_MonadState)
                           (get_fresh_name x) (fun x' ->
                           bind (coq_MonadErrorT coq_MonadState)
                             (inline_exp_aux ec (Cps.M.set x x' sig0) fm s')
                             (fun ec' ->
                             ret (coq_MonadErrorT coq_MonadState) (Eletapp
                               (x', f', t0, ys', ec'))))))
            | None ->
              bind (coq_MonadErrorT coq_MonadState) (get_fresh_name x)
                (fun x' ->
                bind (coq_MonadErrorT coq_MonadState)
                  (inline_exp_aux ec (Cps.M.set x x' sig0) fm s') (fun ec' ->
                  ret (coq_MonadErrorT coq_MonadState) (Eletapp (x', f', t0,
                    ys', ec')))))
      else bind (coq_MonadErrorT coq_MonadState) (get_fresh_name x)
             (fun x' ->
             bind (coq_MonadErrorT coq_MonadState)
               (inline_exp_aux ec (Cps.M.set x x' sig0) fm s') (fun ec' ->
               ret (coq_MonadErrorT coq_MonadState) (Eletapp (x', f', t0,
                 ys', ec'))))
    | Efun (fds, e0) ->
      let fm' = add_fundefs fds fm in
      let (s1, s2) = iH.update_funDef fds sig0 s in
      let names = all_fun_name fds in
      bind (coq_MonadErrorT coq_MonadState) (get_fresh_names names)
        (fun names' ->
        let sig' = set_list (combine names names') sig0 in
        bind (coq_MonadErrorT coq_MonadState)
          (let rec inline_exp_fds fds0 s0 =
             match fds0 with
             | Fcons (f, t0, xs, e1, fds') ->
               let s' = iH.update_inFun f t0 xs e1 sig0 s0 in
               let f' = apply_r sig' f in
               bind (coq_MonadErrorT coq_MonadState) (get_fresh_names xs)
                 (fun xs' ->
                 bind (coq_MonadErrorT coq_MonadState)
                   (inline_exp_aux e1 (set_list (combine xs xs') sig')
                     (Cps.M.remove f fm') s') (fun e' ->
                   bind (coq_MonadErrorT coq_MonadState)
                     (inline_exp_fds fds' s0) (fun fds'' ->
                     ret (coq_MonadErrorT coq_MonadState) (Fcons (f', t0,
                       xs', e', fds'')))))
             | Fnil -> ret (coq_MonadErrorT coq_MonadState) Fnil
           in inline_exp_fds fds s1) (fun fds' ->
          bind (coq_MonadErrorT coq_MonadState)
            (inline_exp_aux e0 sig' fm' s2) (fun e' ->
            ret (coq_MonadErrorT coq_MonadState) (Efun (fds', e')))))
    | Eapp (f, t0, ys) ->
      let f' = apply_r sig0 f in
      let ys' = apply_r_list sig0 ys in
      let (s', inl) = iH.update_App f t0 ys' s in
      let p = ((inl, (Cps.M.get f fm)), d) in
      let (p0, n0) = p in
      let (b, o) = p0 in
      if b
      then (match o with
            | Some p1 ->
              let (p2, e0) = p1 in
              let (_, xs) = p2 in
              (match n0 with
               | O ->
                 ret (coq_MonadErrorT coq_MonadState) (Eapp (f', t0, ys'))
               | S d' ->
                 (match j with
                  | O ->
                    ret (coq_MonadErrorT coq_MonadState) (Eapp (f', t0, ys'))
                  | S j' ->
                    if Nat.eqb (length xs) (length ys)
                    then let sig' = set_list (combine xs ys') sig0 in
                         bind (coq_MonadErrorT coq_MonadState) click
                           (fun _ ->
                           inline_exp iH d' j' e0 sig' (Cps.M.remove f fm) s')
                    else ret (coq_MonadErrorT coq_MonadState) (Eapp (f', t0,
                           ys'))))
            | None ->
              ret (coq_MonadErrorT coq_MonadState) (Eapp (f', t0, ys')))
      else ret (coq_MonadErrorT coq_MonadState) (Eapp (f', t0, ys'))
    | Eprim_val (x, p, e0) ->
      bind (coq_MonadErrorT coq_MonadState) (get_fresh_name x) (fun x' ->
        bind (coq_MonadErrorT coq_MonadState)
          (inline_exp_aux e0 (Cps.M.set x x' sig0) fm s) (fun e' ->
          ret (coq_MonadErrorT coq_MonadState) (Eprim_val (x', p, e'))))
    | Eprim (x, t0, ys, e0) ->
      let ys' = apply_r_list sig0 ys in
      bind (coq_MonadErrorT coq_MonadState) (get_fresh_name x) (fun x' ->
        bind (coq_MonadErrorT coq_MonadState)
          (inline_exp_aux e0 (Cps.M.set x x' sig0) fm s) (fun e' ->
          ret (coq_MonadErrorT coq_MonadState) (Eprim (x', t0, ys', e'))))
    | Ehalt x ->
      let x' = apply_r sig0 x in
      ret (coq_MonadErrorT coq_MonadState) (Ehalt x')
  in inline_exp_aux

(** val restart_names : var -> exp -> comp_data -> comp_data * name_env **)

let restart_names max_var e c =
  let fvs = exp_fv e in
  let new_var =
    match PS.max_elt fvs with
    | Some v -> BinPos.Pos.add (BinPos.Pos.max v max_var) Coq_xH
    | None -> BinPos.Pos.add max_var Coq_xH
  in
  let { next_var = _; nect_ctor_tag = ct; next_ind_tag = it; next_fun_tag =
    ft; cenv = c0; fenv = f; nenv = names; inline_map = imap; log = l } = c
  in
  ({ next_var = new_var; nect_ctor_tag = ct; next_ind_tag = it;
  next_fun_tag = ft; cenv = c0; fenv = f; nenv = Cps.M.empty; inline_map =
  imap; log = l }, names)

(** val inline_top' :
    'a1 coq_InlineHeuristic -> var -> nat -> 'a1 -> exp -> comp_data -> (exp
    error * comp_data) * bool **)

let inline_top' iH max_var d s e c =
  let (c0, nenv0) = restart_names max_var e c in
  let (e', p) =
    run_compM (inline_exp iH d d e Cps.M.empty Cps.M.empty s) c0 (false,
      nenv0)
  in
  let (st', i) = p in let (click0, _) = i in ((e', st'), click0)

(** val inline_top :
    'a1 coq_InlineHeuristic -> var -> nat -> 'a1 -> exp -> comp_data -> exp
    error * comp_data **)

let inline_top iH max_var d s e c =
  let (p, _) = inline_top' iH max_var d s e c in p

(** val inline_loop_aux :
    'a1 coq_InlineHeuristic -> var -> nat -> nat -> 'a1 -> exp -> comp_data
    -> exp error * comp_data **)

let rec inline_loop_aux iH max_var fuel d s e c =
  match fuel with
  | O -> ((Ret e), c)
  | S fuel0 ->
    let (p, click0) = inline_top' iH max_var d s e c in
    let (e', c') = p in
    (match e' with
     | Err s0 -> ((Err s0), c')
     | Ret e'0 ->
       if click0
       then inline_loop_aux iH max_var fuel0 d s (fst (shrink_top e'0)) c'
       else ((Ret (fst (shrink_top e'0))), c'))

(** val inline_loop :
    'a1 coq_InlineHeuristic -> var -> nat -> 'a1 -> exp -> comp_data -> exp
    error * comp_data **)

let inline_loop iH max_var d s e c =
  inline_loop_aux iH max_var (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    O))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    d s e c

(** val coq_CombineInlineHeuristic :
    (bool -> bool -> bool) -> 'a1 coq_InlineHeuristic -> 'a2
    coq_InlineHeuristic -> ('a1 * 'a2) coq_InlineHeuristic **)

let coq_CombineInlineHeuristic deci iH1 iH2 =
  { update_funDef = (fun fds sigma s ->
    let (s1, s2) = s in
    let (s11, s12) = iH1.update_funDef fds sigma s1 in
    let (s21, s22) = iH2.update_funDef fds sigma s2 in
    ((s11, s21), (s12, s22))); update_inFun = (fun f t0 xs e sigma s ->
    let (s1, s2) = s in
    let s1' = iH1.update_inFun f t0 xs e sigma s1 in
    let s2' = iH2.update_inFun f t0 xs e sigma s2 in (s1', s2'));
    update_App = (fun f t0 ys s ->
    let (s1, s2) = s in
    let (s1', b1) = iH1.update_App f t0 ys s1 in
    let (s2', b2) = iH2.update_App f t0 ys s2 in ((s1', s2'), (deci b1 b2)));
    update_letApp = (fun f t0 ys s ->
    let (s1, s2) = s in
    let (p, b1) = iH1.update_letApp f t0 ys s1 in
    let (s1', s1'') = p in
    let (p0, b2) = iH2.update_letApp f t0 ys s2 in
    let (s2', s2'') = p0 in (((s1', s2'), (s1'', s2'')), (deci b1 b2))) }

(** val forall_fundefs : (exp -> bool) -> fundefs -> bool **)

let rec forall_fundefs f = function
| Fcons (_, _, _, e, tl) -> (&&) (f e) (forall_fundefs f tl)
| Fnil -> true

(** val do_inline : var -> exp -> bool **)

let rec do_inline f = function
| Econstr (_, _, _, e0) -> do_inline f e0
| Ecase (_, _) -> false
| Eproj (_, _, _, _, e0) -> do_inline f e0
| Eletapp (_, f', _, _, e0) -> (&&) (negb (Pos.eqb f f')) (do_inline f e0)
| Efun (fns, e0) -> (&&) (forall_fundefs (do_inline f) fns) (do_inline f e0)
| Eapp (f', _, _) -> negb (Pos.eqb f f')
| Eprim_val (_, _, e0) -> do_inline f e0
| Eprim (_, _, _, e0) -> do_inline f e0
| Ehalt _ -> true

(** val coq_InlineSmall : nat -> bool Cps.M.t coq_InlineHeuristic **)

let coq_InlineSmall bound =
  { update_funDef = (fun fds sigma s ->
    let s' =
      let rec upd fds0 sigma0 s0 =
        match fds0 with
        | Fcons (f, _, _, e, fdc') ->
          if (&&) (Nat.ltb (term_size e) bound) (do_inline f e)
          then upd fdc' sigma0 (Cps.M.set f true s0)
          else upd fdc' sigma0 s0
        | Fnil -> s0
      in upd fds sigma s
    in
    (s', s')); update_inFun = (fun f _ _ _ _ s -> Cps.M.remove f s);
    update_App = (fun f _ _ s ->
    match Cps.M.get f s with
    | Some b -> if b then ((Cps.M.remove f s), true) else (s, false)
    | None -> (s, false)); update_letApp = (fun f _ _ s ->
    match Cps.M.get f s with
    | Some b -> if b then (((Cps.M.remove f s), s), true) else ((s, s), false)
    | None -> ((s, s), false)) }

(** val coq_InlinedUncurriedMarked : nat Cps.M.t coq_InlineHeuristic **)

let coq_InlinedUncurriedMarked =
  { update_funDef = (fun _ _ s -> (s, s)); update_inFun = (fun _ _ _ _ _ s ->
    s); update_App = (fun f _ _ s ->
    match Cps.M.get f s with
    | Some y ->
      (match y with
       | O -> (s, false)
       | S n ->
         (match n with
          | O -> (s, true)
          | S n0 -> (match n0 with
                     | O -> (s, true)
                     | S _ -> (s, false))))
    | None -> (s, false)); update_letApp = (fun f _ _ s ->
    match Cps.M.get f s with
    | Some n ->
      (match n with
       | O -> ((s, s), false)
       | S n0 ->
         (match n0 with
          | O -> ((s, s), true)
          | S n1 ->
            (match n1 with
             | O -> ((s, s), true)
             | S _ -> ((s, s), false))))
    | None -> ((s, s), false)) }

(** val coq_InlineSmallOrUncurried :
    nat -> (bool Cps.M.t * nat Cps.M.t) coq_InlineHeuristic **)

let coq_InlineSmallOrUncurried bound =
  coq_CombineInlineHeuristic (||) (coq_InlineSmall bound)
    coq_InlinedUncurriedMarked

(** val inline_uncurry :
    var -> nat -> nat -> exp -> comp_data -> exp error * comp_data **)

let inline_uncurry max_var bound d e c =
  let inline_map0 = c.inline_map in
  inline_top (coq_InlineSmallOrUncurried bound) max_var d (Cps.M.empty,
    inline_map0) e c

(** val inline_shrink_loop :
    var -> nat -> nat -> exp -> comp_data -> exp error * comp_data **)

let inline_shrink_loop max_var bound d e c =
  inline_loop (coq_InlineSmall bound) max_var d Cps.M.empty e c

(** val find_indirect_call : var -> exp -> bool Cps.M.t -> bool Cps.M.t **)

let find_indirect_call _ e s =
  let b =
    let rec is_wrapper = function
    | Econstr (_, _, _, e1) -> is_wrapper e1
    | Ecase (_, _) -> None
    | Eproj (_, _, _, _, e1) -> is_wrapper e1
    | Eletapp (_, _, _, _, _) -> None
    | Efun (_, _) -> None
    | Eapp (g, _, _) -> Some g
    | Eprim_val (_, _, e1) -> is_wrapper e1
    | Eprim (_, _, _, e1) -> is_wrapper e1
    | Ehalt _ -> None
    in is_wrapper e
  in
  (match b with
   | Some g -> Cps.M.set g true s
   | None -> s)

(** val coq_InineLifted : bool Cps.M.t coq_InlineHeuristic **)

let coq_InineLifted =
  { update_funDef = (fun _ _ s -> (s, s)); update_inFun = (fun f _ _ e _ s ->
    Cps.M.remove f (find_indirect_call f e s)); update_App = (fun f _ _ s ->
    match Cps.M.get f s with
    | Some b -> (s, b)
    | None -> (s, false)); update_letApp = (fun f _ _ s ->
    match Cps.M.get f s with
    | Some b -> ((s, s), b)
    | None -> ((s, s), false)) }

(** val inline_lifted :
    var -> nat -> nat -> exp -> comp_data -> exp error * comp_data **)

let inline_lifted max_var _ d e c =
  inline_top coq_InineLifted max_var d Cps.M.empty e c
