open BinNat
open BinNatDef
open BinNums
open BinPos
open Bool
open Byte
open Datatypes
open Frame
open List0
open Nat0
open Rewriting
open Bytestring
open CompM
open Cps
open Cps_proto
open Cps_proto_univ
open Cps_util
open Identifiers
open Proto_util
open State

let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val set_name : var -> var -> String.t -> comp_data -> comp_data **)

let set_name old_var new_var suff cdata =
  let { next_var = _; nect_ctor_tag = c; next_ind_tag = i; next_fun_tag = f;
    cenv = e; fenv = fenv0; nenv = names; inline_map = imap; log = log0 } =
    cdata
  in
  let names' = add_entry names new_var old_var suff in
  { next_var = (Pos.add Coq_xH (Pos.max old_var new_var)); nect_ctor_tag = c;
  next_ind_tag = i; next_fun_tag = f; cenv = e; fenv = fenv0; nenv = names';
  inline_map = imap; log = log0 }

(** val set_names_lst :
    var list -> var list -> String.t -> comp_data -> comp_data **)

let set_names_lst olds news suff cdata =
  fold_right (fun pat cdata0 ->
    let (old, new0) = pat in set_name old new0 suff cdata0) cdata
    (combine olds news)

type coq_St = nat * nat M.tree

type arity_map = fun_tag M.tree

type local_map = bool M.tree

type coq_S_misc = (((bool * arity_map) * local_map) * coq_St) * comp_data

(** val get_fun_tag : coq_N -> coq_S_misc -> fun_tag * coq_S_misc **)

let get_fun_tag n ms = match ms with
| (p, cdata) ->
  let (p0, s) = p in
  let (p1, lm) = p0 in
  let (b, aenv) = p1 in
  let n' = BinNat.N.succ_pos n in
  (match M.get n' aenv with
   | Some ft -> (ft, ms)
   | None ->
     let (mft, p2) = get_ftag n () (cdata, ()) in
     let (cdata0, _) = p2 in
     (match Obj.magic mft with
      | Err _ -> (Coq_xH, ms)
      | Ret ft -> (ft, ((((b, (M.set n' ft aenv)), lm), s), cdata0))))

(** val metadata_update :
    var -> var -> var -> nat -> var list -> var list -> var list -> var list
    -> coq_S_misc -> coq_S_misc **)

let metadata_update f g f1 fp_numargs fv gv fv1 gv1 = function
| (p, cdata) ->
  let (p0, s) = p in
  let (p1, lm) = p0 in
  let (_, aenv) = p1 in
  let b = true in
  let lm0 = M.set g true lm in
  let s0 = ((max (fst s) fp_numargs),
    (M.set f (S O) (M.set g (S (S O)) (snd s))))
  in
  let cdata0 =
    set_name f f1 (String.String (Coq_x5f, (String.String (Coq_x75,
      (String.String (Coq_x6e, (String.String (Coq_x63, (String.String
      (Coq_x75, (String.String (Coq_x72, (String.String (Coq_x72,
      (String.String (Coq_x69, (String.String (Coq_x65, (String.String
      (Coq_x64, String.EmptyString))))))))))))))))))))
      (set_names_lst fv fv1 String.EmptyString
        (set_names_lst gv gv1 String.EmptyString cdata))
  in
  ((((b, aenv), lm0), s0), cdata0)

(** val rw_uncurry :
    coq_Fuel -> exp_univ -> exp_univ univD -> (exp_univ, unit) coq_Delay ->
    (exp_univ, bool) coq_Param -> (exp_univ, coq_S_misc * var) coq_State ->
    (exp_univ, coq_S_misc * var) result **)

let rw_uncurry _ a e d r s =
  let x = fun f ft k fv g gt gv ge k' kt g' fds d0 r0 s0 x x0 ->
    let b = eq_var k k' in
    if b
    then let b0 = eq_var g g' in
         if b0
         then let (s1, v) = s0 in
              let (p, _) = s1 in
              let (p0, _) = p in
              let (_, l) = p0 in
              let b1 =
                (&&) r0
                  (negb (match M.get g l with
                         | Some b1 -> b1
                         | None -> false))
              in
              if b1
              then let b2 = occurs_in_exp g ge in
                   if b2
                   then x0 __
                   else let b3 = occurs_in_exp k ge in
                        if b3
                        then x0 __
                        else let fp_numargs = add (length fv) (length gv) in
                             let h0 = x __ in
                             let success =
                               fun fds' d1 f0 ft0 k0 fv0 g0 gt0 gv0 ge0 k'0 kt0 g'0 fds0 f1 ft1 fv1 gv1 lhs fds'0 rhs ms' x1 x2 ->
                               h0 fds' d1 f0 ft0 k0 fv0 g0 gt0 gv0 ge0 k'0
                                 kt0 g'0 fds0 f1 ft1 fv1 gv1 lhs fds'0 rhs
                                 fp_numargs s1 ms' __ __ __ x1 x2
                             in
                             let p1 = get_fun_tag (N.of_nat fp_numargs) s1 in
                             let (f0, s2) = p1 in
                             let xgv1 = gensyms v gv in
                             let (v0, l0) = xgv1 in
                             let xfv1 = gensyms v0 fv in
                             let (v1, l1) = xfv1 in
                             let success0 =
                               fun fds' d1 f1 ft0 k0 fv0 g0 gt0 gv0 ge0 k'0 kt0 g'0 fds0 ft1 lhs fds'0 rhs ms' x1 x2 ->
                               success fds' d1 f1 ft0 k0 fv0 g0 gt0 gv0 ge0
                                 k'0 kt0 g'0 fds0 v1 ft1 l1 l0 lhs fds'0 rhs
                                 ms' x1 x2
                             in
                             let fds' = Fcons (v1, f0, (app gv fv), ge, fds)
                             in
                             let success1 = fun lhs fds'0 rhs ms' x1 ->
                               success0 fds' d0 f ft k fv g gt gv ge k' kt g'
                                 fds f0 lhs fds'0 rhs ms' x1
                             in
                             let lhs = Fcons (f, ft, (k :: fv), (Efun ((Fcons
                               (g, gt, gv, ge, Fnil)), (Eapp (k', kt,
                               (g' :: []))))), fds)
                             in
                             let rhs = Fcons (f, ft, (k :: l1), (Efun ((Fcons
                               (g, gt, l0, (Eapp (v1, f0, (app l0 l1))),
                               Fnil)), (Eapp (k, kt, (g :: []))))), fds')
                             in
                             success1 lhs fds' rhs s2 r0
                               ((metadata_update f g v1 fp_numargs fv gv l1
                                  l0 s2), (Pos.add v1 Coq_xH))
              else x0 __
         else x0 __
    else x0 __
  in
  let x0 = fun f ft fv g gt gv ge g' fds d0 r0 s0 x0 x1 ->
    let b = eq_var g g' in
    if b
    then let (s1, v) = s0 in
         let (p, _) = s1 in
         let (p0, _) = p in
         let (_, l) = p0 in
         let b0 =
           (&&) (negb r0)
             (negb (match M.get g l with
                    | Some b0 -> b0
                    | None -> false))
         in
         if b0
         then let b1 = occurs_in_exp g ge in
              if b1
              then x1 __
              else let fp_numargs = add (length fv) (length gv) in
                   let h0 = x0 __ in
                   let success =
                     fun fds' d1 f0 ft0 fv0 g0 gt0 gv0 ge0 g'0 fds0 f1 ft1 fv1 gv1 lhs fds'0 rhs ms' x2 x3 ->
                     h0 fds' d1 f0 ft0 fv0 g0 gt0 gv0 ge0 g'0 fds0 f1 ft1 fv1
                       gv1 lhs fds'0 rhs fp_numargs s1 ms' __ __ __ x2 x3
                   in
                   let p1 = get_fun_tag (N.of_nat fp_numargs) s1 in
                   let (f0, s2) = p1 in
                   let xgv1 = gensyms v gv in
                   let (v0, l0) = xgv1 in
                   let xfv1 = gensyms v0 fv in
                   let (v1, l1) = xfv1 in
                   let success0 =
                     fun fds' d1 f1 ft0 fv0 g0 gt0 gv0 ge0 g'0 fds0 ft1 lhs fds'0 rhs ms' x2 x3 ->
                     success fds' d1 f1 ft0 fv0 g0 gt0 gv0 ge0 g'0 fds0 v1
                       ft1 l1 l0 lhs fds'0 rhs ms' x2 x3
                   in
                   let fds' = Fcons (v1, f0, (app gv fv), ge, fds) in
                   let success1 = fun lhs fds'0 rhs ms' x2 ->
                     success0 fds' d0 f ft fv g gt gv ge g' fds f0 lhs fds'0
                       rhs ms' x2
                   in
                   let lhs = Fcons (f, ft, fv, (Efun ((Fcons (g, gt, gv, ge,
                     Fnil)), (Ehalt g'))), fds)
                   in
                   let rhs = Fcons (f, ft, l1, (Efun ((Fcons (g, gt, l0,
                     (Eapp (v1, f0, (app l0 l1))), Fnil)), (Ehalt g))), fds')
                   in
                   success1 lhs fds' rhs s2 r0
                     ((metadata_update f g v1 fp_numargs fv gv l1 l0 s2),
                     (Pos.add v1 Coq_xH))
         else x1 __
    else x1 __
  in
  let x1 = fun _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ x1 x2 x3 _ _ ->
    let r0 = x3 __ __ x1 x2 __ in
    let { resTree = resTree0; resState = resState0 } = r0 in
    { resTree = resTree0; resState = resState0 }
  in
  let x2 = fun _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ x2 x3 x4 _ _ ->
    let r0 = x4 __ __ x2 x3 __ in
    let { resTree = resTree0; resState = resState0 } = r0 in
    { resTree = resTree0; resState = resState0 }
  in
  let x3 = fun ct _ d0 x3 -> x3 ct d0 __ in
  let x4 = fun _ x4 -> x4 __ in
  let x5 = fun _ _ d0 x5 -> x5 d0 d0 __ in
  let x6 = fun v ft lv _ _ d0 x6 -> x6 v ft lv d0 d0 __ in
  let x7 = fun _ x7 -> x7 __ in
  let x8 = fun v ct lv _ d0 x8 -> x8 v ct lv d0 __ in
  let x9 = fun v _ d0 x9 -> x9 v d0 __ in
  let x10 = fun v ct n v0 _ d0 x10 -> x10 v ct n v0 d0 __ in
  let x11 = fun v v0 ft lv _ d0 x11 -> x11 v v0 ft lv d0 __ in
  let x12 = fun _ _ d0 x12 -> x12 d0 d0 __ in
  let x13 = fun v ft lv _ x13 -> x13 v ft lv __ in
  let x14 = fun v p _ d0 x14 -> x14 v p d0 __ in
  let x15 = fun v p lv _ d0 x15 -> x15 v p lv d0 __ in
  let x16 = fun v _ x16 -> x16 v __ in
  let x17 = fun _ e0 x17 x18 r0 s0 ->
    let x19 = x18 __ in
    let r1 = x19 e0 __ __ r0 s0 __ in
    let { resTree = resTree0; resState = resState0 } = r1 in
    let x20 = x17 __ in
    let r2 = x20 resTree0 __ __ r0 resState0 __ in
    let { resTree = resTree1; resState = resState1 } = r2 in
    { resTree = (Obj.magic (resTree0, resTree1)); resState = resState1 }
  in
  let x18 = fun _ s0 -> { resTree = (Obj.magic []); resState = s0 } in
  let x19 = fun _ lpcte x19 x20 r0 s0 ->
    let x21 = x20 __ in
    let r1 = x21 lpcte __ __ r0 s0 __ in
    let { resTree = resTree0; resState = resState0 } = r1 in
    let x22 = x19 __ in
    let r2 = x22 resTree0 __ __ r0 resState0 __ in
    let { resTree = resTree1; resState = resState1 } = r2 in
    { resTree = (Obj.magic (resTree0 :: (Obj.magic resTree1))); resState =
    resState1 }
  in
  let x20 = fun _ ft lv e0 f x20 x21 x22 x23 x24 r0 s0 ->
    let x25 = x24 __ in
    let r1 = x25 ft lv e0 f __ __ r0 s0 __ in
    let { resTree = resTree0; resState = resState0 } = r1 in
    let x26 = x23 __ in
    let r2 = x26 resTree0 lv e0 f __ __ r0 resState0 __ in
    let { resTree = resTree1; resState = resState1 } = r2 in
    let x27 = x22 __ in
    let r3 = x27 resTree0 resTree1 e0 f __ __ r0 resState1 __ in
    let { resTree = resTree2; resState = resState2 } = r3 in
    let x28 = x21 __ in
    let r4 = x28 resTree0 resTree1 resTree2 f __ __ r0 resState2 __ in
    let { resTree = resTree3; resState = resState3 } = r4 in
    let x29 = x20 __ in
    let r5 = x29 resTree0 resTree1 resTree2 resTree3 __ __ r0 resState3 __ in
    let { resTree = resTree4; resState = resState4 } = r5 in
    { resTree =
    (Obj.magic (Fcons ((Obj.magic resTree0), (Obj.magic resTree1),
      (Obj.magic resTree2), (Obj.magic resTree3), (Obj.magic resTree4))));
    resState = resState4 }
  in
  let x21 = fun _ s0 -> { resTree = (Obj.magic Fnil); resState = s0 } in
  let x22 = fun _ ct lv e0 x22 x23 x24 x25 r0 s0 ->
    let x26 = x25 __ in
    let r1 = x26 ct lv e0 __ __ r0 s0 __ in
    let { resTree = resTree0; resState = resState0 } = r1 in
    let x27 = x24 __ in
    let r2 = x27 resTree0 lv e0 __ __ r0 resState0 __ in
    let { resTree = resTree1; resState = resState1 } = r2 in
    let x28 = x23 __ in
    let r3 = x28 resTree0 resTree1 e0 __ __ r0 resState1 __ in
    let { resTree = resTree2; resState = resState2 } = r3 in
    let x29 = x22 __ in
    let r4 = x29 resTree0 resTree1 resTree2 __ __ r0 resState2 __ in
    let { resTree = resTree3; resState = resState3 } = r4 in
    { resTree =
    (Obj.magic (Econstr ((Obj.magic resTree0), (Obj.magic resTree1),
      (Obj.magic resTree2), (Obj.magic resTree3)))); resState = resState3 }
  in
  let x23 = fun _ lpcte x23 x24 r0 s0 ->
    let x25 = x24 __ in
    let r1 = x25 lpcte __ __ r0 s0 __ in
    let { resTree = resTree0; resState = resState0 } = r1 in
    let x26 = x23 __ in
    let r2 = x26 resTree0 __ __ r0 resState0 __ in
    let { resTree = resTree1; resState = resState1 } = r2 in
    { resTree =
    (Obj.magic (Ecase ((Obj.magic resTree0), (Obj.magic resTree1))));
    resState = resState1 }
  in
  let x24 = fun _ ct n v e0 x24 x25 x26 x27 x28 r0 s0 ->
    let x29 = x28 __ in
    let r1 = x29 ct n v e0 __ __ r0 s0 __ in
    let { resTree = resTree0; resState = resState0 } = r1 in
    let x30 = x27 __ in
    let r2 = x30 resTree0 n v e0 __ __ r0 resState0 __ in
    let { resTree = resTree1; resState = resState1 } = r2 in
    let x31 = x26 __ in
    let r3 = x31 resTree0 resTree1 v e0 __ __ r0 resState1 __ in
    let { resTree = resTree2; resState = resState2 } = r3 in
    let x32 = x25 __ in
    let r4 = x32 resTree0 resTree1 resTree2 e0 __ __ r0 resState2 __ in
    let { resTree = resTree3; resState = resState3 } = r4 in
    let x33 = x24 __ in
    let r5 = x33 resTree0 resTree1 resTree2 resTree3 __ __ r0 resState3 __ in
    let { resTree = resTree4; resState = resState4 } = r5 in
    { resTree =
    (Obj.magic (Eproj ((Obj.magic resTree0), (Obj.magic resTree1),
      (Obj.magic resTree2), (Obj.magic resTree3), (Obj.magic resTree4))));
    resState = resState4 }
  in
  let x25 = fun _ v ft lv e0 x25 x26 x27 x28 x29 r0 s0 ->
    let x30 = x29 __ in
    let r1 = x30 v ft lv e0 __ __ r0 s0 __ in
    let { resTree = resTree0; resState = resState0 } = r1 in
    let x31 = x28 __ in
    let r2 = x31 resTree0 ft lv e0 __ __ r0 resState0 __ in
    let { resTree = resTree1; resState = resState1 } = r2 in
    let x32 = x27 __ in
    let r3 = x32 resTree0 resTree1 lv e0 __ __ r0 resState1 __ in
    let { resTree = resTree2; resState = resState2 } = r3 in
    let x33 = x26 __ in
    let r4 = x33 resTree0 resTree1 resTree2 e0 __ __ r0 resState2 __ in
    let { resTree = resTree3; resState = resState3 } = r4 in
    let x34 = x25 __ in
    let r5 = x34 resTree0 resTree1 resTree2 resTree3 __ __ r0 resState3 __ in
    let { resTree = resTree4; resState = resState4 } = r5 in
    { resTree =
    (Obj.magic (Eletapp ((Obj.magic resTree0), (Obj.magic resTree1),
      (Obj.magic resTree2), (Obj.magic resTree3), (Obj.magic resTree4))));
    resState = resState4 }
  in
  let x26 = fun _ e0 x26 x27 r0 s0 ->
    let x28 = x27 __ in
    let r1 = x28 e0 __ __ r0 s0 __ in
    let { resTree = resTree0; resState = resState0 } = r1 in
    let x29 = x26 __ in
    let r2 = x29 resTree0 __ __ r0 resState0 __ in
    let { resTree = resTree1; resState = resState1 } = r2 in
    { resTree =
    (Obj.magic (Efun ((Obj.magic resTree0), (Obj.magic resTree1))));
    resState = resState1 }
  in
  let x27 = fun _ ft lv x27 x28 x29 r0 s0 ->
    let x30 = x29 __ in
    let r1 = x30 ft lv __ __ r0 s0 __ in
    let { resTree = resTree0; resState = resState0 } = r1 in
    let x31 = x28 __ in
    let r2 = x31 resTree0 lv __ __ r0 resState0 __ in
    let { resTree = resTree1; resState = resState1 } = r2 in
    let x32 = x27 __ in
    let r3 = x32 resTree0 resTree1 __ __ r0 resState1 __ in
    let { resTree = resTree2; resState = resState2 } = r3 in
    { resTree =
    (Obj.magic (Eapp ((Obj.magic resTree0), (Obj.magic resTree1),
      (Obj.magic resTree2)))); resState = resState2 }
  in
  let x28 = fun _ p e0 x28 x29 x30 r0 s0 ->
    let x31 = x30 __ in
    let r1 = x31 p e0 __ __ r0 s0 __ in
    let { resTree = resTree0; resState = resState0 } = r1 in
    let x32 = x29 __ in
    let r2 = x32 resTree0 e0 __ __ r0 resState0 __ in
    let { resTree = resTree1; resState = resState1 } = r2 in
    let x33 = x28 __ in
    let r3 = x33 resTree0 resTree1 __ __ r0 resState1 __ in
    let { resTree = resTree2; resState = resState2 } = r3 in
    { resTree =
    (Obj.magic (Eprim_val ((Obj.magic resTree0), (Obj.magic resTree1),
      (Obj.magic resTree2)))); resState = resState2 }
  in
  let x29 = fun _ p lv e0 x29 x30 x31 x32 r0 s0 ->
    let x33 = x32 __ in
    let r1 = x33 p lv e0 __ __ r0 s0 __ in
    let { resTree = resTree0; resState = resState0 } = r1 in
    let x34 = x31 __ in
    let r2 = x34 resTree0 lv e0 __ __ r0 resState0 __ in
    let { resTree = resTree1; resState = resState1 } = r2 in
    let x35 = x30 __ in
    let r3 = x35 resTree0 resTree1 e0 __ __ r0 resState1 __ in
    let { resTree = resTree2; resState = resState2 } = r3 in
    let x36 = x29 __ in
    let r4 = x36 resTree0 resTree1 resTree2 __ __ r0 resState2 __ in
    let { resTree = resTree3; resState = resState3 } = r4 in
    { resTree =
    (Obj.magic (Eprim ((Obj.magic resTree0), (Obj.magic resTree1),
      (Obj.magic resTree2), (Obj.magic resTree3)))); resState = resState3 }
  in
  let x30 = fun _ x30 r0 s0 ->
    let x31 = x30 __ in
    let r1 = x31 __ __ r0 s0 __ in
    let { resTree = resTree0; resState = resState0 } = r1 in
    { resTree = (Obj.magic (Ehalt (Obj.magic resTree0))); resState =
    resState0 }
  in
  let x31 = fun d0 r0 _ _ x31 ->
    let (c, e0) = d0 in
    x3 c e0 r0 (fun ct d1 _ -> x31 __ __ ct ct e0 d1 __ __)
  in
  let x32 = fun d0 r0 _ _ x32 x33 ->
    match d0 with
    | [] -> x4 r0 (fun _ -> x32 __ __ __ __)
    | p :: l -> x5 p l r0 (fun d1 d2 _ -> x33 __ __ p d1 l d2 __ __)
  in
  let x33 = fun d0 r0 s0 s1 x33 x34 x35 x36 ->
    match d0 with
    | Fcons (v, f, l, e0, f0) ->
      (match e0 with
       | Econstr (v0, c, l0, e1) ->
         (match l with
          | [] ->
            x6 v f [] (Econstr (v0, c, l0, e1)) f0 r0
              (fun v1 ft lv d1 d2 _ ->
              x35 __ __ v1 v1 ft ft lv lv (Econstr (v0, c, l0, e1)) d1 f0 d2
                __ __)
          | v1 :: l1 ->
            x6 v f (v1 :: l1) (Econstr (v0, c, l0, e1)) f0 r0
              (fun v2 ft lv d1 d2 _ ->
              x35 __ __ v2 v2 ft ft lv lv (Econstr (v0, c, l0, e1)) d1 f0 d2
                __ __))
       | Ecase (v0, l0) ->
         (match l with
          | [] ->
            x6 v f [] (Ecase (v0, l0)) f0 r0 (fun v1 ft lv d1 d2 _ ->
              x35 __ __ v1 v1 ft ft lv lv (Ecase (v0, l0)) d1 f0 d2 __ __)
          | v1 :: l1 ->
            x6 v f (v1 :: l1) (Ecase (v0, l0)) f0 r0 (fun v2 ft lv d1 d2 _ ->
              x35 __ __ v2 v2 ft ft lv lv (Ecase (v0, l0)) d1 f0 d2 __ __))
       | Eproj (v0, c, n, v1, e1) ->
         (match l with
          | [] ->
            x6 v f [] (Eproj (v0, c, n, v1, e1)) f0 r0
              (fun v2 ft lv d1 d2 _ ->
              x35 __ __ v2 v2 ft ft lv lv (Eproj (v0, c, n, v1, e1)) d1 f0 d2
                __ __)
          | v2 :: l0 ->
            x6 v f (v2 :: l0) (Eproj (v0, c, n, v1, e1)) f0 r0
              (fun v3 ft lv d1 d2 _ ->
              x35 __ __ v3 v3 ft ft lv lv (Eproj (v0, c, n, v1, e1)) d1 f0 d2
                __ __))
       | Eletapp (v0, v1, f1, l0, e1) ->
         (match l with
          | [] ->
            x6 v f [] (Eletapp (v0, v1, f1, l0, e1)) f0 r0
              (fun v2 ft lv d1 d2 _ ->
              x35 __ __ v2 v2 ft ft lv lv (Eletapp (v0, v1, f1, l0, e1)) d1
                f0 d2 __ __)
          | v2 :: l1 ->
            x6 v f (v2 :: l1) (Eletapp (v0, v1, f1, l0, e1)) f0 r0
              (fun v3 ft lv d1 d2 _ ->
              x35 __ __ v3 v3 ft ft lv lv (Eletapp (v0, v1, f1, l0, e1)) d1
                f0 d2 __ __))
       | Efun (f1, e1) ->
         (match f1 with
          | Fcons (v0, f2, l0, e2, f3) ->
            (match f3 with
             | Fcons (v1, f4, l1, e3, f5) ->
               (match l with
                | [] ->
                  x6 v f [] (Efun ((Fcons (v0, f2, l0, e2, (Fcons (v1, f4,
                    l1, e3, f5)))), e1)) f0 r0 (fun v2 ft lv d1 d2 _ ->
                    x35 __ __ v2 v2 ft ft lv lv (Efun ((Fcons (v0, f2, l0,
                      e2, (Fcons (v1, f4, l1, e3, f5)))), e1)) d1 f0 d2 __ __)
                | v2 :: l2 ->
                  x6 v f (v2 :: l2) (Efun ((Fcons (v0, f2, l0, e2, (Fcons
                    (v1, f4, l1, e3, f5)))), e1)) f0 r0
                    (fun v3 ft lv d1 d2 _ ->
                    x35 __ __ v3 v3 ft ft lv lv (Efun ((Fcons (v0, f2, l0,
                      e2, (Fcons (v1, f4, l1, e3, f5)))), e1)) d1 f0 d2 __ __))
             | Fnil ->
               (match e1 with
                | Econstr (v1, c, l1, e3) ->
                  (match l with
                   | [] ->
                     x6 v f [] (Efun ((Fcons (v0, f2, l0, e2, Fnil)),
                       (Econstr (v1, c, l1, e3)))) f0 r0
                       (fun v2 ft lv d1 d2 _ ->
                       x35 __ __ v2 v2 ft ft lv lv (Efun ((Fcons (v0, f2, l0,
                         e2, Fnil)), (Econstr (v1, c, l1, e3)))) d1 f0 d2 __
                         __)
                   | v2 :: l2 ->
                     x6 v f (v2 :: l2) (Efun ((Fcons (v0, f2, l0, e2, Fnil)),
                       (Econstr (v1, c, l1, e3)))) f0 r0
                       (fun v3 ft lv d1 d2 _ ->
                       x35 __ __ v3 v3 ft ft lv lv (Efun ((Fcons (v0, f2, l0,
                         e2, Fnil)), (Econstr (v1, c, l1, e3)))) d1 f0 d2 __
                         __))
                | Ecase (v1, l1) ->
                  (match l with
                   | [] ->
                     x6 v f [] (Efun ((Fcons (v0, f2, l0, e2, Fnil)), (Ecase
                       (v1, l1)))) f0 r0 (fun v2 ft lv d1 d2 _ ->
                       x35 __ __ v2 v2 ft ft lv lv (Efun ((Fcons (v0, f2, l0,
                         e2, Fnil)), (Ecase (v1, l1)))) d1 f0 d2 __ __)
                   | v2 :: l2 ->
                     x6 v f (v2 :: l2) (Efun ((Fcons (v0, f2, l0, e2, Fnil)),
                       (Ecase (v1, l1)))) f0 r0 (fun v3 ft lv d1 d2 _ ->
                       x35 __ __ v3 v3 ft ft lv lv (Efun ((Fcons (v0, f2, l0,
                         e2, Fnil)), (Ecase (v1, l1)))) d1 f0 d2 __ __))
                | Eproj (v1, c, n, v2, e3) ->
                  (match l with
                   | [] ->
                     x6 v f [] (Efun ((Fcons (v0, f2, l0, e2, Fnil)), (Eproj
                       (v1, c, n, v2, e3)))) f0 r0 (fun v3 ft lv d1 d2 _ ->
                       x35 __ __ v3 v3 ft ft lv lv (Efun ((Fcons (v0, f2, l0,
                         e2, Fnil)), (Eproj (v1, c, n, v2, e3)))) d1 f0 d2 __
                         __)
                   | v3 :: l1 ->
                     x6 v f (v3 :: l1) (Efun ((Fcons (v0, f2, l0, e2, Fnil)),
                       (Eproj (v1, c, n, v2, e3)))) f0 r0
                       (fun v4 ft lv d1 d2 _ ->
                       x35 __ __ v4 v4 ft ft lv lv (Efun ((Fcons (v0, f2, l0,
                         e2, Fnil)), (Eproj (v1, c, n, v2, e3)))) d1 f0 d2 __
                         __))
                | Eletapp (v1, v2, f4, l1, e3) ->
                  (match l with
                   | [] ->
                     x6 v f [] (Efun ((Fcons (v0, f2, l0, e2, Fnil)),
                       (Eletapp (v1, v2, f4, l1, e3)))) f0 r0
                       (fun v3 ft lv d1 d2 _ ->
                       x35 __ __ v3 v3 ft ft lv lv (Efun ((Fcons (v0, f2, l0,
                         e2, Fnil)), (Eletapp (v1, v2, f4, l1, e3)))) d1 f0
                         d2 __ __)
                   | v3 :: l2 ->
                     x6 v f (v3 :: l2) (Efun ((Fcons (v0, f2, l0, e2, Fnil)),
                       (Eletapp (v1, v2, f4, l1, e3)))) f0 r0
                       (fun v4 ft lv d1 d2 _ ->
                       x35 __ __ v4 v4 ft ft lv lv (Efun ((Fcons (v0, f2, l0,
                         e2, Fnil)), (Eletapp (v1, v2, f4, l1, e3)))) d1 f0
                         d2 __ __))
                | Efun (f4, e3) ->
                  (match l with
                   | [] ->
                     x6 v f [] (Efun ((Fcons (v0, f2, l0, e2, Fnil)), (Efun
                       (f4, e3)))) f0 r0 (fun v1 ft lv d1 d2 _ ->
                       x35 __ __ v1 v1 ft ft lv lv (Efun ((Fcons (v0, f2, l0,
                         e2, Fnil)), (Efun (f4, e3)))) d1 f0 d2 __ __)
                   | v1 :: l1 ->
                     x6 v f (v1 :: l1) (Efun ((Fcons (v0, f2, l0, e2, Fnil)),
                       (Efun (f4, e3)))) f0 r0 (fun v2 ft lv d1 d2 _ ->
                       x35 __ __ v2 v2 ft ft lv lv (Efun ((Fcons (v0, f2, l0,
                         e2, Fnil)), (Efun (f4, e3)))) d1 f0 d2 __ __))
                | Eapp (v1, f4, l1) ->
                  (match l with
                   | [] ->
                     x6 v f [] (Efun ((Fcons (v0, f2, l0, e2, Fnil)), (Eapp
                       (v1, f4, l1)))) f0 r0 (fun v2 ft lv d1 d2 _ ->
                       x35 __ __ v2 v2 ft ft lv lv (Efun ((Fcons (v0, f2, l0,
                         e2, Fnil)), (Eapp (v1, f4, l1)))) d1 f0 d2 __ __)
                   | v2 :: l2 ->
                     (match l1 with
                      | [] ->
                        x6 v f (v2 :: l2) (Efun ((Fcons (v0, f2, l0, e2,
                          Fnil)), (Eapp (v1, f4, [])))) f0 r0
                          (fun v3 ft lv d1 d2 _ ->
                          x35 __ __ v3 v3 ft ft lv lv (Efun ((Fcons (v0, f2,
                            l0, e2, Fnil)), (Eapp (v1, f4, [])))) d1 f0 d2 __
                            __)
                      | v3 :: l3 ->
                        (match l3 with
                         | [] ->
                           x v f v2 l2 v0 f2 l0 e2 v1 f4 v3 f0 r0 s0 s1
                             (fun _ fds' d1 f5 ft k fv g gt gv ge k' kt g' fds f6 ft1 fv1 gv1 lhs fds'0 rhs fp_numargs ms ms' _ _ _ x37 x38 ->
                             x33 __ __ f5 f6 ft ft1 k k' kt fv fv1 g g' gt gv
                               gv1 ge fds lhs fds'0 rhs fp_numargs ms ms' __
                               fds' d1 v f v2 l2 v0 f2 l0 e2 v1 f4 v3 f0 __
                               __ __ x37 x38) (fun _ ->
                             x6 v f (v2 :: l2) (Efun ((Fcons (v0, f2, l0, e2,
                               Fnil)), (Eapp (v1, f4, (v3 :: []))))) f0 r0
                               (fun v4 ft lv d1 d2 _ ->
                               x35 __ __ v4 v4 ft ft lv lv (Efun ((Fcons (v0,
                                 f2, l0, e2, Fnil)), (Eapp (v1, f4,
                                 (v3 :: []))))) d1 f0 d2 __ __))
                         | v4 :: l4 ->
                           x6 v f (v2 :: l2) (Efun ((Fcons (v0, f2, l0, e2,
                             Fnil)), (Eapp (v1, f4, (v3 :: (v4 :: l4)))))) f0
                             r0 (fun v5 ft lv d1 d2 _ ->
                             x35 __ __ v5 v5 ft ft lv lv (Efun ((Fcons (v0,
                               f2, l0, e2, Fnil)), (Eapp (v1, f4,
                               (v3 :: (v4 :: l4)))))) d1 f0 d2 __ __))))
                | Eprim_val (v1, p, e3) ->
                  (match l with
                   | [] ->
                     x6 v f [] (Efun ((Fcons (v0, f2, l0, e2, Fnil)),
                       (Eprim_val (v1, p, e3)))) f0 r0
                       (fun v2 ft lv d1 d2 _ ->
                       x35 __ __ v2 v2 ft ft lv lv (Efun ((Fcons (v0, f2, l0,
                         e2, Fnil)), (Eprim_val (v1, p, e3)))) d1 f0 d2 __ __)
                   | v2 :: l1 ->
                     x6 v f (v2 :: l1) (Efun ((Fcons (v0, f2, l0, e2, Fnil)),
                       (Eprim_val (v1, p, e3)))) f0 r0
                       (fun v3 ft lv d1 d2 _ ->
                       x35 __ __ v3 v3 ft ft lv lv (Efun ((Fcons (v0, f2, l0,
                         e2, Fnil)), (Eprim_val (v1, p, e3)))) d1 f0 d2 __ __))
                | Eprim (v1, p, l1, e3) ->
                  (match l with
                   | [] ->
                     x6 v f [] (Efun ((Fcons (v0, f2, l0, e2, Fnil)), (Eprim
                       (v1, p, l1, e3)))) f0 r0 (fun v2 ft lv d1 d2 _ ->
                       x35 __ __ v2 v2 ft ft lv lv (Efun ((Fcons (v0, f2, l0,
                         e2, Fnil)), (Eprim (v1, p, l1, e3)))) d1 f0 d2 __ __)
                   | v2 :: l2 ->
                     x6 v f (v2 :: l2) (Efun ((Fcons (v0, f2, l0, e2, Fnil)),
                       (Eprim (v1, p, l1, e3)))) f0 r0
                       (fun v3 ft lv d1 d2 _ ->
                       x35 __ __ v3 v3 ft ft lv lv (Efun ((Fcons (v0, f2, l0,
                         e2, Fnil)), (Eprim (v1, p, l1, e3)))) d1 f0 d2 __ __))
                | Ehalt v1 ->
                  (match l with
                   | [] ->
                     x0 v f [] v0 f2 l0 e2 v1 f0 r0 s0 s1
                       (fun _ fds' d1 f4 ft fv g gt gv ge g' fds f5 ft1 fv1 gv1 lhs fds'0 rhs fp_numargs ms ms' _ _ _ x37 x38 ->
                       x34 __ __ f4 f5 ft ft1 fv fv1 g g' gt gv gv1 ge fds
                         lhs fds'0 rhs fp_numargs ms ms' __ fds' d1 v f [] v0
                         f2 l0 e2 v1 f0 __ __ __ x37 x38) (fun _ ->
                       x6 v f [] (Efun ((Fcons (v0, f2, l0, e2, Fnil)),
                         (Ehalt v1))) f0 r0 (fun v2 ft lv d1 d2 _ ->
                         x35 __ __ v2 v2 ft ft lv lv (Efun ((Fcons (v0, f2,
                           l0, e2, Fnil)), (Ehalt v1))) d1 f0 d2 __ __))
                   | v2 :: l1 ->
                     x0 v f (v2 :: l1) v0 f2 l0 e2 v1 f0 r0 s0 s1
                       (fun _ fds' d1 f4 ft fv g gt gv ge g' fds f5 ft1 fv1 gv1 lhs fds'0 rhs fp_numargs ms ms' _ _ _ x37 x38 ->
                       x34 __ __ f4 f5 ft ft1 fv fv1 g g' gt gv gv1 ge fds
                         lhs fds'0 rhs fp_numargs ms ms' __ fds' d1 v f
                         (v2 :: l1) v0 f2 l0 e2 v1 f0 __ __ __ x37 x38)
                       (fun _ ->
                       x6 v f (v2 :: l1) (Efun ((Fcons (v0, f2, l0, e2,
                         Fnil)), (Ehalt v1))) f0 r0 (fun v3 ft lv d1 d2 _ ->
                         x35 __ __ v3 v3 ft ft lv lv (Efun ((Fcons (v0, f2,
                           l0, e2, Fnil)), (Ehalt v1))) d1 f0 d2 __ __)))))
          | Fnil ->
            (match l with
             | [] ->
               x6 v f [] (Efun (Fnil, e1)) f0 r0 (fun v0 ft lv d1 d2 _ ->
                 x35 __ __ v0 v0 ft ft lv lv (Efun (Fnil, e1)) d1 f0 d2 __ __)
             | v0 :: l0 ->
               x6 v f (v0 :: l0) (Efun (Fnil, e1)) f0 r0
                 (fun v1 ft lv d1 d2 _ ->
                 x35 __ __ v1 v1 ft ft lv lv (Efun (Fnil, e1)) d1 f0 d2 __ __)))
       | Eapp (v0, f1, l0) ->
         (match l with
          | [] ->
            x6 v f [] (Eapp (v0, f1, l0)) f0 r0 (fun v1 ft lv d1 d2 _ ->
              x35 __ __ v1 v1 ft ft lv lv (Eapp (v0, f1, l0)) d1 f0 d2 __ __)
          | v1 :: l1 ->
            x6 v f (v1 :: l1) (Eapp (v0, f1, l0)) f0 r0
              (fun v2 ft lv d1 d2 _ ->
              x35 __ __ v2 v2 ft ft lv lv (Eapp (v0, f1, l0)) d1 f0 d2 __ __))
       | Eprim_val (v0, p, e1) ->
         (match l with
          | [] ->
            x6 v f [] (Eprim_val (v0, p, e1)) f0 r0 (fun v1 ft lv d1 d2 _ ->
              x35 __ __ v1 v1 ft ft lv lv (Eprim_val (v0, p, e1)) d1 f0 d2 __
                __)
          | v1 :: l0 ->
            x6 v f (v1 :: l0) (Eprim_val (v0, p, e1)) f0 r0
              (fun v2 ft lv d1 d2 _ ->
              x35 __ __ v2 v2 ft ft lv lv (Eprim_val (v0, p, e1)) d1 f0 d2 __
                __))
       | Eprim (v0, p, l0, e1) ->
         (match l with
          | [] ->
            x6 v f [] (Eprim (v0, p, l0, e1)) f0 r0 (fun v1 ft lv d1 d2 _ ->
              x35 __ __ v1 v1 ft ft lv lv (Eprim (v0, p, l0, e1)) d1 f0 d2 __
                __)
          | v1 :: l1 ->
            x6 v f (v1 :: l1) (Eprim (v0, p, l0, e1)) f0 r0
              (fun v2 ft lv d1 d2 _ ->
              x35 __ __ v2 v2 ft ft lv lv (Eprim (v0, p, l0, e1)) d1 f0 d2 __
                __))
       | Ehalt v0 ->
         (match l with
          | [] ->
            x6 v f [] (Ehalt v0) f0 r0 (fun v1 ft lv d1 d2 _ ->
              x35 __ __ v1 v1 ft ft lv lv (Ehalt v0) d1 f0 d2 __ __)
          | v1 :: l0 ->
            x6 v f (v1 :: l0) (Ehalt v0) f0 r0 (fun v2 ft lv d1 d2 _ ->
              x35 __ __ v2 v2 ft ft lv lv (Ehalt v0) d1 f0 d2 __ __)))
    | Fnil -> x7 r0 (fun _ -> x36 __ __ __ __)
  in
  let x34 = fun d0 r0 _ _ x34 x35 x36 x37 x38 x39 x40 x41 x42 ->
    match d0 with
    | Econstr (v, c, l, e0) ->
      x8 v c l e0 r0 (fun v0 ct lv d1 _ ->
        x34 __ __ v0 v0 ct ct lv lv e0 d1 __ __)
    | Ecase (v, l) -> x9 v l r0 (fun v0 d1 _ -> x35 __ __ v0 v0 l d1 __ __)
    | Eproj (v, c, n, v0, e0) ->
      x10 v c n v0 e0 r0 (fun v1 ct n0 v2 d1 _ ->
        x36 __ __ v1 v1 ct ct n0 n0 v2 v2 e0 d1 __ __)
    | Eletapp (v, v0, f, l, e0) ->
      x11 v v0 f l e0 r0 (fun v1 v2 ft lv d1 _ ->
        x37 __ __ v1 v1 v2 v2 ft ft lv lv e0 d1 __ __)
    | Efun (f, e0) -> x12 f e0 r0 (fun d1 d2 _ -> x38 __ __ f d1 e0 d2 __ __)
    | Eapp (v, f, l) ->
      x13 v f l r0 (fun v0 ft lv _ -> x39 __ __ v0 v0 ft ft lv lv __ __)
    | Eprim_val (v, p, e0) ->
      x14 v p e0 r0 (fun v0 p0 d1 _ -> x40 __ __ v0 v0 p0 p0 e0 d1 __ __)
    | Eprim (v, p, l, e0) ->
      x15 v p l e0 r0 (fun v0 p0 lv d1 _ ->
        x41 __ __ v0 v0 p0 p0 lv lv e0 d1 __ __)
    | Ehalt v -> x16 v r0 (fun v0 _ -> x42 __ __ v0 v0 __ __)
  in
  let x35 = fun _ _ _ x35 -> x35 __ in
  let x36 = fun _ _ _ x36 -> x36 __ in
  let x37 = fun _ _ _ x37 -> x37 __ in
  let x38 = fun _ _ _ x38 -> x38 __ in
  let rec fixS_F _ a0 e0 d0 r0 s0 =
    match a0 with
    | Coq_exp_univ_prod_ctor_tag_exp ->
      let { resTree = resTree0; resState = resState0 } =
        Obj.magic x31 e0 d0 r0 s0 (fun _ _ _ ct e1 d1 _ _ ->
          Obj.magic x17 ct e1 (fun _ _ _ _ r1 s1 _ ->
            let a1 = Coq_exp_univ_exp in fixS_F __ a1 e1 d1 r1 s1)
            (fun _ _ _ _ _ s1 _ -> { resTree = ct; resState = s1 }) r0 s0)
      in
      x35 resTree0 r0 resState0 (fun _ -> { resTree = resTree0; resState =
        resState0 })
    | Coq_exp_univ_list_prod_ctor_tag_exp ->
      let { resTree = resTree0; resState = resState0 } =
        Obj.magic x32 e0 d0 r0 s0 (fun _ _ _ _ -> x18 r0 s0)
          (fun _ _ pcte d1 lpcte d2 _ _ ->
          Obj.magic x19 pcte lpcte (fun _ _ _ _ r1 s1 _ ->
            let a1 = Coq_exp_univ_list_prod_ctor_tag_exp in
            fixS_F __ a1 lpcte d2 r1 s1) (fun _ _ _ _ r1 s1 _ ->
            let a1 = Coq_exp_univ_prod_ctor_tag_exp in
            fixS_F __ a1 pcte d1 r1 s1) r0 s0)
      in
      x36 resTree0 r0 resState0 (fun _ -> { resTree = resTree0; resState =
        resState0 })
    | Coq_exp_univ_fundefs ->
      let { resTree = resTree0; resState = resState0 } =
        Obj.magic x33 e0 d0 r0 s0
          (fun _ _ f f1 ft ft1 k k' kt fv fv1 g g' gt gv gv1 ge fds lhs fds' rhs fp_numargs ms ms' _ fds'0 d1 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ x39 x40 ->
          x1 f f1 ft ft1 k k' kt fv fv1 g g' gt gv gv1 ge fds lhs fds' rhs
            fp_numargs ms ms' x39 x40 (fun _ _ r1 s1 _ ->
            Obj.magic x20 f ft (k :: fv1) (Efun ((Fcons (g, gt, gv1, (Eapp
              (f1, ft1, (app gv1 fv1))), Fnil)), (Eapp (k, kt, (g :: [])))))
              (coq_Rec fds') (fun _ _ _ _ _ _ _ r2 s2 _ ->
              let a1 = Coq_exp_univ_fundefs in fixS_F __ a1 fds'0 d1 r2 s2)
              (fun _ _ _ _ _ _ _ r2 s2 _ ->
              Obj.magic x26 (Fcons (g, gt, gv1, (Eapp (f1, ft1,
                (app gv1 fv1))), Fnil)) (Eapp (k, kt, (g :: [])))
                (fun _ _ _ _ r3 s3 _ ->
                Obj.magic x27 k kt (g :: []) (fun _ _ _ _ _ _ s4 _ ->
                  { resTree = (Obj.magic (g :: [])); resState = s4 })
                  (fun _ _ _ _ _ _ s4 _ -> { resTree = (Obj.magic kt);
                  resState = s4 }) (fun _ _ _ _ _ _ s4 _ -> { resTree =
                  (Obj.magic k); resState = s4 }) r3 s3)
                (fun _ _ _ _ r3 s3 _ ->
                Obj.magic x20 g gt gv1 (Eapp (f1, ft1, (app gv1 fv1))) Fnil
                  (fun _ _ _ _ _ _ _ r4 s4 _ -> x21 r4 s4)
                  (fun _ _ _ _ _ _ _ r4 s4 _ ->
                  Obj.magic x27 f1 ft1 (app gv1 fv1) (fun _ _ _ _ _ _ s5 _ ->
                    { resTree = (Obj.magic app gv1 fv1); resState = s5 })
                    (fun _ _ _ _ _ _ s5 _ -> { resTree = (Obj.magic ft1);
                    resState = s5 }) (fun _ _ _ _ _ _ s5 _ -> { resTree =
                    (Obj.magic f1); resState = s5 }) r4 s4)
                  (fun _ _ _ _ _ _ _ _ s4 _ -> { resTree = (Obj.magic gv1);
                  resState = s4 }) (fun _ _ _ _ _ _ _ _ s4 _ -> { resTree =
                  (Obj.magic gt); resState = s4 })
                  (fun _ _ _ _ _ _ _ _ s4 _ -> { resTree = (Obj.magic g);
                  resState = s4 }) r3 s3) r2 s2) (fun _ _ _ _ _ _ _ _ s2 _ ->
              { resTree = (Obj.magic (k :: fv1)); resState = s2 })
              (fun _ _ _ _ _ _ _ _ s2 _ -> { resTree = (Obj.magic ft);
              resState = s2 }) (fun _ _ _ _ _ _ _ _ s2 _ -> { resTree =
              (Obj.magic f); resState = s2 }) r1 s1) x39 s0)
          (fun _ _ f f1 ft ft1 fv fv1 g g' gt gv gv1 ge fds lhs fds' rhs fp_numargs ms ms' _ fds'0 d1 _ _ _ _ _ _ _ _ _ _ _ _ x39 x40 ->
          x2 f f1 ft ft1 fv fv1 g g' gt gv gv1 ge fds lhs fds' rhs fp_numargs
            ms ms' x39 x40 (fun _ _ r1 s1 _ ->
            Obj.magic x20 f ft fv1 (Efun ((Fcons (g, gt, gv1, (Eapp (f1, ft1,
              (app gv1 fv1))), Fnil)), (Ehalt g))) (coq_Rec fds')
              (fun _ _ _ _ _ _ _ r2 s2 _ ->
              let a1 = Coq_exp_univ_fundefs in fixS_F __ a1 fds'0 d1 r2 s2)
              (fun _ _ _ _ _ _ _ r2 s2 _ ->
              Obj.magic x26 (Fcons (g, gt, gv1, (Eapp (f1, ft1,
                (app gv1 fv1))), Fnil)) (Ehalt g) (fun _ _ _ _ r3 s3 _ ->
                Obj.magic x30 g (fun _ _ _ _ s4 _ -> { resTree =
                  (Obj.magic g); resState = s4 }) r3 s3)
                (fun _ _ _ _ r3 s3 _ ->
                Obj.magic x20 g gt gv1 (Eapp (f1, ft1, (app gv1 fv1))) Fnil
                  (fun _ _ _ _ _ _ _ r4 s4 _ -> x21 r4 s4)
                  (fun _ _ _ _ _ _ _ r4 s4 _ ->
                  Obj.magic x27 f1 ft1 (app gv1 fv1) (fun _ _ _ _ _ _ s5 _ ->
                    { resTree = (Obj.magic app gv1 fv1); resState = s5 })
                    (fun _ _ _ _ _ _ s5 _ -> { resTree = (Obj.magic ft1);
                    resState = s5 }) (fun _ _ _ _ _ _ s5 _ -> { resTree =
                    (Obj.magic f1); resState = s5 }) r4 s4)
                  (fun _ _ _ _ _ _ _ _ s4 _ -> { resTree = (Obj.magic gv1);
                  resState = s4 }) (fun _ _ _ _ _ _ _ _ s4 _ -> { resTree =
                  (Obj.magic gt); resState = s4 })
                  (fun _ _ _ _ _ _ _ _ s4 _ -> { resTree = (Obj.magic g);
                  resState = s4 }) r3 s3) r2 s2) (fun _ _ _ _ _ _ _ _ s2 _ ->
              { resTree = (Obj.magic fv1); resState = s2 })
              (fun _ _ _ _ _ _ _ _ s2 _ -> { resTree = (Obj.magic ft);
              resState = s2 }) (fun _ _ _ _ _ _ _ _ s2 _ -> { resTree =
              (Obj.magic f); resState = s2 }) r1 s1) x39 s0)
          (fun _ _ _ v _ ft _ lv e1 d1 f d2 _ _ ->
          Obj.magic x20 v ft lv e1 f (fun _ _ _ _ _ _ _ r1 s1 _ ->
            let a1 = Coq_exp_univ_fundefs in fixS_F __ a1 f d2 r1 s1)
            (fun _ _ _ _ _ _ _ r1 s1 _ ->
            let a1 = Coq_exp_univ_exp in fixS_F __ a1 e1 d1 r1 s1)
            (fun _ _ _ _ _ _ _ _ s1 _ -> { resTree = (Obj.magic lv);
            resState = s1 }) (fun _ _ _ _ _ _ _ _ s1 _ -> { resTree =
            (Obj.magic ft); resState = s1 }) (fun _ _ _ _ _ _ _ _ s1 _ ->
            { resTree = (Obj.magic v); resState = s1 }) r0 s0)
          (fun _ _ _ _ -> x21 r0 s0)
      in
      x37 resTree0 r0 resState0 (fun _ -> { resTree = resTree0; resState =
        resState0 })
    | Coq_exp_univ_exp ->
      let { resTree = resTree0; resState = resState0 } =
        Obj.magic x34 e0 d0 r0 s0 (fun _ _ _ v _ ct _ lv e1 d1 _ _ ->
          Obj.magic x22 v ct lv e1 (fun _ _ _ _ _ _ r1 s1 _ ->
            let a1 = Coq_exp_univ_exp in fixS_F __ a1 e1 d1 r1 s1)
            (fun _ _ _ _ _ _ _ s1 _ -> { resTree = (Obj.magic lv); resState =
            s1 }) (fun _ _ _ _ _ _ _ s1 _ -> { resTree = (Obj.magic ct);
            resState = s1 }) (fun _ _ _ _ _ _ _ s1 _ -> { resTree =
            (Obj.magic v); resState = s1 }) r0 s0)
          (fun _ _ _ v lpcte d1 _ _ ->
          Obj.magic x23 v lpcte (fun _ _ _ _ r1 s1 _ ->
            let a1 = Coq_exp_univ_list_prod_ctor_tag_exp in
            fixS_F __ a1 lpcte d1 r1 s1) (fun _ _ _ _ _ s1 _ -> { resTree =
            (Obj.magic v); resState = s1 }) r0 s0)
          (fun _ _ _ v _ ct _ n _ v0 e1 d1 _ _ ->
          Obj.magic x24 v ct n v0 e1 (fun _ _ _ _ _ _ _ r1 s1 _ ->
            let a1 = Coq_exp_univ_exp in fixS_F __ a1 e1 d1 r1 s1)
            (fun _ _ _ _ _ _ _ _ s1 _ -> { resTree = (Obj.magic v0);
            resState = s1 }) (fun _ _ _ _ _ _ _ _ s1 _ -> { resTree =
            (Obj.magic n); resState = s1 }) (fun _ _ _ _ _ _ _ _ s1 _ ->
            { resTree = (Obj.magic ct); resState = s1 })
            (fun _ _ _ _ _ _ _ _ s1 _ -> { resTree = (Obj.magic v);
            resState = s1 }) r0 s0) (fun _ _ _ v _ v0 _ ft _ lv e1 d1 _ _ ->
          Obj.magic x25 v v0 ft lv e1 (fun _ _ _ _ _ _ _ r1 s1 _ ->
            let a1 = Coq_exp_univ_exp in fixS_F __ a1 e1 d1 r1 s1)
            (fun _ _ _ _ _ _ _ _ s1 _ -> { resTree = (Obj.magic lv);
            resState = s1 }) (fun _ _ _ _ _ _ _ _ s1 _ -> { resTree =
            (Obj.magic ft); resState = s1 }) (fun _ _ _ _ _ _ _ _ s1 _ ->
            { resTree = (Obj.magic v0); resState = s1 })
            (fun _ _ _ _ _ _ _ _ s1 _ -> { resTree = (Obj.magic v);
            resState = s1 }) r0 s0) (fun _ _ f d1 e1 d2 _ _ ->
          Obj.magic x26 f e1 (fun _ _ _ _ r1 s1 _ ->
            let a1 = Coq_exp_univ_exp in fixS_F __ a1 e1 d2 r1 s1)
            (fun _ _ _ _ r1 s1 _ ->
            let a1 = Coq_exp_univ_fundefs in fixS_F __ a1 f d1 r1 s1) r0 s0)
          (fun _ _ _ v _ ft _ lv _ _ ->
          Obj.magic x27 v ft lv (fun _ _ _ _ _ _ s1 _ -> { resTree =
            (Obj.magic lv); resState = s1 }) (fun _ _ _ _ _ _ s1 _ ->
            { resTree = (Obj.magic ft); resState = s1 })
            (fun _ _ _ _ _ _ s1 _ -> { resTree = (Obj.magic v); resState =
            s1 }) r0 s0) (fun _ _ _ v _ p e1 d1 _ _ ->
          Obj.magic x28 v p e1 (fun _ _ _ _ _ r1 s1 _ ->
            let a1 = Coq_exp_univ_exp in fixS_F __ a1 e1 d1 r1 s1)
            (fun _ _ _ _ _ _ s1 _ -> { resTree = (Obj.magic p); resState =
            s1 }) (fun _ _ _ _ _ _ s1 _ -> { resTree = (Obj.magic v);
            resState = s1 }) r0 s0) (fun _ _ _ v _ p _ lv e1 d1 _ _ ->
          Obj.magic x29 v p lv e1 (fun _ _ _ _ _ _ r1 s1 _ ->
            let a1 = Coq_exp_univ_exp in fixS_F __ a1 e1 d1 r1 s1)
            (fun _ _ _ _ _ _ _ s1 _ -> { resTree = (Obj.magic lv); resState =
            s1 }) (fun _ _ _ _ _ _ _ s1 _ -> { resTree = (Obj.magic p);
            resState = s1 }) (fun _ _ _ _ _ _ _ s1 _ -> { resTree =
            (Obj.magic v); resState = s1 }) r0 s0) (fun _ _ _ v _ _ ->
          Obj.magic x30 v (fun _ _ _ _ s1 _ -> { resTree = (Obj.magic v);
            resState = s1 }) r0 s0)
      in
      x38 resTree0 r0 resState0 (fun _ -> { resTree = resTree0; resState =
        resState0 })
    | _ -> { resTree = e0; resState = s0 }
  in fixS_F __ a e d r s

(** val uncurry_top : bool -> exp -> comp_data -> exp error * comp_data **)

let uncurry_top cps e c =
  let r = Pos.ltb_spec0 (max_var e Coq_xH) c.next_var in
  (match r with
   | ReflectT ->
     let res =
       run_rewriter' Coq_exp_univ_exp false
         (Obj.magic (fun a _ e0 -> univ_size a e0)) { delayD = (fun _ x _ ->
         x); delay_id = (fun _ _ -> ()) } (fun _ _ _ _ _ x -> x)
         (fun _ _ _ _ _ _ h -> h) (fun _ _ _ _ _ _ h -> h)
         (fun x x0 _ _ x1 x2 _ _ x3 x4 _ -> rw_uncurry x x0 x1 x2 x3 x4)
         (Obj.magic e) cps (((((false, M.empty), M.empty), (O, M.empty)), c),
         c.next_var)
     in
     let { resTree = resTree0; resState = resState0 } = res in
     let (s, v) = resState0 in
     let (p, c0) = s in
     let (_, s0) = p in
     ((Ret (Obj.magic resTree0)), { next_var = v; nect_ctor_tag =
     c0.nect_ctor_tag; next_ind_tag = c0.next_ind_tag; next_fun_tag =
     c0.next_fun_tag; cenv = c0.cenv; fenv = c0.fenv; nenv = c0.nenv;
     inline_map = (snd s0); log = c0.log })
   | ReflectF ->
     ((Err (String.String (Coq_x75, (String.String (Coq_x6e, (String.String
       (Coq_x63, (String.String (Coq_x75, (String.String (Coq_x72,
       (String.String (Coq_x72, (String.String (Coq_x79, (String.String
       (Coq_x5f, (String.String (Coq_x74, (String.String (Coq_x6f,
       (String.String (Coq_x70, (String.String (Coq_x3a, (String.String
       (Coq_x20, (String.String (Coq_x6d, (String.String (Coq_x61,
       (String.String (Coq_x78, (String.String (Coq_x5f, (String.String
       (Coq_x76, (String.String (Coq_x61, (String.String (Coq_x72,
       (String.String (Coq_x20, (String.String (Coq_x63, (String.String
       (Coq_x6f, (String.String (Coq_x6d, (String.String (Coq_x70,
       (String.String (Coq_x75, (String.String (Coq_x74, (String.String
       (Coq_x61, (String.String (Coq_x74, (String.String (Coq_x69,
       (String.String (Coq_x6f, (String.String (Coq_x6e, (String.String
       (Coq_x20, (String.String (Coq_x66, (String.String (Coq_x61,
       (String.String (Coq_x69, (String.String (Coq_x6c, (String.String
       (Coq_x65, (String.String (Coq_x64,
       String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))),
       c))
