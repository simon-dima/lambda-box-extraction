open BinNat
open BinNums
open BinPos
open Bool
open Byte
open Datatypes
open List0
open List_util
open Monad0
open Nat0
open OrdersEx
open Bytestring
open CompM
open Cps
open Set_util
open State

type live_fun = bool list M.t

(** val get_fun_vars : live_fun -> var -> bool list option **)

let get_fun_vars m f =
  M.get f m

(** val set_fun_vars : live_fun -> var -> bool list -> bool list M.tree **)

let set_fun_vars m f b =
  M.set f b m

(** val live_args : 'a1 list -> bool list -> 'a1 list **)

let rec live_args ys = function
| [] -> ys
| b :: bs' ->
  (match ys with
   | [] -> ys
   | y :: ys' -> if b then y :: (live_args ys' bs') else live_args ys' bs')

(** val get_bool_false : 'a1 list -> bool list **)

let rec get_bool_false = function
| [] -> []
| _ :: ys' -> false :: (get_bool_false ys')

(** val init_live_fun_aux : live_fun -> fundefs -> live_fun **)

let rec init_live_fun_aux m = function
| Fcons (f, _, xs, _, b') ->
  init_live_fun_aux (set_fun_vars m f (get_bool_false xs)) b'
| Fnil -> m

(** val init_live_fun : fundefs -> live_fun **)

let init_live_fun b =
  init_live_fun_aux M.empty b

(** val remove_escaping : live_fun -> var -> live_fun **)

let remove_escaping l f =
  match get_fun_vars l f with
  | Some _ -> M.remove f l
  | None -> l

(** val remove_escapings : live_fun -> var list -> live_fun **)

let remove_escapings l fs =
  fold_left remove_escaping fs l

(** val escaping_fun_exp : exp -> live_fun -> live_fun **)

let rec escaping_fun_exp e l =
  match e with
  | Econstr (_, _, ys, e') -> escaping_fun_exp e' (remove_escapings l ys)
  | Ecase (_, p) ->
    fold_left (fun l0 pat -> let (_, e') = pat in escaping_fun_exp e' l0) p l
  | Eproj (_, _, _, y, e') -> escaping_fun_exp e' (remove_escaping l y)
  | Eletapp (_, _, _, ys, e') -> escaping_fun_exp e' (remove_escapings l ys)
  | Efun (fl, e') -> escaping_fun_exp e' (escaping_fun_fundefs fl l)
  | Eapp (_, _, ys) -> remove_escapings l ys
  | Eprim_val (_, _, e') -> escaping_fun_exp e' l
  | Eprim (_, _, ys, e') -> escaping_fun_exp e' (remove_escapings l ys)
  | Ehalt x -> remove_escaping l x

(** val escaping_fun_fundefs : fundefs -> live_fun -> live_fun **)

and escaping_fun_fundefs b l =
  match b with
  | Fcons (_, _, _, e, b') ->
    let l' = escaping_fun_exp e l in escaping_fun_fundefs b' l'
  | Fnil -> l

(** val add_fun_vars : live_fun -> var -> var list -> PS.t -> PS.t **)

let add_fun_vars l f xs s =
  match get_fun_vars l f with
  | Some bs -> let xs' = live_args xs bs in union_list s xs'
  | None -> union_list s xs

(** val live_expr : live_fun -> exp -> PS.t -> PS.t **)

let rec live_expr l e s =
  match e with
  | Econstr (_, _, ys, e') -> live_expr l e' (union_list s ys)
  | Ecase (x, p) ->
    PS.add x
      (fold_left (fun s0 pat -> let (_, e') = pat in live_expr l e' s0) p s)
  | Eproj (_, _, _, y, e') -> live_expr l e' (PS.add y s)
  | Eletapp (_, f, _, ys, e') ->
    let s' = PS.add f s in
    let s'' = add_fun_vars l f ys s' in live_expr l e' s''
  | Efun (_, _) -> s
  | Eapp (f, _, ys) -> let s' = PS.add f s in add_fun_vars l f ys s'
  | Eprim_val (_, _, e') -> live_expr l e' s
  | Eprim (_, _, ys, e') -> live_expr l e' (union_list s ys)
  | Ehalt x -> PS.add x s

(** val update_bs : PS.t -> PS.elt list -> bool list -> bool list * bool **)

let rec update_bs s xs bs =
  match xs with
  | [] -> (bs, false)
  | x :: xs0 ->
    (match bs with
     | [] -> (bs, false)
     | b :: bs0 ->
       let (bs', d) = update_bs s xs0 bs0 in
       if b
       then ((b :: bs'), d)
       else let b' = PS.mem x s in
            ((b' :: bs'), ((||) (negb (Bool.eqb b b')) d)))

(** val update_live_fun :
    live_fun -> var -> var list -> PS.t -> live_fun * bool **)

let update_live_fun l f xs s =
  match get_fun_vars l f with
  | Some bs ->
    let (bs0, diff) = update_bs s xs bs in
    if diff then ((set_fun_vars l f bs0), diff) else (l, diff)
  | None -> (l, false)

(** val live : fundefs -> live_fun -> bool -> live_fun * bool **)

let rec live b l diff =
  match b with
  | Fcons (f, _, xs, e, b') ->
    let s = live_expr l e PS.empty in
    let (l', d) = update_live_fun l f xs s in live b' l' ((||) d diff)
  | Fnil -> (l, diff)

(** val find_live_helper : fundefs -> live_fun -> nat -> live_fun error **)

let rec find_live_helper b prev_L = function
| O -> Ret prev_L
| S n' ->
  let (curr_L, diff) = live b prev_L false in
  if diff then find_live_helper b curr_L n' else Ret curr_L

(** val num_vars : fundefs -> nat -> nat **)

let rec num_vars b n =
  match b with
  | Fcons (_, _, xs, _, b') -> num_vars b' (add n (length xs))
  | Fnil -> n

(** val find_live : exp -> live_fun error **)

let find_live = function
| Efun (b, e') ->
  let initial_L = init_live_fun b in
  let l' = escaping_fun_exp e' (escaping_fun_fundefs b initial_L) in
  let n = add (num_vars b O) (S O) in find_live_helper b l' n
| _ -> Ret M.empty

type arityMap = fun_tag M.t

type ftagMap = fun_tag M.t

type 'a elimM = (unit, 'a) compM'

(** val make_arityMap : exp -> arityMap -> arityMap **)

let rec make_arityMap e m =
  match e with
  | Econstr (_, _, _, e0) -> make_arityMap e0 m
  | Ecase (_, bs) -> fold_left (fun m0 p -> make_arityMap (snd p) m0) bs m
  | Eproj (_, _, _, _, e0) -> make_arityMap e0 m
  | Eletapp (_, _, ft, xs, e0) ->
    make_arityMap e0 (M.set (Positive_as_DT.of_succ_nat (length xs)) ft m)
  | Efun (b, e0) -> make_arityMap e0 (make_arityMap_fundefs b m)
  | Eapp (_, ft, xs) -> M.set (Positive_as_DT.of_succ_nat (length xs)) ft m
  | Eprim_val (_, _, e0) -> make_arityMap e0 m
  | Eprim (_, _, _, e0) -> make_arityMap e0 m
  | Ehalt _ -> m

(** val make_arityMap_fundefs : fundefs -> arityMap -> arityMap **)

and make_arityMap_fundefs b m =
  match b with
  | Fcons (_, ft, xs, e, b0) ->
    let m0 = M.set (Positive_as_DT.of_succ_nat (length xs)) ft m in
    make_arityMap_fundefs b0 (make_arityMap e m0)
  | Fnil -> m

(** val make_ftag : nat -> comp_data -> fun_tag * comp_data **)

let make_ftag arity c =
  let { next_var = x; nect_ctor_tag = c0; next_ind_tag = i; next_fun_tag = f;
    cenv = e; fenv = fenv0; nenv = names; inline_map = imap; log = log0 } = c
  in
  (f, { next_var = x; nect_ctor_tag = c0; next_ind_tag = i; next_fun_tag =
  (Pos.add f Coq_xH); cenv = e; fenv =
  (M.set f ((N.of_nat arity), (fromN N0 arity)) fenv0); nenv = names;
  inline_map = imap; log = log0 })

(** val create_fun_tag :
    live_fun -> arityMap -> fundefs -> comp_data -> ftagMap ->
    ftagMap * comp_data **)

let rec create_fun_tag l m b c fmap =
  match b with
  | Fcons (f, _, ys, _, b0) ->
    (match get_fun_vars l f with
     | Some bs ->
       let n = length (live_args ys bs) in
       let p = Positive_as_DT.of_succ_nat n in
       (match M.get p m with
        | Some t0 -> create_fun_tag l m b0 c (M.set f t0 fmap)
        | None ->
          let (ft, c') = make_ftag n c in
          create_fun_tag l m b0 c' (M.set f ft fmap))
     | None -> create_fun_tag l m b0 c fmap)
  | Fnil -> (fmap, c)

(** val is_hoisted_exp : exp -> bool **)

let rec is_hoisted_exp = function
| Econstr (_, _, _, e0) -> is_hoisted_exp e0
| Ecase (_, bs) -> forallb (fun p -> is_hoisted_exp (snd p)) bs
| Eproj (_, _, _, _, e0) -> is_hoisted_exp e0
| Eletapp (_, _, _, _, e0) -> is_hoisted_exp e0
| Efun (_, _) -> false
| Eprim_val (_, _, e0) -> is_hoisted_exp e0
| Eprim (_, _, _, e0) -> is_hoisted_exp e0
| _ -> true

(** val is_hoisted_fundefs : fundefs -> bool **)

let rec is_hoisted_fundefs = function
| Fcons (_, _, _, e, b0) -> (&&) (is_hoisted_exp e) (is_hoisted_fundefs b0)
| Fnil -> true

(** val is_hoisted : exp -> bool **)

let is_hoisted e = match e with
| Efun (b, e0) -> (&&) (is_hoisted_fundefs b) (is_hoisted_exp e0)
| _ -> is_hoisted_exp e

(** val get_fun_tag : ftagMap -> var -> fun_tag **)

let get_fun_tag fmap f =
  match M.get f fmap with
  | Some t0 -> t0
  | None -> Coq_xH

(** val eliminate_expr : ftagMap -> live_fun -> exp -> exp elimM **)

let rec eliminate_expr fmap l e = match e with
| Econstr (x, t0, ys, e') ->
  bind (coq_MonadErrorT coq_MonadState) (eliminate_expr fmap l e')
    (fun e'' ->
    ret (coq_MonadErrorT coq_MonadState) (Econstr (x, t0, ys, e'')))
| Ecase (x, p) ->
  bind (coq_MonadErrorT coq_MonadState)
    (let rec mapM_LD = function
     | [] -> ret (coq_MonadErrorT coq_MonadState) []
     | p0 :: l' ->
       let (c', e') = p0 in
       bind (coq_MonadErrorT coq_MonadState) (eliminate_expr fmap l e')
         (fun e'0 ->
         bind (coq_MonadErrorT coq_MonadState) (mapM_LD l') (fun l'0 ->
           ret (coq_MonadErrorT coq_MonadState) ((c', e'0) :: l'0)))
     in mapM_LD p) (fun p' ->
    ret (coq_MonadErrorT coq_MonadState) (Ecase (x, p')))
| Eproj (x, t0, m, y, e') ->
  bind (coq_MonadErrorT coq_MonadState) (eliminate_expr fmap l e')
    (fun e'' ->
    ret (coq_MonadErrorT coq_MonadState) (Eproj (x, t0, m, y, e'')))
| Eletapp (x, f, ft, ys, e') ->
  (match get_fun_vars l f with
   | Some bs ->
     let ys' = live_args ys bs in
     bind (coq_MonadErrorT coq_MonadState) (eliminate_expr fmap l e')
       (fun e'' ->
       let ft' = get_fun_tag fmap f in
       ret (coq_MonadErrorT coq_MonadState) (Eletapp (x, f, ft', ys', e'')))
   | None ->
     bind (coq_MonadErrorT coq_MonadState) (eliminate_expr fmap l e')
       (fun e'' ->
       ret (coq_MonadErrorT coq_MonadState) (Eletapp (x, f, ft, ys, e''))))
| Efun (_, _) -> ret (coq_MonadErrorT coq_MonadState) e
| Eapp (f, ft, ys) ->
  (match get_fun_vars l f with
   | Some bs ->
     let ys' = live_args ys bs in
     let ft' = get_fun_tag fmap f in
     ret (coq_MonadErrorT coq_MonadState) (Eapp (f, ft', ys'))
   | None -> ret (coq_MonadErrorT coq_MonadState) (Eapp (f, ft, ys)))
| Eprim_val (x, p, e') ->
  bind (coq_MonadErrorT coq_MonadState) (eliminate_expr fmap l e')
    (fun e'' -> ret (coq_MonadErrorT coq_MonadState) (Eprim_val (x, p, e'')))
| Eprim (x, f, ys, e') ->
  bind (coq_MonadErrorT coq_MonadState) (eliminate_expr fmap l e')
    (fun e'' -> ret (coq_MonadErrorT coq_MonadState) (Eprim (x, f, ys, e'')))
| Ehalt x -> ret (coq_MonadErrorT coq_MonadState) (Ehalt x)

(** val eliminate_fundefs :
    ftagMap -> live_fun -> fundefs -> fundefs elimM **)

let rec eliminate_fundefs fmap l = function
| Fcons (f, ft, ys, e, b') ->
  (match get_fun_vars l f with
   | Some bs ->
     let ys' = live_args ys bs in
     bind (coq_MonadErrorT coq_MonadState) (eliminate_expr fmap l e)
       (fun e' ->
       bind (coq_MonadErrorT coq_MonadState) (eliminate_fundefs fmap l b')
         (fun b'' ->
         let ft' = get_fun_tag fmap f in
         ret (coq_MonadErrorT coq_MonadState) (Fcons (f, ft', ys', e', b''))))
   | None ->
     bind (coq_MonadErrorT coq_MonadState) (eliminate_expr fmap l e)
       (fun e' ->
       bind (coq_MonadErrorT coq_MonadState) (eliminate_fundefs fmap l b')
         (fun b'' ->
         ret (coq_MonadErrorT coq_MonadState) (Fcons (f, ft, ys, e', b'')))))
| Fnil -> ret (coq_MonadErrorT coq_MonadState) Fnil

(** val coq_DPE : exp -> comp_data -> exp error * comp_data **)

let coq_DPE e c_data =
  if is_hoisted e
  then (match e with
        | Efun (b, e') ->
          (match find_live e with
           | Err s -> ((Ret e), (add_log s c_data))
           | Ret l ->
             let m = make_arityMap e M.empty in
             let (ftagMap0, c_data0) = create_fun_tag l m b c_data M.empty in
             let (e0, p) =
               run_compM (eliminate_fundefs ftagMap0 l b) c_data0 ()
             in
             (match e0 with
              | Err s -> let (c_data1, _) = p in ((Err s), c_data1)
              | Ret b' ->
                let (c_data1, _) = p in
                let (e1, p0) =
                  run_compM (eliminate_expr ftagMap0 l e') c_data1 ()
                in
                (match e1 with
                 | Err s -> let (c_data2, _) = p0 in ((Err s), c_data2)
                 | Ret e'' ->
                   let (c_data2, _) = p0 in ((Ret (Efun (b', e''))), c_data2))))
        | _ -> ((Ret e), c_data))
  else ((Ret e),
         (add_log (String.String (Coq_x49, (String.String (Coq_x6e,
           (String.String (Coq_x74, (String.String (Coq_x65, (String.String
           (Coq_x72, (String.String (Coq_x6e, (String.String (Coq_x61,
           (String.String (Coq_x6c, (String.String (Coq_x20, (String.String
           (Coq_x65, (String.String (Coq_x72, (String.String (Coq_x72,
           (String.String (Coq_x6f, (String.String (Coq_x72, (String.String
           (Coq_x3a, (String.String (Coq_x20, (String.String (Coq_x70,
           (String.String (Coq_x72, (String.String (Coq_x6f, (String.String
           (Coq_x67, (String.String (Coq_x72, (String.String (Coq_x61,
           (String.String (Coq_x6d, (String.String (Coq_x20, (String.String
           (Coq_x69, (String.String (Coq_x73, (String.String (Coq_x20,
           (String.String (Coq_x6e, (String.String (Coq_x6f, (String.String
           (Coq_x74, (String.String (Coq_x20, (String.String (Coq_x68,
           (String.String (Coq_x6f, (String.String (Coq_x69, (String.String
           (Coq_x73, (String.String (Coq_x74, (String.String (Coq_x65,
           (String.String (Coq_x64,
           String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
           c_data))
