open BinNums
open BinPos
open Datatypes
open List0
open List_util
open Nat0
open Specif
open CompM
open Cps
open Cps_util
open Ctx
open Identifiers
open Inline_letapp
open Map_util
open Rename
open Set_util
open State

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

type svalue =
| SVconstr of ctor_tag * var list
| SVfun of fun_tag * var list * exp

type ctx_map = svalue Cps.M.t

type r_map = var Cps.M.t

type c_map = nat Cps.M.t

type b_map = bool Cps.M.t

(** val getd : 'a1 -> positive -> 'a1 Cps.M.tree -> 'a1 **)

let getd d v sub0 =
  match Cps.M.get v sub0 with
  | Some e -> e
  | None -> d

(** val term_size : exp -> nat **)

let rec term_size = function
| Econstr (_, _, _, e0) -> add (S O) (term_size e0)
| Ecase (_, cl) ->
  add (S O)
    (fold_right (fun p n -> let (_, e0) = p in add n (term_size e0)) O cl)
| Eproj (_, _, _, _, e0) -> add (S O) (term_size e0)
| Eletapp (_, _, _, _, e0) -> add (S O) (term_size e0)
| Efun (fds, e0) -> add (add (S O) (funs_size fds)) (term_size e0)
| Eprim_val (_, _, e0) -> add (S O) (term_size e0)
| Eprim (_, _, _, e0) -> add (S O) (term_size e0)
| _ -> S O

(** val funs_size : fundefs -> nat **)

and funs_size = function
| Fcons (_, _, _, e, fds') -> add (add (S O) (funs_size fds')) (term_size e)
| Fnil -> S O

(** val update_census_list :
    r_map -> var list -> (var -> c_map -> nat) -> c_map -> c_map **)

let rec update_census_list sig0 ys fun_delta count =
  match ys with
  | [] -> count
  | y :: ys' ->
    let y' = apply_r sig0 y in
    update_census_list sig0 ys' fun_delta
      (Cps.M.set y' (fun_delta y' count) count)

(** val update_census :
    r_map -> exp -> (var -> c_map -> nat) -> c_map -> c_map **)

let rec update_census sig0 e fun_delta count =
  match e with
  | Econstr (_, _, ys, e0) ->
    let count' = update_census_list sig0 ys fun_delta count in
    update_census sig0 e0 fun_delta count'
  | Ecase (v, cl) ->
    let count' = update_census_list sig0 (v :: []) fun_delta count in
    fold_right (fun p c ->
      let (_, e0) = p in update_census sig0 e0 fun_delta c) count' cl
  | Eproj (_, _, _, y, e0) ->
    let count' = update_census_list sig0 (y :: []) fun_delta count in
    update_census sig0 e0 fun_delta count'
  | Eletapp (_, f, _, ys, e0) ->
    let count' = update_census_list sig0 (f :: ys) fun_delta count in
    update_census sig0 e0 fun_delta count'
  | Efun (fl, e0) ->
    let count' = update_census_f sig0 fl fun_delta count in
    update_census sig0 e0 fun_delta count'
  | Eapp (f, _, ys) -> update_census_list sig0 (f :: ys) fun_delta count
  | Eprim_val (_, _, e0) -> update_census sig0 e0 fun_delta count
  | Eprim (_, _, ys, e0) ->
    let count' = update_census_list sig0 ys fun_delta count in
    update_census sig0 e0 fun_delta count'
  | Ehalt v -> update_census_list sig0 (v :: []) fun_delta count

(** val update_census_f :
    r_map -> fundefs -> (var -> c_map -> nat) -> c_map -> c_map **)

and update_census_f sig0 fds fun_delta count =
  match fds with
  | Fcons (_, _, _, e, fds') ->
    let count' = update_census sig0 e fun_delta count in
    update_census_f sig0 fds' fun_delta count'
  | Fnil -> count

(** val init_census : exp -> c_map **)

let init_census e =
  update_census Cps.M.empty e (fun v c -> add (getd O v c) (S O)) Cps.M.empty

(** val dec_census : r_map -> exp -> c_map -> c_map **)

let dec_census sig0 e count =
  update_census sig0 e (fun v c -> sub (getd O v c) (S O)) count

(** val dec_census_list : r_map -> var list -> c_map -> c_map **)

let dec_census_list sig0 ys count =
  update_census_list sig0 ys (fun v c -> sub (getd O v c) (S O)) count

(** val dec_census_all_case : r_map -> (var * exp) list -> c_map -> c_map **)

let rec dec_census_all_case sig0 cl count =
  match cl with
  | [] -> count
  | p :: cl' ->
    let (_, e) = p in
    let count' = dec_census_all_case sig0 cl' count in
    dec_census sig0 e count'

(** val dec_census_case :
    r_map -> (var * exp) list -> var -> c_map -> c_map **)

let rec dec_census_case sig0 cl y count =
  match cl with
  | [] -> count
  | p :: cl' ->
    let (k, e) = p in
    if var_dec k y
    then dec_census_all_case sig0 cl' count
    else let count' = dec_census_case sig0 cl' y count in
         dec_census sig0 e count'

(** val update_count_inlined : var list -> var list -> c_map -> c_map **)

let rec update_count_inlined ys xs count =
  match ys with
  | [] -> count
  | y :: ys' ->
    (match xs with
     | [] -> count
     | x :: xs' ->
       let cy = getd O y count in
       let cx = getd O x count in
       update_count_inlined ys' xs'
         (Cps.M.set y (sub (add cy cx) (S O)) (Cps.M.set x O count)))

(** val update_count_letapp : var -> var -> c_map -> c_map **)

let update_count_letapp x' x count =
  if var_dec x x'
  then count
  else let count' = Cps.M.set x O count in
       let c_x = getd O x count in
       let c_x' = getd O x' count in
       Cps.M.set x' (sub (add c_x c_x') (S O)) count'

type 'a shrinkT = ((('a * nat) * c_map) * b_map, __) sigT

(** val precontractfun :
    r_map -> c_map -> ctx_map -> fundefs -> (fundefs * c_map) * ctx_map **)

let rec precontractfun sig0 count sub0 = function
| Fcons (f, t0, ys, e, fds') ->
  (match getd O f count with
   | O ->
     let count' = dec_census sig0 e count in
     precontractfun sig0 count' sub0 fds'
   | S _ ->
     let (p, sub') = precontractfun sig0 count sub0 fds' in
     let (fds'', count') = p in
     (((Fcons (f, t0, ys, e, fds'')), count'),
     (Cps.M.set f (SVfun (t0, ys, e)) sub')))
| Fnil -> ((Fnil, count), sub0)

(** val contractcases :
    ((exp * ctx_map) * b_map) -> (r_map -> c_map -> ((exp * ctx_map) * b_map)
    -> __ -> exp shrinkT) -> r_map -> c_map -> b_map -> ctx_map ->
    (var * exp) list -> (var * exp) list shrinkT **)

let rec contractcases oes fcon sig0 count inl sub0 = function
| [] -> Coq_existT (((([], O), count), inl), __)
| p :: cl' ->
  let (y, e) = p in
  let Coq_existT (x, _) = fcon sig0 count ((e, sub0), inl) __ in
  let (p0, inl') = x in
  let (p1, count') = p0 in
  let (e', steps') = p1 in
  let Coq_existT (x0, _) = contractcases oes fcon sig0 count' inl' sub0 cl' in
  let (p2, inl'') = x0 in
  let (p3, count'') = p2 in
  let (cl'', steps'') = p3 in
  Coq_existT ((((((y, e') :: cl''), (add steps' steps'')), count''), inl''),
  __)

(** val postcontractfun :
    ((exp * ctx_map) * b_map) -> (r_map -> c_map -> ((exp * ctx_map) * b_map)
    -> __ -> exp shrinkT) -> r_map -> c_map -> b_map -> ctx_map -> fundefs ->
    nat -> fundefs shrinkT **)

let rec postcontractfun oes fcon sig0 count inl sub0 fds steps =
  match fds with
  | Fcons (f, t0, ys, e, fds') ->
    if getd false f inl
    then postcontractfun oes fcon sig0 count inl sub0 fds' steps
    else (match getd O f count with
          | O ->
            let count' = dec_census sig0 e count in
            postcontractfun oes fcon sig0 count' inl sub0 fds' steps
          | S _ ->
            let Coq_existT (x, _) = fcon sig0 count ((e, sub0), inl) __ in
            let (p, inl') = x in
            let (p0, count') = p in
            let (e', steps') = p0 in
            let Coq_existT (x0, _) =
              postcontractfun oes fcon sig0 count' inl' sub0 fds' steps
            in
            let (p1, inl'') = x0 in
            let (p2, count'') = p1 in
            let (fds'', steps'') = p2 in
            Coq_existT (((((Fcons (f, t0, ys, e', fds'')),
            (add steps' steps'')), count''), inl''), __))
  | Fnil -> Coq_existT ((((Fnil, steps), count), inl), __)

type contractT = exp shrinkT

(** val incr_steps : b_map -> contractT -> contractT **)

let incr_steps _ = function
| Coq_existT (x, _) ->
  let (p, im') = x in
  let (p0, count) = p in
  let (e_body, steps) = p0 in
  Coq_existT ((((e_body, (add steps (S O))), count), im'), __)

(** val contract_func :
    (r_map, (c_map, (exp, (ctx_map, b_map) sigT) sigT) sigT) sigT -> contractT **)

let rec contract_func x =
  let sig0 = projT1 x in
  let count = projT1 (projT2 x) in
  let e = projT1 (projT2 (projT2 x)) in
  let sub0 = projT1 (projT2 (projT2 (projT2 x))) in
  let im = projT2 (projT2 (projT2 (projT2 x))) in
  let contract0 = fun sig1 count0 e0 sub1 im0 ->
    contract_func (Coq_existT (sig1, (Coq_existT (count0, (Coq_existT (e0,
      (Coq_existT (sub1, im0))))))))
  in
  (match e with
   | Econstr (x0, t0, ys, e') ->
     (match getd O x0 count with
      | O ->
        let count' = dec_census_list sig0 ys count in
        incr_steps im (contract0 sig0 count' e' sub0 im)
      | S _ ->
        let Coq_existT (x1, _) =
          contract0 sig0 count e' (Cps.M.set x0 (SVconstr (t0, ys)) sub0) im
        in
        let (p, im') = x1 in
        let (p0, count') = p in
        let (e'', steps') = p0 in
        (match getd O x0 count' with
         | O ->
           let count'' = dec_census_list sig0 ys count' in
           Coq_existT ((((e'', (add steps' (S O))), count''), im'), __)
         | S _ ->
           let ys' = apply_r_list sig0 ys in
           Coq_existT (((((Econstr (x0, t0, ys', e'')), steps'), count'),
           im'), __)))
   | Ecase (v, cl) ->
     let v' = apply_r sig0 v in
     (match Cps.M.get v' sub0 with
      | Some s ->
        (match s with
         | SVconstr (t0, _) ->
           let filtered_var = findtag cl t0 in
           (match filtered_var with
            | Some k ->
              incr_steps im
                (contract0 sig0
                  (dec_census_case sig0 cl t0
                    (dec_census_list sig0 (v :: []) count)) k sub0 im)
            | None ->
              let filtered_var0 =
                contractcases (((Ecase (v, cl)), sub0), im)
                  (fun rm cm es _ ->
                  contract0 rm cm (fst (fst es)) (snd (fst es)) (snd es))
                  sig0 count im sub0 cl
              in
              let Coq_existT (x0, _) = filtered_var0 in
              let (p, im') = x0 in
              let (p0, count') = p in
              let (cl', steps') = p0 in
              Coq_existT (((((Ecase (v', cl')), steps'), count'), im'), __))
         | SVfun (_, _, _) ->
           let filtered_var =
             contractcases (((Ecase (v, cl)), sub0), im) (fun rm cm es _ ->
               contract0 rm cm (fst (fst es)) (snd (fst es)) (snd es)) sig0
               count im sub0 cl
           in
           let Coq_existT (x0, _) = filtered_var in
           let (p, im') = x0 in
           let (p0, count') = p in
           let (cl', steps') = p0 in
           Coq_existT (((((Ecase (v', cl')), steps'), count'), im'), __))
      | None ->
        let filtered_var =
          contractcases (((Ecase (v, cl)), sub0), im) (fun rm cm es _ ->
            contract0 rm cm (fst (fst es)) (snd (fst es)) (snd es)) sig0
            count im sub0 cl
        in
        let Coq_existT (x0, _) = filtered_var in
        let (p, im') = x0 in
        let (p0, count') = p in
        let (cl', steps') = p0 in
        Coq_existT (((((Ecase (v', cl')), steps'), count'), im'), __))
   | Eproj (v, t0, n, y, e0) ->
     (match getd O v count with
      | O ->
        let count' = dec_census_list sig0 (y :: []) count in
        incr_steps im (contract0 sig0 count' e0 sub0 im)
      | S _ ->
        let y' = apply_r sig0 y in
        (match Cps.M.get y' sub0 with
         | Some s ->
           (match s with
            | SVconstr (_, ys) ->
              (match nthN ys n with
               | Some yn ->
                 let yn' = apply_r sig0 yn in
                 let count' = Cps.M.set y' (sub (getd O y' count) (S O)) count
                 in
                 let count'' =
                   Cps.M.set v O
                     (Cps.M.set yn' (add (getd O v count) (getd O yn' count))
                       count')
                 in
                 incr_steps im
                   (contract0 (Cps.M.set v yn' sig0) count'' e0 sub0 im)
               | None ->
                 let Coq_existT (x0, _) = contract0 sig0 count e0 sub0 im in
                 let (p, im') = x0 in
                 let (p0, count') = p in
                 let (e', steps') = p0 in
                 (match getd O v count' with
                  | O ->
                    let count'' = dec_census_list sig0 (y :: []) count' in
                    Coq_existT ((((e', (add steps' (S O))), count''), im'),
                    __)
                  | S _ ->
                    Coq_existT (((((Eproj (v, t0, n, y', e')), steps'),
                      count'), im'), __)))
            | SVfun (_, _, _) ->
              let Coq_existT (x0, _) = contract0 sig0 count e0 sub0 im in
              let (p, im') = x0 in
              let (p0, count') = p in
              let (e', steps') = p0 in
              (match getd O v count' with
               | O ->
                 let count'' = dec_census_list sig0 (y :: []) count' in
                 Coq_existT ((((e', (add steps' (S O))), count''), im'), __)
               | S _ ->
                 Coq_existT (((((Eproj (v, t0, n, y', e')), steps'), count'),
                   im'), __)))
         | None ->
           let Coq_existT (x0, _) = contract0 sig0 count e0 sub0 im in
           let (p, im') = x0 in
           let (p0, count') = p in
           let (e', steps') = p0 in
           (match getd O v count' with
            | O ->
              let count'' = dec_census_list sig0 (y :: []) count' in
              Coq_existT ((((e', (add steps' (S O))), count''), im'), __)
            | S _ ->
              Coq_existT (((((Eproj (v, t0, n, y', e')), steps'), count'),
                im'), __))))
   | Eletapp (x0, f, t0, ys, e0) ->
     let f' = apply_r sig0 f in
     let ys' = apply_r_list sig0 ys in
     let f_no = getd O f' count in
     let f_info = Cps.M.get f' sub0 in
     (match f_no with
      | O ->
        let Coq_existT (x1, _) = contract0 sig0 count e0 sub0 im in
        let (p, im') = x1 in
        let (p0, count') = p in
        let (e', steps) = p0 in
        Coq_existT (((((Eletapp (x0, f', t0, ys', e')), steps), count'),
        im'), __)
      | S n0 ->
        (match n0 with
         | O ->
           (match f_info with
            | Some s ->
              (match s with
               | SVconstr (_, _) ->
                 let Coq_existT (x1, _) = contract0 sig0 count e0 sub0 im in
                 let (p, im') = x1 in
                 let (p0, count') = p in
                 let (e', steps) = p0 in
                 Coq_existT (((((Eletapp (x0, f', t0, ys', e')), steps),
                 count'), im'), __)
               | SVfun (t', xs, e_body) ->
                 if (&&) ((&&) (Pos.eqb t' t0) (eqb (length ys) (length xs)))
                      (negb (getd false f' im))
                 then let inl = inline_letapp e_body x0 in
                      (match inl with
                       | Some p ->
                         let (c_inl, x') = p in
                         let im' = Cps.M.set f' true im in
                         let sig' = set_list (combine xs ys') sig0 in
                         let count' =
                           update_count_letapp (apply_r sig' x') x0
                             (update_count_inlined ys' xs
                               (Cps.M.set f' O count))
                         in
                         let Coq_existT (x1, _) =
                           contract0 sig' count'
                             (app_ctx_f c_inl
                               (rename_all_ns (Cps.M.set x0 x' Cps.M.empty)
                                 e0)) sub0 im'
                         in
                         let (p0, i) = x1 in
                         let (p1, c) = p0 in
                         let (e', steps') = p1 in
                         Coq_existT ((((e', (add steps' (S O))), c), i), __)
                       | None ->
                         let Coq_existT (x1, _) =
                           contract0 sig0 count e0 sub0 im
                         in
                         let (p, im') = x1 in
                         let (p0, count') = p in
                         let (e', steps) = p0 in
                         Coq_existT (((((Eletapp (x0, f', t0, ys', e')),
                         steps), count'), im'), __))
                 else let Coq_existT (x1, _) = contract0 sig0 count e0 sub0 im
                      in
                      let (p, im') = x1 in
                      let (p0, count') = p in
                      let (e', steps) = p0 in
                      Coq_existT (((((Eletapp (x0, f', t0, ys', e')), steps),
                      count'), im'), __))
            | None ->
              let Coq_existT (x1, _) = contract0 sig0 count e0 sub0 im in
              let (p, im') = x1 in
              let (p0, count') = p in
              let (e', steps) = p0 in
              Coq_existT (((((Eletapp (x0, f', t0, ys', e')), steps),
              count'), im'), __))
         | S _ ->
           let Coq_existT (x1, _) = contract0 sig0 count e0 sub0 im in
           let (p, im') = x1 in
           let (p0, count') = p in
           let (e', steps) = p0 in
           Coq_existT (((((Eletapp (x0, f', t0, ys', e')), steps), count'),
           im'), __)))
   | Efun (fl, e0) ->
     let filtered_var = precontractfun sig0 count sub0 fl in
     let (p, sub') = filtered_var in
     let (fl', count') = p in
     let Coq_existT (x0, _) = contract0 sig0 count' e0 sub' im in
     let (p0, im') = x0 in
     let (p1, count'') = p0 in
     let (e', steps') = p1 in
     let Coq_existT (x1, _) =
       postcontractfun (((Efun (fl', e0)), sub0), im') (fun rm cm es _ ->
         contract0 rm cm (fst (fst es)) (snd (fst es)) (snd es)) sig0 count''
         im' sub0 fl' O
     in
     let (p2, im'') = x1 in
     let (p3, count''') = p2 in
     let (fl'', steps'') = p3 in
     (match fl'' with
      | Fcons (_, _, _, _, _) ->
        Coq_existT (((((Efun (fl'', e')), (add steps' steps'')), count'''),
          im''), __)
      | Fnil ->
        Coq_existT ((((e', (add (add steps' steps'') (S O))), count'''),
          im''), __))
   | Eapp (f, t0, ys) ->
     let f' = apply_r sig0 f in
     let ys' = apply_r_list sig0 ys in
     let filtered_var = getd O f' count in
     (match filtered_var with
      | O -> Coq_existT (((((Eapp (f', t0, ys')), O), count), im), __)
      | S n ->
        (match n with
         | O ->
           let filtered_var0 = Cps.M.get f' sub0 in
           (match filtered_var0 with
            | Some s ->
              (match s with
               | SVconstr (_, _) ->
                 Coq_existT (((((Eapp (f', t0, ys')), O), count), im), __)
               | SVfun (t', xs, m) ->
                 let filtered_var1 =
                   (&&) (Pos.eqb t' t0)
                     ((&&) (eqb (length ys) (length xs))
                       (negb (getd false f' im)))
                 in
                 if filtered_var1
                 then let im' = Cps.M.set f' true im in
                      let count' =
                        update_count_inlined ys' xs (Cps.M.set f' O count)
                      in
                      let Coq_existT (x0, _) =
                        contract0 (set_list (combine xs ys') sig0) count' m
                          sub0 im'
                      in
                      let (p, i) = x0 in
                      let (p0, c) = p in
                      let (e0, steps') = p0 in
                      Coq_existT ((((e0, (add steps' (S O))), c), i), __)
                 else Coq_existT (((((Eapp (f', t0, ys')), O), count), im),
                        __))
            | None ->
              Coq_existT (((((Eapp (f', t0, ys')), O), count), im), __))
         | S _ -> Coq_existT (((((Eapp (f', t0, ys')), O), count), im), __)))
   | Eprim_val (x0, p, e0) ->
     let Coq_existT (x1, _) = contract0 sig0 count e0 sub0 im in
     let (p0, im') = x1 in
     let (p1, count') = p0 in
     let (e', steps') = p1 in
     Coq_existT (((((Eprim_val (x0, p, e')), steps'), count'), im'), __)
   | Eprim (x0, f, ys, e0) ->
     let Coq_existT (x1, _) = contract0 sig0 count e0 sub0 im in
     let (p, im') = x1 in
     let (p0, count') = p in
     let (e', steps') = p0 in
     let ys' = apply_r_list sig0 ys in
     Coq_existT (((((Eprim (x0, f, ys', e')), steps'), count'), im'), __)
   | Ehalt v -> Coq_existT (((((Ehalt (apply_r sig0 v)), O), count), im), __))

(** val contract : r_map -> c_map -> exp -> ctx_map -> b_map -> contractT **)

let contract sig0 count e sub0 im =
  contract_func (Coq_existT (sig0, (Coq_existT (count, (Coq_existT (e,
    (Coq_existT (sub0, im))))))))

(** val shrink_top : exp -> exp * nat **)

let shrink_top e =
  let fvs = PS.elements (exp_fv e) in
  let f = Pos.add (max_var e Coq_xH) Coq_xH in
  let t0 = Coq_xH in
  let e0 = Efun ((Fcons (f, t0, fvs, e, Fnil)), (Ehalt f)) in
  let count = init_census e0 in
  let Coq_existT (x, _) =
    contract Cps.M.empty count e0 Cps.M.empty Cps.M.empty
  in
  let (p, _) = x in
  let (p0, _) = p in
  let (e', steps) = p0 in
  (match e' with
   | Efun (f0, _) ->
     (match f0 with
      | Fcons (_, _, _, e'0, f2) ->
        (match f2 with
         | Fcons (_, _, _, _, _) -> (e', steps)
         | Fnil -> (e'0, steps))
      | Fnil -> (e', steps))
   | _ -> (e', steps))

(** val shrink_err : exp -> comp_data -> exp error * comp_data **)

let shrink_err e c =
  ((Ret (fst (shrink_top e))), c)
