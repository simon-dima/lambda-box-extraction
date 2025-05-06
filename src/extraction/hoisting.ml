open Datatypes
open List0
open Nat0
open Closure_conversion
open CompM
open Cps
open State

(** val erase_fundefs :
    exp -> fundefs -> (((exp * fundefs) * nat) -> (exp * fundefs) * nat) ->
    (exp * fundefs) * nat **)

let rec erase_fundefs e defs f =
  match e with
  | Econstr (x, tag, ys, e') ->
    erase_fundefs e' defs (fun p ->
      let (p0, n) = p in
      let (e0, defs0) = p0 in f (((Econstr (x, tag, ys, e0)), defs0), n))
  | Ecase (x, tes) ->
    fold_left (fun f0 te p ->
      let (c, e0) = te in
      let (p0, n1) = p in
      let (e1, defs1) = p0 in
      erase_fundefs e0 defs1 (fun p2 ->
        let (p1, n2) = p2 in
        let (e2, defs2) = p1 in
        (match e1 with
         | Ecase (x', tes') ->
           f0 (((Ecase (x', ((c, e2) :: tes'))), defs2), (max n1 n2))
         | _ -> f0 (((Ecase (x, ((c, e2) :: []))), defs2), (max n1 n2)))))
      tes f (((Ecase (x, [])), defs), O)
  | Eproj (x, tag, n, y, e') ->
    erase_fundefs e' defs (fun p ->
      let (p0, m) = p in
      let (e0, defs0) = p0 in f (((Eproj (x, tag, n, y, e0)), defs0), m))
  | Eletapp (x, g, ft, ys, e') ->
    erase_fundefs e' defs (fun p ->
      let (p0, n) = p in
      let (e0, defs0) = p0 in f (((Eletapp (x, g, ft, ys, e0)), defs0), n))
  | Efun (fdefs, e') ->
    erase_fundefs e' defs (fun p ->
      let (p0, n) = p in
      let (e'', defs'') = p0 in
      erase_nested_fundefs fdefs e'' defs'' (fun p1 ->
        let (p2, m) = p1 in f (p2, (add (S O) (max n m)))))
  | Eprim_val (x, pv, e') ->
    erase_fundefs e' defs (fun p ->
      let (p0, n) = p in
      let (e'0, defs') = p0 in f (((Eprim_val (x, pv, e'0)), defs'), n))
  | Eprim (x, prim, ys, e') ->
    erase_fundefs e' defs (fun p ->
      let (p0, n) = p in
      let (e'0, defs') = p0 in f (((Eprim (x, prim, ys, e'0)), defs'), n))
  | x -> f ((x, defs), O)

(** val erase_nested_fundefs :
    fundefs -> exp -> fundefs -> (((exp * fundefs) * nat) ->
    (exp * fundefs) * nat) -> (exp * fundefs) * nat **)

and erase_nested_fundefs defs e hdefs f =
  match defs with
  | Fcons (g, t, xs, e', defs0) ->
    erase_nested_fundefs defs0 e hdefs (fun p1 ->
      let (p, n1) = p1 in
      let (e1, defs1) = p in
      erase_fundefs e' defs1 (fun p2 ->
        let (p0, n2) = p2 in
        let (e2, defs2) = p0 in
        f ((e1, (Fcons (g, t, xs, e2, defs2))), (max n1 n2))))
  | Fnil -> f ((e, hdefs), O)

(** val exp_hoist : exp -> exp * nat **)

let exp_hoist e =
  let (p, n) = erase_fundefs e Fnil (Obj.magic id) in
  let (e0, defs) = p in
  (match defs with
   | Fcons (_, _, _, _, _) -> ((Efun (defs, e0)), n)
   | Fnil -> (e0, n))

(** val closure_conversion_hoist :
    ctor_tag -> ind_tag -> exp -> comp_data -> exp error * comp_data **)

let closure_conversion_hoist clo_tag clo_itag e c =
  let (e'_err, c') = closure_conversion_top clo_tag clo_itag e c in
  (match e'_err with
   | Err str -> ((Err str), c')
   | Ret e' -> ((Ret (fst (exp_hoist e'))), c'))
