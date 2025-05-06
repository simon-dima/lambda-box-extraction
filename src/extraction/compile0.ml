open Ast0
open AstCommon
open BasicAst
open Byte
open Classes1
open Datatypes
open EAst
open EPrimitive
open EProgram
open ESpineView
open Erasure0
open ExtractionCorrectness
open Kernames
open List0
open MCList
open Nat0
open PeanoNat
open Primitive
open Specif
open Bytestring

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

type projection = inductive * nat

(** val project_dec : projection -> projection -> bool **)

let project_dec s1 s2 =
  let (i, n) = s1 in
  let (i0, n0) = s2 in
  let s = inductive_dec i i0 in if s then Nat.eq_dec n n0 else false

type coq_Term =
| TRel of nat
| TProof
| TLambda of name * coq_Term
| TLetIn of name * coq_Term * coq_Term
| TApp of coq_Term * coq_Term
| TConst of kername
| TConstruct of inductive * nat * coq_Terms
| TCase of inductive * coq_Term * coq_Brs
| TFix of coq_Defs * nat
| TPrim of primitive
| TWrong of String.t
and coq_Terms =
| Coq_tnil
| Coq_tcons of coq_Term * coq_Terms
and coq_Brs =
| Coq_bnil
| Coq_bcons of name list * coq_Term * coq_Brs
and coq_Defs =
| Coq_dnil
| Coq_dcons of name * coq_Term * nat * coq_Defs

(** val coq_Term_rect :
    (nat -> 'a1) -> 'a1 -> (name -> coq_Term -> 'a1 -> 'a1) -> (name ->
    coq_Term -> 'a1 -> coq_Term -> 'a1 -> 'a1) -> (coq_Term -> 'a1 ->
    coq_Term -> 'a1 -> 'a1) -> (kername -> 'a1) -> (inductive -> nat ->
    coq_Terms -> 'a1) -> (inductive -> coq_Term -> 'a1 -> coq_Brs -> 'a1) ->
    (coq_Defs -> nat -> 'a1) -> (primitive -> 'a1) -> (String.t -> 'a1) ->
    coq_Term -> 'a1 **)

let rec coq_Term_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 = function
| TRel n -> f n
| TProof -> f0
| TLambda (n, t1) ->
  f1 n t1 (coq_Term_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 t1)
| TLetIn (n, t1, t2) ->
  f2 n t1 (coq_Term_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 t1) t2
    (coq_Term_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 t2)
| TApp (t1, t2) ->
  f3 t1 (coq_Term_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 t1) t2
    (coq_Term_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 t2)
| TConst k -> f4 k
| TConstruct (i, n, t1) -> f5 i n t1
| TCase (i, t1, b) ->
  f6 i t1 (coq_Term_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 t1) b
| TFix (d, n) -> f7 d n
| TPrim p -> f8 p
| TWrong t1 -> f9 t1

(** val coq_Term_rec :
    (nat -> 'a1) -> 'a1 -> (name -> coq_Term -> 'a1 -> 'a1) -> (name ->
    coq_Term -> 'a1 -> coq_Term -> 'a1 -> 'a1) -> (coq_Term -> 'a1 ->
    coq_Term -> 'a1 -> 'a1) -> (kername -> 'a1) -> (inductive -> nat ->
    coq_Terms -> 'a1) -> (inductive -> coq_Term -> 'a1 -> coq_Brs -> 'a1) ->
    (coq_Defs -> nat -> 'a1) -> (primitive -> 'a1) -> (String.t -> 'a1) ->
    coq_Term -> 'a1 **)

let rec coq_Term_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 = function
| TRel n -> f n
| TProof -> f0
| TLambda (n, t1) -> f1 n t1 (coq_Term_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 t1)
| TLetIn (n, t1, t2) ->
  f2 n t1 (coq_Term_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 t1) t2
    (coq_Term_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 t2)
| TApp (t1, t2) ->
  f3 t1 (coq_Term_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 t1) t2
    (coq_Term_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 t2)
| TConst k -> f4 k
| TConstruct (i, n, t1) -> f5 i n t1
| TCase (i, t1, b) ->
  f6 i t1 (coq_Term_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 t1) b
| TFix (d, n) -> f7 d n
| TPrim p -> f8 p
| TWrong t1 -> f9 t1

(** val coq_Terms_rect :
    'a1 -> (coq_Term -> coq_Terms -> 'a1 -> 'a1) -> coq_Terms -> 'a1 **)

let rec coq_Terms_rect f f0 = function
| Coq_tnil -> f
| Coq_tcons (t1, t2) -> f0 t1 t2 (coq_Terms_rect f f0 t2)

(** val coq_Terms_rec :
    'a1 -> (coq_Term -> coq_Terms -> 'a1 -> 'a1) -> coq_Terms -> 'a1 **)

let rec coq_Terms_rec f f0 = function
| Coq_tnil -> f
| Coq_tcons (t1, t2) -> f0 t1 t2 (coq_Terms_rec f f0 t2)

(** val coq_Brs_rect :
    'a1 -> (name list -> coq_Term -> coq_Brs -> 'a1 -> 'a1) -> coq_Brs -> 'a1 **)

let rec coq_Brs_rect f f0 = function
| Coq_bnil -> f
| Coq_bcons (l, t0, b0) -> f0 l t0 b0 (coq_Brs_rect f f0 b0)

(** val coq_Brs_rec :
    'a1 -> (name list -> coq_Term -> coq_Brs -> 'a1 -> 'a1) -> coq_Brs -> 'a1 **)

let rec coq_Brs_rec f f0 = function
| Coq_bnil -> f
| Coq_bcons (l, t0, b0) -> f0 l t0 b0 (coq_Brs_rec f f0 b0)

(** val coq_Defs_rect :
    'a1 -> (name -> coq_Term -> nat -> coq_Defs -> 'a1 -> 'a1) -> coq_Defs ->
    'a1 **)

let rec coq_Defs_rect f f0 = function
| Coq_dnil -> f
| Coq_dcons (n, t0, n0, d0) -> f0 n t0 n0 d0 (coq_Defs_rect f f0 d0)

(** val coq_Defs_rec :
    'a1 -> (name -> coq_Term -> nat -> coq_Defs -> 'a1 -> 'a1) -> coq_Defs ->
    'a1 **)

let rec coq_Defs_rec f f0 = function
| Coq_dnil -> f
| Coq_dcons (n, t0, n0, d0) -> f0 n t0 n0 d0 (coq_Defs_rec f f0 d0)

(** val coq_Terms_list : coq_Terms -> coq_Term list **)

let rec coq_Terms_list = function
| Coq_tnil -> []
| Coq_tcons (u, us) -> u :: (coq_Terms_list us)

(** val tlength : coq_Terms -> nat **)

let rec tlength = function
| Coq_tnil -> O
| Coq_tcons (_, ts0) -> S (tlength ts0)

type coq_R_tlength =
| R_tlength_0 of coq_Terms
| R_tlength_1 of coq_Terms * coq_Term * coq_Terms * nat * coq_R_tlength

(** val coq_R_tlength_rect :
    (coq_Terms -> __ -> 'a1) -> (coq_Terms -> coq_Term -> coq_Terms -> __ ->
    nat -> coq_R_tlength -> 'a1 -> 'a1) -> coq_Terms -> nat -> coq_R_tlength
    -> 'a1 **)

let rec coq_R_tlength_rect f f0 _ _ = function
| R_tlength_0 ts -> f ts __
| R_tlength_1 (ts, _x, ts0, _res, r0) ->
  f0 ts _x ts0 __ _res r0 (coq_R_tlength_rect f f0 ts0 _res r0)

(** val coq_R_tlength_rec :
    (coq_Terms -> __ -> 'a1) -> (coq_Terms -> coq_Term -> coq_Terms -> __ ->
    nat -> coq_R_tlength -> 'a1 -> 'a1) -> coq_Terms -> nat -> coq_R_tlength
    -> 'a1 **)

let rec coq_R_tlength_rec f f0 _ _ = function
| R_tlength_0 ts -> f ts __
| R_tlength_1 (ts, _x, ts0, _res, r0) ->
  f0 ts _x ts0 __ _res r0 (coq_R_tlength_rec f f0 ts0 _res r0)

(** val tlength_rect :
    (coq_Terms -> __ -> 'a1) -> (coq_Terms -> coq_Term -> coq_Terms -> __ ->
    'a1 -> 'a1) -> coq_Terms -> 'a1 **)

let rec tlength_rect f f0 ts =
  let f1 = f ts in
  let f2 = f0 ts in
  (match ts with
   | Coq_tnil -> f1 __
   | Coq_tcons (t0, t1) ->
     let f3 = f2 t0 t1 __ in let hrec = tlength_rect f f0 t1 in f3 hrec)

(** val tlength_rec :
    (coq_Terms -> __ -> 'a1) -> (coq_Terms -> coq_Term -> coq_Terms -> __ ->
    'a1 -> 'a1) -> coq_Terms -> 'a1 **)

let tlength_rec =
  tlength_rect

(** val coq_R_tlength_correct : coq_Terms -> nat -> coq_R_tlength **)

let coq_R_tlength_correct ts _res =
  tlength_rect (fun y _ _ _ -> R_tlength_0 y) (fun y y0 y1 _ y3 _ _ ->
    R_tlength_1 (y, y0, y1, (tlength y1), (y3 (tlength y1) __))) ts _res __

(** val blength : coq_Brs -> nat **)

let rec blength = function
| Coq_bnil -> O
| Coq_bcons (_, _, ts0) -> S (blength ts0)

type coq_R_blength =
| R_blength_0 of coq_Brs
| R_blength_1 of coq_Brs * name list * coq_Term * coq_Brs * nat
   * coq_R_blength

(** val coq_R_blength_rect :
    (coq_Brs -> __ -> 'a1) -> (coq_Brs -> name list -> coq_Term -> coq_Brs ->
    __ -> nat -> coq_R_blength -> 'a1 -> 'a1) -> coq_Brs -> nat ->
    coq_R_blength -> 'a1 **)

let rec coq_R_blength_rect f f0 _ _ = function
| R_blength_0 ts -> f ts __
| R_blength_1 (ts, _x, _x0, ts0, _res, r0) ->
  f0 ts _x _x0 ts0 __ _res r0 (coq_R_blength_rect f f0 ts0 _res r0)

(** val coq_R_blength_rec :
    (coq_Brs -> __ -> 'a1) -> (coq_Brs -> name list -> coq_Term -> coq_Brs ->
    __ -> nat -> coq_R_blength -> 'a1 -> 'a1) -> coq_Brs -> nat ->
    coq_R_blength -> 'a1 **)

let rec coq_R_blength_rec f f0 _ _ = function
| R_blength_0 ts -> f ts __
| R_blength_1 (ts, _x, _x0, ts0, _res, r0) ->
  f0 ts _x _x0 ts0 __ _res r0 (coq_R_blength_rec f f0 ts0 _res r0)

(** val blength_rect :
    (coq_Brs -> __ -> 'a1) -> (coq_Brs -> name list -> coq_Term -> coq_Brs ->
    __ -> 'a1 -> 'a1) -> coq_Brs -> 'a1 **)

let rec blength_rect f f0 ts =
  let f1 = f ts in
  let f2 = f0 ts in
  (match ts with
   | Coq_bnil -> f1 __
   | Coq_bcons (l, t0, b) ->
     let f3 = f2 l t0 b __ in let hrec = blength_rect f f0 b in f3 hrec)

(** val blength_rec :
    (coq_Brs -> __ -> 'a1) -> (coq_Brs -> name list -> coq_Term -> coq_Brs ->
    __ -> 'a1 -> 'a1) -> coq_Brs -> 'a1 **)

let blength_rec =
  blength_rect

(** val coq_R_blength_correct : coq_Brs -> nat -> coq_R_blength **)

let coq_R_blength_correct ts _res =
  blength_rect (fun y _ _ _ -> R_blength_0 y) (fun y y0 y1 y2 _ y4 _ _ ->
    R_blength_1 (y, y0, y1, y2, (blength y2), (y4 (blength y2) __))) ts _res
    __

(** val tappend : coq_Terms -> coq_Terms -> coq_Terms **)

let rec tappend ts1 ts2 =
  match ts1 with
  | Coq_tnil -> ts2
  | Coq_tcons (t0, ts) -> Coq_tcons (t0, (tappend ts ts2))

type coq_R_tappend =
| R_tappend_0 of coq_Terms * coq_Terms
| R_tappend_1 of coq_Terms * coq_Terms * coq_Term * coq_Terms * coq_Terms
   * coq_R_tappend

(** val coq_R_tappend_rect :
    (coq_Terms -> coq_Terms -> __ -> 'a1) -> (coq_Terms -> coq_Terms ->
    coq_Term -> coq_Terms -> __ -> coq_Terms -> coq_R_tappend -> 'a1 -> 'a1)
    -> coq_Terms -> coq_Terms -> coq_Terms -> coq_R_tappend -> 'a1 **)

let rec coq_R_tappend_rect f f0 _ _ _ = function
| R_tappend_0 (ts1, ts2) -> f ts1 ts2 __
| R_tappend_1 (ts1, ts2, t0, ts, _res, r0) ->
  f0 ts1 ts2 t0 ts __ _res r0 (coq_R_tappend_rect f f0 ts ts2 _res r0)

(** val coq_R_tappend_rec :
    (coq_Terms -> coq_Terms -> __ -> 'a1) -> (coq_Terms -> coq_Terms ->
    coq_Term -> coq_Terms -> __ -> coq_Terms -> coq_R_tappend -> 'a1 -> 'a1)
    -> coq_Terms -> coq_Terms -> coq_Terms -> coq_R_tappend -> 'a1 **)

let rec coq_R_tappend_rec f f0 _ _ _ = function
| R_tappend_0 (ts1, ts2) -> f ts1 ts2 __
| R_tappend_1 (ts1, ts2, t0, ts, _res, r0) ->
  f0 ts1 ts2 t0 ts __ _res r0 (coq_R_tappend_rec f f0 ts ts2 _res r0)

(** val tappend_rect :
    (coq_Terms -> coq_Terms -> __ -> 'a1) -> (coq_Terms -> coq_Terms ->
    coq_Term -> coq_Terms -> __ -> 'a1 -> 'a1) -> coq_Terms -> coq_Terms ->
    'a1 **)

let rec tappend_rect f f0 ts1 ts2 =
  let f1 = f ts1 ts2 in
  let f2 = f0 ts1 ts2 in
  (match ts1 with
   | Coq_tnil -> f1 __
   | Coq_tcons (t0, t1) ->
     let f3 = f2 t0 t1 __ in let hrec = tappend_rect f f0 t1 ts2 in f3 hrec)

(** val tappend_rec :
    (coq_Terms -> coq_Terms -> __ -> 'a1) -> (coq_Terms -> coq_Terms ->
    coq_Term -> coq_Terms -> __ -> 'a1 -> 'a1) -> coq_Terms -> coq_Terms ->
    'a1 **)

let tappend_rec =
  tappend_rect

(** val coq_R_tappend_correct :
    coq_Terms -> coq_Terms -> coq_Terms -> coq_R_tappend **)

let coq_R_tappend_correct ts1 ts2 _res =
  tappend_rect (fun y y0 _ _ _ -> R_tappend_0 (y, y0))
    (fun y y0 y1 y2 _ y4 _ _ -> R_tappend_1 (y, y0, y1, y2, (tappend y2 y0),
    (y4 (tappend y2 y0) __))) ts1 ts2 _res __

(** val tdrop : nat -> coq_Terms -> coq_Terms **)

let rec tdrop n ts =
  match n with
  | O -> ts
  | S m ->
    (match ts with
     | Coq_tnil -> Coq_tnil
     | Coq_tcons (_, us) -> tdrop m us)

(** val treverse : coq_Terms -> coq_Terms **)

let rec treverse = function
| Coq_tnil -> Coq_tnil
| Coq_tcons (b, bs) -> tappend (treverse bs) (Coq_tcons (b, Coq_tnil))

(** val dlength : coq_Defs -> nat **)

let rec dlength = function
| Coq_dnil -> O
| Coq_dcons (_, _, _, ts0) -> S (dlength ts0)

(** val isApp_dec : coq_Term -> bool **)

let isApp_dec = function
| TApp (_, _) -> true
| _ -> false

(** val lift : nat -> coq_Term -> coq_Term **)

let rec lift n t0 = match t0 with
| TRel m -> TRel (match Nat.compare m n with
                  | Lt -> m
                  | _ -> S m)
| TProof -> TProof
| TLambda (nm, bod) -> TLambda (nm, (lift (S n) bod))
| TLetIn (nm, df, bod) -> TLetIn (nm, (lift n df), (lift (S n) bod))
| TApp (fn, arg) -> TApp ((lift n fn), (lift n arg))
| TConstruct (i, x, args) -> TConstruct (i, x, (lifts n args))
| TCase (i, mch, brs) -> TCase (i, (lift n mch), (liftBs n brs))
| TFix (ds, y) -> TFix ((liftDs (add n (dlength ds)) ds), y)
| _ -> t0

(** val lifts : nat -> coq_Terms -> coq_Terms **)

and lifts n = function
| Coq_tnil -> Coq_tnil
| Coq_tcons (u, us) -> Coq_tcons ((lift n u), (lifts n us))

(** val liftBs : nat -> coq_Brs -> coq_Brs **)

and liftBs n = function
| Coq_bnil -> Coq_bnil
| Coq_bcons (m, b, bs) ->
  Coq_bcons (m, (lift (add (length m) n) b), (liftBs n bs))

(** val liftDs : nat -> coq_Defs -> coq_Defs **)

and liftDs n = function
| Coq_dnil -> Coq_dnil
| Coq_dcons (nm, u, j, es) -> Coq_dcons (nm, (lift n u), j, (liftDs n es))

type coq_R_lift =
| R_lift_0 of nat * coq_Term * nat
| R_lift_1 of nat * coq_Term * nat * comparison
| R_lift_2 of nat * coq_Term
| R_lift_3 of nat * coq_Term * name * coq_Term * coq_Term * coq_R_lift
| R_lift_4 of nat * coq_Term * name * coq_Term * coq_Term * coq_Term
   * coq_R_lift * coq_Term * coq_R_lift
| R_lift_5 of nat * coq_Term * coq_Term * coq_Term * coq_Term * coq_R_lift
   * coq_Term * coq_R_lift
| R_lift_6 of nat * coq_Term * inductive * nat * coq_Terms * coq_Terms
   * coq_R_lifts
| R_lift_7 of nat * coq_Term * inductive * coq_Term * coq_Brs * coq_Term
   * coq_R_lift * coq_Brs * coq_R_liftBs
| R_lift_8 of nat * coq_Term * coq_Defs * nat * coq_Defs * coq_R_liftDs
| R_lift_9 of nat * coq_Term * coq_Term
and coq_R_lifts =
| R_lifts_0 of nat * coq_Terms
| R_lifts_1 of nat * coq_Terms * coq_Term * coq_Terms * coq_Term * coq_R_lift
   * coq_Terms * coq_R_lifts
and coq_R_liftBs =
| R_liftBs_0 of nat * coq_Brs
| R_liftBs_1 of nat * coq_Brs * name list * coq_Term * coq_Brs * coq_Term
   * coq_R_lift * coq_Brs * coq_R_liftBs
and coq_R_liftDs =
| R_liftDs_0 of nat * coq_Defs
| R_liftDs_1 of nat * coq_Defs * name * coq_Term * nat * coq_Defs * coq_Term
   * coq_R_lift * coq_Defs * coq_R_liftDs

(** val coq_R_lift_rect :
    (nat -> coq_Term -> nat -> __ -> __ -> 'a1) -> (nat -> coq_Term -> nat ->
    __ -> comparison -> __ -> __ -> 'a1) -> (nat -> coq_Term -> __ -> 'a1) ->
    (nat -> coq_Term -> name -> coq_Term -> __ -> coq_Term -> coq_R_lift ->
    'a1 -> 'a1) -> (nat -> coq_Term -> name -> coq_Term -> coq_Term -> __ ->
    coq_Term -> coq_R_lift -> 'a1 -> coq_Term -> coq_R_lift -> 'a1 -> 'a1) ->
    (nat -> coq_Term -> coq_Term -> coq_Term -> __ -> coq_Term -> coq_R_lift
    -> 'a1 -> coq_Term -> coq_R_lift -> 'a1 -> 'a1) -> (nat -> coq_Term ->
    inductive -> nat -> coq_Terms -> __ -> coq_Terms -> coq_R_lifts -> 'a1)
    -> (nat -> coq_Term -> inductive -> coq_Term -> coq_Brs -> __ -> coq_Term
    -> coq_R_lift -> 'a1 -> coq_Brs -> coq_R_liftBs -> 'a1) -> (nat ->
    coq_Term -> coq_Defs -> nat -> __ -> coq_Defs -> coq_R_liftDs -> 'a1) ->
    (nat -> coq_Term -> coq_Term -> __ -> __ -> 'a1) -> nat -> coq_Term ->
    coq_Term -> coq_R_lift -> 'a1 **)

let rec coq_R_lift_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 _ _ _ = function
| R_lift_0 (n, t0, m) -> f n t0 m __ __
| R_lift_1 (n, t0, m, _x) -> f0 n t0 m __ _x __ __
| R_lift_2 (n, t0) -> f1 n t0 __
| R_lift_3 (n, t0, nm, bod, _res, r0) ->
  f2 n t0 nm bod __ _res r0
    (coq_R_lift_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 (S n) bod _res r0)
| R_lift_4 (n, t0, nm, df, bod, _res0, r0, _res, r1) ->
  f3 n t0 nm df bod __ _res0 r0
    (coq_R_lift_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 n df _res0 r0) _res r1
    (coq_R_lift_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 (S n) bod _res r1)
| R_lift_5 (n, t0, fn, arg, _res0, r0, _res, r1) ->
  f4 n t0 fn arg __ _res0 r0
    (coq_R_lift_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 n fn _res0 r0) _res r1
    (coq_R_lift_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 n arg _res r1)
| R_lift_6 (n, t0, i, x, args, _res, r0) -> f5 n t0 i x args __ _res r0
| R_lift_7 (n, t0, i, mch, brs, _res0, r0, _res, r1) ->
  f6 n t0 i mch brs __ _res0 r0
    (coq_R_lift_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 n mch _res0 r0) _res r1
| R_lift_8 (n, t0, ds, y, _res, r0) -> f7 n t0 ds y __ _res r0
| R_lift_9 (n, t0, _x) -> f8 n t0 _x __ __

(** val coq_R_lift_rec :
    (nat -> coq_Term -> nat -> __ -> __ -> 'a1) -> (nat -> coq_Term -> nat ->
    __ -> comparison -> __ -> __ -> 'a1) -> (nat -> coq_Term -> __ -> 'a1) ->
    (nat -> coq_Term -> name -> coq_Term -> __ -> coq_Term -> coq_R_lift ->
    'a1 -> 'a1) -> (nat -> coq_Term -> name -> coq_Term -> coq_Term -> __ ->
    coq_Term -> coq_R_lift -> 'a1 -> coq_Term -> coq_R_lift -> 'a1 -> 'a1) ->
    (nat -> coq_Term -> coq_Term -> coq_Term -> __ -> coq_Term -> coq_R_lift
    -> 'a1 -> coq_Term -> coq_R_lift -> 'a1 -> 'a1) -> (nat -> coq_Term ->
    inductive -> nat -> coq_Terms -> __ -> coq_Terms -> coq_R_lifts -> 'a1)
    -> (nat -> coq_Term -> inductive -> coq_Term -> coq_Brs -> __ -> coq_Term
    -> coq_R_lift -> 'a1 -> coq_Brs -> coq_R_liftBs -> 'a1) -> (nat ->
    coq_Term -> coq_Defs -> nat -> __ -> coq_Defs -> coq_R_liftDs -> 'a1) ->
    (nat -> coq_Term -> coq_Term -> __ -> __ -> 'a1) -> nat -> coq_Term ->
    coq_Term -> coq_R_lift -> 'a1 **)

let rec coq_R_lift_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 _ _ _ = function
| R_lift_0 (n, t0, m) -> f n t0 m __ __
| R_lift_1 (n, t0, m, _x) -> f0 n t0 m __ _x __ __
| R_lift_2 (n, t0) -> f1 n t0 __
| R_lift_3 (n, t0, nm, bod, _res, r0) ->
  f2 n t0 nm bod __ _res r0
    (coq_R_lift_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 (S n) bod _res r0)
| R_lift_4 (n, t0, nm, df, bod, _res0, r0, _res, r1) ->
  f3 n t0 nm df bod __ _res0 r0
    (coq_R_lift_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 n df _res0 r0) _res r1
    (coq_R_lift_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 (S n) bod _res r1)
| R_lift_5 (n, t0, fn, arg, _res0, r0, _res, r1) ->
  f4 n t0 fn arg __ _res0 r0
    (coq_R_lift_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 n fn _res0 r0) _res r1
    (coq_R_lift_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 n arg _res r1)
| R_lift_6 (n, t0, i, x, args, _res, r0) -> f5 n t0 i x args __ _res r0
| R_lift_7 (n, t0, i, mch, brs, _res0, r0, _res, r1) ->
  f6 n t0 i mch brs __ _res0 r0
    (coq_R_lift_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 n mch _res0 r0) _res r1
| R_lift_8 (n, t0, ds, y, _res, r0) -> f7 n t0 ds y __ _res r0
| R_lift_9 (n, t0, _x) -> f8 n t0 _x __ __

(** val coq_R_lifts_rect :
    (nat -> coq_Terms -> __ -> 'a1) -> (nat -> coq_Terms -> coq_Term ->
    coq_Terms -> __ -> coq_Term -> coq_R_lift -> coq_Terms -> coq_R_lifts ->
    'a1 -> 'a1) -> nat -> coq_Terms -> coq_Terms -> coq_R_lifts -> 'a1 **)

let rec coq_R_lifts_rect f f0 _ _ _ = function
| R_lifts_0 (n, ts) -> f n ts __
| R_lifts_1 (n, ts, u, us, _res0, r0, _res, r1) ->
  f0 n ts u us __ _res0 r0 _res r1 (coq_R_lifts_rect f f0 n us _res r1)

(** val coq_R_lifts_rec :
    (nat -> coq_Terms -> __ -> 'a1) -> (nat -> coq_Terms -> coq_Term ->
    coq_Terms -> __ -> coq_Term -> coq_R_lift -> coq_Terms -> coq_R_lifts ->
    'a1 -> 'a1) -> nat -> coq_Terms -> coq_Terms -> coq_R_lifts -> 'a1 **)

let rec coq_R_lifts_rec f f0 _ _ _ = function
| R_lifts_0 (n, ts) -> f n ts __
| R_lifts_1 (n, ts, u, us, _res0, r0, _res, r1) ->
  f0 n ts u us __ _res0 r0 _res r1 (coq_R_lifts_rec f f0 n us _res r1)

(** val coq_R_liftBs_rect :
    (nat -> coq_Brs -> __ -> 'a1) -> (nat -> coq_Brs -> name list -> coq_Term
    -> coq_Brs -> __ -> coq_Term -> coq_R_lift -> coq_Brs -> coq_R_liftBs ->
    'a1 -> 'a1) -> nat -> coq_Brs -> coq_Brs -> coq_R_liftBs -> 'a1 **)

let rec coq_R_liftBs_rect f f0 _ _ _ = function
| R_liftBs_0 (n, ts) -> f n ts __
| R_liftBs_1 (n, ts, m, b, bs, _res0, r0, _res, r1) ->
  f0 n ts m b bs __ _res0 r0 _res r1 (coq_R_liftBs_rect f f0 n bs _res r1)

(** val coq_R_liftBs_rec :
    (nat -> coq_Brs -> __ -> 'a1) -> (nat -> coq_Brs -> name list -> coq_Term
    -> coq_Brs -> __ -> coq_Term -> coq_R_lift -> coq_Brs -> coq_R_liftBs ->
    'a1 -> 'a1) -> nat -> coq_Brs -> coq_Brs -> coq_R_liftBs -> 'a1 **)

let rec coq_R_liftBs_rec f f0 _ _ _ = function
| R_liftBs_0 (n, ts) -> f n ts __
| R_liftBs_1 (n, ts, m, b, bs, _res0, r0, _res, r1) ->
  f0 n ts m b bs __ _res0 r0 _res r1 (coq_R_liftBs_rec f f0 n bs _res r1)

(** val coq_R_liftDs_rect :
    (nat -> coq_Defs -> __ -> 'a1) -> (nat -> coq_Defs -> name -> coq_Term ->
    nat -> coq_Defs -> __ -> coq_Term -> coq_R_lift -> coq_Defs ->
    coq_R_liftDs -> 'a1 -> 'a1) -> nat -> coq_Defs -> coq_Defs ->
    coq_R_liftDs -> 'a1 **)

let rec coq_R_liftDs_rect f f0 _ _ _ = function
| R_liftDs_0 (n, ds) -> f n ds __
| R_liftDs_1 (n, ds, nm, u, j, es, _res0, r0, _res, r1) ->
  f0 n ds nm u j es __ _res0 r0 _res r1 (coq_R_liftDs_rect f f0 n es _res r1)

(** val coq_R_liftDs_rec :
    (nat -> coq_Defs -> __ -> 'a1) -> (nat -> coq_Defs -> name -> coq_Term ->
    nat -> coq_Defs -> __ -> coq_Term -> coq_R_lift -> coq_Defs ->
    coq_R_liftDs -> 'a1 -> 'a1) -> nat -> coq_Defs -> coq_Defs ->
    coq_R_liftDs -> 'a1 **)

let rec coq_R_liftDs_rec f f0 _ _ _ = function
| R_liftDs_0 (n, ds) -> f n ds __
| R_liftDs_1 (n, ds, nm, u, j, es, _res0, r0, _res, r1) ->
  f0 n ds nm u j es __ _res0 r0 _res r1 (coq_R_liftDs_rec f f0 n es _res r1)

(** val lift_rect :
    (nat -> coq_Term -> nat -> __ -> __ -> 'a1) -> (nat -> coq_Term -> nat ->
    __ -> comparison -> __ -> __ -> 'a1) -> (nat -> coq_Term -> __ -> 'a1) ->
    (nat -> coq_Term -> name -> coq_Term -> __ -> 'a1 -> 'a1) -> (nat ->
    coq_Term -> name -> coq_Term -> coq_Term -> __ -> 'a1 -> 'a1 -> 'a1) ->
    (nat -> coq_Term -> coq_Term -> coq_Term -> __ -> 'a1 -> 'a1 -> 'a1) ->
    (nat -> coq_Term -> inductive -> nat -> coq_Terms -> __ -> 'a1) -> (nat
    -> coq_Term -> inductive -> coq_Term -> coq_Brs -> __ -> 'a1 -> 'a1) ->
    (nat -> coq_Term -> coq_Defs -> nat -> __ -> 'a1) -> (nat -> coq_Term ->
    coq_Term -> __ -> __ -> 'a1) -> nat -> coq_Term -> 'a1 **)

let rec lift_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 n t0 =
  let f9 = f n t0 in
  let f10 = f0 n t0 in
  let f11 = f1 n t0 in
  let f12 = f2 n t0 in
  let f13 = f3 n t0 in
  let f14 = f4 n t0 in
  let f15 = f5 n t0 in
  let f16 = f6 n t0 in
  let f17 = f7 n t0 in
  let f18 = f8 n t0 in
  let f19 = f18 t0 __ in
  (match t0 with
   | TRel n0 ->
     let f20 = f9 n0 __ in
     let f21 = f10 n0 __ in
     let f22 = let _x = Nat.compare n0 n in f21 _x __ in
     (match Nat.compare n0 n with
      | Lt -> f20 __
      | _ -> f22 __)
   | TProof -> f11 __
   | TLambda (n0, t1) ->
     let f20 = f12 n0 t1 __ in
     let hrec = lift_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 (S n) t1 in f20 hrec
   | TLetIn (n0, t1, t2) ->
     let f20 = f13 n0 t1 t2 __ in
     let f21 =
       let hrec = lift_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 n t1 in f20 hrec
     in
     let hrec = lift_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 (S n) t2 in f21 hrec
   | TApp (t1, t2) ->
     let f20 = f14 t1 t2 __ in
     let f21 =
       let hrec = lift_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 n t1 in f20 hrec
     in
     let hrec = lift_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 n t2 in f21 hrec
   | TConstruct (i, n0, t1) -> f15 i n0 t1 __
   | TCase (i, t1, b) ->
     let f20 = f16 i t1 b __ in
     let hrec = lift_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 n t1 in f20 hrec
   | TFix (d, n0) -> f17 d n0 __
   | _ -> f19 __)

(** val lift_rec :
    (nat -> coq_Term -> nat -> __ -> __ -> 'a1) -> (nat -> coq_Term -> nat ->
    __ -> comparison -> __ -> __ -> 'a1) -> (nat -> coq_Term -> __ -> 'a1) ->
    (nat -> coq_Term -> name -> coq_Term -> __ -> 'a1 -> 'a1) -> (nat ->
    coq_Term -> name -> coq_Term -> coq_Term -> __ -> 'a1 -> 'a1 -> 'a1) ->
    (nat -> coq_Term -> coq_Term -> coq_Term -> __ -> 'a1 -> 'a1 -> 'a1) ->
    (nat -> coq_Term -> inductive -> nat -> coq_Terms -> __ -> 'a1) -> (nat
    -> coq_Term -> inductive -> coq_Term -> coq_Brs -> __ -> 'a1 -> 'a1) ->
    (nat -> coq_Term -> coq_Defs -> nat -> __ -> 'a1) -> (nat -> coq_Term ->
    coq_Term -> __ -> __ -> 'a1) -> nat -> coq_Term -> 'a1 **)

let lift_rec =
  lift_rect

(** val lifts_rect :
    (nat -> coq_Terms -> __ -> 'a1) -> (nat -> coq_Terms -> coq_Term ->
    coq_Terms -> __ -> 'a1 -> 'a1) -> nat -> coq_Terms -> 'a1 **)

let rec lifts_rect f f0 n ts =
  let f1 = f n ts in
  let f2 = f0 n ts in
  (match ts with
   | Coq_tnil -> f1 __
   | Coq_tcons (t0, t1) ->
     let f3 = f2 t0 t1 __ in let hrec = lifts_rect f f0 n t1 in f3 hrec)

(** val lifts_rec :
    (nat -> coq_Terms -> __ -> 'a1) -> (nat -> coq_Terms -> coq_Term ->
    coq_Terms -> __ -> 'a1 -> 'a1) -> nat -> coq_Terms -> 'a1 **)

let lifts_rec =
  lifts_rect

(** val liftBs_rect :
    (nat -> coq_Brs -> __ -> 'a1) -> (nat -> coq_Brs -> name list -> coq_Term
    -> coq_Brs -> __ -> 'a1 -> 'a1) -> nat -> coq_Brs -> 'a1 **)

let rec liftBs_rect f f0 n ts =
  let f1 = f n ts in
  let f2 = f0 n ts in
  (match ts with
   | Coq_bnil -> f1 __
   | Coq_bcons (l, t0, b) ->
     let f3 = f2 l t0 b __ in let hrec = liftBs_rect f f0 n b in f3 hrec)

(** val liftBs_rec :
    (nat -> coq_Brs -> __ -> 'a1) -> (nat -> coq_Brs -> name list -> coq_Term
    -> coq_Brs -> __ -> 'a1 -> 'a1) -> nat -> coq_Brs -> 'a1 **)

let liftBs_rec =
  liftBs_rect

(** val liftDs_rect :
    (nat -> coq_Defs -> __ -> 'a1) -> (nat -> coq_Defs -> name -> coq_Term ->
    nat -> coq_Defs -> __ -> 'a1 -> 'a1) -> nat -> coq_Defs -> 'a1 **)

let rec liftDs_rect f f0 n ds =
  let f1 = f n ds in
  let f2 = f0 n ds in
  (match ds with
   | Coq_dnil -> f1 __
   | Coq_dcons (n0, t0, n1, d) ->
     let f3 = f2 n0 t0 n1 d __ in let hrec = liftDs_rect f f0 n d in f3 hrec)

(** val liftDs_rec :
    (nat -> coq_Defs -> __ -> 'a1) -> (nat -> coq_Defs -> name -> coq_Term ->
    nat -> coq_Defs -> __ -> 'a1 -> 'a1) -> nat -> coq_Defs -> 'a1 **)

let liftDs_rec =
  liftDs_rect

(** val coq_R_lift_correct : nat -> coq_Term -> coq_Term -> coq_R_lift **)

let coq_R_lift_correct n t0 _res =
  let f = fun y y0 y1 _ -> R_lift_0 (y, y0, y1) in
  let f0 = fun y y0 y1 y3 _ -> R_lift_1 (y, y0, y1, y3) in
  let f1 = fun y y0 _ -> R_lift_2 (y, y0) in
  let f2 = fun y y0 y1 y2 y4 _ -> R_lift_3 (y, y0, y1, y2, (lift (S y) y2),
    (y4 (lift (S y) y2) __))
  in
  let f3 = fun y y0 y1 y2 y3 y5 y6 _ -> R_lift_4 (y, y0, y1, y2, y3,
    (lift y y2), (y5 (lift y y2) __), (lift (S y) y3),
    (y6 (lift (S y) y3) __))
  in
  let f4 = fun y y0 y1 y2 y4 y5 _ -> R_lift_5 (y, y0, y1, y2, (lift y y1),
    (y4 (lift y y1) __), (lift y y2), (y5 (lift y y2) __))
  in
  let f5 = fun y y0 y1 y2 y3 y5 _ -> R_lift_6 (y, y0, y1, y2, y3,
    (lifts y y3), (y5 (lifts y y3) __))
  in
  let f6 = fun y y0 y1 y2 y3 y5 y6 _ -> R_lift_7 (y, y0, y1, y2, y3,
    (lift y y2), (y5 (lift y y2) __), (liftBs y y3), (y6 (liftBs y y3) __))
  in
  let f7 = fun y y0 y1 y2 y4 _ -> R_lift_8 (y, y0, y1, y2,
    (liftDs (add y (dlength y1)) y1),
    (y4 (liftDs (add y (dlength y1)) y1) __))
  in
  let f8 = fun y y0 y1 _ -> R_lift_9 (y, y0, y1) in
  let f9 = fun y y0 _ -> R_lifts_0 (y, y0) in
  let f10 = fun y y0 y1 y2 y4 y5 _ -> R_lifts_1 (y, y0, y1, y2, (lift y y1),
    (y4 (lift y y1) __), (lifts y y2), (y5 (lifts y y2) __))
  in
  let f11 = fun y y0 _ -> R_liftBs_0 (y, y0) in
  let f12 = fun y y0 y1 y2 y3 y5 y6 _ -> R_liftBs_1 (y, y0, y1, y2, y3,
    (lift (add (length y1) y) y2), (y5 (lift (add (length y1) y) y2) __),
    (liftBs y y3), (y6 (liftBs y y3) __))
  in
  let f13 = fun y y0 _ -> R_liftDs_0 (y, y0) in
  let f14 = fun y y0 y1 y2 y3 y4 y6 y7 _ -> R_liftDs_1 (y, y0, y1, y2, y3,
    y4, (lift y y2), (y6 (lift y y2) __), (liftDs y y4),
    (y7 (liftDs y y4) __))
  in
  let rec lift0 n0 t1 =
    let f15 = fun y1 z -> f n0 t1 y1 z in
    let f16 = fun y1 y3 z -> f0 n0 t1 y1 y3 z in
    let f17 = fun z -> f1 n0 t1 z in
    let f18 = fun y1 y2 y4 z -> f2 n0 t1 y1 y2 y4 z in
    let f19 = fun y1 y2 y3 y5 y6 z -> f3 n0 t1 y1 y2 y3 y5 y6 z in
    let f20 = fun y1 y2 y4 y5 z -> f4 n0 t1 y1 y2 y4 y5 z in
    let f21 = fun y1 y2 y3 y5 z -> f5 n0 t1 y1 y2 y3 y5 z in
    let f22 = fun y1 y2 y3 y5 y6 z -> f6 n0 t1 y1 y2 y3 y5 y6 z in
    let f23 = fun y1 y2 y4 z -> f7 n0 t1 y1 y2 y4 z in
    let f24 = fun y1 z -> f8 n0 t1 y1 z in
    let f25 = fun z -> f24 t1 z in
    (match t1 with
     | TRel n1 ->
       let f26 = fun z -> f15 n1 z in
       let f27 = fun y3 z -> f16 n1 y3 z in
       let f28 = let _x = Nat.compare n1 n0 in (fun _ z _ -> f27 _x z) in
       (match Nat.compare n1 n0 with
        | Lt -> (fun z _ -> f26 z)
        | _ -> f28 __)
     | TProof -> (fun z _ -> f17 z)
     | TLambda (n1, t2) ->
       let f26 = fun y4 z -> f18 n1 t2 y4 z in
       let hrec = lift0 (S n0) t2 in (fun z _ -> f26 hrec z)
     | TLetIn (n1, t2, t3) ->
       let f26 = fun y5 y6 z -> f19 n1 t2 t3 y5 y6 z in
       let f27 = let hrec = lift0 n0 t2 in (fun y6 z _ -> f26 hrec y6 z) in
       let hrec = lift0 (S n0) t3 in f27 hrec
     | TApp (t2, t3) ->
       let f26 = fun y4 y5 z -> f20 t2 t3 y4 y5 z in
       let f27 = let hrec = lift0 n0 t2 in (fun y5 z _ -> f26 hrec y5 z) in
       let hrec = lift0 n0 t3 in f27 hrec
     | TConstruct (i, n1, t2) ->
       let f26 = fun y5 z -> f21 i n1 t2 y5 z in
       let hrec = lifts0 n0 t2 in (fun z _ -> f26 hrec z)
     | TCase (i, t2, b) ->
       let f26 = fun y5 y6 z -> f22 i t2 b y5 y6 z in
       let f27 = let hrec = lift0 n0 t2 in (fun y6 z _ -> f26 hrec y6 z) in
       let hrec = liftBs0 n0 b in f27 hrec
     | TFix (d, n1) ->
       let f26 = fun y4 z -> f23 d n1 y4 z in
       let hrec = liftDs0 (add n0 (dlength d)) d in (fun z _ -> f26 hrec z)
     | _ -> (fun z _ -> f25 z))
  and lifts0 n0 ts =
    let f15 = fun z -> f9 n0 ts z in
    let f16 = fun y1 y2 y4 y5 z -> f10 n0 ts y1 y2 y4 y5 z in
    (match ts with
     | Coq_tnil -> (fun z _ -> f15 z)
     | Coq_tcons (t1, t2) ->
       let f17 = fun y4 y5 z -> f16 t1 t2 y4 y5 z in
       let f18 = let hrec = lift0 n0 t1 in (fun y5 z _ -> f17 hrec y5 z) in
       let hrec = lifts0 n0 t2 in f18 hrec)
  and liftBs0 n0 ts =
    let f15 = fun z -> f11 n0 ts z in
    let f16 = fun y1 y2 y3 y5 y6 z -> f12 n0 ts y1 y2 y3 y5 y6 z in
    (match ts with
     | Coq_bnil -> (fun z _ -> f15 z)
     | Coq_bcons (l, t1, b) ->
       let f17 = fun y5 y6 z -> f16 l t1 b y5 y6 z in
       let f18 =
         let hrec = lift0 (add (length l) n0) t1 in
         (fun y6 z _ -> f17 hrec y6 z)
       in
       let hrec = liftBs0 n0 b in f18 hrec)
  and liftDs0 n0 ds =
    let f15 = fun z -> f13 n0 ds z in
    let f16 = fun y1 y2 y3 y4 y6 y7 z -> f14 n0 ds y1 y2 y3 y4 y6 y7 z in
    (match ds with
     | Coq_dnil -> (fun z _ -> f15 z)
     | Coq_dcons (n1, t1, n2, d) ->
       let f17 = fun y6 y7 z -> f16 n1 t1 n2 d y6 y7 z in
       let f18 = let hrec = lift0 n0 t1 in (fun y7 z _ -> f17 hrec y7 z) in
       let hrec = liftDs0 n0 d in f18 hrec)
  in lift0 n t0 _res __

(** val coq_R_lifts_correct : nat -> coq_Terms -> coq_Terms -> coq_R_lifts **)

let coq_R_lifts_correct n ts _res =
  let f = fun y y0 y1 _ -> R_lift_0 (y, y0, y1) in
  let f0 = fun y y0 y1 y3 _ -> R_lift_1 (y, y0, y1, y3) in
  let f1 = fun y y0 _ -> R_lift_2 (y, y0) in
  let f2 = fun y y0 y1 y2 y4 _ -> R_lift_3 (y, y0, y1, y2, (lift (S y) y2),
    (y4 (lift (S y) y2) __))
  in
  let f3 = fun y y0 y1 y2 y3 y5 y6 _ -> R_lift_4 (y, y0, y1, y2, y3,
    (lift y y2), (y5 (lift y y2) __), (lift (S y) y3),
    (y6 (lift (S y) y3) __))
  in
  let f4 = fun y y0 y1 y2 y4 y5 _ -> R_lift_5 (y, y0, y1, y2, (lift y y1),
    (y4 (lift y y1) __), (lift y y2), (y5 (lift y y2) __))
  in
  let f5 = fun y y0 y1 y2 y3 y5 _ -> R_lift_6 (y, y0, y1, y2, y3,
    (lifts y y3), (y5 (lifts y y3) __))
  in
  let f6 = fun y y0 y1 y2 y3 y5 y6 _ -> R_lift_7 (y, y0, y1, y2, y3,
    (lift y y2), (y5 (lift y y2) __), (liftBs y y3), (y6 (liftBs y y3) __))
  in
  let f7 = fun y y0 y1 y2 y4 _ -> R_lift_8 (y, y0, y1, y2,
    (liftDs (add y (dlength y1)) y1),
    (y4 (liftDs (add y (dlength y1)) y1) __))
  in
  let f8 = fun y y0 y1 _ -> R_lift_9 (y, y0, y1) in
  let f9 = fun y y0 _ -> R_lifts_0 (y, y0) in
  let f10 = fun y y0 y1 y2 y4 y5 _ -> R_lifts_1 (y, y0, y1, y2, (lift y y1),
    (y4 (lift y y1) __), (lifts y y2), (y5 (lifts y y2) __))
  in
  let f11 = fun y y0 _ -> R_liftBs_0 (y, y0) in
  let f12 = fun y y0 y1 y2 y3 y5 y6 _ -> R_liftBs_1 (y, y0, y1, y2, y3,
    (lift (add (length y1) y) y2), (y5 (lift (add (length y1) y) y2) __),
    (liftBs y y3), (y6 (liftBs y y3) __))
  in
  let f13 = fun y y0 _ -> R_liftDs_0 (y, y0) in
  let f14 = fun y y0 y1 y2 y3 y4 y6 y7 _ -> R_liftDs_1 (y, y0, y1, y2, y3,
    y4, (lift y y2), (y6 (lift y y2) __), (liftDs y y4),
    (y7 (liftDs y y4) __))
  in
  let rec lift0 n0 t0 =
    let f15 = fun y1 z -> f n0 t0 y1 z in
    let f16 = fun y1 y3 z -> f0 n0 t0 y1 y3 z in
    let f17 = fun z -> f1 n0 t0 z in
    let f18 = fun y1 y2 y4 z -> f2 n0 t0 y1 y2 y4 z in
    let f19 = fun y1 y2 y3 y5 y6 z -> f3 n0 t0 y1 y2 y3 y5 y6 z in
    let f20 = fun y1 y2 y4 y5 z -> f4 n0 t0 y1 y2 y4 y5 z in
    let f21 = fun y1 y2 y3 y5 z -> f5 n0 t0 y1 y2 y3 y5 z in
    let f22 = fun y1 y2 y3 y5 y6 z -> f6 n0 t0 y1 y2 y3 y5 y6 z in
    let f23 = fun y1 y2 y4 z -> f7 n0 t0 y1 y2 y4 z in
    let f24 = fun y1 z -> f8 n0 t0 y1 z in
    let f25 = fun z -> f24 t0 z in
    (match t0 with
     | TRel n1 ->
       let f26 = fun z -> f15 n1 z in
       let f27 = fun y3 z -> f16 n1 y3 z in
       let f28 = let _x = Nat.compare n1 n0 in (fun _ z _ -> f27 _x z) in
       (match Nat.compare n1 n0 with
        | Lt -> (fun z _ -> f26 z)
        | _ -> f28 __)
     | TProof -> (fun z _ -> f17 z)
     | TLambda (n1, t1) ->
       let f26 = fun y4 z -> f18 n1 t1 y4 z in
       let hrec = lift0 (S n0) t1 in (fun z _ -> f26 hrec z)
     | TLetIn (n1, t1, t2) ->
       let f26 = fun y5 y6 z -> f19 n1 t1 t2 y5 y6 z in
       let f27 = let hrec = lift0 n0 t1 in (fun y6 z _ -> f26 hrec y6 z) in
       let hrec = lift0 (S n0) t2 in f27 hrec
     | TApp (t1, t2) ->
       let f26 = fun y4 y5 z -> f20 t1 t2 y4 y5 z in
       let f27 = let hrec = lift0 n0 t1 in (fun y5 z _ -> f26 hrec y5 z) in
       let hrec = lift0 n0 t2 in f27 hrec
     | TConstruct (i, n1, t1) ->
       let f26 = fun y5 z -> f21 i n1 t1 y5 z in
       let hrec = lifts0 n0 t1 in (fun z _ -> f26 hrec z)
     | TCase (i, t1, b) ->
       let f26 = fun y5 y6 z -> f22 i t1 b y5 y6 z in
       let f27 = let hrec = lift0 n0 t1 in (fun y6 z _ -> f26 hrec y6 z) in
       let hrec = liftBs0 n0 b in f27 hrec
     | TFix (d, n1) ->
       let f26 = fun y4 z -> f23 d n1 y4 z in
       let hrec = liftDs0 (add n0 (dlength d)) d in (fun z _ -> f26 hrec z)
     | _ -> (fun z _ -> f25 z))
  and lifts0 n0 ts0 =
    let f15 = fun z -> f9 n0 ts0 z in
    let f16 = fun y1 y2 y4 y5 z -> f10 n0 ts0 y1 y2 y4 y5 z in
    (match ts0 with
     | Coq_tnil -> (fun z _ -> f15 z)
     | Coq_tcons (t0, t1) ->
       let f17 = fun y4 y5 z -> f16 t0 t1 y4 y5 z in
       let f18 = let hrec = lift0 n0 t0 in (fun y5 z _ -> f17 hrec y5 z) in
       let hrec = lifts0 n0 t1 in f18 hrec)
  and liftBs0 n0 ts0 =
    let f15 = fun z -> f11 n0 ts0 z in
    let f16 = fun y1 y2 y3 y5 y6 z -> f12 n0 ts0 y1 y2 y3 y5 y6 z in
    (match ts0 with
     | Coq_bnil -> (fun z _ -> f15 z)
     | Coq_bcons (l, t0, b) ->
       let f17 = fun y5 y6 z -> f16 l t0 b y5 y6 z in
       let f18 =
         let hrec = lift0 (add (length l) n0) t0 in
         (fun y6 z _ -> f17 hrec y6 z)
       in
       let hrec = liftBs0 n0 b in f18 hrec)
  and liftDs0 n0 ds =
    let f15 = fun z -> f13 n0 ds z in
    let f16 = fun y1 y2 y3 y4 y6 y7 z -> f14 n0 ds y1 y2 y3 y4 y6 y7 z in
    (match ds with
     | Coq_dnil -> (fun z _ -> f15 z)
     | Coq_dcons (n1, t0, n2, d) ->
       let f17 = fun y6 y7 z -> f16 n1 t0 n2 d y6 y7 z in
       let f18 = let hrec = lift0 n0 t0 in (fun y7 z _ -> f17 hrec y7 z) in
       let hrec = liftDs0 n0 d in f18 hrec)
  in lifts0 n ts _res __

(** val coq_R_liftBs_correct : nat -> coq_Brs -> coq_Brs -> coq_R_liftBs **)

let coq_R_liftBs_correct n ts _res =
  let f = fun y y0 y1 _ -> R_lift_0 (y, y0, y1) in
  let f0 = fun y y0 y1 y3 _ -> R_lift_1 (y, y0, y1, y3) in
  let f1 = fun y y0 _ -> R_lift_2 (y, y0) in
  let f2 = fun y y0 y1 y2 y4 _ -> R_lift_3 (y, y0, y1, y2, (lift (S y) y2),
    (y4 (lift (S y) y2) __))
  in
  let f3 = fun y y0 y1 y2 y3 y5 y6 _ -> R_lift_4 (y, y0, y1, y2, y3,
    (lift y y2), (y5 (lift y y2) __), (lift (S y) y3),
    (y6 (lift (S y) y3) __))
  in
  let f4 = fun y y0 y1 y2 y4 y5 _ -> R_lift_5 (y, y0, y1, y2, (lift y y1),
    (y4 (lift y y1) __), (lift y y2), (y5 (lift y y2) __))
  in
  let f5 = fun y y0 y1 y2 y3 y5 _ -> R_lift_6 (y, y0, y1, y2, y3,
    (lifts y y3), (y5 (lifts y y3) __))
  in
  let f6 = fun y y0 y1 y2 y3 y5 y6 _ -> R_lift_7 (y, y0, y1, y2, y3,
    (lift y y2), (y5 (lift y y2) __), (liftBs y y3), (y6 (liftBs y y3) __))
  in
  let f7 = fun y y0 y1 y2 y4 _ -> R_lift_8 (y, y0, y1, y2,
    (liftDs (add y (dlength y1)) y1),
    (y4 (liftDs (add y (dlength y1)) y1) __))
  in
  let f8 = fun y y0 y1 _ -> R_lift_9 (y, y0, y1) in
  let f9 = fun y y0 _ -> R_lifts_0 (y, y0) in
  let f10 = fun y y0 y1 y2 y4 y5 _ -> R_lifts_1 (y, y0, y1, y2, (lift y y1),
    (y4 (lift y y1) __), (lifts y y2), (y5 (lifts y y2) __))
  in
  let f11 = fun y y0 _ -> R_liftBs_0 (y, y0) in
  let f12 = fun y y0 y1 y2 y3 y5 y6 _ -> R_liftBs_1 (y, y0, y1, y2, y3,
    (lift (add (length y1) y) y2), (y5 (lift (add (length y1) y) y2) __),
    (liftBs y y3), (y6 (liftBs y y3) __))
  in
  let f13 = fun y y0 _ -> R_liftDs_0 (y, y0) in
  let f14 = fun y y0 y1 y2 y3 y4 y6 y7 _ -> R_liftDs_1 (y, y0, y1, y2, y3,
    y4, (lift y y2), (y6 (lift y y2) __), (liftDs y y4),
    (y7 (liftDs y y4) __))
  in
  let rec lift0 n0 t0 =
    let f15 = fun y1 z -> f n0 t0 y1 z in
    let f16 = fun y1 y3 z -> f0 n0 t0 y1 y3 z in
    let f17 = fun z -> f1 n0 t0 z in
    let f18 = fun y1 y2 y4 z -> f2 n0 t0 y1 y2 y4 z in
    let f19 = fun y1 y2 y3 y5 y6 z -> f3 n0 t0 y1 y2 y3 y5 y6 z in
    let f20 = fun y1 y2 y4 y5 z -> f4 n0 t0 y1 y2 y4 y5 z in
    let f21 = fun y1 y2 y3 y5 z -> f5 n0 t0 y1 y2 y3 y5 z in
    let f22 = fun y1 y2 y3 y5 y6 z -> f6 n0 t0 y1 y2 y3 y5 y6 z in
    let f23 = fun y1 y2 y4 z -> f7 n0 t0 y1 y2 y4 z in
    let f24 = fun y1 z -> f8 n0 t0 y1 z in
    let f25 = fun z -> f24 t0 z in
    (match t0 with
     | TRel n1 ->
       let f26 = fun z -> f15 n1 z in
       let f27 = fun y3 z -> f16 n1 y3 z in
       let f28 = let _x = Nat.compare n1 n0 in (fun _ z _ -> f27 _x z) in
       (match Nat.compare n1 n0 with
        | Lt -> (fun z _ -> f26 z)
        | _ -> f28 __)
     | TProof -> (fun z _ -> f17 z)
     | TLambda (n1, t1) ->
       let f26 = fun y4 z -> f18 n1 t1 y4 z in
       let hrec = lift0 (S n0) t1 in (fun z _ -> f26 hrec z)
     | TLetIn (n1, t1, t2) ->
       let f26 = fun y5 y6 z -> f19 n1 t1 t2 y5 y6 z in
       let f27 = let hrec = lift0 n0 t1 in (fun y6 z _ -> f26 hrec y6 z) in
       let hrec = lift0 (S n0) t2 in f27 hrec
     | TApp (t1, t2) ->
       let f26 = fun y4 y5 z -> f20 t1 t2 y4 y5 z in
       let f27 = let hrec = lift0 n0 t1 in (fun y5 z _ -> f26 hrec y5 z) in
       let hrec = lift0 n0 t2 in f27 hrec
     | TConstruct (i, n1, t1) ->
       let f26 = fun y5 z -> f21 i n1 t1 y5 z in
       let hrec = lifts0 n0 t1 in (fun z _ -> f26 hrec z)
     | TCase (i, t1, b) ->
       let f26 = fun y5 y6 z -> f22 i t1 b y5 y6 z in
       let f27 = let hrec = lift0 n0 t1 in (fun y6 z _ -> f26 hrec y6 z) in
       let hrec = liftBs0 n0 b in f27 hrec
     | TFix (d, n1) ->
       let f26 = fun y4 z -> f23 d n1 y4 z in
       let hrec = liftDs0 (add n0 (dlength d)) d in (fun z _ -> f26 hrec z)
     | _ -> (fun z _ -> f25 z))
  and lifts0 n0 ts0 =
    let f15 = fun z -> f9 n0 ts0 z in
    let f16 = fun y1 y2 y4 y5 z -> f10 n0 ts0 y1 y2 y4 y5 z in
    (match ts0 with
     | Coq_tnil -> (fun z _ -> f15 z)
     | Coq_tcons (t0, t1) ->
       let f17 = fun y4 y5 z -> f16 t0 t1 y4 y5 z in
       let f18 = let hrec = lift0 n0 t0 in (fun y5 z _ -> f17 hrec y5 z) in
       let hrec = lifts0 n0 t1 in f18 hrec)
  and liftBs0 n0 ts0 =
    let f15 = fun z -> f11 n0 ts0 z in
    let f16 = fun y1 y2 y3 y5 y6 z -> f12 n0 ts0 y1 y2 y3 y5 y6 z in
    (match ts0 with
     | Coq_bnil -> (fun z _ -> f15 z)
     | Coq_bcons (l, t0, b) ->
       let f17 = fun y5 y6 z -> f16 l t0 b y5 y6 z in
       let f18 =
         let hrec = lift0 (add (length l) n0) t0 in
         (fun y6 z _ -> f17 hrec y6 z)
       in
       let hrec = liftBs0 n0 b in f18 hrec)
  and liftDs0 n0 ds =
    let f15 = fun z -> f13 n0 ds z in
    let f16 = fun y1 y2 y3 y4 y6 y7 z -> f14 n0 ds y1 y2 y3 y4 y6 y7 z in
    (match ds with
     | Coq_dnil -> (fun z _ -> f15 z)
     | Coq_dcons (n1, t0, n2, d) ->
       let f17 = fun y6 y7 z -> f16 n1 t0 n2 d y6 y7 z in
       let f18 = let hrec = lift0 n0 t0 in (fun y7 z _ -> f17 hrec y7 z) in
       let hrec = liftDs0 n0 d in f18 hrec)
  in liftBs0 n ts _res __

(** val coq_R_liftDs_correct : nat -> coq_Defs -> coq_Defs -> coq_R_liftDs **)

let coq_R_liftDs_correct n ds _res =
  let f = fun y y0 y1 _ -> R_lift_0 (y, y0, y1) in
  let f0 = fun y y0 y1 y3 _ -> R_lift_1 (y, y0, y1, y3) in
  let f1 = fun y y0 _ -> R_lift_2 (y, y0) in
  let f2 = fun y y0 y1 y2 y4 _ -> R_lift_3 (y, y0, y1, y2, (lift (S y) y2),
    (y4 (lift (S y) y2) __))
  in
  let f3 = fun y y0 y1 y2 y3 y5 y6 _ -> R_lift_4 (y, y0, y1, y2, y3,
    (lift y y2), (y5 (lift y y2) __), (lift (S y) y3),
    (y6 (lift (S y) y3) __))
  in
  let f4 = fun y y0 y1 y2 y4 y5 _ -> R_lift_5 (y, y0, y1, y2, (lift y y1),
    (y4 (lift y y1) __), (lift y y2), (y5 (lift y y2) __))
  in
  let f5 = fun y y0 y1 y2 y3 y5 _ -> R_lift_6 (y, y0, y1, y2, y3,
    (lifts y y3), (y5 (lifts y y3) __))
  in
  let f6 = fun y y0 y1 y2 y3 y5 y6 _ -> R_lift_7 (y, y0, y1, y2, y3,
    (lift y y2), (y5 (lift y y2) __), (liftBs y y3), (y6 (liftBs y y3) __))
  in
  let f7 = fun y y0 y1 y2 y4 _ -> R_lift_8 (y, y0, y1, y2,
    (liftDs (add y (dlength y1)) y1),
    (y4 (liftDs (add y (dlength y1)) y1) __))
  in
  let f8 = fun y y0 y1 _ -> R_lift_9 (y, y0, y1) in
  let f9 = fun y y0 _ -> R_lifts_0 (y, y0) in
  let f10 = fun y y0 y1 y2 y4 y5 _ -> R_lifts_1 (y, y0, y1, y2, (lift y y1),
    (y4 (lift y y1) __), (lifts y y2), (y5 (lifts y y2) __))
  in
  let f11 = fun y y0 _ -> R_liftBs_0 (y, y0) in
  let f12 = fun y y0 y1 y2 y3 y5 y6 _ -> R_liftBs_1 (y, y0, y1, y2, y3,
    (lift (add (length y1) y) y2), (y5 (lift (add (length y1) y) y2) __),
    (liftBs y y3), (y6 (liftBs y y3) __))
  in
  let f13 = fun y y0 _ -> R_liftDs_0 (y, y0) in
  let f14 = fun y y0 y1 y2 y3 y4 y6 y7 _ -> R_liftDs_1 (y, y0, y1, y2, y3,
    y4, (lift y y2), (y6 (lift y y2) __), (liftDs y y4),
    (y7 (liftDs y y4) __))
  in
  let rec lift0 n0 t0 =
    let f15 = fun y1 z -> f n0 t0 y1 z in
    let f16 = fun y1 y3 z -> f0 n0 t0 y1 y3 z in
    let f17 = fun z -> f1 n0 t0 z in
    let f18 = fun y1 y2 y4 z -> f2 n0 t0 y1 y2 y4 z in
    let f19 = fun y1 y2 y3 y5 y6 z -> f3 n0 t0 y1 y2 y3 y5 y6 z in
    let f20 = fun y1 y2 y4 y5 z -> f4 n0 t0 y1 y2 y4 y5 z in
    let f21 = fun y1 y2 y3 y5 z -> f5 n0 t0 y1 y2 y3 y5 z in
    let f22 = fun y1 y2 y3 y5 y6 z -> f6 n0 t0 y1 y2 y3 y5 y6 z in
    let f23 = fun y1 y2 y4 z -> f7 n0 t0 y1 y2 y4 z in
    let f24 = fun y1 z -> f8 n0 t0 y1 z in
    let f25 = fun z -> f24 t0 z in
    (match t0 with
     | TRel n1 ->
       let f26 = fun z -> f15 n1 z in
       let f27 = fun y3 z -> f16 n1 y3 z in
       let f28 = let _x = Nat.compare n1 n0 in (fun _ z _ -> f27 _x z) in
       (match Nat.compare n1 n0 with
        | Lt -> (fun z _ -> f26 z)
        | _ -> f28 __)
     | TProof -> (fun z _ -> f17 z)
     | TLambda (n1, t1) ->
       let f26 = fun y4 z -> f18 n1 t1 y4 z in
       let hrec = lift0 (S n0) t1 in (fun z _ -> f26 hrec z)
     | TLetIn (n1, t1, t2) ->
       let f26 = fun y5 y6 z -> f19 n1 t1 t2 y5 y6 z in
       let f27 = let hrec = lift0 n0 t1 in (fun y6 z _ -> f26 hrec y6 z) in
       let hrec = lift0 (S n0) t2 in f27 hrec
     | TApp (t1, t2) ->
       let f26 = fun y4 y5 z -> f20 t1 t2 y4 y5 z in
       let f27 = let hrec = lift0 n0 t1 in (fun y5 z _ -> f26 hrec y5 z) in
       let hrec = lift0 n0 t2 in f27 hrec
     | TConstruct (i, n1, t1) ->
       let f26 = fun y5 z -> f21 i n1 t1 y5 z in
       let hrec = lifts0 n0 t1 in (fun z _ -> f26 hrec z)
     | TCase (i, t1, b) ->
       let f26 = fun y5 y6 z -> f22 i t1 b y5 y6 z in
       let f27 = let hrec = lift0 n0 t1 in (fun y6 z _ -> f26 hrec y6 z) in
       let hrec = liftBs0 n0 b in f27 hrec
     | TFix (d, n1) ->
       let f26 = fun y4 z -> f23 d n1 y4 z in
       let hrec = liftDs0 (add n0 (dlength d)) d in (fun z _ -> f26 hrec z)
     | _ -> (fun z _ -> f25 z))
  and lifts0 n0 ts =
    let f15 = fun z -> f9 n0 ts z in
    let f16 = fun y1 y2 y4 y5 z -> f10 n0 ts y1 y2 y4 y5 z in
    (match ts with
     | Coq_tnil -> (fun z _ -> f15 z)
     | Coq_tcons (t0, t1) ->
       let f17 = fun y4 y5 z -> f16 t0 t1 y4 y5 z in
       let f18 = let hrec = lift0 n0 t0 in (fun y5 z _ -> f17 hrec y5 z) in
       let hrec = lifts0 n0 t1 in f18 hrec)
  and liftBs0 n0 ts =
    let f15 = fun z -> f11 n0 ts z in
    let f16 = fun y1 y2 y3 y5 y6 z -> f12 n0 ts y1 y2 y3 y5 y6 z in
    (match ts with
     | Coq_bnil -> (fun z _ -> f15 z)
     | Coq_bcons (l, t0, b) ->
       let f17 = fun y5 y6 z -> f16 l t0 b y5 y6 z in
       let f18 =
         let hrec = lift0 (add (length l) n0) t0 in
         (fun y6 z _ -> f17 hrec y6 z)
       in
       let hrec = liftBs0 n0 b in f18 hrec)
  and liftDs0 n0 ds0 =
    let f15 = fun z -> f13 n0 ds0 z in
    let f16 = fun y1 y2 y3 y4 y6 y7 z -> f14 n0 ds0 y1 y2 y3 y4 y6 y7 z in
    (match ds0 with
     | Coq_dnil -> (fun z _ -> f15 z)
     | Coq_dcons (n1, t0, n2, d) ->
       let f17 = fun y6 y7 z -> f16 n1 t0 n2 d y6 y7 z in
       let f18 = let hrec = lift0 n0 t0 in (fun y7 z _ -> f17 hrec y7 z) in
       let hrec = liftDs0 n0 d in f18 hrec)
  in liftDs0 n ds _res __

(** val coq_TmkApps : coq_Term -> coq_Terms -> coq_Term **)

let rec coq_TmkApps u = function
| Coq_tnil -> u
| Coq_tcons (t0, v0) -> coq_TmkApps (TApp (u, t0)) v0

(** val list_terms : coq_Term list -> coq_Terms **)

let rec list_terms = function
| [] -> Coq_tnil
| t0 :: ts -> Coq_tcons (t0, (list_terms ts))

(** val list_Brs : (name list * coq_Term) list -> coq_Brs **)

let rec list_Brs = function
| [] -> Coq_bnil
| y :: ts -> let (x, t0) = y in Coq_bcons (x, t0, (list_Brs ts))

(** val list_Defs : coq_Term def list -> coq_Defs **)

let rec list_Defs = function
| [] -> Coq_dnil
| t0 :: ts -> Coq_dcons (t0.dname, t0.dbody, t0.rarg, (list_Defs ts))

(** val trans_prim_val : 'a1 prim_val -> primitive option **)

let trans_prim_val = function
| Coq_existT (x, p0) ->
  (match x with
   | Coq_primInt ->
     (match p0 with
      | Coq_primIntModel i ->
        Some (Coq_existT (AstCommon.Coq_primInt, (Obj.magic i)))
      | _ -> assert false (* absurd case *))
   | Coq_primFloat ->
     (match p0 with
      | Coq_primFloatModel f ->
        Some (Coq_existT (AstCommon.Coq_primFloat, (Obj.magic f)))
      | _ -> assert false (* absurd case *))
   | Coq_primArray -> None)

type 't trans_prim_val_graph =
| Coq_trans_prim_val_graph_equation_1 of Uint63.t
| Coq_trans_prim_val_graph_equation_2 of Float64.t
| Coq_trans_prim_val_graph_equation_3 of 't prim_model

(** val trans_prim_val_graph_rect :
    (__ -> Uint63.t -> 'a1) -> (__ -> Float64.t -> 'a1) -> (__ -> __
    prim_model -> 'a1) -> 'a2 prim_val -> primitive option -> 'a2
    trans_prim_val_graph -> 'a1 **)

let trans_prim_val_graph_rect f f0 f1 _ _ = function
| Coq_trans_prim_val_graph_equation_1 i -> f __ i
| Coq_trans_prim_val_graph_equation_2 i -> f0 __ i
| Coq_trans_prim_val_graph_equation_3 p -> Obj.magic f1 __ p

(** val trans_prim_val_graph_correct :
    'a1 prim_val -> 'a1 trans_prim_val_graph **)

let trans_prim_val_graph_correct = function
| Coq_existT (x, p0) ->
  (match x with
   | Coq_primInt ->
     (match p0 with
      | Coq_primIntModel i -> Coq_trans_prim_val_graph_equation_1 i
      | _ -> assert false (* absurd case *))
   | Coq_primFloat ->
     (match p0 with
      | Coq_primFloatModel f -> Coq_trans_prim_val_graph_equation_2 f
      | _ -> assert false (* absurd case *))
   | Coq_primArray -> Coq_trans_prim_val_graph_equation_3 p0)

(** val trans_prim_val_elim :
    (__ -> Uint63.t -> 'a1) -> (__ -> Float64.t -> 'a1) -> (__ -> __
    prim_model -> 'a1) -> 'a2 prim_val -> 'a1 **)

let trans_prim_val_elim f f0 f1 p =
  match trans_prim_val_graph_correct p with
  | Coq_trans_prim_val_graph_equation_1 i -> f __ i
  | Coq_trans_prim_val_graph_equation_2 i -> f0 __ i
  | Coq_trans_prim_val_graph_equation_3 p0 -> Obj.magic f1 __ p0

(** val coq_FunctionalElimination_trans_prim_val :
    (__ -> Uint63.t -> __) -> (__ -> Float64.t -> __) -> (__ -> __ prim_model
    -> __) -> __ prim_val -> __ **)

let coq_FunctionalElimination_trans_prim_val =
  trans_prim_val_elim

(** val coq_FunctionalInduction_trans_prim_val :
    (__ -> __ prim_val -> primitive option) coq_FunctionalInduction **)

let coq_FunctionalInduction_trans_prim_val =
  Obj.magic (fun _ -> trans_prim_val_graph_correct)

(** val compile_clause_1_clause_12 :
    term prim_val -> primitive option -> (term -> __ -> coq_Term) -> coq_Term **)

let compile_clause_1_clause_12 _ refine _ =
  match refine with
  | Some p -> TPrim p
  | None ->
    TWrong (String.String (Coq_x75, (String.String (Coq_x6e, (String.String
      (Coq_x73, (String.String (Coq_x75, (String.String (Coq_x70,
      (String.String (Coq_x70, (String.String (Coq_x6f, (String.String
      (Coq_x72, (String.String (Coq_x74, (String.String (Coq_x65,
      (String.String (Coq_x64, (String.String (Coq_x20, (String.String
      (Coq_x70, (String.String (Coq_x72, (String.String (Coq_x69,
      (String.String (Coq_x6d, (String.String (Coq_x74, (String.String
      (Coq_x69, (String.String (Coq_x76, (String.String (Coq_x65,
      (String.String (Coq_x20, (String.String (Coq_x74, (String.String
      (Coq_x79, (String.String (Coq_x70, (String.String (Coq_x65,
      String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))

(** val compile_clause_1 :
    term -> TermSpineView.t -> (term -> __ -> coq_Term) -> coq_Term **)

let compile_clause_1 _ refine compile0 =
  match refine with
  | TermSpineView.Coq_tBox -> TProof
  | TermSpineView.Coq_tRel n -> TRel n
  | TermSpineView.Coq_tVar _ ->
    TWrong (String.String (Coq_x56, (String.String (Coq_x61, (String.String
      (Coq_x72, String.EmptyString))))))
  | TermSpineView.Coq_tEvar (_, _) ->
    TWrong (String.String (Coq_x45, (String.String (Coq_x76, (String.String
      (Coq_x61, (String.String (Coq_x72, String.EmptyString))))))))
  | TermSpineView.Coq_tLambda (n, b) -> TLambda (n, (compile0 b __))
  | TermSpineView.Coq_tLetIn (n, b, b') ->
    TLetIn (n, (compile0 b __), (compile0 b' __))
  | TermSpineView.Coq_tApp (f, l) ->
    coq_TmkApps (compile0 f __)
      (list_terms (map_InP l (fun x _ -> compile0 x __)))
  | TermSpineView.Coq_tConst kn -> TConst kn
  | TermSpineView.Coq_tConstruct (i, n, args) ->
    TConstruct (i, n, (list_terms (map_InP args (fun x _ -> compile0 x __))))
  | TermSpineView.Coq_tCase (ci, p, brs) ->
    let brs' =
      map_InP brs (fun x _ -> ((List0.rev (fst x)), (compile0 (snd x) __)))
    in
    TCase ((fst ci), (compile0 p __), (list_Brs brs'))
  | TermSpineView.Coq_tProj (_, _) ->
    TWrong (String.String (Coq_x50, (String.String (Coq_x72, (String.String
      (Coq_x6f, (String.String (Coq_x6a, String.EmptyString))))))))
  | TermSpineView.Coq_tFix (mfix, idx) ->
    let mfix' =
      map_InP mfix (fun d _ -> { E.dname = d.dname; E.dbody =
        (compile0 d.dbody __); E.rarg = d.rarg })
    in
    TFix ((list_Defs mfix'), idx)
  | TermSpineView.Coq_tCoFix (_, _) ->
    TWrong (String.String (Coq_x54, (String.String (Coq_x43, (String.String
      (Coq_x6f, (String.String (Coq_x66, (String.String (Coq_x69,
      (String.String (Coq_x78, String.EmptyString))))))))))))
  | TermSpineView.Coq_tPrim p ->
    (match trans_prim_val p with
     | Some p0 -> TPrim p0
     | None ->
       TWrong (String.String (Coq_x75, (String.String (Coq_x6e,
         (String.String (Coq_x73, (String.String (Coq_x75, (String.String
         (Coq_x70, (String.String (Coq_x70, (String.String (Coq_x6f,
         (String.String (Coq_x72, (String.String (Coq_x74, (String.String
         (Coq_x65, (String.String (Coq_x64, (String.String (Coq_x20,
         (String.String (Coq_x70, (String.String (Coq_x72, (String.String
         (Coq_x69, (String.String (Coq_x6d, (String.String (Coq_x74,
         (String.String (Coq_x69, (String.String (Coq_x76, (String.String
         (Coq_x65, (String.String (Coq_x20, (String.String (Coq_x74,
         (String.String (Coq_x79, (String.String (Coq_x70, (String.String
         (Coq_x65,
         String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))
  | TermSpineView.Coq_tLazy p -> TLambda (Coq_nAnon, (lift O (compile0 p __)))
  | TermSpineView.Coq_tForce p ->
    coq_TmkApps (compile0 p __) (Coq_tcons (TProof, Coq_tnil))

(** val compile_functional : term -> (term -> __ -> coq_Term) -> coq_Term **)

let compile_functional e compile0 =
  match TermSpineView.view e with
  | TermSpineView.Coq_tBox -> TProof
  | TermSpineView.Coq_tRel n -> TRel n
  | TermSpineView.Coq_tVar _ ->
    TWrong (String.String (Coq_x56, (String.String (Coq_x61, (String.String
      (Coq_x72, String.EmptyString))))))
  | TermSpineView.Coq_tEvar (_, _) ->
    TWrong (String.String (Coq_x45, (String.String (Coq_x76, (String.String
      (Coq_x61, (String.String (Coq_x72, String.EmptyString))))))))
  | TermSpineView.Coq_tLambda (n, b) -> TLambda (n, (compile0 b __))
  | TermSpineView.Coq_tLetIn (n, b, b') ->
    TLetIn (n, (compile0 b __), (compile0 b' __))
  | TermSpineView.Coq_tApp (f, l) ->
    coq_TmkApps (compile0 f __)
      (list_terms (map_InP l (fun x _ -> compile0 x __)))
  | TermSpineView.Coq_tConst kn -> TConst kn
  | TermSpineView.Coq_tConstruct (i, n, args) ->
    TConstruct (i, n, (list_terms (map_InP args (fun x _ -> compile0 x __))))
  | TermSpineView.Coq_tCase (ci, p, brs) ->
    let brs' =
      map_InP brs (fun x _ -> ((List0.rev (fst x)), (compile0 (snd x) __)))
    in
    TCase ((fst ci), (compile0 p __), (list_Brs brs'))
  | TermSpineView.Coq_tProj (_, _) ->
    TWrong (String.String (Coq_x50, (String.String (Coq_x72, (String.String
      (Coq_x6f, (String.String (Coq_x6a, String.EmptyString))))))))
  | TermSpineView.Coq_tFix (mfix, idx) ->
    let mfix' =
      map_InP mfix (fun d _ -> { E.dname = d.dname; E.dbody =
        (compile0 d.dbody __); E.rarg = d.rarg })
    in
    TFix ((list_Defs mfix'), idx)
  | TermSpineView.Coq_tCoFix (_, _) ->
    TWrong (String.String (Coq_x54, (String.String (Coq_x43, (String.String
      (Coq_x6f, (String.String (Coq_x66, (String.String (Coq_x69,
      (String.String (Coq_x78, String.EmptyString))))))))))))
  | TermSpineView.Coq_tPrim p ->
    (match trans_prim_val p with
     | Some p0 -> TPrim p0
     | None ->
       TWrong (String.String (Coq_x75, (String.String (Coq_x6e,
         (String.String (Coq_x73, (String.String (Coq_x75, (String.String
         (Coq_x70, (String.String (Coq_x70, (String.String (Coq_x6f,
         (String.String (Coq_x72, (String.String (Coq_x74, (String.String
         (Coq_x65, (String.String (Coq_x64, (String.String (Coq_x20,
         (String.String (Coq_x70, (String.String (Coq_x72, (String.String
         (Coq_x69, (String.String (Coq_x6d, (String.String (Coq_x74,
         (String.String (Coq_x69, (String.String (Coq_x76, (String.String
         (Coq_x65, (String.String (Coq_x20, (String.String (Coq_x74,
         (String.String (Coq_x79, (String.String (Coq_x70, (String.String
         (Coq_x65,
         String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))
  | TermSpineView.Coq_tLazy p -> TLambda (Coq_nAnon, (lift O (compile0 p __)))
  | TermSpineView.Coq_tForce p ->
    coq_TmkApps (compile0 p __) (Coq_tcons (TProof, Coq_tnil))

(** val compile : term -> coq_Term **)

let rec compile x =
  match TermSpineView.view x with
  | TermSpineView.Coq_tBox -> TProof
  | TermSpineView.Coq_tRel n -> TRel n
  | TermSpineView.Coq_tVar _ ->
    TWrong (String.String (Coq_x56, (String.String (Coq_x61, (String.String
      (Coq_x72, String.EmptyString))))))
  | TermSpineView.Coq_tEvar (_, _) ->
    TWrong (String.String (Coq_x45, (String.String (Coq_x76, (String.String
      (Coq_x61, (String.String (Coq_x72, String.EmptyString))))))))
  | TermSpineView.Coq_tLambda (n, b) -> TLambda (n, (compile b))
  | TermSpineView.Coq_tLetIn (n, b, b') ->
    TLetIn (n, (compile b), (compile b'))
  | TermSpineView.Coq_tApp (f, l) ->
    coq_TmkApps (compile f) (list_terms (map_InP l (fun x0 _ -> compile x0)))
  | TermSpineView.Coq_tConst kn -> TConst kn
  | TermSpineView.Coq_tConstruct (i, n, args) ->
    TConstruct (i, n, (list_terms (map_InP args (fun x0 _ -> compile x0))))
  | TermSpineView.Coq_tCase (ci, p, brs) ->
    let brs' =
      map_InP brs (fun x0 _ -> ((List0.rev (fst x0)), (compile (snd x0))))
    in
    TCase ((fst ci), (compile p), (list_Brs brs'))
  | TermSpineView.Coq_tProj (_, _) ->
    TWrong (String.String (Coq_x50, (String.String (Coq_x72, (String.String
      (Coq_x6f, (String.String (Coq_x6a, String.EmptyString))))))))
  | TermSpineView.Coq_tFix (mfix, idx) ->
    let mfix' =
      map_InP mfix (fun d _ -> { E.dname = d.dname; E.dbody =
        (compile d.dbody); E.rarg = d.rarg })
    in
    TFix ((list_Defs mfix'), idx)
  | TermSpineView.Coq_tCoFix (_, _) ->
    TWrong (String.String (Coq_x54, (String.String (Coq_x43, (String.String
      (Coq_x6f, (String.String (Coq_x66, (String.String (Coq_x69,
      (String.String (Coq_x78, String.EmptyString))))))))))))
  | TermSpineView.Coq_tPrim p ->
    (match trans_prim_val p with
     | Some p0 -> TPrim p0
     | None ->
       TWrong (String.String (Coq_x75, (String.String (Coq_x6e,
         (String.String (Coq_x73, (String.String (Coq_x75, (String.String
         (Coq_x70, (String.String (Coq_x70, (String.String (Coq_x6f,
         (String.String (Coq_x72, (String.String (Coq_x74, (String.String
         (Coq_x65, (String.String (Coq_x64, (String.String (Coq_x20,
         (String.String (Coq_x70, (String.String (Coq_x72, (String.String
         (Coq_x69, (String.String (Coq_x6d, (String.String (Coq_x74,
         (String.String (Coq_x69, (String.String (Coq_x76, (String.String
         (Coq_x65, (String.String (Coq_x20, (String.String (Coq_x74,
         (String.String (Coq_x79, (String.String (Coq_x70, (String.String
         (Coq_x65,
         String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))
  | TermSpineView.Coq_tLazy p -> TLambda (Coq_nAnon, (lift O (compile p)))
  | TermSpineView.Coq_tForce p ->
    coq_TmkApps (compile p) (Coq_tcons (TProof, Coq_tnil))

(** val compile_unfold_clause_1_clause_12 :
    term prim_val -> primitive option -> coq_Term **)

let compile_unfold_clause_1_clause_12 _ = function
| Some p -> TPrim p
| None ->
  TWrong (String.String (Coq_x75, (String.String (Coq_x6e, (String.String
    (Coq_x73, (String.String (Coq_x75, (String.String (Coq_x70,
    (String.String (Coq_x70, (String.String (Coq_x6f, (String.String
    (Coq_x72, (String.String (Coq_x74, (String.String (Coq_x65,
    (String.String (Coq_x64, (String.String (Coq_x20, (String.String
    (Coq_x70, (String.String (Coq_x72, (String.String (Coq_x69,
    (String.String (Coq_x6d, (String.String (Coq_x74, (String.String
    (Coq_x69, (String.String (Coq_x76, (String.String (Coq_x65,
    (String.String (Coq_x20, (String.String (Coq_x74, (String.String
    (Coq_x79, (String.String (Coq_x70, (String.String (Coq_x65,
    String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))

(** val compile_unfold_clause_1 : term -> TermSpineView.t -> coq_Term **)

let compile_unfold_clause_1 _ = function
| TermSpineView.Coq_tBox -> TProof
| TermSpineView.Coq_tRel n -> TRel n
| TermSpineView.Coq_tVar _ ->
  TWrong (String.String (Coq_x56, (String.String (Coq_x61, (String.String
    (Coq_x72, String.EmptyString))))))
| TermSpineView.Coq_tEvar (_, _) ->
  TWrong (String.String (Coq_x45, (String.String (Coq_x76, (String.String
    (Coq_x61, (String.String (Coq_x72, String.EmptyString))))))))
| TermSpineView.Coq_tLambda (n, b) -> TLambda (n, (compile b))
| TermSpineView.Coq_tLetIn (n, b, b') -> TLetIn (n, (compile b), (compile b'))
| TermSpineView.Coq_tApp (f, l) ->
  coq_TmkApps (compile f) (list_terms (map_InP l (fun x _ -> compile x)))
| TermSpineView.Coq_tConst kn -> TConst kn
| TermSpineView.Coq_tConstruct (i, n, args) ->
  TConstruct (i, n, (list_terms (map_InP args (fun x _ -> compile x))))
| TermSpineView.Coq_tCase (ci, p, brs) ->
  let brs' = map_InP brs (fun x _ -> ((List0.rev (fst x)), (compile (snd x))))
  in
  TCase ((fst ci), (compile p), (list_Brs brs'))
| TermSpineView.Coq_tProj (_, _) ->
  TWrong (String.String (Coq_x50, (String.String (Coq_x72, (String.String
    (Coq_x6f, (String.String (Coq_x6a, String.EmptyString))))))))
| TermSpineView.Coq_tFix (mfix, idx) ->
  let mfix' =
    map_InP mfix (fun d _ -> { E.dname = d.dname; E.dbody =
      (compile d.dbody); E.rarg = d.rarg })
  in
  TFix ((list_Defs mfix'), idx)
| TermSpineView.Coq_tCoFix (_, _) ->
  TWrong (String.String (Coq_x54, (String.String (Coq_x43, (String.String
    (Coq_x6f, (String.String (Coq_x66, (String.String (Coq_x69,
    (String.String (Coq_x78, String.EmptyString))))))))))))
| TermSpineView.Coq_tPrim p ->
  (match trans_prim_val p with
   | Some p0 -> TPrim p0
   | None ->
     TWrong (String.String (Coq_x75, (String.String (Coq_x6e, (String.String
       (Coq_x73, (String.String (Coq_x75, (String.String (Coq_x70,
       (String.String (Coq_x70, (String.String (Coq_x6f, (String.String
       (Coq_x72, (String.String (Coq_x74, (String.String (Coq_x65,
       (String.String (Coq_x64, (String.String (Coq_x20, (String.String
       (Coq_x70, (String.String (Coq_x72, (String.String (Coq_x69,
       (String.String (Coq_x6d, (String.String (Coq_x74, (String.String
       (Coq_x69, (String.String (Coq_x76, (String.String (Coq_x65,
       (String.String (Coq_x20, (String.String (Coq_x74, (String.String
       (Coq_x79, (String.String (Coq_x70, (String.String (Coq_x65,
       String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))
| TermSpineView.Coq_tLazy p -> TLambda (Coq_nAnon, (lift O (compile p)))
| TermSpineView.Coq_tForce p ->
  coq_TmkApps (compile p) (Coq_tcons (TProof, Coq_tnil))

(** val compile_unfold : term -> coq_Term **)

let compile_unfold e =
  match TermSpineView.view e with
  | TermSpineView.Coq_tBox -> TProof
  | TermSpineView.Coq_tRel n -> TRel n
  | TermSpineView.Coq_tVar _ ->
    TWrong (String.String (Coq_x56, (String.String (Coq_x61, (String.String
      (Coq_x72, String.EmptyString))))))
  | TermSpineView.Coq_tEvar (_, _) ->
    TWrong (String.String (Coq_x45, (String.String (Coq_x76, (String.String
      (Coq_x61, (String.String (Coq_x72, String.EmptyString))))))))
  | TermSpineView.Coq_tLambda (n, b) -> TLambda (n, (compile b))
  | TermSpineView.Coq_tLetIn (n, b, b') ->
    TLetIn (n, (compile b), (compile b'))
  | TermSpineView.Coq_tApp (f, l) ->
    coq_TmkApps (compile f) (list_terms (map_InP l (fun x _ -> compile x)))
  | TermSpineView.Coq_tConst kn -> TConst kn
  | TermSpineView.Coq_tConstruct (i, n, args) ->
    TConstruct (i, n, (list_terms (map_InP args (fun x _ -> compile x))))
  | TermSpineView.Coq_tCase (ci, p, brs) ->
    let brs' =
      map_InP brs (fun x _ -> ((List0.rev (fst x)), (compile (snd x))))
    in
    TCase ((fst ci), (compile p), (list_Brs brs'))
  | TermSpineView.Coq_tProj (_, _) ->
    TWrong (String.String (Coq_x50, (String.String (Coq_x72, (String.String
      (Coq_x6f, (String.String (Coq_x6a, String.EmptyString))))))))
  | TermSpineView.Coq_tFix (mfix, idx) ->
    let mfix' =
      map_InP mfix (fun d _ -> { E.dname = d.dname; E.dbody =
        (compile d.dbody); E.rarg = d.rarg })
    in
    TFix ((list_Defs mfix'), idx)
  | TermSpineView.Coq_tCoFix (_, _) ->
    TWrong (String.String (Coq_x54, (String.String (Coq_x43, (String.String
      (Coq_x6f, (String.String (Coq_x66, (String.String (Coq_x69,
      (String.String (Coq_x78, String.EmptyString))))))))))))
  | TermSpineView.Coq_tPrim p ->
    (match trans_prim_val p with
     | Some p0 -> TPrim p0
     | None ->
       TWrong (String.String (Coq_x75, (String.String (Coq_x6e,
         (String.String (Coq_x73, (String.String (Coq_x75, (String.String
         (Coq_x70, (String.String (Coq_x70, (String.String (Coq_x6f,
         (String.String (Coq_x72, (String.String (Coq_x74, (String.String
         (Coq_x65, (String.String (Coq_x64, (String.String (Coq_x20,
         (String.String (Coq_x70, (String.String (Coq_x72, (String.String
         (Coq_x69, (String.String (Coq_x6d, (String.String (Coq_x74,
         (String.String (Coq_x69, (String.String (Coq_x76, (String.String
         (Coq_x65, (String.String (Coq_x20, (String.String (Coq_x74,
         (String.String (Coq_x79, (String.String (Coq_x70, (String.String
         (Coq_x65,
         String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))
  | TermSpineView.Coq_tLazy p -> TLambda (Coq_nAnon, (lift O (compile p)))
  | TermSpineView.Coq_tForce p ->
    coq_TmkApps (compile p) (Coq_tcons (TProof, Coq_tnil))

type compile_graph =
| Coq_compile_graph_refinement_1 of term * compile_clause_1_graph
and compile_clause_1_graph =
| Coq_compile_clause_1_graph_equation_1
| Coq_compile_clause_1_graph_equation_2 of nat
| Coq_compile_clause_1_graph_equation_3 of ident
| Coq_compile_clause_1_graph_equation_4 of nat * term list
| Coq_compile_clause_1_graph_equation_5 of name * term * compile_graph
| Coq_compile_clause_1_graph_equation_6 of name * term * term * compile_graph
   * compile_graph
| Coq_compile_clause_1_graph_equation_7 of term * term list * compile_graph
   * (term -> __ -> compile_graph)
| Coq_compile_clause_1_graph_equation_8 of kername
| Coq_compile_clause_1_graph_equation_9 of inductive * nat * term list
   * (term -> __ -> compile_graph)
| Coq_compile_clause_1_graph_equation_10 of (inductive * nat) * term
   * (name list * term) list * ((name list * term) -> __ -> compile_graph)
   * compile_graph
| Coq_compile_clause_1_graph_equation_11 of Kernames.projection * term
| Coq_compile_clause_1_graph_equation_12 of term mfixpoint * nat
   * (term def -> __ -> compile_graph)
| Coq_compile_clause_1_graph_equation_13 of term mfixpoint * nat
| Coq_compile_clause_1_graph_refinement_14 of term prim_val
   * compile_clause_1_clause_12_graph
| Coq_compile_clause_1_graph_equation_15 of term * compile_graph
| Coq_compile_clause_1_graph_equation_16 of term * compile_graph
and compile_clause_1_clause_12_graph =
| Coq_compile_clause_1_clause_12_graph_equation_1 of term prim_val * primitive
| Coq_compile_clause_1_clause_12_graph_equation_2 of term prim_val

(** val compile_clause_1_clause_12_graph_mut :
    (term -> compile_clause_1_graph -> 'a2 -> 'a1) -> 'a2 -> (nat -> 'a2) ->
    (ident -> 'a2) -> (nat -> term list -> 'a2) -> (name -> term ->
    compile_graph -> 'a1 -> 'a2) -> (name -> term -> term -> compile_graph ->
    'a1 -> compile_graph -> 'a1 -> 'a2) -> (term -> term list -> __ -> __ ->
    compile_graph -> 'a1 -> (term -> __ -> compile_graph) -> (term -> __ ->
    'a1) -> 'a2) -> (kername -> 'a2) -> (inductive -> nat -> term list ->
    (term -> __ -> compile_graph) -> (term -> __ -> 'a1) -> 'a2) ->
    ((inductive * nat) -> term -> (name list * term) list -> ((name
    list * term) -> __ -> compile_graph) -> ((name list * term) -> __ -> 'a1)
    -> compile_graph -> 'a1 -> 'a2) -> (Kernames.projection -> term -> 'a2)
    -> (term mfixpoint -> nat -> (term def -> __ -> compile_graph) -> (term
    def -> __ -> 'a1) -> 'a2) -> (term mfixpoint -> nat -> 'a2) -> (term
    prim_val -> compile_clause_1_clause_12_graph -> 'a3 -> 'a2) -> (term ->
    compile_graph -> 'a1 -> 'a2) -> (term -> compile_graph -> 'a1 -> 'a2) ->
    (term prim_val -> primitive -> 'a3) -> (term prim_val -> 'a3) -> term
    prim_val -> primitive option -> coq_Term ->
    compile_clause_1_clause_12_graph -> 'a3 **)

let compile_clause_1_clause_12_graph_mut _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ f16 f17 _ _ _ = function
| Coq_compile_clause_1_clause_12_graph_equation_1 (p, pv) -> f16 p pv
| Coq_compile_clause_1_clause_12_graph_equation_2 p -> f17 p

(** val compile_clause_1_graph_mut :
    (term -> compile_clause_1_graph -> 'a2 -> 'a1) -> 'a2 -> (nat -> 'a2) ->
    (ident -> 'a2) -> (nat -> term list -> 'a2) -> (name -> term ->
    compile_graph -> 'a1 -> 'a2) -> (name -> term -> term -> compile_graph ->
    'a1 -> compile_graph -> 'a1 -> 'a2) -> (term -> term list -> __ -> __ ->
    compile_graph -> 'a1 -> (term -> __ -> compile_graph) -> (term -> __ ->
    'a1) -> 'a2) -> (kername -> 'a2) -> (inductive -> nat -> term list ->
    (term -> __ -> compile_graph) -> (term -> __ -> 'a1) -> 'a2) ->
    ((inductive * nat) -> term -> (name list * term) list -> ((name
    list * term) -> __ -> compile_graph) -> ((name list * term) -> __ -> 'a1)
    -> compile_graph -> 'a1 -> 'a2) -> (Kernames.projection -> term -> 'a2)
    -> (term mfixpoint -> nat -> (term def -> __ -> compile_graph) -> (term
    def -> __ -> 'a1) -> 'a2) -> (term mfixpoint -> nat -> 'a2) -> (term
    prim_val -> compile_clause_1_clause_12_graph -> 'a3 -> 'a2) -> (term ->
    compile_graph -> 'a1 -> 'a2) -> (term -> compile_graph -> 'a1 -> 'a2) ->
    (term prim_val -> primitive -> 'a3) -> (term prim_val -> 'a3) -> term ->
    TermSpineView.t -> coq_Term -> compile_clause_1_graph -> 'a2 **)

let compile_clause_1_graph_mut f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 =
  let rec f18 _ _ = function
  | Coq_compile_graph_refinement_1 (e, hind) ->
    f e hind
      (f19 e (TermSpineView.view e)
        (match TermSpineView.view e with
         | TermSpineView.Coq_tBox -> TProof
         | TermSpineView.Coq_tRel n -> TRel n
         | TermSpineView.Coq_tVar _ ->
           TWrong (String.String (Coq_x56, (String.String (Coq_x61,
             (String.String (Coq_x72, String.EmptyString))))))
         | TermSpineView.Coq_tEvar (_, _) ->
           TWrong (String.String (Coq_x45, (String.String (Coq_x76,
             (String.String (Coq_x61, (String.String (Coq_x72,
             String.EmptyString))))))))
         | TermSpineView.Coq_tLambda (n, b) -> TLambda (n, (compile b))
         | TermSpineView.Coq_tLetIn (n, b, b') ->
           TLetIn (n, (compile b), (compile b'))
         | TermSpineView.Coq_tApp (f21, l) ->
           coq_TmkApps (compile f21)
             (list_terms (map_InP l (fun x _ -> compile x)))
         | TermSpineView.Coq_tConst kn -> TConst kn
         | TermSpineView.Coq_tConstruct (i, n, args) ->
           TConstruct (i, n,
             (list_terms (map_InP args (fun x _ -> compile x))))
         | TermSpineView.Coq_tCase (ci, p, brs) ->
           let brs' =
             map_InP brs (fun x _ -> ((List0.rev (fst x)), (compile (snd x))))
           in
           TCase ((fst ci), (compile p), (list_Brs brs'))
         | TermSpineView.Coq_tProj (_, _) ->
           TWrong (String.String (Coq_x50, (String.String (Coq_x72,
             (String.String (Coq_x6f, (String.String (Coq_x6a,
             String.EmptyString))))))))
         | TermSpineView.Coq_tFix (mfix, idx) ->
           let mfix' =
             map_InP mfix (fun d _ -> { E.dname = d.dname; E.dbody =
               (compile d.dbody); E.rarg = d.rarg })
           in
           TFix ((list_Defs mfix'), idx)
         | TermSpineView.Coq_tCoFix (_, _) ->
           TWrong (String.String (Coq_x54, (String.String (Coq_x43,
             (String.String (Coq_x6f, (String.String (Coq_x66, (String.String
             (Coq_x69, (String.String (Coq_x78, String.EmptyString))))))))))))
         | TermSpineView.Coq_tPrim p ->
           (match trans_prim_val p with
            | Some p0 -> TPrim p0
            | None ->
              TWrong (String.String (Coq_x75, (String.String (Coq_x6e,
                (String.String (Coq_x73, (String.String (Coq_x75,
                (String.String (Coq_x70, (String.String (Coq_x70,
                (String.String (Coq_x6f, (String.String (Coq_x72,
                (String.String (Coq_x74, (String.String (Coq_x65,
                (String.String (Coq_x64, (String.String (Coq_x20,
                (String.String (Coq_x70, (String.String (Coq_x72,
                (String.String (Coq_x69, (String.String (Coq_x6d,
                (String.String (Coq_x74, (String.String (Coq_x69,
                (String.String (Coq_x76, (String.String (Coq_x65,
                (String.String (Coq_x20, (String.String (Coq_x74,
                (String.String (Coq_x79, (String.String (Coq_x70,
                (String.String (Coq_x65,
                String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))
         | TermSpineView.Coq_tLazy p ->
           TLambda (Coq_nAnon, (lift O (compile p)))
         | TermSpineView.Coq_tForce p ->
           coq_TmkApps (compile p) (Coq_tcons (TProof, Coq_tnil))) hind)
  and f19 _ _ _ = function
  | Coq_compile_clause_1_graph_equation_1 -> f0
  | Coq_compile_clause_1_graph_equation_2 n -> f1 n
  | Coq_compile_clause_1_graph_equation_3 n0 -> f2 n0
  | Coq_compile_clause_1_graph_equation_4 (n1, e0) -> f3 n1 e0
  | Coq_compile_clause_1_graph_equation_5 (nm, bod, hind) ->
    f4 nm bod hind (f18 bod (compile bod) hind)
  | Coq_compile_clause_1_graph_equation_6 (nm, dfn, bod, hind, hind0) ->
    f5 nm dfn bod hind (f18 dfn (compile dfn) hind) hind0
      (f18 bod (compile bod) hind0)
  | Coq_compile_clause_1_graph_equation_7 (fn, args, hind, hind0) ->
    f6 fn args __ __ hind (f18 fn (compile fn) hind) hind0 (fun x _ ->
      f18 x (compile x) (hind0 x __))
  | Coq_compile_clause_1_graph_equation_8 nm -> f7 nm
  | Coq_compile_clause_1_graph_equation_9 (i, m, args, hind) ->
    f8 i m args hind (fun x _ -> f18 x (compile x) (hind x __))
  | Coq_compile_clause_1_graph_equation_10 (i, mch, brs, hind, hind0) ->
    f9 i mch brs hind (fun x _ -> f18 (snd x) (compile (snd x)) (hind x __))
      hind0 (f18 mch (compile mch) hind0)
  | Coq_compile_clause_1_graph_equation_11 (p, bod) -> f10 p bod
  | Coq_compile_clause_1_graph_equation_12 (mfix, idx, hind) ->
    f11 mfix idx hind (fun d _ -> f18 d.dbody (compile d.dbody) (hind d __))
  | Coq_compile_clause_1_graph_equation_13 (mfix, idx) -> f12 mfix idx
  | Coq_compile_clause_1_graph_refinement_14 (p, hind) ->
    f13 p hind
      (f20 p (trans_prim_val p)
        (match trans_prim_val p with
         | Some p0 -> TPrim p0
         | None ->
           TWrong (String.String (Coq_x75, (String.String (Coq_x6e,
             (String.String (Coq_x73, (String.String (Coq_x75, (String.String
             (Coq_x70, (String.String (Coq_x70, (String.String (Coq_x6f,
             (String.String (Coq_x72, (String.String (Coq_x74, (String.String
             (Coq_x65, (String.String (Coq_x64, (String.String (Coq_x20,
             (String.String (Coq_x70, (String.String (Coq_x72, (String.String
             (Coq_x69, (String.String (Coq_x6d, (String.String (Coq_x74,
             (String.String (Coq_x69, (String.String (Coq_x76, (String.String
             (Coq_x65, (String.String (Coq_x20, (String.String (Coq_x74,
             (String.String (Coq_x79, (String.String (Coq_x70, (String.String
             (Coq_x65,
             String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))
        hind)
  | Coq_compile_clause_1_graph_equation_15 (t0, hind) ->
    f14 t0 hind (f18 t0 (compile t0) hind)
  | Coq_compile_clause_1_graph_equation_16 (t0, hind) ->
    f15 t0 hind (f18 t0 (compile t0) hind)
  and f20 _ _ _ = function
  | Coq_compile_clause_1_clause_12_graph_equation_1 (p, pv) -> f16 p pv
  | Coq_compile_clause_1_clause_12_graph_equation_2 p -> f17 p
  in f19

(** val compile_graph_mut :
    (term -> compile_clause_1_graph -> 'a2 -> 'a1) -> 'a2 -> (nat -> 'a2) ->
    (ident -> 'a2) -> (nat -> term list -> 'a2) -> (name -> term ->
    compile_graph -> 'a1 -> 'a2) -> (name -> term -> term -> compile_graph ->
    'a1 -> compile_graph -> 'a1 -> 'a2) -> (term -> term list -> __ -> __ ->
    compile_graph -> 'a1 -> (term -> __ -> compile_graph) -> (term -> __ ->
    'a1) -> 'a2) -> (kername -> 'a2) -> (inductive -> nat -> term list ->
    (term -> __ -> compile_graph) -> (term -> __ -> 'a1) -> 'a2) ->
    ((inductive * nat) -> term -> (name list * term) list -> ((name
    list * term) -> __ -> compile_graph) -> ((name list * term) -> __ -> 'a1)
    -> compile_graph -> 'a1 -> 'a2) -> (Kernames.projection -> term -> 'a2)
    -> (term mfixpoint -> nat -> (term def -> __ -> compile_graph) -> (term
    def -> __ -> 'a1) -> 'a2) -> (term mfixpoint -> nat -> 'a2) -> (term
    prim_val -> compile_clause_1_clause_12_graph -> 'a3 -> 'a2) -> (term ->
    compile_graph -> 'a1 -> 'a2) -> (term -> compile_graph -> 'a1 -> 'a2) ->
    (term prim_val -> primitive -> 'a3) -> (term prim_val -> 'a3) -> term ->
    coq_Term -> compile_graph -> 'a1 **)

let compile_graph_mut f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 =
  let rec f18 _ _ = function
  | Coq_compile_graph_refinement_1 (e, hind) ->
    f e hind
      (f19 e (TermSpineView.view e)
        (match TermSpineView.view e with
         | TermSpineView.Coq_tBox -> TProof
         | TermSpineView.Coq_tRel n -> TRel n
         | TermSpineView.Coq_tVar _ ->
           TWrong (String.String (Coq_x56, (String.String (Coq_x61,
             (String.String (Coq_x72, String.EmptyString))))))
         | TermSpineView.Coq_tEvar (_, _) ->
           TWrong (String.String (Coq_x45, (String.String (Coq_x76,
             (String.String (Coq_x61, (String.String (Coq_x72,
             String.EmptyString))))))))
         | TermSpineView.Coq_tLambda (n, b) -> TLambda (n, (compile b))
         | TermSpineView.Coq_tLetIn (n, b, b') ->
           TLetIn (n, (compile b), (compile b'))
         | TermSpineView.Coq_tApp (f21, l) ->
           coq_TmkApps (compile f21)
             (list_terms (map_InP l (fun x _ -> compile x)))
         | TermSpineView.Coq_tConst kn -> TConst kn
         | TermSpineView.Coq_tConstruct (i, n, args) ->
           TConstruct (i, n,
             (list_terms (map_InP args (fun x _ -> compile x))))
         | TermSpineView.Coq_tCase (ci, p, brs) ->
           let brs' =
             map_InP brs (fun x _ -> ((List0.rev (fst x)), (compile (snd x))))
           in
           TCase ((fst ci), (compile p), (list_Brs brs'))
         | TermSpineView.Coq_tProj (_, _) ->
           TWrong (String.String (Coq_x50, (String.String (Coq_x72,
             (String.String (Coq_x6f, (String.String (Coq_x6a,
             String.EmptyString))))))))
         | TermSpineView.Coq_tFix (mfix, idx) ->
           let mfix' =
             map_InP mfix (fun d _ -> { E.dname = d.dname; E.dbody =
               (compile d.dbody); E.rarg = d.rarg })
           in
           TFix ((list_Defs mfix'), idx)
         | TermSpineView.Coq_tCoFix (_, _) ->
           TWrong (String.String (Coq_x54, (String.String (Coq_x43,
             (String.String (Coq_x6f, (String.String (Coq_x66, (String.String
             (Coq_x69, (String.String (Coq_x78, String.EmptyString))))))))))))
         | TermSpineView.Coq_tPrim p ->
           (match trans_prim_val p with
            | Some p0 -> TPrim p0
            | None ->
              TWrong (String.String (Coq_x75, (String.String (Coq_x6e,
                (String.String (Coq_x73, (String.String (Coq_x75,
                (String.String (Coq_x70, (String.String (Coq_x70,
                (String.String (Coq_x6f, (String.String (Coq_x72,
                (String.String (Coq_x74, (String.String (Coq_x65,
                (String.String (Coq_x64, (String.String (Coq_x20,
                (String.String (Coq_x70, (String.String (Coq_x72,
                (String.String (Coq_x69, (String.String (Coq_x6d,
                (String.String (Coq_x74, (String.String (Coq_x69,
                (String.String (Coq_x76, (String.String (Coq_x65,
                (String.String (Coq_x20, (String.String (Coq_x74,
                (String.String (Coq_x79, (String.String (Coq_x70,
                (String.String (Coq_x65,
                String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))
         | TermSpineView.Coq_tLazy p ->
           TLambda (Coq_nAnon, (lift O (compile p)))
         | TermSpineView.Coq_tForce p ->
           coq_TmkApps (compile p) (Coq_tcons (TProof, Coq_tnil))) hind)
  and f19 _ _ _ = function
  | Coq_compile_clause_1_graph_equation_1 -> f0
  | Coq_compile_clause_1_graph_equation_2 n -> f1 n
  | Coq_compile_clause_1_graph_equation_3 n0 -> f2 n0
  | Coq_compile_clause_1_graph_equation_4 (n1, e0) -> f3 n1 e0
  | Coq_compile_clause_1_graph_equation_5 (nm, bod, hind) ->
    f4 nm bod hind (f18 bod (compile bod) hind)
  | Coq_compile_clause_1_graph_equation_6 (nm, dfn, bod, hind, hind0) ->
    f5 nm dfn bod hind (f18 dfn (compile dfn) hind) hind0
      (f18 bod (compile bod) hind0)
  | Coq_compile_clause_1_graph_equation_7 (fn, args, hind, hind0) ->
    f6 fn args __ __ hind (f18 fn (compile fn) hind) hind0 (fun x _ ->
      f18 x (compile x) (hind0 x __))
  | Coq_compile_clause_1_graph_equation_8 nm -> f7 nm
  | Coq_compile_clause_1_graph_equation_9 (i, m, args, hind) ->
    f8 i m args hind (fun x _ -> f18 x (compile x) (hind x __))
  | Coq_compile_clause_1_graph_equation_10 (i, mch, brs, hind, hind0) ->
    f9 i mch brs hind (fun x _ -> f18 (snd x) (compile (snd x)) (hind x __))
      hind0 (f18 mch (compile mch) hind0)
  | Coq_compile_clause_1_graph_equation_11 (p, bod) -> f10 p bod
  | Coq_compile_clause_1_graph_equation_12 (mfix, idx, hind) ->
    f11 mfix idx hind (fun d _ -> f18 d.dbody (compile d.dbody) (hind d __))
  | Coq_compile_clause_1_graph_equation_13 (mfix, idx) -> f12 mfix idx
  | Coq_compile_clause_1_graph_refinement_14 (p, hind) ->
    f13 p hind
      (f20 p (trans_prim_val p)
        (match trans_prim_val p with
         | Some p0 -> TPrim p0
         | None ->
           TWrong (String.String (Coq_x75, (String.String (Coq_x6e,
             (String.String (Coq_x73, (String.String (Coq_x75, (String.String
             (Coq_x70, (String.String (Coq_x70, (String.String (Coq_x6f,
             (String.String (Coq_x72, (String.String (Coq_x74, (String.String
             (Coq_x65, (String.String (Coq_x64, (String.String (Coq_x20,
             (String.String (Coq_x70, (String.String (Coq_x72, (String.String
             (Coq_x69, (String.String (Coq_x6d, (String.String (Coq_x74,
             (String.String (Coq_x69, (String.String (Coq_x76, (String.String
             (Coq_x65, (String.String (Coq_x20, (String.String (Coq_x74,
             (String.String (Coq_x79, (String.String (Coq_x70, (String.String
             (Coq_x65,
             String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))
        hind)
  | Coq_compile_clause_1_graph_equation_15 (t0, hind) ->
    f14 t0 hind (f18 t0 (compile t0) hind)
  | Coq_compile_clause_1_graph_equation_16 (t0, hind) ->
    f15 t0 hind (f18 t0 (compile t0) hind)
  and f20 _ _ _ = function
  | Coq_compile_clause_1_clause_12_graph_equation_1 (p, pv) -> f16 p pv
  | Coq_compile_clause_1_clause_12_graph_equation_2 p -> f17 p
  in f18

(** val compile_graph_rect :
    (term -> compile_clause_1_graph -> 'a2 -> 'a1) -> 'a2 -> (nat -> 'a2) ->
    (ident -> 'a2) -> (nat -> term list -> 'a2) -> (name -> term ->
    compile_graph -> 'a1 -> 'a2) -> (name -> term -> term -> compile_graph ->
    'a1 -> compile_graph -> 'a1 -> 'a2) -> (term -> term list -> __ -> __ ->
    compile_graph -> 'a1 -> (term -> __ -> compile_graph) -> (term -> __ ->
    'a1) -> 'a2) -> (kername -> 'a2) -> (inductive -> nat -> term list ->
    (term -> __ -> compile_graph) -> (term -> __ -> 'a1) -> 'a2) ->
    ((inductive * nat) -> term -> (name list * term) list -> ((name
    list * term) -> __ -> compile_graph) -> ((name list * term) -> __ -> 'a1)
    -> compile_graph -> 'a1 -> 'a2) -> (Kernames.projection -> term -> 'a2)
    -> (term mfixpoint -> nat -> (term def -> __ -> compile_graph) -> (term
    def -> __ -> 'a1) -> 'a2) -> (term mfixpoint -> nat -> 'a2) -> (term
    prim_val -> compile_clause_1_clause_12_graph -> 'a3 -> 'a2) -> (term ->
    compile_graph -> 'a1 -> 'a2) -> (term -> compile_graph -> 'a1 -> 'a2) ->
    (term prim_val -> primitive -> 'a3) -> (term prim_val -> 'a3) -> term ->
    coq_Term -> compile_graph -> 'a1 **)

let compile_graph_rect =
  compile_graph_mut

(** val compile_graph_correct : term -> compile_graph **)

let rec compile_graph_correct x =
  Coq_compile_graph_refinement_1 (x,
    (let refine = TermSpineView.view x in
     match refine with
     | TermSpineView.Coq_tBox -> Coq_compile_clause_1_graph_equation_1
     | TermSpineView.Coq_tRel n -> Coq_compile_clause_1_graph_equation_2 n
     | TermSpineView.Coq_tVar n -> Coq_compile_clause_1_graph_equation_3 n
     | TermSpineView.Coq_tEvar (n, e) ->
       Coq_compile_clause_1_graph_equation_4 (n, e)
     | TermSpineView.Coq_tLambda (n, b) ->
       Coq_compile_clause_1_graph_equation_5 (n, b, (compile_graph_correct b))
     | TermSpineView.Coq_tLetIn (n, b, b') ->
       Coq_compile_clause_1_graph_equation_6 (n, b, b',
         (compile_graph_correct b), (compile_graph_correct b'))
     | TermSpineView.Coq_tApp (f, l) ->
       Coq_compile_clause_1_graph_equation_7 (f, l,
         (compile_graph_correct f), (fun x0 _ -> compile_graph_correct x0))
     | TermSpineView.Coq_tConst kn -> Coq_compile_clause_1_graph_equation_8 kn
     | TermSpineView.Coq_tConstruct (i, n, args) ->
       Coq_compile_clause_1_graph_equation_9 (i, n, args, (fun x0 _ ->
         compile_graph_correct x0))
     | TermSpineView.Coq_tCase (ci, p, brs) ->
       Coq_compile_clause_1_graph_equation_10 (ci, p, brs, (fun x0 _ ->
         compile_graph_correct (snd x0)), (compile_graph_correct p))
     | TermSpineView.Coq_tProj (p, c) ->
       Coq_compile_clause_1_graph_equation_11 (p, c)
     | TermSpineView.Coq_tFix (mfix, idx) ->
       Coq_compile_clause_1_graph_equation_12 (mfix, idx, (fun d _ ->
         compile_graph_correct d.dbody))
     | TermSpineView.Coq_tCoFix (mfix, idx) ->
       Coq_compile_clause_1_graph_equation_13 (mfix, idx)
     | TermSpineView.Coq_tPrim p ->
       Coq_compile_clause_1_graph_refinement_14 (p,
         (let refine0 = trans_prim_val p in
          match refine0 with
          | Some p0 -> Coq_compile_clause_1_clause_12_graph_equation_1 (p, p0)
          | None -> Coq_compile_clause_1_clause_12_graph_equation_2 p))
     | TermSpineView.Coq_tLazy p ->
       Coq_compile_clause_1_graph_equation_15 (p, (compile_graph_correct p))
     | TermSpineView.Coq_tForce p ->
       Coq_compile_clause_1_graph_equation_16 (p, (compile_graph_correct p))))

(** val compile_elim :
    (__ -> 'a1) -> (nat -> __ -> 'a1) -> (ident -> __ -> 'a1) -> (nat -> term
    list -> __ -> 'a1) -> (name -> term -> 'a1 -> __ -> 'a1) -> (name -> term
    -> term -> 'a1 -> 'a1 -> __ -> 'a1) -> (term -> term list -> __ -> __ ->
    'a1 -> (term -> __ -> 'a1) -> __ -> 'a1) -> (kername -> __ -> 'a1) ->
    (inductive -> nat -> term list -> (term -> __ -> 'a1) -> __ -> 'a1) ->
    ((inductive * nat) -> term -> (name list * term) list -> ((name
    list * term) -> __ -> 'a1) -> 'a1 -> __ -> 'a1) -> (Kernames.projection
    -> term -> __ -> 'a1) -> (term mfixpoint -> nat -> (term def -> __ ->
    'a1) -> __ -> 'a1) -> (term mfixpoint -> nat -> __ -> 'a1) -> (term ->
    'a1 -> __ -> 'a1) -> (term -> 'a1 -> __ -> 'a1) -> (term prim_val ->
    primitive -> __ -> __ -> 'a1) -> (term prim_val -> __ -> __ -> 'a1) ->
    term -> 'a1 **)

let compile_elim f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f14 f15 f16 f17 t0 =
  compile_graph_mut (fun _ _ x -> x __) f0 f1 f2 f3 (fun nm bod _ ->
    f4 nm bod) (fun nm dfn bod _ x _ -> f5 nm dfn bod x)
    (fun fn args _ _ _ x _ -> f6 fn args __ __ x) f7 (fun i m args _ ->
    f8 i m args) (fun i mch brs _ x _ -> f9 i mch brs x) f10
    (fun mfix idx _ -> f11 mfix idx) f12 (fun _ _ x -> x __) (fun t1 _ ->
    f14 t1) (fun t1 _ -> f15 t1) f16 f17 t0 (compile t0)
    (compile_graph_correct t0)

(** val coq_FunctionalElimination_compile :
    (__ -> __) -> (nat -> __ -> __) -> (ident -> __ -> __) -> (nat -> term
    list -> __ -> __) -> (name -> term -> __ -> __ -> __) -> (name -> term ->
    term -> __ -> __ -> __ -> __) -> (term -> term list -> __ -> __ -> __ ->
    (term -> __ -> __) -> __ -> __) -> (kername -> __ -> __) -> (inductive ->
    nat -> term list -> (term -> __ -> __) -> __ -> __) -> ((inductive * nat)
    -> term -> (name list * term) list -> ((name list * term) -> __ -> __) ->
    __ -> __ -> __) -> (Kernames.projection -> term -> __ -> __) -> (term
    mfixpoint -> nat -> (term def -> __ -> __) -> __ -> __) -> (term
    mfixpoint -> nat -> __ -> __) -> (term -> __ -> __ -> __) -> (term -> __
    -> __ -> __) -> (term prim_val -> primitive -> __ -> __ -> __) -> (term
    prim_val -> __ -> __ -> __) -> term -> __ **)

let coq_FunctionalElimination_compile =
  compile_elim

(** val coq_FunctionalInduction_compile :
    (term -> coq_Term) coq_FunctionalInduction **)

let coq_FunctionalInduction_compile =
  Obj.magic compile_graph_correct

(** val compile_global_decl : global_decl -> coq_Term envClass **)

let compile_global_decl = function
| ConstantDecl c ->
  (match c with
   | Some t0 -> Coq_ecTrm (compile t0)
   | None -> ecAx)
| InductiveDecl m ->
  let ibs = ibodies_itypPack m.ind_bodies in Coq_ecTyp (O, ibs)

(** val compile_ctx :
    global_declarations -> (kername * coq_Term envClass) list **)

let rec compile_ctx = function
| [] -> []
| p :: rest ->
  let (n, decl) = p in (n, (compile_global_decl decl)) :: (compile_ctx rest)

(** val compile_program :
    erasure_configuration -> inductives_mapping -> Env.program -> coq_Term
    coq_Program **)

let compile_program econf imap p =
  let p0 = run_erase_program fake_guard_impl econf (imap, p) in
  { main = (compile (snd p0)); env = (compile_ctx (fst p0)) }

(** val program_Program :
    erasure_configuration -> inductives_mapping -> Env.program -> coq_Term
    coq_Program **)

let program_Program =
  compile_program
