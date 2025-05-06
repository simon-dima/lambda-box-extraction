open Ast0
open AstCommon
open BasicAst
open BinNat
open BinNums
open Classes1
open Datatypes
open EAst
open EPrimitive
open EProgram
open ESpineView
open Erasure0
open Kernames
open List0
open Bytestring
open Compile0
open Expression

type __ = Obj.t

module LambdaBoxMutt = Compile0

(** val dcon_of_con : inductive -> nat -> inductive * coq_N **)

let dcon_of_con i n =
  (i, (N.of_nat n))

type env = (kername * exp) list

(** val cst_offset : env -> kername -> coq_N **)

let rec cst_offset e s =
  match e with
  | [] -> N0
  | p :: tl ->
    let (c, _) = p in
    if Kername.reflect_kername s c
    then N0
    else N.add (Npos Coq_xH) (cst_offset tl s)

(** val find_prim :
    ((((kername * String.t) * bool) * nat) * positive) list -> kername ->
    positive option **)

let rec find_prim prims n =
  match prims with
  | [] -> None
  | p :: prims0 ->
    let (p0, pos) = p in
    let (p1, _) = p0 in
    let (p2, _) = p1 in
    let (c, _) = p2 in
    if Kername.reflect_kername n c then Some pos else find_prim prims0 n

type ienv = (kername * itypPack) list

(** val map_terms :
    (LambdaBoxMutt.coq_Term -> exp) -> LambdaBoxMutt.coq_Terms -> exps **)

let rec map_terms f = function
| LambdaBoxMutt.Coq_tnil -> Coq_enil
| LambdaBoxMutt.Coq_tcons (t0, ts) -> Coq_econs ((f t0), (map_terms f ts))

(** val trans_args :
    (coq_N -> LambdaBoxMutt.coq_Term -> exp) -> coq_N ->
    LambdaBoxMutt.coq_Terms -> exps **)

let trans_args trans0 k t0 =
  map_terms (trans0 k) t0

(** val trans_brs :
    (coq_N -> LambdaBoxMutt.coq_Term -> exp) -> inductive -> coq_N -> coq_N
    -> LambdaBoxMutt.coq_Brs -> branches_e **)

let rec trans_brs trans0 ind k n = function
| LambdaBoxMutt.Coq_bnil -> Coq_brnil_e
| LambdaBoxMutt.Coq_bcons (nargs, t0, ts) ->
  Coq_brcons_e ((ind, n), ((N.of_nat (length nargs)), nargs),
    (trans0 (N.add k (N.of_nat (length nargs))) t0),
    (trans_brs trans0 ind k (N.add n (Npos Coq_xH)) ts))

(** val trans_fixes :
    (coq_N -> LambdaBoxMutt.coq_Term -> exp) -> coq_N ->
    LambdaBoxMutt.coq_Defs -> efnlst **)

let rec trans_fixes trans0 k = function
| LambdaBoxMutt.Coq_dnil -> Coq_eflnil
| LambdaBoxMutt.Coq_dcons (na, t0, _, l') ->
  Coq_eflcons (na, (trans0 k t0), (trans_fixes trans0 k l'))

(** val trans :
    env -> ((((kername * String.t) * bool) * nat) * positive) list -> coq_N
    -> LambdaBoxMutt.coq_Term -> exp **)

let rec trans e prims k = function
| LambdaBoxMutt.TRel n -> Var_e (N.of_nat n)
| LambdaBoxMutt.TLambda (n, t1) ->
  Lam_e (n, (trans e prims (N.add (Npos Coq_xH) k) t1))
| LambdaBoxMutt.TLetIn (n, t1, u) ->
  Let_e (n, (trans e prims k t1), (trans e prims (N.add (Npos Coq_xH) k) u))
| LambdaBoxMutt.TApp (t1, u) ->
  App_e ((trans e prims k t1), (trans e prims k u))
| LambdaBoxMutt.TConst s ->
  (match find_prim prims s with
   | Some p -> Prim_e p
   | None -> Var_e (N.add (cst_offset e s) k))
| LambdaBoxMutt.TConstruct (ind, c, args) ->
  let args' = trans_args (trans e prims) k args in
  Con_e ((dcon_of_con ind c), args')
| LambdaBoxMutt.TCase (ind, t1, brs) ->
  let brs' = trans_brs (trans e prims) ind k N0 brs in
  Match_e ((trans e prims k t1), N0, brs')
| LambdaBoxMutt.TFix (d, n) ->
  let len = LambdaBoxMutt.dlength d in
  let defs' = trans_fixes (trans e prims) (N.add (N.of_nat len) k) d in
  Fix_e (defs', (N.of_nat n))
| LambdaBoxMutt.TPrim p -> Prim_val_e p
| _ -> Prf_e

(** val translate :
    env -> ((((kername * String.t) * bool) * nat) * positive) list ->
    LambdaBoxMutt.coq_Term -> exp **)

let translate e prims t0 =
  trans e prims N0 t0

(** val translate_entry :
    ((((kername * String.t) * bool) * nat) * positive) list ->
    (kername * LambdaBoxMutt.coq_Term envClass) -> env -> (kername * exp) list **)

let translate_entry prims x acc =
  let (s, y) = x in
  (match y with
   | Coq_ecTrm t0 -> let t' = translate acc prims t0 in (s, t') :: acc
   | Coq_ecTyp (_, _) -> acc)

(** val translate_env_aux :
    ((((kername * String.t) * bool) * nat) * positive) list -> coq_Term
    environ -> env -> env **)

let translate_env_aux prims e k =
  fold_right (translate_entry prims) k e

(** val translate_env :
    ((((kername * String.t) * bool) * nat) * positive) list -> coq_Term
    environ -> env **)

let translate_env prims e =
  translate_env_aux prims e []

(** val inductive_entry_aux : (kername * 'a1 envClass) -> ienv -> ienv **)

let inductive_entry_aux x acc =
  let (s, e) = x in
  (match e with
   | Coq_ecTrm _ -> acc
   | Coq_ecTyp (_, pack) -> (s, pack) :: acc)

(** val inductive_env : coq_Term environ -> ienv **)

let inductive_env e =
  fold_right inductive_entry_aux [] e

(** val mkLets : env -> exp -> exp **)

let mkLets e t0 =
  fold_left (fun acc x -> Let_e ((nNameds (string_of_kername (fst x))),
    (snd x), acc)) e t0

(** val translate_program :
    ((((kername * String.t) * bool) * nat) * positive) list -> coq_Term
    environ -> LambdaBoxMutt.coq_Term -> exp **)

let translate_program prims e t0 =
  let e' = translate_env prims e in mkLets e' (translate e' prims t0)
