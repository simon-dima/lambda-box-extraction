open AstCommon
open BasicAst
open Byte
open Datatypes
open EAst
open Kernames
open List0
open Nat0
open PeanoNat
open RandyPrelude
open Bytestring
open Compile0

(** val print_term : coq_Term -> String.t **)

let rec print_term = function
| TRel n ->
  String.append (String.String (Coq_x28, (String.String (Coq_x52,
    (String.String (Coq_x65, (String.String (Coq_x6c, (String.String
    (Coq_x20, String.EmptyString))))))))))
    (String.append (nat_to_string n) (String.String (Coq_x29,
      String.EmptyString)))
| TProof ->
  String.String (Coq_x28, (String.String (Coq_x50, (String.String (Coq_x52,
    (String.String (Coq_x46, (String.String (Coq_x29,
    String.EmptyString)))))))))
| TLambda (nm, t1) ->
  String.append (String.String (Coq_x28, (String.String (Coq_x4c,
    (String.String (Coq_x41, (String.String (Coq_x4d, (String.String
    (Coq_x20, String.EmptyString))))))))))
    (String.append (print_name nm)
      (String.append (String.String (Coq_x20, (String.String (Coq_x5b,
        String.EmptyString))))
        (String.append (print_term t1) (String.String (Coq_x5d,
          (String.String (Coq_x29, String.EmptyString)))))))
| TLetIn (nm, _, _) ->
  String.append (String.String (Coq_x28, (String.String (Coq_x4c,
    (String.String (Coq_x45, (String.String (Coq_x54, (String.String
    (Coq_x20, String.EmptyString))))))))))
    (String.append (print_name nm) (String.String (Coq_x29,
      String.EmptyString)))
| TApp (fn, arg) ->
  String.append (String.String (Coq_x28, String.EmptyString))
    (String.append (print_term fn)
      (String.append (String.String (Coq_x20, (String.String (Coq_x40,
        (String.String (Coq_x20, String.EmptyString))))))
        (String.append (print_term arg) (String.String (Coq_x29,
          String.EmptyString)))))
| TConst s ->
  String.append (String.String (Coq_x5b, String.EmptyString))
    (String.append (string_of_kername s) (String.String (Coq_x5d,
      String.EmptyString)))
| TConstruct (i, n, _) ->
  String.append (String.String (Coq_x28, (String.String (Coq_x43,
    (String.String (Coq_x53, (String.String (Coq_x54, (String.String
    (Coq_x52, (String.String (Coq_x20, String.EmptyString))))))))))))
    (String.append (print_inductive i)
      (String.append (String.String (Coq_x20, String.EmptyString))
        (String.append (nat_to_string n) (String.String (Coq_x29,
          String.EmptyString)))))
| TCase (i, mch, _) ->
  String.append (String.String (Coq_x28, (String.String (Coq_x43,
    (String.String (Coq_x41, (String.String (Coq_x53, (String.String
    (Coq_x45, (String.String (Coq_x20, String.EmptyString))))))))))))
    (String.append (print_inductive i)
      (String.append (String.String (Coq_x3a, String.EmptyString))
        (String.append (print_term mch)
          (String.append (String.String (Coq_x20, (String.String (Coq_x5f,
            (String.String (Coq_x20, String.EmptyString)))))) (String.String
            (Coq_x29, (String.String (Coq_x20, String.EmptyString))))))))
| TFix (_, n) ->
  String.append (String.String (Coq_x20, (String.String (Coq_x28,
    (String.String (Coq_x46, (String.String (Coq_x49, (String.String
    (Coq_x58, (String.String (Coq_x20, String.EmptyString))))))))))))
    (String.append (nat_to_string n) (String.String (Coq_x29, (String.String
      (Coq_x20, String.EmptyString)))))
| TPrim p ->
  String.append (String.String (Coq_x28, (String.String (Coq_x50,
    (String.String (Coq_x52, (String.String (Coq_x49, (String.String
    (Coq_x4d, (String.String (Coq_x20, String.EmptyString))))))))))))
    (String.append (string_of_prim p) (String.String (Coq_x29,
      String.EmptyString)))
| TWrong str ->
  String.append (String.String (Coq_x28, (String.String (Coq_x54,
    (String.String (Coq_x57, (String.String (Coq_x72, (String.String
    (Coq_x6f, (String.String (Coq_x6e, (String.String (Coq_x67,
    (String.String (Coq_x3a, String.EmptyString))))))))))))))))
    (String.append str (String.String (Coq_x29, String.EmptyString)))

(** val dnth : nat -> coq_Defs -> coq_Term def option **)

let rec dnth n = function
| Coq_dnil -> None
| Coq_dcons (nm, tm, args, xs) ->
  (match n with
   | O -> Some { dname = nm; dbody = tm; rarg = args }
   | S m -> dnth m xs)

(** val bnth : nat -> coq_Brs -> (name list * coq_Term) option **)

let rec bnth n = function
| Coq_bnil -> None
| Coq_bcons (ix, x, bs) -> (match n with
                            | O -> Some (ix, x)
                            | S m -> bnth m bs)

(** val instantiate : coq_Term -> nat -> coq_Term -> coq_Term **)

let instantiate tin =
  let rec instantiate0 n tbod = match tbod with
  | TRel m ->
    (match Nat.compare n m with
     | Eq -> tin
     | Lt -> TRel (pred m)
     | Gt -> TRel m)
  | TProof -> TProof
  | TLambda (nm, bod) -> TLambda (nm, (instantiate0 (S n) bod))
  | TLetIn (nm, tdef, bod) ->
    TLetIn (nm, (instantiate0 n tdef), (instantiate0 (S n) bod))
  | TApp (t0, a) -> TApp ((instantiate0 n t0), (instantiate0 n a))
  | TConstruct (i, m, args) -> TConstruct (i, m, (instantiates n args))
  | TCase (i, s, ts) -> TCase (i, (instantiate0 n s), (instantiateBrs n ts))
  | TFix (ds, m) -> TFix ((instantiateDefs (add n (dlength ds)) ds), m)
  | _ -> tbod
  and instantiates n = function
  | Coq_tnil -> Coq_tnil
  | Coq_tcons (t0, ts) -> Coq_tcons ((instantiate0 n t0), (instantiates n ts))
  and instantiateBrs n = function
  | Coq_bnil -> Coq_bnil
  | Coq_bcons (m, t0, ts) ->
    Coq_bcons (m, (instantiate0 (add (length m) n) t0), (instantiateBrs n ts))
  and instantiateDefs n = function
  | Coq_dnil -> Coq_dnil
  | Coq_dcons (nm, bod, rarg0, ds0) ->
    Coq_dcons (nm, (instantiate0 n bod), rarg0, (instantiateDefs n ds0))
  in instantiate0

(** val instantiatel : coq_Terms -> nat -> coq_Term -> coq_Term **)

let rec instantiatel s k t0 =
  match s with
  | Coq_tnil -> t0
  | Coq_tcons (a, s0) ->
    instantiatel s0 k (instantiate a (add (tlength s0) k) t0)

(** val whBetaStep : coq_Term -> coq_Term -> coq_Term **)

let whBetaStep bod arg =
  instantiate arg O bod

(** val whCaseStep : nat -> coq_Terms -> coq_Brs -> coq_Term option **)

let whCaseStep cstrNbr ts brs =
  match bnth cstrNbr brs with
  | Some p ->
    let (nargs, t0) = p in
    if Nat.eqb (length nargs) (tlength ts)
    then Some (instantiatel ts O t0)
    else None
  | None -> None

(** val whFixStep : coq_Defs -> nat -> coq_Term option **)

let whFixStep =
  let fl = fun body dts ->
    fold_left (fun bod ndx -> instantiate (TFix (dts, ndx)) O bod)
      (list_to_zero (dlength dts)) body
  in
  (fun dts m ->
  match dnth m dts with
  | Some d ->
    let { dname = _; dbody = body; rarg = _ } = d in Some (fl body dts)
  | None -> None)
