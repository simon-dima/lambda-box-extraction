open AST
open Archi
open AstCommon
open BasicAst
open Basics
open BinInt
open BinNat
open BinNums
open BinPos
open Binary
open Byte
open Clight
open Cop
open Ctypes
open Ctypesdefs
open Datatypes
open Errors0
open FloatOps
open Floats
open List0
open Maps
open Monad0
open MonadState
open Nat0
open OptionMonad
open PeanoNat
open Specif
open StateMonad
open Uint0
open Bytestring
open Cps
open Cps_show
open ExceptionMonad
open Identifiers
open Toplevel0

(** val maxArgs : coq_Z **)

let maxArgs =
  Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO Coq_xH))))))))))

(** val makeArgList' : positive list -> coq_N list **)

let rec makeArgList' = function
| [] -> []
| _ :: vs' -> (N.of_nat (length vs')) :: (makeArgList' vs')

(** val makeArgList : positive list -> coq_N list **)

let makeArgList vs =
  rev (makeArgList' vs)

type fun_info_env = (positive * fun_tag) M.t

(** val compute_fun_env' : nat -> fun_env -> exp -> fun_env **)

let rec compute_fun_env' n fenv e =
  match n with
  | O -> fenv
  | S n' ->
    (match e with
     | Econstr (_, _, _, e') -> compute_fun_env' n' fenv e'
     | Ecase (_, cs) -> fold_left (compute_fun_env' n') (map snd cs) fenv
     | Eproj (_, _, _, _, e') -> compute_fun_env' n' fenv e'
     | Eletapp (_, _, t0, vs, e') ->
       compute_fun_env' n'
         (M.set t0 ((N.of_nat (length vs)), (makeArgList vs)) fenv) e'
     | Efun (fnd, e') ->
       compute_fun_env' n' (compute_fun_env_fundefs n' fnd fenv) e'
     | Eapp (_, t0, vs) ->
       M.set t0 ((N.of_nat (length vs)), (makeArgList vs)) fenv
     | Eprim_val (_, _, e') -> compute_fun_env' n' fenv e'
     | Eprim (_, _, _, e') -> compute_fun_env' n' fenv e'
     | Ehalt _ -> fenv)

(** val compute_fun_env_fundefs : nat -> fundefs -> fun_env -> fun_env **)

and compute_fun_env_fundefs n fnd fenv =
  match n with
  | O -> fenv
  | S n' ->
    (match fnd with
     | Fcons (_, t0, vs, e, fnd') ->
       let fenv' = M.set t0 ((N.of_nat (length vs)), (makeArgList vs)) fenv in
       compute_fun_env_fundefs n' fnd' (compute_fun_env' n' fenv' e)
     | Fnil -> fenv)

(** val max_depth : exp -> nat **)

let rec max_depth = function
| Econstr (_, _, _, e') -> S (max_depth e')
| Ecase (_, cs) ->
  S (fold_left Nat.max (map (compose max_depth snd) cs) (S O))
| Eproj (_, _, _, _, e') -> S (max_depth e')
| Eletapp (_, _, _, _, e') -> S (max_depth e')
| Efun (fnd, e') -> S (Nat.max (max_depth_fundefs fnd) (max_depth e'))
| Eprim_val (_, _, e') -> S (max_depth e')
| Eprim (_, _, _, e') -> S (max_depth e')
| _ -> S O

(** val max_depth_fundefs : fundefs -> nat **)

and max_depth_fundefs = function
| Fcons (_, _, _, e, fnd') ->
  S (Nat.max (max_depth e) (max_depth_fundefs fnd'))
| Fnil -> S O

(** val compute_fun_env : exp -> fun_env **)

let compute_fun_env e =
  compute_fun_env' (max_depth e) M.empty e

(** val get_allocs : exp -> positive list **)

let rec get_allocs = function
| Econstr (x, _, _, e') -> x :: (get_allocs e')
| Ecase (_, cs) ->
  let rec helper = function
  | [] -> []
  | p :: cs' -> let (_, e') = p in app (get_allocs e') (helper cs')
  in helper cs
| Eproj (x, _, _, _, e') -> x :: (get_allocs e')
| Eletapp (x, _, _, _, e') -> x :: (get_allocs e')
| Efun (fnd, e') -> app (get_allocs_fundefs fnd) (get_allocs e')
| Eprim_val (x, _, e') -> x :: (get_allocs e')
| Eprim (x, _, _, e') -> x :: (get_allocs e')
| _ -> []

(** val get_allocs_fundefs : fundefs -> positive list **)

and get_allocs_fundefs = function
| Fcons (_, _, vs, e, fnd') ->
  app vs (app (get_allocs e) (get_allocs_fundefs fnd'))
| Fnil -> []

(** val max_allocs : exp -> nat **)

let rec max_allocs = function
| Econstr (_, _, vs, e') ->
  (match vs with
   | [] -> max_allocs e'
   | _ :: _ -> S (add (max_allocs e') (length vs)))
| Ecase (_, cs) ->
  let rec helper = function
  | [] -> O
  | p :: cs' -> let (_, e') = p in max (max_allocs e') (helper cs')
  in helper cs
| Eproj (_, _, _, _, e') -> max_allocs e'
| Eletapp (_, _, _, _, e') -> max_allocs e'
| Efun (fnd, e') -> max (max_allocs_fundefs fnd) (max_allocs e')
| Eprim_val (_, _, e') -> max_allocs e'
| Eprim (_, _, _, e') -> max_allocs e'
| _ -> O

(** val max_allocs_fundefs : fundefs -> nat **)

and max_allocs_fundefs = function
| Fcons (_, _, vs, e, fnd') ->
  max (add (length vs) (max_allocs e)) (max_allocs_fundefs fnd')
| Fnil -> O

type n_ind_ty_info = name * ctor_ty_info list

type n_ind_env = n_ind_ty_info M.t

(** val update_ind_env :
    n_ind_env -> positive -> ctor_ty_info -> n_ind_env **)

let update_ind_env ienv _ cInf =
  let { ctor_name = _; ctor_ind_name = nameTy; ctor_ind_tag = t0;
    ctor_arity = _; ctor_ordinal = _ } = cInf
  in
  (match M.get t0 ienv with
   | Some n ->
     let (nameTy0, iInf) = n in M.set t0 (nameTy0, (cInf :: iInf)) ienv
   | None -> M.set t0 (nameTy, (cInf :: [])) ienv)

(** val compute_ind_env : ctor_env -> n_ind_env **)

let compute_ind_env cenv =
  M.fold update_ind_env cenv M.empty

type ctor_rep =
| Coq_enum of coq_N
| Coq_boxed of coq_N * coq_N

(** val make_ctor_rep : ctor_env -> ctor_tag -> ctor_rep option **)

let make_ctor_rep cenv ct =
  Monad0.bind (Obj.magic coq_Monad_option) (M.get ct (Obj.magic cenv))
    (fun p ->
    if N.eqb p.ctor_arity N0
    then Monad0.ret (Obj.magic coq_Monad_option) (Coq_enum p.ctor_ordinal)
    else Monad0.ret (Obj.magic coq_Monad_option) (Coq_boxed (p.ctor_ordinal,
           p.ctor_arity)))

(** val threadStructInf : ident -> coq_type **)

let threadStructInf threadInfIdent =
  Tstruct (threadInfIdent, noattr)

(** val threadInf : ident -> coq_type **)

let threadInf threadInfIdent =
  Tpointer ((threadStructInf threadInfIdent), noattr)

(** val uintTy : coq_type **)

let uintTy =
  Tint (I32, Unsigned, { attr_volatile = false; attr_alignas = None })

(** val ulongTy : coq_type **)

let ulongTy =
  Tlong (Unsigned, { attr_volatile = false; attr_alignas = None })

(** val coq_val : coq_type **)

let coq_val =
  talignas (if ptr64 then Npos (Coq_xI Coq_xH) else Npos (Coq_xO Coq_xH))
    (tptr tvoid)

(** val uval : coq_type **)

let uval =
  if ptr64 then ulongTy else uintTy

(** val val_typ : typ **)

let val_typ =
  if ptr64 then AST.Tlong else Tany32

(** val coq_Init_int : coq_Z -> init_data **)

let coq_Init_int x =
  if ptr64
  then Init_int64 (Integers.Int64.repr x)
  else Init_int32 (Integers.Int.repr x)

(** val make_cint : coq_Z -> coq_type -> expr **)

let make_cint z t0 =
  if ptr64
  then Econst_long ((Integers.Int64.repr z), t0)
  else Econst_int ((Integers.Int.repr z), t0)

(** val gcTy : ident -> coq_type **)

let gcTy threadInfIdent =
  Tfunction ((Tcons ((Tpointer (coq_val, noattr)), (Tcons
    ((threadInf threadInfIdent), Tnil)))), Tvoid, cc_default)

(** val isptrTy : coq_type **)

let isptrTy =
  Tfunction ((Tcons (coq_val, Tnil)), (Tint (IBool, Unsigned, noattr)),
    cc_default)

(** val valPtr : coq_type **)

let valPtr =
  Tpointer (coq_val, { attr_volatile = false; attr_alignas = None })

(** val boolTy : coq_type **)

let boolTy =
  Tint (IBool, Unsigned, noattr)

(** val mkFunTyList : nat -> typelist **)

let rec mkFunTyList = function
| O -> Tnil
| S n' -> Tcons (coq_val, (mkFunTyList n'))

(** val mkFunTy : ident -> nat -> coq_type **)

let mkFunTy threadInfIdent n =
  Tfunction ((Tcons ((threadInf threadInfIdent), (mkFunTyList n))), Tvoid,
    cc_default)

(** val mkPrimTy : nat -> coq_type **)

let mkPrimTy n =
  Tfunction ((mkFunTyList n), coq_val, cc_default)

(** val mkPrimTyTinfo : ident -> nat -> coq_type **)

let mkPrimTyTinfo threadInfIdent n =
  Tfunction ((Tcons ((threadInf threadInfIdent), (mkFunTyList n))), coq_val,
    cc_default)

(** val allocPtr : ident -> expr **)

let allocPtr allocIdent =
  Etempvar (allocIdent, valPtr)

(** val limitPtr : ident -> expr **)

let limitPtr limitIdent =
  Etempvar (limitIdent, valPtr)

(** val args : ident -> expr **)

let args argsIdent =
  Etempvar (argsIdent, valPtr)

(** val gc : ident -> ident -> expr **)

let gc gcIdent threadInfIdent =
  Evar (gcIdent, (gcTy threadInfIdent))

(** val ptr : ident -> expr **)

let ptr isptrIdent =
  Evar (isptrIdent, isptrTy)

(** val tinf : ident -> ident -> expr **)

let tinf threadInfIdent tinfIdent =
  Etempvar (tinfIdent, (threadInf threadInfIdent))

(** val tinfd : ident -> ident -> expr **)

let tinfd threadInfIdent tinfIdent =
  Ederef ((tinf threadInfIdent tinfIdent), (threadStructInf threadInfIdent))

(** val add : expr -> expr -> expr **)

let add a b =
  Ebinop (Oadd, a, b, valPtr)

(** val sub : expr -> expr -> expr **)

let sub a b =
  Ebinop (Osub, a, b, valPtr)

(** val not : expr -> expr **)

let not a =
  Eunop (Onotbool, a, type_bool)

(** val c_int : coq_Z -> coq_type -> expr **)

let c_int n t0 =
  if ptr64
  then Econst_long ((Integers.Int64.repr n), t0)
  else Econst_int ((Integers.Int.repr n), t0)

(** val reserve_body :
    ident -> ident -> ident -> ident -> ident -> positive -> coq_Z ->
    statement **)

let reserve_body allocIdent limitIdent gcIdent threadInfIdent tinfIdent funInf l =
  let arr = Evar (funInf, (Tarray (uval, l, noattr))) in
  Sifthenelse
  ((not (Ebinop (Ole, (Ederef (arr, uval)),
     (sub (limitPtr limitIdent) (allocPtr allocIdent)), type_bool))),
  (Ssequence ((Scall (None, (gc gcIdent threadInfIdent),
  (arr :: ((tinf threadInfIdent tinfIdent) :: [])))), (Sset (allocIdent,
  (Efield ((tinfd threadInfIdent tinfIdent), allocIdent, valPtr)))))), Sskip)

(** val makeTagZ : ctor_env -> ctor_tag -> coq_Z option **)

let makeTagZ cenv ct =
  match make_ctor_rep cenv ct with
  | Some c ->
    (match c with
     | Coq_enum t0 ->
       Some
         (BinInt.Z.add (BinInt.Z.shiftl (BinInt.Z.of_N t0) (Zpos Coq_xH))
           (Zpos Coq_xH))
     | Coq_boxed (t0, a) ->
       Some
         (BinInt.Z.add
           (BinInt.Z.shiftl (BinInt.Z.of_N a) (Zpos (Coq_xO (Coq_xI (Coq_xO
             Coq_xH))))) (BinInt.Z.of_N t0)))
  | None -> None

(** val makeTag : ctor_env -> ctor_tag -> expr option **)

let makeTag cenv ct =
  match makeTagZ cenv ct with
  | Some t0 -> Some (c_int t0 coq_val)
  | None -> None

(** val mkFunVar : ident -> nat -> ident -> coq_N list -> expr **)

let mkFunVar threadInfIdent nParam x locs =
  Evar (x, (mkFunTy threadInfIdent (length (firstn nParam locs))))

(** val makeVar :
    ident -> nat -> positive -> fun_env -> fun_info_env -> expr **)

let makeVar threadInfIdent nParam x fenv map0 =
  match M.get x map0 with
  | Some p ->
    let (_, t0) = p in
    (match M.get t0 fenv with
     | Some f -> let (_, locs) = f in mkFunVar threadInfIdent nParam x locs
     | None -> Etempvar (x, coq_val))
  | None -> Etempvar (x, coq_val)

(** val assignConstructorS' :
    ident -> nat -> fun_env -> fun_info_env -> positive -> nat -> positive
    list -> statement **)

let rec assignConstructorS' threadInfIdent nParam fenv map0 x cur = function
| [] -> Sskip
| v :: vs' ->
  (match vs' with
   | [] ->
     let vv = makeVar threadInfIdent nParam v fenv map0 in
     Sassign ((Ederef
     ((add (Ecast ((Etempvar (x, coq_val)), valPtr))
        (c_int (BinInt.Z.of_nat cur) coq_val)), coq_val)), vv)
   | _ :: _ ->
     let vv = makeVar threadInfIdent nParam v fenv map0 in
     let prog =
       assignConstructorS' threadInfIdent nParam fenv map0 x
         (Nat0.add cur (S O)) vs'
     in
     Ssequence ((Sassign ((Ederef
     ((add (Ecast ((Etempvar (x, coq_val)), valPtr))
        (c_int (BinInt.Z.of_nat cur) coq_val)), coq_val)), vv)), prog))

(** val assignConstructorS :
    ident -> ident -> nat -> ctor_env -> n_ind_env -> fun_env -> fun_info_env
    -> positive -> ctor_tag -> positive list -> statement option **)

let assignConstructorS allocIdent threadInfIdent nParam cenv _ fenv map0 x t0 vs =
  Monad0.bind (Obj.magic coq_Monad_option) (Obj.magic makeTag cenv t0)
    (fun tag ->
    Monad0.bind (Obj.magic coq_Monad_option)
      (Obj.magic make_ctor_rep cenv t0) (fun rep ->
      match rep with
      | Coq_enum _ -> Monad0.ret (Obj.magic coq_Monad_option) (Sset (x, tag))
      | Coq_boxed (_, a) ->
        let stm = assignConstructorS' threadInfIdent nParam fenv map0 x O vs
        in
        Monad0.ret (Obj.magic coq_Monad_option) (Ssequence ((Ssequence
          ((Ssequence ((Sset (x, (Ecast
          ((add (allocPtr allocIdent) (c_int BinInt.Z.one coq_val)),
          coq_val)))), (Sset (allocIdent,
          (add (allocPtr allocIdent)
            (c_int (BinInt.Z.of_N (N.add a (Npos Coq_xH))) coq_val)))))),
          (Sassign ((Ederef
          ((add (Ecast ((Etempvar (x, coq_val)), valPtr))
             (c_int (Zneg Coq_xH) coq_val)), coq_val)), tag)))), stm))))

(** val isPtr : ident -> positive -> positive -> statement **)

let isPtr isptrIdent retId v =
  Scall ((Some retId), (ptr isptrIdent), ((Ecast ((Etempvar (v, coq_val)),
    coq_val)) :: []))

(** val mkCallVars :
    ident -> nat -> fun_env -> fun_info_env -> nat -> positive list -> expr
    list option **)

let rec mkCallVars threadInfIdent nParam fenv map0 n vs =
  match n with
  | O -> (match vs with
          | [] -> Some []
          | _ :: _ -> None)
  | S n0 ->
    (match vs with
     | [] -> None
     | v :: vs' ->
       let vv = makeVar threadInfIdent nParam v fenv map0 in
       Monad0.bind (Obj.magic coq_Monad_option)
         (mkCallVars threadInfIdent nParam fenv map0 n0 vs') (fun rest ->
         Monad0.ret (Obj.magic coq_Monad_option) (vv :: rest)))

(** val mkCall :
    ident -> ident -> nat -> fun_env -> fun_info_env -> expr -> nat ->
    positive list -> statement option **)

let mkCall threadInfIdent tinfIdent nParam fenv map0 f n vs =
  match mkCallVars threadInfIdent nParam fenv map0 n (firstn nParam vs) with
  | Some v -> Some (Scall (None, f, ((tinf threadInfIdent tinfIdent) :: v)))
  | None -> None

(** val mkPrimCall :
    ident -> nat -> positive -> positive -> nat -> fun_env -> fun_info_env ->
    positive list -> statement option **)

let mkPrimCall threadInfIdent nParam res pr ar fenv map0 vs =
  Monad0.bind (Obj.magic coq_Monad_option)
    (Obj.magic mkCallVars threadInfIdent nParam fenv map0 ar vs)
    (fun args0 ->
    Monad0.ret (Obj.magic coq_Monad_option) (Scall ((Some res), (Ecast ((Evar
      (pr, (mkPrimTy ar))), (mkPrimTy ar))), args0)))

(** val mkPrimCallTinfo :
    ident -> ident -> nat -> positive -> positive -> nat -> fun_env ->
    fun_info_env -> positive list -> statement option **)

let mkPrimCallTinfo threadInfIdent tinfIdent nParam res pr ar fenv map0 vs =
  Monad0.bind (Obj.magic coq_Monad_option)
    (Obj.magic mkCallVars threadInfIdent nParam fenv map0 ar vs)
    (fun args0 ->
    Monad0.ret (Obj.magic coq_Monad_option) (Scall ((Some res), (Ecast ((Evar
      (pr, (mkPrimTy ar))), (mkPrimTyTinfo threadInfIdent ar))),
      ((tinf threadInfIdent tinfIdent) :: args0))))

(** val asgnFunVars' :
    ident -> positive list -> coq_N list -> statement option **)

let rec asgnFunVars' argsIdent vs ind =
  match vs with
  | [] ->
    (match ind with
     | [] -> Monad0.ret (Obj.magic coq_Monad_option) Sskip
     | _ :: _ -> None)
  | v :: vs' ->
    (match ind with
     | [] -> None
     | i :: ind' ->
       Monad0.bind (Obj.magic coq_Monad_option)
         (asgnFunVars' argsIdent vs' ind') (fun rest ->
         Monad0.ret (Obj.magic coq_Monad_option) (Ssequence ((Sset (v,
           (Ederef ((add (args argsIdent) (c_int (BinInt.Z.of_N i) coq_val)),
           coq_val)))), rest))))

(** val asgnFunVars :
    ident -> nat -> positive list -> coq_N list -> statement option **)

let asgnFunVars argsIdent nParam vs ind =
  asgnFunVars' argsIdent (skipn nParam vs) (skipn nParam ind)

(** val asgnAppVars'' :
    ident -> ident -> nat -> positive list -> coq_N list -> fun_env ->
    fun_info_env -> statement option **)

let rec asgnAppVars'' argsIdent threadInfIdent nParam vs ind fenv map0 =
  match vs with
  | [] ->
    (match ind with
     | [] -> Monad0.ret (Obj.magic coq_Monad_option) Sskip
     | _ :: _ -> None)
  | v :: vs' ->
    (match ind with
     | [] -> None
     | i :: ind' ->
       let s_iv = Sassign ((Ederef
         ((add (args argsIdent) (c_int (BinInt.Z.of_N i) coq_val)),
         coq_val)), (makeVar threadInfIdent nParam v fenv map0))
       in
       Monad0.bind (Obj.magic coq_Monad_option)
         (asgnAppVars'' argsIdent threadInfIdent nParam vs' ind' fenv map0)
         (fun rest ->
         Monad0.ret (Obj.magic coq_Monad_option) (Ssequence (rest, s_iv))))

(** val asgnAppVars' :
    ident -> ident -> nat -> positive list -> coq_N list -> fun_env ->
    fun_info_env -> statement option **)

let asgnAppVars' argsIdent threadInfIdent nParam vs ind fenv map0 =
  asgnAppVars'' argsIdent threadInfIdent nParam (skipn nParam vs)
    (skipn nParam ind) fenv map0

(** val asgnAppVars :
    ident -> ident -> ident -> nat -> positive list -> coq_N list -> fun_env
    -> fun_info_env -> statement option **)

let asgnAppVars argsIdent threadInfIdent tinfIdent nParam vs ind fenv map0 =
  match asgnAppVars' argsIdent threadInfIdent nParam vs ind fenv map0 with
  | Some s ->
    Monad0.ret (Obj.magic coq_Monad_option) (Ssequence ((Sset (argsIdent,
      (Efield ((tinfd threadInfIdent tinfIdent), argsIdent, (Tarray (uval,
      maxArgs, noattr)))))), s))
  | None -> None

(** val reserve :
    ident -> ident -> ident -> ident -> ident -> ident -> nat -> positive ->
    coq_Z -> positive list -> coq_N list -> fun_env -> fun_info_env ->
    statement option **)

let reserve argsIdent allocIdent limitIdent gcIdent threadInfIdent tinfIdent nParam funInf l vs ind fenv map0 =
  let arr = Evar (funInf, (Tarray (uval, l, noattr))) in
  (match asgnAppVars'' argsIdent threadInfIdent nParam (firstn nParam vs)
           (firstn nParam ind) fenv map0 with
   | Some bef ->
     (match asgnFunVars' argsIdent (firstn nParam vs) (firstn nParam ind) with
      | Some aft ->
        Some (Sifthenelse
          ((not (Ebinop (Ole, (Ederef (arr, uval)),
             (sub (limitPtr limitIdent) (allocPtr allocIdent)), type_bool))),
          (Ssequence ((Ssequence ((Ssequence (bef, (Scall (None,
          (gc gcIdent threadInfIdent),
          (arr :: ((tinf threadInfIdent tinfIdent) :: [])))))), (Sset
          (allocIdent, (Efield ((tinfd threadInfIdent tinfIdent), allocIdent,
          valPtr)))))), aft)), Sskip))
      | None -> None)
   | None -> None)

(** val make_case_switch :
    ident -> ident -> positive -> labeled_statements -> labeled_statements ->
    statement **)

let make_case_switch isptrIdent caseIdent x ls ls' =
  Ssequence ((isPtr isptrIdent caseIdent x), (Sifthenelse ((Etempvar
    (caseIdent, boolTy)), (Sswitch ((Ebinop (Oand, (Ederef
    ((add (Ecast ((Etempvar (x, coq_val)), valPtr))
       (c_int (Zneg Coq_xH) coq_val)), coq_val)),
    (make_cint (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
      Coq_xH)))))))) coq_val), coq_val)), ls)), (Sswitch ((Ebinop (Oshr,
    (Etempvar (x, coq_val)), (make_cint (Zpos Coq_xH) coq_val), coq_val)),
    ls')))))

(** val to_int64 : Uint63.t -> Integers.Int64.int **)

let to_int64 i =
  BinInt.Z.add (BinInt.Z.mul (to_Z i) (Zpos (Coq_xO Coq_xH))) (Zpos Coq_xH)

(** val float64_to_model : Float64.t -> float64_model **)

let float64_to_model =
  coq_Prim2SF

(** val model_to_ff : float64_model -> full_float **)

let model_to_ff =
  coq_SF2FF

(** val to_float :
    ident -> ident -> ident -> ident -> ident -> ident -> String.t -> ident
    -> ident -> ident -> ident -> ident -> ident -> nat -> prim_env ->
    Float64.t -> float **)

let to_float _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ f =
  coq_FF2B (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))) (Zpos
    (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO Coq_xH))))))))))) (model_to_ff (float64_to_model f))

(** val compile_float :
    ident -> ctor_env -> n_ind_env -> fun_env -> fun_info_env -> positive ->
    float -> statement **)

let compile_float allocIdent _ _ _ _ x f =
  let tag =
    c_int (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
      (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))))))) (Tlong (Unsigned, noattr))
  in
  Ssequence ((Ssequence ((Ssequence ((Sset (x, (Ecast
  ((add (allocPtr allocIdent) (c_int BinInt.Z.one coq_val)), coq_val)))),
  (Sset (allocIdent,
  (add (allocPtr allocIdent) (c_int (Zpos (Coq_xO Coq_xH)) coq_val)))))),
  (Sassign ((Ederef
  ((add (Ecast ((Etempvar (x, coq_val)), valPtr))
     (c_int (Zneg Coq_xH) coq_val)), coq_val)), tag)))), (Sassign ((Ederef
  ((add (Ecast ((Etempvar (x, coq_val)), valPtr)) (c_int Z0 coq_val)),
  coq_val)), (Econst_float (f, (Tfloat (F64, noattr)))))))

(** val compile_primitive :
    ident -> ident -> ident -> ident -> ident -> ident -> String.t -> ident
    -> ident -> ident -> ident -> ident -> ident -> nat -> prim_env ->
    ctor_env -> n_ind_env -> fun_env -> fun_info_env -> positive -> primitive
    -> statement **)

let compile_primitive argsIdent allocIdent limitIdent gcIdent mainIdent bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent numArgsIdent isptrIdent caseIdent nParam prims cenv ienv fenv map0 x p =
  let i = projT2 p in
  (match projT1 p with
   | Coq_primInt ->
     Sset (x, (Econst_long ((to_int64 (Obj.magic i)), (Tlong (Unsigned,
       noattr)))))
   | Coq_primFloat ->
     compile_float allocIdent cenv ienv fenv map0 x
       (to_float argsIdent allocIdent limitIdent gcIdent mainIdent bodyIdent
         bodyName threadInfIdent tinfIdent heapInfIdent numArgsIdent
         isptrIdent caseIdent nParam prims (Obj.magic i)))

(** val translate_body :
    ident -> ident -> ident -> ident -> ident -> ident -> String.t -> ident
    -> ident -> ident -> ident -> ident -> ident -> nat -> prim_env -> exp ->
    fun_env -> ctor_env -> n_ind_env -> fun_info_env -> statement option **)

let rec translate_body argsIdent allocIdent limitIdent gcIdent mainIdent bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent numArgsIdent isptrIdent caseIdent nParam prims e fenv cenv ienv map0 =
  match e with
  | Econstr (x, t0, vs, e') ->
    Monad0.bind (Obj.magic coq_Monad_option)
      (assignConstructorS allocIdent threadInfIdent nParam cenv ienv fenv
        map0 x t0 vs) (fun prog ->
      Monad0.bind (Obj.magic coq_Monad_option)
        (translate_body argsIdent allocIdent limitIdent gcIdent mainIdent
          bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent
          numArgsIdent isptrIdent caseIdent nParam prims e' fenv cenv ienv
          map0) (fun prog' ->
        Monad0.ret (Obj.magic coq_Monad_option) (Ssequence (prog, prog'))))
  | Ecase (x, cs) ->
    Monad0.bind (Obj.magic coq_Monad_option)
      (let rec makeCases = function
       | [] -> Monad0.ret (Obj.magic coq_Monad_option) (LSnil, LSnil)
       | p :: l' ->
         Monad0.bind (Obj.magic coq_Monad_option)
           (translate_body argsIdent allocIdent limitIdent gcIdent mainIdent
             bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent
             numArgsIdent isptrIdent caseIdent nParam prims (snd p) fenv cenv
             ienv map0) (fun prog ->
           Monad0.bind (Obj.magic coq_Monad_option) (makeCases l') (fun p' ->
             let (ls, ls') = p' in
             (match make_ctor_rep cenv (fst p) with
              | Some c ->
                (match c with
                 | Coq_enum t0 ->
                   let tag =
                     BinInt.Z.add
                       (BinInt.Z.shiftl (BinInt.Z.of_N t0) (Zpos Coq_xH))
                       (Zpos Coq_xH)
                   in
                   (match ls' with
                    | LSnil ->
                      Monad0.ret (Obj.magic coq_Monad_option) (ls, (LScons
                        (None, (Ssequence (prog, Sbreak)), ls')))
                    | LScons (_, _, _) ->
                      Monad0.ret (Obj.magic coq_Monad_option) (ls, (LScons
                        ((Some (BinInt.Z.shiftr tag (Zpos Coq_xH))),
                        (Ssequence (prog, Sbreak)), ls'))))
                 | Coq_boxed (t0, a) ->
                   let tag =
                     BinInt.Z.add
                       (BinInt.Z.shiftl (BinInt.Z.of_N a) (Zpos (Coq_xO
                         (Coq_xI (Coq_xO Coq_xH))))) (BinInt.Z.of_N t0)
                   in
                   (match ls with
                    | LSnil ->
                      Monad0.ret (Obj.magic coq_Monad_option) ((LScons (None,
                        (Ssequence (prog, Sbreak)), ls)), ls')
                    | LScons (_, _, _) ->
                      Monad0.ret (Obj.magic coq_Monad_option) ((LScons ((Some
                        (BinInt.Z.coq_land tag (Zpos (Coq_xI (Coq_xI (Coq_xI
                          (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))))))),
                        (Ssequence (prog, Sbreak)), ls)), ls')))
              | None -> None)))
       in makeCases cs) (fun p ->
      let (ls, ls') = p in
      Monad0.ret (Obj.magic coq_Monad_option)
        (make_case_switch isptrIdent caseIdent x ls ls'))
  | Eproj (x, _, n, v, e') ->
    Monad0.bind (Obj.magic coq_Monad_option)
      (translate_body argsIdent allocIdent limitIdent gcIdent mainIdent
        bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent numArgsIdent
        isptrIdent caseIdent nParam prims e' fenv cenv ienv map0)
      (fun prog ->
      Monad0.ret (Obj.magic coq_Monad_option) (Ssequence ((Sset (x, (Ederef
        ((add (Ecast ((Etempvar (v, coq_val)), valPtr))
           (c_int (BinInt.Z.of_N n) coq_val)), coq_val)))), prog)))
  | Eletapp (x, f, t0, vs, e') ->
    Monad0.bind (Obj.magic coq_Monad_option)
      (translate_body argsIdent allocIdent limitIdent gcIdent mainIdent
        bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent numArgsIdent
        isptrIdent caseIdent nParam prims e' fenv cenv ienv map0)
      (fun prog ->
      Monad0.bind (Obj.magic coq_Monad_option) (M.get t0 (Obj.magic fenv))
        (fun inf ->
        Monad0.bind (Obj.magic coq_Monad_option)
          (asgnAppVars argsIdent threadInfIdent tinfIdent nParam vs (snd inf)
            fenv map0) (fun asgn ->
          let vv = makeVar threadInfIdent nParam f fenv map0 in
          let pnum = min (N.to_nat (fst inf)) nParam in
          Monad0.bind (Obj.magic coq_Monad_option)
            (mkCall threadInfIdent tinfIdent nParam fenv map0 (Ecast (vv,
              (Tpointer ((mkFunTy threadInfIdent pnum), noattr)))) pnum vs)
            (fun c ->
            Monad0.ret (Obj.magic coq_Monad_option) (Ssequence ((Ssequence
              ((Ssequence ((Ssequence ((Ssequence ((Ssequence (asgn, (Sassign
              ((Efield ((tinfd threadInfIdent tinfIdent), allocIdent,
              valPtr)), (allocPtr allocIdent))))), (Sassign ((Efield
              ((tinfd threadInfIdent tinfIdent), limitIdent, valPtr)),
              (limitPtr limitIdent))))), c)), (Sset (allocIdent, (Efield
              ((tinfd threadInfIdent tinfIdent), allocIdent, valPtr)))))),
              (Sset (x, (Ederef
              ((add (Ecast ((args argsIdent), valPtr))
                 (c_int (BinInt.Z.of_nat (S O)) coq_val)), coq_val)))))),
              prog))))))
  | Efun (_, _) -> None
  | Eapp (x, t0, vs) ->
    Monad0.bind (Obj.magic coq_Monad_option) (M.get t0 (Obj.magic fenv))
      (fun inf ->
      Monad0.bind (Obj.magic coq_Monad_option)
        (asgnAppVars argsIdent threadInfIdent tinfIdent nParam vs (snd inf)
          fenv map0) (fun asgn ->
        let vv = makeVar threadInfIdent nParam x fenv map0 in
        let pnum = min (N.to_nat (fst inf)) nParam in
        Monad0.bind (Obj.magic coq_Monad_option)
          (mkCall threadInfIdent tinfIdent nParam fenv map0 (Ecast (vv,
            (Tpointer ((mkFunTy threadInfIdent pnum), noattr)))) pnum vs)
          (fun c ->
          Monad0.ret (Obj.magic coq_Monad_option) (Ssequence ((Ssequence
            ((Ssequence (asgn, (Sassign ((Efield
            ((tinfd threadInfIdent tinfIdent), allocIdent, valPtr)),
            (allocPtr allocIdent))))), (Sassign ((Efield
            ((tinfd threadInfIdent tinfIdent), limitIdent, valPtr)),
            (limitPtr limitIdent))))), c)))))
  | Eprim_val (x, p, e') ->
    Monad0.bind (Obj.magic coq_Monad_option)
      (translate_body argsIdent allocIdent limitIdent gcIdent mainIdent
        bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent numArgsIdent
        isptrIdent caseIdent nParam prims e' fenv cenv ienv map0)
      (fun prog ->
      Monad0.ret (Obj.magic coq_Monad_option) (Ssequence
        ((compile_primitive argsIdent allocIdent limitIdent gcIdent mainIdent
           bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent
           numArgsIdent isptrIdent caseIdent nParam prims cenv ienv fenv map0
           x p), prog)))
  | Eprim (x, p, vs, e') ->
    (match PTree.get p prims with
     | Some p0 ->
       let (p1, _) = p0 in
       let (_, b) = p1 in
       if b
       then Monad0.bind (Obj.magic coq_Monad_option)
              (translate_body argsIdent allocIdent limitIdent gcIdent
                mainIdent bodyIdent bodyName threadInfIdent tinfIdent
                heapInfIdent numArgsIdent isptrIdent caseIdent nParam prims
                e' fenv cenv ienv map0) (fun prog ->
              Monad0.bind (Obj.magic coq_Monad_option)
                (mkPrimCallTinfo threadInfIdent tinfIdent nParam x p
                  (length vs) fenv map0 vs) (fun pr_call ->
                Monad0.ret (Obj.magic coq_Monad_option) (Ssequence
                  ((Ssequence ((Ssequence ((Ssequence ((Ssequence ((Sassign
                  ((Efield ((tinfd threadInfIdent tinfIdent), allocIdent,
                  valPtr)), (allocPtr allocIdent))), (Sassign ((Efield
                  ((tinfd threadInfIdent tinfIdent), limitIdent, valPtr)),
                  (limitPtr limitIdent))))), pr_call)), (Sset (allocIdent,
                  (Efield ((tinfd threadInfIdent tinfIdent), allocIdent,
                  valPtr)))))), (Sset (limitIdent, (Efield
                  ((tinfd threadInfIdent tinfIdent), limitIdent,
                  valPtr)))))), prog))))
       else Monad0.bind (Obj.magic coq_Monad_option)
              (translate_body argsIdent allocIdent limitIdent gcIdent
                mainIdent bodyIdent bodyName threadInfIdent tinfIdent
                heapInfIdent numArgsIdent isptrIdent caseIdent nParam prims
                e' fenv cenv ienv map0) (fun prog ->
              Monad0.bind (Obj.magic coq_Monad_option)
                (mkPrimCall threadInfIdent nParam x p (length vs) fenv map0
                  vs) (fun pr_call ->
                Monad0.ret (Obj.magic coq_Monad_option) (Ssequence (pr_call,
                  prog))))
     | None -> None)
  | Ehalt x ->
    Monad0.ret (Obj.magic coq_Monad_option) (Ssequence ((Ssequence ((Sassign
      ((Efield ((tinfd threadInfIdent tinfIdent), allocIdent, valPtr)),
      (allocPtr allocIdent))), (Sassign ((Efield
      ((tinfd threadInfIdent tinfIdent), limitIdent, valPtr)),
      (limitPtr limitIdent))))), (Sassign ((Ederef
      ((add (args argsIdent) (c_int (BinInt.Z.of_nat (S O)) coq_val)),
      coq_val)), (makeVar threadInfIdent nParam x fenv map0)))))

(** val mkFun :
    ident -> ident -> ident -> ident -> ident -> ident -> nat -> positive
    list -> positive list -> statement -> coq_function **)

let mkFun argsIdent allocIdent limitIdent threadInfIdent tinfIdent caseIdent nParam vs loc body =
  { fn_return = Tvoid; fn_callconv = cc_default; fn_params = ((tinfIdent,
    (threadInf threadInfIdent)) :: (map (fun x -> (x, coq_val))
                                     (firstn nParam vs))); fn_vars = [];
    fn_temps =
    (app (map (fun x -> (x, coq_val)) (app (skipn nParam vs) loc))
      ((allocIdent, valPtr) :: ((limitIdent, valPtr) :: ((argsIdent,
      valPtr) :: ((caseIdent, boolTy) :: []))))); fn_body = body }

(** val translate_fundefs :
    ident -> ident -> ident -> ident -> ident -> ident -> String.t -> ident
    -> ident -> ident -> ident -> ident -> ident -> nat -> prim_env ->
    fundefs -> fun_env -> ctor_env -> n_ind_env -> fun_info_env ->
    (positive * (Clight.fundef, coq_type) globdef) list option **)

let rec translate_fundefs argsIdent allocIdent limitIdent gcIdent mainIdent bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent numArgsIdent isptrIdent caseIdent nParam prims fnd fenv cenv ienv map0 =
  match fnd with
  | Fcons (f, t0, vs, e, fnd') ->
    (match translate_fundefs argsIdent allocIdent limitIdent gcIdent
             mainIdent bodyIdent bodyName threadInfIdent tinfIdent
             heapInfIdent numArgsIdent isptrIdent caseIdent nParam prims fnd'
             fenv cenv ienv map0 with
     | Some rest ->
       (match translate_body argsIdent allocIdent limitIdent gcIdent
                mainIdent bodyIdent bodyName threadInfIdent tinfIdent
                heapInfIdent numArgsIdent isptrIdent caseIdent nParam prims e
                fenv cenv ienv map0 with
        | Some body ->
          (match M.get t0 fenv with
           | Some inf ->
             let (l, locs) = inf in
             (match asgnFunVars argsIdent nParam vs locs with
              | Some asgn ->
                (match M.get f map0 with
                 | Some gcArrIdent ->
                   (match reserve argsIdent allocIdent limitIdent gcIdent
                            threadInfIdent tinfIdent nParam (fst gcArrIdent)
                            (BinInt.Z.of_N (N.add l (Npos (Coq_xO Coq_xH))))
                            vs locs fenv map0 with
                    | Some res ->
                      Monad0.ret (Obj.magic coq_Monad_option) ((f, (Gfun
                        (Internal
                        (mkFun argsIdent allocIdent limitIdent threadInfIdent
                          tinfIdent caseIdent nParam vs (get_allocs e)
                          (Ssequence ((Ssequence ((Ssequence ((Ssequence
                          ((Ssequence ((Sset (allocIdent, (Efield
                          ((tinfd threadInfIdent tinfIdent), allocIdent,
                          valPtr)))), (Sset (limitIdent, (Efield
                          ((tinfd threadInfIdent tinfIdent), limitIdent,
                          valPtr)))))), (Sset (argsIdent, (Efield
                          ((tinfd threadInfIdent tinfIdent), argsIdent,
                          (Tarray (uval, maxArgs, noattr)))))))), res)),
                          asgn)), body)))))) :: rest)
                    | None -> None)
                 | None -> None)
              | None -> None)
           | None -> None)
        | None -> None)
     | None -> None)
  | Fnil -> Monad0.ret (Obj.magic coq_Monad_option) []

(** val make_extern_decl :
    name_env -> (positive * (Clight.fundef, coq_type) globdef) -> bool ->
    (positive * (Clight.fundef, coq_type) globdef) option **)

let make_extern_decl nenv def gv =
  let (vIdent, g) = def in
  (match g with
   | Gfun f0 ->
     (match f0 with
      | Internal f ->
        (match M.get vIdent nenv with
         | Some n ->
           (match n with
            | Coq_nAnon -> None
            | Coq_nNamed f_string ->
              Some (vIdent, (Gfun (External ((EF_external
                ((String.to_string f_string),
                (signature_of_type (type_of_params f.fn_params) f.fn_return
                  f.fn_callconv))), (type_of_params f.fn_params),
                f.fn_return, f.fn_callconv)))))
         | None -> None)
      | External (_, _, _, _) -> None)
   | Gvar v ->
     let { gvar_info = v_info; gvar_init = _; gvar_readonly = v_r;
       gvar_volatile = v_v } = v
     in
     if gv
     then Some (vIdent, (Gvar { gvar_info = v_info; gvar_init = [];
            gvar_readonly = v_r; gvar_volatile = v_v }))
     else None)

(** val make_extern_decls :
    name_env -> (positive * (Clight.fundef, coq_type) globdef) list -> bool
    -> (positive * (Clight.fundef, coq_type) globdef) list **)

let rec make_extern_decls nenv defs gv =
  match defs with
  | [] -> []
  | fdefs :: defs' ->
    let decls = make_extern_decls nenv defs' gv in
    (match make_extern_decl nenv fdefs gv with
     | Some decl -> decl :: decls
     | None -> decls)

(** val body_external_decl :
    ident -> String.t -> ident -> ident -> positive * (Clight.fundef,
    coq_type) globdef **)

let body_external_decl bodyIdent bodyName threadInfIdent tinfIdent =
  let params = type_of_params ((tinfIdent, (threadInf threadInfIdent)) :: [])
  in
  (bodyIdent, (Gfun (External ((EF_external ((String.to_string bodyName),
  (signature_of_type params Tvoid cc_default))), params, Tvoid, cc_default))))

(** val translate_funs :
    ident -> ident -> ident -> ident -> ident -> ident -> String.t -> ident
    -> ident -> ident -> ident -> ident -> ident -> nat -> prim_env -> exp ->
    fun_env -> ctor_env -> n_ind_env -> fun_info_env ->
    (positive * (Clight.fundef, coq_type) globdef) list option **)

let translate_funs argsIdent allocIdent limitIdent gcIdent mainIdent bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent numArgsIdent isptrIdent caseIdent nParam prims e fenv cenv ienv m =
  match e with
  | Efun (fnd, e0) ->
    Monad0.bind (Obj.magic coq_Monad_option)
      (translate_fundefs argsIdent allocIdent limitIdent gcIdent mainIdent
        bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent numArgsIdent
        isptrIdent caseIdent nParam prims fnd fenv cenv ienv m) (fun funs ->
      let localVars = get_allocs e0 in
      Monad0.bind (Obj.magic coq_Monad_option)
        (Obj.magic translate_body argsIdent allocIdent limitIdent gcIdent
          mainIdent bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent
          numArgsIdent isptrIdent caseIdent nParam prims e0 fenv cenv ienv m)
        (fun body ->
        Monad0.bind (Obj.magic coq_Monad_option)
          (M.get mainIdent (Obj.magic m)) (fun x ->
          let (gcArrIdent, _) = x in
          let argsExpr = Efield ((tinfd threadInfIdent tinfIdent), argsIdent,
            (Tarray (uval, maxArgs, noattr)))
          in
          Monad0.ret (Obj.magic coq_Monad_option) ((bodyIdent, (Gfun
            (Internal { fn_return = coq_val; fn_callconv = cc_default;
            fn_params = ((tinfIdent, (threadInf threadInfIdent)) :: []);
            fn_vars = []; fn_temps =
            (app (map (fun x0 -> (x0, coq_val)) localVars) ((allocIdent,
              valPtr) :: ((limitIdent, valPtr) :: ((argsIdent,
              valPtr) :: [])))); fn_body = (Ssequence ((Ssequence ((Ssequence
            ((Ssequence ((Ssequence ((Sset (allocIdent, (Efield
            ((tinfd threadInfIdent tinfIdent), allocIdent, valPtr)))), (Sset
            (limitIdent, (Efield ((tinfd threadInfIdent tinfIdent),
            limitIdent, valPtr)))))), (Sset (argsIdent, argsExpr)))),
            (reserve_body allocIdent limitIdent gcIdent threadInfIdent
              tinfIdent gcArrIdent (Zpos (Coq_xO Coq_xH))))), body)),
            (Sreturn (Some (Ederef
            ((add (Ecast (argsExpr, valPtr))
               (c_int (BinInt.Z.of_nat (S O)) coq_val)),
            coq_val)))))) }))) :: funs))))
  | _ -> None

type 't nState = (positive, 't) state

(** val getName : positive nState **)

let getName =
  Monad0.bind (Obj.magic coq_Monad_state)
    (Obj.magic coq_MonadState_state).get (fun n ->
    Monad0.bind (Obj.magic coq_Monad_state)
      ((Obj.magic coq_MonadState_state).put (Pos.add n Coq_xH)) (fun _ ->
      Monad0.ret (Obj.magic coq_Monad_state) n))

(** val make_ind_array : coq_N list -> init_data list **)

let rec make_ind_array = function
| [] -> []
| n :: l' -> (coq_Init_int (BinInt.Z.of_N n)) :: (make_ind_array l')

(** val update_name_env_fun_info :
    positive -> positive -> name_env -> name_env **)

let update_name_env_fun_info f f_inf nenv =
  match M.get f nenv with
  | Some n ->
    (match n with
     | Coq_nAnon ->
       M.set f_inf (Coq_nNamed
         (String.append
           (String.append (String.String (Coq_x78, String.EmptyString))
             (show_pos f)) (String.String (Coq_x5f, (String.String (Coq_x69,
           (String.String (Coq_x6e, (String.String (Coq_x66, (String.String
           (Coq_x6f, String.EmptyString)))))))))))) nenv
     | Coq_nNamed s ->
       M.set f_inf (Coq_nNamed
         (String.append s (String.String (Coq_x5f, (String.String (Coq_x69,
           (String.String (Coq_x6e, (String.String (Coq_x66, (String.String
           (Coq_x6f, String.EmptyString)))))))))))) nenv)
  | None ->
    M.set f_inf (Coq_nNamed
      (String.append (show_pos f) (String.String (Coq_x5f, (String.String
        (Coq_x69, (String.String (Coq_x6e, (String.String (Coq_x66,
        (String.String (Coq_x6f, String.EmptyString)))))))))))) nenv

(** val make_fundef_info :
    fundefs -> fun_env -> name_env -> (((positive * (Clight.fundef, coq_type)
    globdef) list * fun_info_env) * name_env) option nState **)

let rec make_fundef_info fnd fenv nenv =
  match fnd with
  | Fcons (x, t0, _, e, fnd') ->
    (match M.get t0 fenv with
     | Some inf ->
       let (_, l) = inf in
       Monad0.bind (Obj.magic coq_Monad_state)
         (make_fundef_info fnd' fenv nenv) (fun rest ->
         match rest with
         | Some rest' ->
           let (p, nenv') = rest' in
           let (defs, map0) = p in
           Monad0.bind (Obj.magic coq_Monad_state) (Obj.magic getName)
             (fun info_name ->
             let len = BinInt.Z.of_nat (length l) in
             let ind = { gvar_info = (Tarray (uval,
               (BinInt.Z.add len (Zpos (Coq_xO Coq_xH))), noattr));
               gvar_init =
               ((coq_Init_int (BinInt.Z.of_nat (max_allocs e))) :: ((coq_Init_int
                                                                    len) :: 
               (make_ind_array l))); gvar_readonly = true; gvar_volatile =
               false }
             in
             Monad0.ret (Obj.magic coq_Monad_state) (Some ((((info_name,
               (Gvar ind)) :: defs), (M.set x (info_name, t0) map0)),
               (update_name_env_fun_info x info_name nenv'))))
         | None -> Monad0.ret (Obj.magic coq_Monad_state) None)
     | None -> Monad0.ret (Obj.magic coq_Monad_state) None)
  | Fnil ->
    Monad0.ret (Obj.magic coq_Monad_state) (Some (([], M.empty), nenv))

(** val add_bodyinfo :
    ident -> exp -> fun_env -> name_env -> fun_info_env ->
    (positive * (Clight.fundef, coq_type) globdef) list ->
    (((positive * (Clight.fundef, coq_type) globdef)
    list * (positive * positive) M.tree) * name M.tree) option nState **)

let add_bodyinfo mainIdent e _ nenv map0 defs =
  Monad0.bind (Obj.magic coq_Monad_state) (Obj.magic getName)
    (fun info_name ->
    let ind = { gvar_info = (Tarray (uval, (Zpos (Coq_xO Coq_xH)), noattr));
      gvar_init =
      ((coq_Init_int (BinInt.Z.of_nat (max_allocs e))) :: ((coq_Init_int Z0) :: []));
      gvar_readonly = true; gvar_volatile = false }
    in
    Monad0.ret (Obj.magic coq_Monad_state) (Some ((((info_name, (Gvar
      ind)) :: defs), (M.set mainIdent (info_name, Coq_xH) map0)),
      (M.set info_name (Coq_nNamed (String.String (Coq_x62, (String.String
        (Coq_x6f, (String.String (Coq_x64, (String.String (Coq_x79,
        (String.String (Coq_x5f, (String.String (Coq_x69, (String.String
        (Coq_x6e, (String.String (Coq_x66, (String.String (Coq_x6f,
        String.EmptyString))))))))))))))))))) nenv))))

(** val make_funinfo :
    ident -> exp -> fun_env -> name_env -> (((positive * (Clight.fundef,
    coq_type) globdef) list * fun_info_env) * name_env) option nState **)

let make_funinfo mainIdent e fenv nenv =
  match e with
  | Econstr (_, _, _, _) -> Monad0.ret (Obj.magic coq_Monad_state) None
  | Ecase (_, _) -> Monad0.ret (Obj.magic coq_Monad_state) None
  | Eproj (_, _, _, _, _) -> Monad0.ret (Obj.magic coq_Monad_state) None
  | Eletapp (_, _, _, _, _) -> Monad0.ret (Obj.magic coq_Monad_state) None
  | Efun (fnd, e') ->
    Monad0.bind (Obj.magic coq_Monad_state) (make_fundef_info fnd fenv nenv)
      (fun p ->
      match p with
      | Some p' ->
        let (p0, nenv') = p' in
        let (defs, map0) = p0 in
        add_bodyinfo mainIdent e' fenv nenv' map0 defs
      | None -> Monad0.ret (Obj.magic coq_Monad_state) None)
  | Eapp (_, _, _) -> Monad0.ret (Obj.magic coq_Monad_state) None
  | Eprim_val (_, _, _) -> Monad0.ret (Obj.magic coq_Monad_state) None
  | Eprim (_, _, _, _) -> Monad0.ret (Obj.magic coq_Monad_state) None
  | Ehalt _ -> Monad0.ret (Obj.magic coq_Monad_state) None

(** val global_defs :
    exp -> (positive * (Clight.fundef, coq_type) globdef) list **)

let global_defs _ =
  []

(** val make_defs :
    ident -> ident -> ident -> ident -> ident -> ident -> String.t -> ident
    -> ident -> ident -> ident -> ident -> ident -> nat -> prim_env -> exp ->
    fun_env -> ctor_env -> n_ind_env -> name M.t -> (name
    M.t * (positive * (Clight.fundef, coq_type) globdef) list) coq_exception
    nState **)

let make_defs argsIdent allocIdent limitIdent gcIdent mainIdent bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent numArgsIdent isptrIdent caseIdent nParam prims e fenv cenv ienv nenv =
  Monad0.bind (Obj.magic coq_Monad_state)
    (Obj.magic make_funinfo mainIdent e fenv nenv) (fun fun_inf' ->
    match fun_inf' with
    | Some p ->
      let (p0, nenv') = p in
      let (fun_inf, map0) = p0 in
      (match translate_funs argsIdent allocIdent limitIdent gcIdent mainIdent
               bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent
               numArgsIdent isptrIdent caseIdent nParam prims e fenv cenv
               ienv map0 with
       | Some fun_defs' ->
         let fun_defs = rev fun_defs' in
         Monad0.ret (Obj.magic coq_Monad_state) (Ret (nenv',
           (app (global_defs e) (app fun_inf fun_defs))))
       | None ->
         Monad0.ret (Obj.magic coq_Monad_state) (Exc (String.String (Coq_x74,
           (String.String (Coq_x72, (String.String (Coq_x61, (String.String
           (Coq_x6e, (String.String (Coq_x73, (String.String (Coq_x6c,
           (String.String (Coq_x61, (String.String (Coq_x74, (String.String
           (Coq_x65, (String.String (Coq_x5f, (String.String (Coq_x66,
           (String.String (Coq_x75, (String.String (Coq_x6e, (String.String
           (Coq_x73, String.EmptyString))))))))))))))))))))))))))))))
    | None ->
      Monad0.ret (Obj.magic coq_Monad_state) (Exc (String.String (Coq_x6d,
        (String.String (Coq_x61, (String.String (Coq_x6b, (String.String
        (Coq_x65, (String.String (Coq_x5f, (String.String (Coq_x66,
        (String.String (Coq_x75, (String.String (Coq_x6e, (String.String
        (Coq_x69, (String.String (Coq_x6e, (String.String (Coq_x66,
        (String.String (Coq_x6f, String.EmptyString))))))))))))))))))))))))))

(** val composites : composite_definition list **)

let composites =
  []

(** val mk_prog_opt :
    ident -> (ident * (Clight.fundef, coq_type) globdef) list -> ident ->
    bool -> Clight.program option **)

let mk_prog_opt bodyIdent defs main add_comp =
  let composites0 = if add_comp then composites else [] in
  let res = make_program composites0 defs (bodyIdent :: []) main in
  (match res with
   | OK p -> Some p
   | Error _ -> None)

(** val wrap_in_fun : exp -> exp **)

let wrap_in_fun e = match e with
| Efun (_, _) -> e
| _ -> Efun (Fnil, e)

(** val add_inf_vars :
    ident -> ident -> ident -> ident -> ident -> ident -> String.t -> ident
    -> ident -> ident -> ident -> ident -> ident -> name_env -> name_env **)

let add_inf_vars argsIdent allocIdent limitIdent gcIdent mainIdent bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent numArgsIdent isptrIdent caseIdent nenv =
  M.set isptrIdent (Coq_nNamed (String.String (Coq_x69, (String.String
    (Coq_x73, (String.String (Coq_x5f, (String.String (Coq_x70,
    (String.String (Coq_x74, (String.String (Coq_x72,
    String.EmptyString)))))))))))))
    (M.set argsIdent (Coq_nNamed (String.String (Coq_x61, (String.String
      (Coq_x72, (String.String (Coq_x67, (String.String (Coq_x73,
      String.EmptyString)))))))))
      (M.set allocIdent (Coq_nNamed (String.String (Coq_x61, (String.String
        (Coq_x6c, (String.String (Coq_x6c, (String.String (Coq_x6f,
        (String.String (Coq_x63, String.EmptyString)))))))))))
        (M.set limitIdent (Coq_nNamed (String.String (Coq_x6c, (String.String
          (Coq_x69, (String.String (Coq_x6d, (String.String (Coq_x69,
          (String.String (Coq_x74, String.EmptyString)))))))))))
          (M.set gcIdent (Coq_nNamed (String.String (Coq_x67, (String.String
            (Coq_x61, (String.String (Coq_x72, (String.String (Coq_x62,
            (String.String (Coq_x61, (String.String (Coq_x67, (String.String
            (Coq_x65, (String.String (Coq_x5f, (String.String (Coq_x63,
            (String.String (Coq_x6f, (String.String (Coq_x6c, (String.String
            (Coq_x6c, (String.String (Coq_x65, (String.String (Coq_x63,
            (String.String (Coq_x74,
            String.EmptyString)))))))))))))))))))))))))))))))
            (M.set mainIdent (Coq_nNamed (String.String (Coq_x6d,
              (String.String (Coq_x61, (String.String (Coq_x69,
              (String.String (Coq_x6e, String.EmptyString)))))))))
              (M.set bodyIdent (Coq_nNamed bodyName)
                (M.set threadInfIdent (Coq_nNamed (String.String (Coq_x74,
                  (String.String (Coq_x68, (String.String (Coq_x72,
                  (String.String (Coq_x65, (String.String (Coq_x61,
                  (String.String (Coq_x64, (String.String (Coq_x5f,
                  (String.String (Coq_x69, (String.String (Coq_x6e,
                  (String.String (Coq_x66, (String.String (Coq_x6f,
                  String.EmptyString)))))))))))))))))))))))
                  (M.set tinfIdent (Coq_nNamed (String.String (Coq_x74,
                    (String.String (Coq_x69, (String.String (Coq_x6e,
                    (String.String (Coq_x66, (String.String (Coq_x6f,
                    String.EmptyString)))))))))))
                    (M.set heapInfIdent (Coq_nNamed (String.String (Coq_x68,
                      (String.String (Coq_x65, (String.String (Coq_x61,
                      (String.String (Coq_x70, String.EmptyString)))))))))
                      (M.set caseIdent (Coq_nNamed (String.String (Coq_x61,
                        (String.String (Coq_x72, (String.String (Coq_x67,
                        String.EmptyString)))))))
                        (M.set numArgsIdent (Coq_nNamed (String.String
                          (Coq_x6e, (String.String (Coq_x75, (String.String
                          (Coq_x6d, (String.String (Coq_x5f, (String.String
                          (Coq_x61, (String.String (Coq_x72, (String.String
                          (Coq_x67, (String.String (Coq_x73,
                          String.EmptyString))))))))))))))))) nenv)))))))))))

(** val ensure_unique : name M.t -> name M.t **)

let ensure_unique l =
  M.map (fun x n ->
    match n with
    | Coq_nAnon -> Coq_nAnon
    | Coq_nNamed s ->
      Coq_nNamed
        (String.append s
          (String.append (String.String (Coq_x5f, String.EmptyString))
            (show_pos x)))) l

(** val make_tinfoIdent : positive **)

let make_tinfoIdent =
  Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))

(** val exportIdent : positive **)

let exportIdent =
  Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))

(** val make_tinfo_rec :
    ident -> positive * (Clight.fundef, coq_type) globdef **)

let make_tinfo_rec threadInfIdent =
  (make_tinfoIdent, (Gfun (External ((EF_external
    ((String.to_string (String.String (Coq_x6d, (String.String (Coq_x61,
       (String.String (Coq_x6b, (String.String (Coq_x65, (String.String
       (Coq_x5f, (String.String (Coq_x74, (String.String (Coq_x69,
       (String.String (Coq_x6e, (String.String (Coq_x66, (String.String
       (Coq_x6f, String.EmptyString))))))))))))))))))))), { sig_args = [];
    sig_res = (Tret val_typ); sig_cc = cc_default })), Tnil,
    (threadInf threadInfIdent), cc_default))))

(** val export_rec : ident -> positive * (Clight.fundef, coq_type) globdef **)

let export_rec threadInfIdent =
  (exportIdent, (Gfun (External ((EF_external
    ((String.to_string (String.String (Coq_x65, (String.String (Coq_x78,
       (String.String (Coq_x70, (String.String (Coq_x6f, (String.String
       (Coq_x72, (String.String (Coq_x74, String.EmptyString))))))))))))),
    { sig_args = (val_typ :: []); sig_res = (Tret val_typ); sig_cc =
    cc_default })), (Tcons ((threadInf threadInfIdent), Tnil)), valPtr,
    cc_default))))

(** val make_empty_header :
    ctor_env -> n_ind_env -> exp -> name_env ->
    (name_env * (ident * (Clight.fundef, coq_type) globdef) list) option
    nState **)

let make_empty_header _ _ _ nenv =
  Monad0.ret (Obj.magic coq_Monad_state) (Some (nenv, []))

(** val compile :
    ident -> ident -> ident -> ident -> ident -> ident -> String.t -> ident
    -> ident -> ident -> ident -> ident -> ident -> nat -> prim_env -> exp ->
    ctor_env -> name M.t -> ((name M.t * Clight.program
    option) * Clight.program option) coq_exception **)

let compile argsIdent allocIdent limitIdent gcIdent mainIdent bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent numArgsIdent isptrIdent caseIdent nParam prims e cenv nenv =
  let e0 = wrap_in_fun e in
  let fenv = compute_fun_env e0 in
  let ienv = compute_ind_env cenv in
  let p'' =
    make_defs argsIdent allocIdent limitIdent gcIdent mainIdent bodyIdent
      bodyName threadInfIdent tinfIdent heapInfIdent numArgsIdent isptrIdent
      caseIdent nParam prims e0 fenv cenv ienv nenv
  in
  let n =
    Pos.add
      (max_var e0 (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
        Coq_xH))))))) Coq_xH
  in
  let p' = p'' n in
  (match fst p' with
   | Exc s ->
     Exc
       (String.append (String.String (Coq_x4c, (String.String (Coq_x61,
         (String.String (Coq_x6d, (String.String (Coq_x62, (String.String
         (Coq_x64, (String.String (Coq_x61, (String.String (Coq_x41,
         (String.String (Coq_x4e, (String.String (Coq_x46, (String.String
         (Coq_x5f, (String.String (Coq_x74, (String.String (Coq_x6f,
         (String.String (Coq_x5f, (String.String (Coq_x43, (String.String
         (Coq_x6c, (String.String (Coq_x69, (String.String (Coq_x67,
         (String.String (Coq_x68, (String.String (Coq_x74, (String.String
         (Coq_x3a, (String.String (Coq_x20, (String.String (Coq_x46,
         (String.String (Coq_x61, (String.String (Coq_x69, (String.String
         (Coq_x6c, (String.String (Coq_x75, (String.String (Coq_x72,
         (String.String (Coq_x65, (String.String (Coq_x20, (String.String
         (Coq_x69, (String.String (Coq_x6e, (String.String (Coq_x20,
         (String.String (Coq_x6d, (String.String (Coq_x61, (String.String
         (Coq_x6b, (String.String (Coq_x65, (String.String (Coq_x5f,
         (String.String (Coq_x64, (String.String (Coq_x65, (String.String
         (Coq_x66, (String.String (Coq_x73, (String.String (Coq_x3a,
         String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
         s)
   | Ret p ->
     let (nenv0, defs) = p in
     let nenv1 =
       add_inf_vars argsIdent allocIdent limitIdent gcIdent mainIdent
         bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent
         numArgsIdent isptrIdent caseIdent (ensure_unique nenv0)
     in
     let forward_defs = make_extern_decls nenv1 defs false in
     let header_pre = make_empty_header cenv ienv e0 nenv1 in
     let header_p =
       header_pre (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI
         (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI
         (Coq_xO (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))))))))))))))))
     in
     (match fst header_p with
      | Some p0 ->
        let (nenv2, hdefs) = p0 in
        Ret
        (((M.set make_tinfoIdent (Coq_nNamed (String.String (Coq_x6d,
            (String.String (Coq_x61, (String.String (Coq_x6b, (String.String
            (Coq_x65, (String.String (Coq_x5f, (String.String (Coq_x74,
            (String.String (Coq_x69, (String.String (Coq_x6e, (String.String
            (Coq_x66, (String.String (Coq_x6f,
            String.EmptyString)))))))))))))))))))))
            (M.set exportIdent (Coq_nNamed (String.String (Coq_x65,
              (String.String (Coq_x78, (String.String (Coq_x70,
              (String.String (Coq_x6f, (String.String (Coq_x72,
              (String.String (Coq_x74, String.EmptyString))))))))))))) nenv2)),
        (mk_prog_opt bodyIdent
          ((body_external_decl bodyIdent bodyName threadInfIdent tinfIdent) :: 
          (make_extern_decls nenv2 hdefs true)) mainIdent false)),
        (mk_prog_opt bodyIdent
          ((make_tinfo_rec threadInfIdent) :: ((export_rec threadInfIdent) :: 
          (app forward_defs (app defs hdefs)))) mainIdent true))
      | None ->
        Exc (String.String (Coq_x4c, (String.String (Coq_x61, (String.String
          (Coq_x6d, (String.String (Coq_x62, (String.String (Coq_x64,
          (String.String (Coq_x61, (String.String (Coq_x41, (String.String
          (Coq_x4e, (String.String (Coq_x46, (String.String (Coq_x5f,
          (String.String (Coq_x74, (String.String (Coq_x6f, (String.String
          (Coq_x5f, (String.String (Coq_x43, (String.String (Coq_x6c,
          (String.String (Coq_x69, (String.String (Coq_x67, (String.String
          (Coq_x68, (String.String (Coq_x74, (String.String (Coq_x3a,
          (String.String (Coq_x20, (String.String (Coq_x46, (String.String
          (Coq_x61, (String.String (Coq_x69, (String.String (Coq_x6c,
          (String.String (Coq_x75, (String.String (Coq_x72, (String.String
          (Coq_x65, (String.String (Coq_x20, (String.String (Coq_x69,
          (String.String (Coq_x6e, (String.String (Coq_x20, (String.String
          (Coq_x6d, (String.String (Coq_x61, (String.String (Coq_x6b,
          (String.String (Coq_x65, (String.String (Coq_x5f, (String.String
          (Coq_x68, (String.String (Coq_x65, (String.String (Coq_x61,
          (String.String (Coq_x64, (String.String (Coq_x65, (String.String
          (Coq_x72,
          String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
