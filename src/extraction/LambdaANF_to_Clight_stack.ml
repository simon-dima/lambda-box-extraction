open AST
open Archi
open Ascii
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
open Nat0
open PeanoNat
open Specif
open String0
open Uint0
open Bytestring
open CompM
open Cps
open Cps_show
open Identifiers
open Set_util
open State
open Toplevel0

(** val show_name : name -> String.t **)

let show_name = function
| Coq_nAnon ->
  String.String (Coq_x61, (String.String (Coq_x6e, (String.String (Coq_x6f,
    (String.String (Coq_x6e, String.EmptyString)))))))
| Coq_nNamed d -> d

(** val maxArgs : coq_Z **)

let maxArgs =
  Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO Coq_xH))))))))))

(** val makeArgList' : positive list -> coq_N list **)

let rec makeArgList' = function
| [] -> []
| _ :: vs' -> (N.of_nat (Datatypes.length vs')) :: (makeArgList' vs')

(** val makeArgList : positive list -> coq_N list **)

let makeArgList vs =
  rev (makeArgList' vs)

type fun_info_env = (positive * fun_tag) M.t

(** val compute_fun_env' : nat -> name_env -> fun_env -> exp -> fun_env **)

let rec compute_fun_env' n nenv fenv e =
  match n with
  | O -> fenv
  | S n' ->
    (match e with
     | Econstr (_, _, _, e') -> compute_fun_env' n' nenv fenv e'
     | Ecase (_, cs) ->
       fold_left (fun p e0 -> compute_fun_env' n' nenv p e0) (map snd cs) fenv
     | Eproj (_, _, _, _, e') -> compute_fun_env' n' nenv fenv e'
     | Eletapp (_, _, t0, vs, e') ->
       compute_fun_env' n' nenv
         (M.set t0 ((N.of_nat (Datatypes.length vs)), (makeArgList vs)) fenv)
         e'
     | Efun (fnd, e') ->
       let fenv' = compute_fun_env_fundefs n' nenv fnd fenv in
       compute_fun_env' n' nenv fenv' e'
     | Eapp (_, t0, vs) ->
       M.set t0 ((N.of_nat (Datatypes.length vs)), (makeArgList vs)) fenv
     | Eprim_val (_, _, e') -> compute_fun_env' n' nenv fenv e'
     | Eprim (_, _, _, e') -> compute_fun_env' n' nenv fenv e'
     | Ehalt _ -> fenv)

(** val compute_fun_env_fundefs :
    nat -> name_env -> fundefs -> fun_env -> fun_env **)

and compute_fun_env_fundefs n nenv fnd fenv =
  match n with
  | O -> fenv
  | S n' ->
    (match fnd with
     | Fcons (_, t0, vs, e, fnd') ->
       let fenv' =
         M.set t0 ((N.of_nat (Datatypes.length vs)), (makeArgList vs)) fenv
       in
       let fenv'' = compute_fun_env' n' nenv fenv' e in
       compute_fun_env_fundefs n' nenv fnd' fenv''
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

(** val compute_fun_env : name_env -> exp -> fun_env **)

let compute_fun_env nenv e =
  compute_fun_env' (max_depth e) nenv M.empty e

(** val get_locals : exp -> positive list **)

let rec get_locals = function
| Econstr (x, _, _, e') -> x :: (get_locals e')
| Ecase (_, cs) ->
  let rec helper = function
  | [] -> []
  | p :: cs' -> let (_, e') = p in app (get_locals e') (helper cs')
  in helper cs
| Eproj (x, _, _, _, e') -> x :: (get_locals e')
| Eletapp (x, _, _, _, e') -> x :: (get_locals e')
| Efun (fnd, e') -> app (get_locals_fundefs fnd) (get_locals e')
| Eprim_val (x, _, e') -> x :: (get_locals e')
| Eprim (x, _, _, e') -> x :: (get_locals e')
| _ -> []

(** val get_locals_fundefs : fundefs -> positive list **)

and get_locals_fundefs = function
| Fcons (_, _, vs, e, fnd') ->
  app vs (app (get_locals e) (get_locals_fundefs fnd'))
| Fnil -> []

(** val max_allocs : exp -> nat **)

let rec max_allocs = function
| Econstr (_, _, vs, e') ->
  (match vs with
   | [] -> max_allocs e'
   | _ :: _ -> S (add (max_allocs e') (Datatypes.length vs)))
| Ecase (_, cs) ->
  let rec helper = function
  | [] -> O
  | p :: cs' -> let (_, e') = p in max (max_allocs e') (helper cs')
  in helper cs
| Eproj (_, _, _, _, e') -> max_allocs e'
| Efun (fnd, e') -> max (max_allocs_fundefs fnd) (max_allocs e')
| Eprim_val (_, _, e') -> max_allocs e'
| Eprim (_, _, _, e') -> max_allocs e'
| _ -> O

(** val max_allocs_fundefs : fundefs -> nat **)

and max_allocs_fundefs = function
| Fcons (_, _, vs, e, fnd') ->
  max (add (Datatypes.length vs) (max_allocs e)) (max_allocs_fundefs fnd')
| Fnil -> O

type n_ind_ty_info = name * (((name * ctor_tag) * coq_N) * coq_N) list

type n_ind_env = n_ind_ty_info M.t

(** val update_ind_env :
    n_ind_env -> positive -> ctor_ty_info -> n_ind_env **)

let update_ind_env ienv p cInf =
  let { ctor_name = name0; ctor_ind_name = nameTy; ctor_ind_tag = t0;
    ctor_arity = arity; ctor_ordinal = ord } = cInf
  in
  (match M.get t0 ienv with
   | Some n ->
     let (nameTy0, iInf) = n in
     M.set t0 (nameTy0, ((((name0, p), arity), ord) :: iInf)) ienv
   | None -> M.set t0 (nameTy, ((((name0, p), arity), ord) :: [])) ienv)

(** val compute_ind_env : ctor_env -> n_ind_env **)

let compute_ind_env cenv =
  M.fold update_ind_env cenv M.empty

type ctor_rep =
| Coq_enum of coq_N
| Coq_boxed of coq_N * coq_N

(** val make_ctor_rep : ctor_env -> ctor_tag -> ctor_rep error **)

let make_ctor_rep cenv ct =
  match M.get ct cenv with
  | Some p ->
    if N.eqb p.ctor_arity N0
    then ret (Obj.magic coq_MonadError) (Coq_enum p.ctor_ordinal)
    else ret (Obj.magic coq_MonadError) (Coq_boxed (p.ctor_ordinal,
           p.ctor_arity))
  | None ->
    Err
      (String.append (String.String (Coq_x6d, (String.String (Coq_x61,
        (String.String (Coq_x6b, (String.String (Coq_x65, (String.String
        (Coq_x5f, (String.String (Coq_x63, (String.String (Coq_x74,
        (String.String (Coq_x6f, (String.String (Coq_x72, (String.String
        (Coq_x5f, (String.String (Coq_x72, (String.String (Coq_x65,
        (String.String (Coq_x70, (String.String (Coq_x3a, (String.String
        (Coq_x20, (String.String (Coq_x75, (String.String (Coq_x6e,
        (String.String (Coq_x6b, (String.String (Coq_x6e, (String.String
        (Coq_x6f, (String.String (Coq_x77, (String.String (Coq_x6e,
        (String.String (Coq_x20, (String.String (Coq_x63, (String.String
        (Coq_x6f, (String.String (Coq_x6e, (String.String (Coq_x73,
        (String.String (Coq_x74, (String.String (Coq_x72, (String.String
        (Coq_x75, (String.String (Coq_x63, (String.String (Coq_x74,
        (String.String (Coq_x6f, (String.String (Coq_x72, (String.String
        (Coq_x20, (String.String (Coq_x77, (String.String (Coq_x69,
        (String.String (Coq_x74, (String.String (Coq_x68, (String.String
        (Coq_x20, (String.String (Coq_x74, (String.String (Coq_x61,
        (String.String (Coq_x67, (String.String (Coq_x20,
        String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
        (show_pos ct))

(** val coq_val : coq_type **)

let coq_val =
  talignas (if ptr64 then Npos (Coq_xI Coq_xH) else Npos (Coq_xO Coq_xH))
    (tptr tvoid)

(** val uval : coq_type **)

let uval =
  if ptr64
  then Tlong (Unsigned, { attr_volatile = false; attr_alignas = None })
  else Tint (I32, Unsigned, { attr_volatile = false; attr_alignas = None })

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

(** val floatType : coq_type **)

let floatType =
  Tfloat (F64, noattr)

(** val mkFunTyList : nat -> typelist **)

let rec mkFunTyList = function
| O -> Tnil
| S n' -> Tcons (coq_val, (mkFunTyList n'))

(** val mkFunTy : ident -> nat -> coq_type **)

let mkFunTy threadInfIdent n =
  Tfunction ((Tcons ((Tpointer ((Tstruct (threadInfIdent, noattr)), noattr)),
    (mkFunTyList n))), coq_val, cc_default)

(** val mkPrimTy : nat -> coq_type **)

let mkPrimTy n =
  Tfunction ((mkFunTyList n), coq_val, cc_default)

(** val mkPrimTyTinfo : ident -> nat -> coq_type **)

let mkPrimTyTinfo threadInfIdent n =
  Tfunction ((Tcons ((Tpointer ((Tstruct (threadInfIdent, noattr)), noattr)),
    (mkFunTyList n))), coq_val, cc_default)

(** val add : expr -> expr -> expr **)

let add a b =
  Ebinop (Oadd, a, b, (Tpointer (coq_val, { attr_volatile = false;
    attr_alignas = None })))

(** val sub : expr -> expr -> expr **)

let sub a b =
  Ebinop (Osub, a, b, (Tpointer (coq_val, { attr_volatile = false;
    attr_alignas = None })))

(** val not : expr -> expr **)

let not a =
  Eunop (Onotbool, a, type_bool)

(** val c_int' : coq_Z -> coq_type -> expr **)

let c_int' n t0 =
  if ptr64
  then Econst_long ((Integers.Int64.repr n), t0)
  else Econst_int ((Integers.Int.repr n), t0)

(** val stackframeT : ident -> coq_type **)

let stackframeT stackframeTIdent =
  Tstruct (stackframeTIdent, noattr)

(** val stackframeTPtr : ident -> coq_type **)

let stackframeTPtr stackframeTIdent =
  Tpointer ((stackframeT stackframeTIdent), noattr)

(** val rootT : coq_Z -> coq_type **)

let rootT size =
  Tarray (coq_val, size, noattr)

(** val rootTPtr : coq_type **)

let rootTPtr =
  Tpointer (coq_val, { attr_volatile = false; attr_alignas = None })

(** val stack_decl :
    ident -> ident -> ident -> coq_Z -> (ident * coq_type) list **)

let stack_decl stackframeTIdent frameIdent rootIdent size =
  (frameIdent, (stackframeT stackframeTIdent)) :: ((rootIdent,
    (rootT size)) :: [])

(** val init_stack :
    ident -> ident -> ident -> ident -> ident -> ident -> ident -> ident ->
    ident -> statement **)

let init_stack threadInfIdent tinfIdent stackframeTIdent frameIdent rootIdent fpIdent nextFld rootFld prevFld =
  Ssequence ((Ssequence ((Sassign ((Efield ((Evar (frameIdent,
    (stackframeT stackframeTIdent))), nextFld, (Tpointer (coq_val,
    { attr_volatile = false; attr_alignas = None })))), (Evar (rootIdent,
    rootTPtr)))), (Sassign ((Efield ((Evar (frameIdent,
    (stackframeT stackframeTIdent))), rootFld, rootTPtr)), (Evar (rootIdent,
    rootTPtr)))))), (Sassign ((Efield ((Evar (frameIdent,
    (stackframeT stackframeTIdent))), prevFld,
    (stackframeTPtr stackframeTIdent))), (Efield ((Ederef ((Etempvar
    (tinfIdent, (Tpointer ((Tstruct (threadInfIdent, noattr)), noattr)))),
    (Tstruct (threadInfIdent, noattr)))), fpIdent,
    (stackframeTPtr stackframeTIdent))))))

(** val set_stack :
    ident -> ident -> ident -> ident -> ident -> coq_N -> bool -> statement **)

let set_stack threadInfIdent tinfIdent stackframeTIdent frameIdent fpIdent sp b =
  if (||) (N.eqb sp N0) b
  then Sskip
  else Sassign ((Efield ((Ederef ((Etempvar (tinfIdent, (Tpointer ((Tstruct
         (threadInfIdent, noattr)), noattr)))), (Tstruct (threadInfIdent,
         noattr)))), fpIdent, (stackframeTPtr stackframeTIdent))), (Eaddrof
         ((Evar (frameIdent, (stackframeT stackframeTIdent))),
         (stackframeTPtr stackframeTIdent))))

(** val update_stack :
    ident -> ident -> ident -> ident -> coq_N -> statement **)

let update_stack stackframeTIdent frameIdent rootIdent nextFld sp =
  if N.eqb sp N0
  then Sskip
  else Sassign ((Efield ((Evar (frameIdent, (stackframeT stackframeTIdent))),
         nextFld, (Tpointer (coq_val, { attr_volatile = false; attr_alignas =
         None })))),
         (add (Evar (rootIdent, (Tpointer (coq_val, { attr_volatile = false;
           attr_alignas = None })))) (c_int' (BinInt.Z.of_N sp) coq_val)))

(** val reset_stack :
    ident -> ident -> ident -> ident -> ident -> ident -> coq_N -> bool ->
    statement **)

let reset_stack threadInfIdent tinfIdent stackframeTIdent frameIdent fpIdent prevFld sp b =
  if (||) (N.eqb sp N0) b
  then Sskip
  else Sassign ((Efield ((Ederef ((Etempvar (tinfIdent, (Tpointer ((Tstruct
         (threadInfIdent, noattr)), noattr)))), (Tstruct (threadInfIdent,
         noattr)))), fpIdent, (stackframeTPtr stackframeTIdent))), (Efield
         ((Evar (frameIdent, (stackframeT stackframeTIdent))), prevFld,
         (Tpointer (coq_val, { attr_volatile = false; attr_alignas =
         None })))))

(** val push_var : ident -> coq_N -> positive -> statement **)

let push_var rootIdent sp x =
  Sassign ((Ederef
    ((add (Evar (rootIdent, (Tpointer (coq_val, { attr_volatile = false;
       attr_alignas = None })))) (c_int' (BinInt.Z.of_N sp) coq_val)),
    coq_val)), (Etempvar (x, (Tpointer (coq_val, { attr_volatile = false;
    attr_alignas = None })))))

(** val pop_var : ident -> coq_N -> positive -> statement **)

let pop_var rootIdent sp x =
  Sset (x, (Ederef
    ((add (Evar (rootIdent, (Tpointer (coq_val, { attr_volatile = false;
       attr_alignas = None })))) (c_int' (BinInt.Z.of_N sp) coq_val)),
    coq_val)))

(** val push_live_vars_offset :
    ident -> coq_N -> positive list -> statement * coq_N **)

let push_live_vars_offset rootIdent off xs =
  let rec aux xs0 n stmt =
    match xs0 with
    | [] -> (stmt, n)
    | x :: xs1 ->
      aux xs1 (N.add n (Npos Coq_xH)) (Ssequence ((push_var rootIdent n x),
        stmt))
  in aux xs off Sskip

(** val pop_live_vars_offset :
    ident -> coq_N -> positive list -> statement **)

let pop_live_vars_offset rootIdent off xs =
  let rec aux xs0 n stmt =
    match xs0 with
    | [] -> stmt
    | x :: xs1 ->
      aux xs1 (N.add n (Npos Coq_xH)) (Ssequence ((pop_var rootIdent n x),
        stmt))
  in aux xs off Sskip

(** val push_live_vars : ident -> positive list -> statement * coq_N **)

let push_live_vars rootIdent xs =
  push_live_vars_offset rootIdent N0 xs

(** val pop_live_vars : ident -> positive list -> statement **)

let pop_live_vars rootIdent xs =
  pop_live_vars_offset rootIdent N0 xs

(** val makeTagZ : ctor_env -> ctor_tag -> coq_Z error **)

let makeTagZ cenv ct =
  bind (Obj.magic coq_MonadError) (Obj.magic make_ctor_rep cenv ct) (fun p ->
    match p with
    | Coq_enum t0 ->
      ret (Obj.magic coq_MonadError)
        (BinInt.Z.add (BinInt.Z.shiftl (BinInt.Z.of_N t0) (Zpos Coq_xH))
          (Zpos Coq_xH))
    | Coq_boxed (t0, a) ->
      ret (Obj.magic coq_MonadError)
        (BinInt.Z.add
          (BinInt.Z.shiftl (BinInt.Z.of_N a) (Zpos (Coq_xO (Coq_xI (Coq_xO
            Coq_xH))))) (BinInt.Z.of_N t0)))

(** val makeTag : ctor_env -> ctor_tag -> expr error **)

let makeTag cenv ct =
  bind (Obj.magic coq_MonadError) (Obj.magic makeTagZ cenv ct) (fun t0 ->
    ret (Obj.magic coq_MonadError) (c_int' t0 coq_val))

(** val mkFunVar : ident -> nat -> ident -> coq_N list -> expr **)

let mkFunVar threadInfIdent nParam x locs =
  Evar (x, (mkFunTy threadInfIdent (Datatypes.length (firstn nParam locs))))

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
     ((add (Ecast ((Etempvar (x, coq_val)), (Tpointer (coq_val,
        { attr_volatile = false; attr_alignas = None }))))
        (c_int' (BinInt.Z.of_nat cur) coq_val)), coq_val)), vv)
   | _ :: _ ->
     let vv = makeVar threadInfIdent nParam v fenv map0 in
     let prog =
       assignConstructorS' threadInfIdent nParam fenv map0 x
         (Nat0.add cur (S O)) vs'
     in
     Ssequence ((Sassign ((Ederef
     ((add (Ecast ((Etempvar (x, coq_val)), (Tpointer (coq_val,
        { attr_volatile = false; attr_alignas = None }))))
        (c_int' (BinInt.Z.of_nat cur) coq_val)), coq_val)), vv)), prog))

(** val assignConstructorS :
    ident -> ident -> nat -> ctor_env -> n_ind_env -> fun_env -> fun_info_env
    -> positive -> ctor_tag -> positive list -> statement error **)

let assignConstructorS allocIdent threadInfIdent nParam cenv _ fenv map0 x t0 vs =
  bind (Obj.magic coq_MonadError) (Obj.magic makeTag cenv t0) (fun tag ->
    bind (Obj.magic coq_MonadError) (Obj.magic make_ctor_rep cenv t0)
      (fun rep ->
      match rep with
      | Coq_enum _ -> ret (Obj.magic coq_MonadError) (Sset (x, tag))
      | Coq_boxed (_, a) ->
        let stm = assignConstructorS' threadInfIdent nParam fenv map0 x O vs
        in
        ret (Obj.magic coq_MonadError) (Ssequence ((Ssequence ((Ssequence
          ((Sset (x, (Ecast
          ((add (Etempvar (allocIdent, (Tpointer (coq_val, { attr_volatile =
             false; attr_alignas = None })))) (c_int' BinInt.Z.one coq_val)),
          coq_val)))), (Sset (allocIdent,
          (add (Etempvar (allocIdent, (Tpointer (coq_val, { attr_volatile =
            false; attr_alignas = None }))))
            (c_int' (BinInt.Z.of_N (N.add a (Npos Coq_xH))) coq_val)))))),
          (Sassign ((Ederef
          ((add (Ecast ((Etempvar (x, coq_val)), (Tpointer (coq_val,
             { attr_volatile = false; attr_alignas = None }))))
             (c_int' (Zneg Coq_xH) coq_val)), coq_val)), tag)))), stm))))

(** val isPtr : positive -> positive -> expr **)

let isPtr _ v =
  Ebinop (Oeq, (Ebinop (Oand, (Etempvar (v, coq_val)), (Econst_int
    (Integers.Int.one, (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Tint (IBool, Unsigned, noattr)))),
    (Econst_int (Integers.Int.zero, (Tint (I32, Signed, { attr_volatile =
    false; attr_alignas = None })))), (Tint (IBool, Unsigned, noattr)))

(** val mkCallVars :
    ident -> nat -> fun_env -> fun_info_env -> nat -> positive list -> expr
    list error **)

let rec mkCallVars threadInfIdent nParam fenv map0 n vs =
  match n with
  | O ->
    (match vs with
     | [] -> ret (Obj.magic coq_MonadError) []
     | _ :: _ ->
       Err (String.String (Coq_x6d, (String.String (Coq_x6b, (String.String
         (Coq_x43, (String.String (Coq_x61, (String.String (Coq_x6c,
         (String.String (Coq_x6c, (String.String (Coq_x56, (String.String
         (Coq_x61, (String.String (Coq_x72, (String.String (Coq_x73,
         String.EmptyString)))))))))))))))))))))
  | S n0 ->
    (match vs with
     | [] ->
       Err (String.String (Coq_x6d, (String.String (Coq_x6b, (String.String
         (Coq_x43, (String.String (Coq_x61, (String.String (Coq_x6c,
         (String.String (Coq_x6c, (String.String (Coq_x56, (String.String
         (Coq_x61, (String.String (Coq_x72, (String.String (Coq_x73,
         String.EmptyString))))))))))))))))))))
     | v :: vs' ->
       let vv = makeVar threadInfIdent nParam v fenv map0 in
       bind (Obj.magic coq_MonadError)
         (mkCallVars threadInfIdent nParam fenv map0 n0 vs') (fun rest ->
         ret (Obj.magic coq_MonadError) (vv :: rest)))

(** val mkCall :
    ident -> ident -> nat -> positive option -> fun_env -> fun_info_env ->
    expr -> nat -> positive list -> statement error **)

let mkCall threadInfIdent tinfIdent nParam loc fenv map0 f n vs =
  bind (Obj.magic coq_MonadError)
    (Obj.magic mkCallVars threadInfIdent nParam fenv map0 n
      (firstn nParam vs)) (fun v ->
    ret (Obj.magic coq_MonadError) (Scall (loc, f, ((Etempvar (tinfIdent,
      (Tpointer ((Tstruct (threadInfIdent, noattr)), noattr)))) :: v))))

(** val mkPrimCall :
    ident -> nat -> positive -> positive -> nat -> fun_env -> fun_info_env ->
    positive list -> statement error **)

let mkPrimCall threadInfIdent nParam res pr ar fenv map0 vs =
  bind (Obj.magic coq_MonadError)
    (Obj.magic mkCallVars threadInfIdent nParam fenv map0 ar vs) (fun args ->
    ret (Obj.magic coq_MonadError) (Scall ((Some res), (Ecast ((Evar (pr,
      (mkPrimTy ar))), (mkPrimTy ar))), args)))

(** val mkPrimCallTinfo :
    ident -> ident -> nat -> positive -> positive -> nat -> fun_env ->
    fun_info_env -> positive list -> statement error **)

let mkPrimCallTinfo threadInfIdent tinfIdent nParam res pr ar fenv map0 vs =
  bind (Obj.magic coq_MonadError)
    (Obj.magic mkCallVars threadInfIdent nParam fenv map0 ar vs) (fun args ->
    ret (Obj.magic coq_MonadError) (Scall ((Some res), (Ecast ((Evar (pr,
      (mkPrimTyTinfo threadInfIdent ar))),
      (mkPrimTyTinfo threadInfIdent ar))), ((Etempvar (tinfIdent, (Tpointer
      ((Tstruct (threadInfIdent, noattr)), noattr)))) :: args))))

(** val asgnFunVars' :
    ident -> positive list -> coq_N list -> statement error **)

let rec asgnFunVars' argsIdent vs ind =
  match vs with
  | [] ->
    (match ind with
     | [] -> ret (Obj.magic coq_MonadError) Sskip
     | _ :: _ ->
       Err (String.String (Coq_x61, (String.String (Coq_x73, (String.String
         (Coq_x67, (String.String (Coq_x6e, (String.String (Coq_x46,
         (String.String (Coq_x75, (String.String (Coq_x6e, (String.String
         (Coq_x56, (String.String (Coq_x61, (String.String (Coq_x72,
         (String.String (Coq_x73, (String.String (Coq_x27, (String.String
         (Coq_x3a, (String.String (Coq_x20, (String.String (Coq_x6e,
         (String.String (Coq_x69, (String.String (Coq_x6c, (String.String
         (Coq_x6c, (String.String (Coq_x20, (String.String (Coq_x65,
         (String.String (Coq_x78, (String.String (Coq_x70, (String.String
         (Coq_x65, (String.String (Coq_x63, (String.String (Coq_x74,
         (String.String (Coq_x65, (String.String (Coq_x64,
         String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | v :: vs' ->
    (match ind with
     | [] ->
       Err (String.String (Coq_x61, (String.String (Coq_x73, (String.String
         (Coq_x67, (String.String (Coq_x6e, (String.String (Coq_x46,
         (String.String (Coq_x75, (String.String (Coq_x6e, (String.String
         (Coq_x56, (String.String (Coq_x61, (String.String (Coq_x72,
         (String.String (Coq_x73, (String.String (Coq_x27, (String.String
         (Coq_x3a, (String.String (Coq_x20, (String.String (Coq_x63,
         (String.String (Coq_x6f, (String.String (Coq_x6e, (String.String
         (Coq_x73, (String.String (Coq_x20, (String.String (Coq_x65,
         (String.String (Coq_x78, (String.String (Coq_x70, (String.String
         (Coq_x65, (String.String (Coq_x63, (String.String (Coq_x74,
         (String.String (Coq_x65, (String.String (Coq_x64,
         String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))
     | i :: ind' ->
       bind (Obj.magic coq_MonadError) (asgnFunVars' argsIdent vs' ind')
         (fun rest ->
         ret (Obj.magic coq_MonadError) (Ssequence ((Sset (v, (Ederef
           ((add (Etempvar (argsIdent, (Tpointer (coq_val, { attr_volatile =
              false; attr_alignas = None }))))
              (c_int' (BinInt.Z.of_N i) coq_val)), coq_val)))), rest))))

(** val asgnFunVars :
    ident -> nat -> positive list -> coq_N list -> statement error **)

let asgnFunVars argsIdent nParam vs ind =
  asgnFunVars' argsIdent (skipn nParam vs) (skipn nParam ind)

(** val asgnAppVars'' :
    ident -> ident -> nat -> positive list -> coq_N list -> fun_env ->
    fun_info_env -> String.t -> statement error **)

let rec asgnAppVars'' argsIdent threadInfIdent nParam vs ind fenv map0 name0 =
  match vs with
  | [] ->
    (match ind with
     | [] -> ret (Obj.magic coq_MonadError) Sskip
     | _ :: _ ->
       Err
         (String.append (String.String (Coq_x61, (String.String (Coq_x73,
           (String.String (Coq_x67, (String.String (Coq_x6e, (String.String
           (Coq_x41, (String.String (Coq_x70, (String.String (Coq_x70,
           (String.String (Coq_x56, (String.String (Coq_x61, (String.String
           (Coq_x72, (String.String (Coq_x73, (String.String (Coq_x27,
           (String.String (Coq_x27, (String.String (Coq_x20,
           String.EmptyString)))))))))))))))))))))))))))) name0))
  | v :: vs' ->
    (match ind with
     | [] ->
       Err
         (String.append (String.String (Coq_x61, (String.String (Coq_x73,
           (String.String (Coq_x67, (String.String (Coq_x6e, (String.String
           (Coq_x41, (String.String (Coq_x70, (String.String (Coq_x70,
           (String.String (Coq_x56, (String.String (Coq_x61, (String.String
           (Coq_x72, (String.String (Coq_x73, (String.String (Coq_x27,
           (String.String (Coq_x27, (String.String (Coq_x20,
           String.EmptyString)))))))))))))))))))))))))))) name0)
     | i :: ind' ->
       let s_iv = Sassign ((Ederef
         ((add (Etempvar (argsIdent, (Tpointer (coq_val, { attr_volatile =
            false; attr_alignas = None }))))
            (c_int' (BinInt.Z.of_N i) coq_val)), coq_val)),
         (makeVar threadInfIdent nParam v fenv map0))
       in
       bind (Obj.magic coq_MonadError)
         (asgnAppVars'' argsIdent threadInfIdent nParam vs' ind' fenv map0
           name0) (fun rest ->
         ret (Obj.magic coq_MonadError) (Ssequence (rest, s_iv))))

(** val asgnAppVars' :
    ident -> ident -> nat -> positive list -> coq_N list -> fun_env ->
    fun_info_env -> String.t -> statement error **)

let asgnAppVars' argsIdent threadInfIdent nParam vs ind fenv map0 name0 =
  asgnAppVars'' argsIdent threadInfIdent nParam (skipn nParam vs)
    (skipn nParam ind) fenv map0 name0

(** val get_ind : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 -> nat error **)

let rec get_ind aeq l a =
  match l with
  | [] ->
    Err (String.String (Coq_x67, (String.String (Coq_x65, (String.String
      (Coq_x74, (String.String (Coq_x5f, (String.String (Coq_x69,
      (String.String (Coq_x6e, (String.String (Coq_x64, (String.String
      (Coq_x3a, (String.String (Coq_x20, (String.String (Coq_x63,
      (String.String (Coq_x6f, (String.String (Coq_x6e, (String.String
      (Coq_x73, (String.String (Coq_x20, (String.String (Coq_x65,
      (String.String (Coq_x78, (String.String (Coq_x70, (String.String
      (Coq_x65, (String.String (Coq_x63, (String.String (Coq_x74,
      (String.String (Coq_x65, (String.String (Coq_x64,
      String.EmptyString))))))))))))))))))))))))))))))))))))))))))))
  | x :: l' ->
    if aeq a x
    then ret (Obj.magic coq_MonadError) O
    else bind (Obj.magic coq_MonadError) (get_ind aeq l' a) (fun n ->
           ret (Obj.magic coq_MonadError) (S n))

(** val remove_AppVars :
    positive list -> positive list -> coq_N list -> coq_N list -> (positive
    list * coq_N list) error **)

let rec remove_AppVars myvs vs myind ind =
  match vs with
  | [] ->
    (match ind with
     | [] -> ret (Obj.magic coq_MonadError) ([], [])
     | _ :: _ ->
       Err (String.String (Coq_x72, (String.String (Coq_x65, (String.String
         (Coq_x6d, (String.String (Coq_x6f, (String.String (Coq_x76,
         (String.String (Coq_x65, (String.String (Coq_x5f, (String.String
         (Coq_x41, (String.String (Coq_x70, (String.String (Coq_x70,
         (String.String (Coq_x56, (String.String (Coq_x61, (String.String
         (Coq_x72, (String.String (Coq_x73,
         String.EmptyString)))))))))))))))))))))))))))))
  | v :: vs0 ->
    (match ind with
     | [] ->
       Err (String.String (Coq_x72, (String.String (Coq_x65, (String.String
         (Coq_x6d, (String.String (Coq_x6f, (String.String (Coq_x76,
         (String.String (Coq_x65, (String.String (Coq_x5f, (String.String
         (Coq_x41, (String.String (Coq_x70, (String.String (Coq_x70,
         (String.String (Coq_x56, (String.String (Coq_x61, (String.String
         (Coq_x72, (String.String (Coq_x73,
         String.EmptyString))))))))))))))))))))))))))))
     | i :: ind0 ->
       bind (Obj.magic coq_MonadError) (remove_AppVars myvs vs0 myind ind0)
         (fun x ->
         let (vs', ind') = x in
         bind (Obj.magic coq_MonadError) (Obj.magic get_ind Pos.eqb myvs v)
           (fun n ->
           match nth_error myind n with
           | Some i' ->
             if N.eqb i i'
             then ret (Obj.magic coq_MonadError) (vs', ind')
             else ret (Obj.magic coq_MonadError) ((v :: vs'), (i :: ind'))
           | None -> ret (Obj.magic coq_MonadError) ((v :: vs'), (i :: ind')))))

(** val asgnAppVars_fast' :
    ident -> ident -> nat -> positive list -> positive list -> coq_N list ->
    coq_N list -> fun_env -> fun_info_env -> String.t -> statement error **)

let asgnAppVars_fast' argsIdent threadInfIdent nParam myvs vs myind ind fenv map0 name0 =
  bind (Obj.magic coq_MonadError)
    (Obj.magic remove_AppVars myvs (skipn nParam vs) myind (skipn nParam ind))
    (fun x ->
    let (vs', ind') = x in
    asgnAppVars'' argsIdent threadInfIdent nParam vs' ind' fenv map0 name0)

(** val asgnAppVars :
    ident -> ident -> ident -> nat -> positive list -> coq_N list -> fun_env
    -> fun_info_env -> String.t -> statement error **)

let asgnAppVars argsIdent threadInfIdent tinfIdent nParam vs ind fenv map0 name0 =
  bind (Obj.magic coq_MonadError)
    (asgnAppVars' argsIdent threadInfIdent nParam vs ind fenv map0 name0)
    (fun s ->
    ret (Obj.magic coq_MonadError) (Ssequence ((Sset (argsIdent, (Efield
      ((Ederef ((Etempvar (tinfIdent, (Tpointer ((Tstruct (threadInfIdent,
      noattr)), noattr)))), (Tstruct (threadInfIdent, noattr)))), argsIdent,
      (Tarray (uval, maxArgs, noattr)))))), s)))

(** val asgnAppVars_fast :
    ident -> ident -> ident -> nat -> positive list -> positive list -> coq_N
    list -> coq_N list -> fun_env -> fun_info_env -> String.t -> statement
    error **)

let asgnAppVars_fast argsIdent threadInfIdent tinfIdent nParam myvs vs myind ind fenv map0 name0 =
  bind (Obj.magic coq_MonadError)
    (asgnAppVars_fast' argsIdent threadInfIdent nParam myvs vs myind ind fenv
      map0 name0) (fun s ->
    ret (Obj.magic coq_MonadError) (Ssequence ((Sset (argsIdent, (Efield
      ((Ederef ((Etempvar (tinfIdent, (Tpointer ((Tstruct (threadInfIdent,
      noattr)), noattr)))), (Tstruct (threadInfIdent, noattr)))), argsIdent,
      (Tarray (uval, maxArgs, noattr)))))), s)))

(** val set_nalloc : ident -> ident -> ident -> expr -> statement **)

let set_nalloc nallocIdent threadInfIdent tinfIdent num =
  Sassign ((Efield ((Ederef ((Etempvar (tinfIdent, (Tpointer ((Tstruct
    (threadInfIdent, noattr)), noattr)))), (Tstruct (threadInfIdent,
    noattr)))), nallocIdent, coq_val)), num)

(** val make_GC_call :
    ident -> ident -> ident -> ident -> ident -> ident -> ident -> ident ->
    ident -> ident -> ident -> ident -> nat -> positive list -> coq_N ->
    statement * coq_N **)

let make_GC_call allocIdent nallocIdent limitIdent gcIdent threadInfIdent tinfIdent stackframeTIdent frameIdent rootIdent fpIdent nextFld prevFld num_allocs stack_vars stack_offset =
  let after_call = negb (N.eqb stack_offset N0) in
  let (push, slots) = push_live_vars_offset rootIdent stack_offset stack_vars
  in
  let make_gc_stack = Ssequence ((Ssequence (push,
    (update_stack stackframeTIdent frameIdent rootIdent nextFld slots))),
    (set_stack threadInfIdent tinfIdent stackframeTIdent frameIdent fpIdent
      slots after_call))
  in
  let discard_stack = Ssequence
    ((pop_live_vars_offset rootIdent stack_offset stack_vars),
    (reset_stack threadInfIdent tinfIdent stackframeTIdent frameIdent fpIdent
      prevFld slots after_call))
  in
  let nallocs = c_int' (BinInt.Z.of_nat num_allocs) coq_val in
  if Nat.eqb num_allocs O
  then (Sskip, stack_offset)
  else ((Sifthenelse
         ((not (Ebinop (Ole, nallocs,
            (sub (Etempvar (limitIdent, (Tpointer (coq_val, { attr_volatile =
              false; attr_alignas = None })))) (Etempvar (allocIdent,
              (Tpointer (coq_val, { attr_volatile = false; attr_alignas =
              None }))))), type_bool))), (Ssequence ((Ssequence ((Ssequence
         ((Ssequence ((Ssequence (make_gc_stack,
         (set_nalloc nallocIdent threadInfIdent tinfIdent nallocs))), (Scall
         (None, (Evar (gcIdent, (Tfunction ((Tcons ((Tpointer (coq_val,
         noattr)), (Tcons ((Tpointer ((Tstruct (threadInfIdent, noattr)),
         noattr)), Tnil)))), Tvoid, cc_default)))), ((Etempvar (tinfIdent,
         (Tpointer ((Tstruct (threadInfIdent, noattr)),
         noattr)))) :: []))))), discard_stack)), (Sset (allocIdent, (Efield
         ((Ederef ((Etempvar (tinfIdent, (Tpointer ((Tstruct (threadInfIdent,
         noattr)), noattr)))), (Tstruct (threadInfIdent, noattr)))),
         allocIdent, (Tpointer (coq_val, { attr_volatile = false;
         attr_alignas = None })))))))), (Sset (limitIdent, (Efield ((Ederef
         ((Etempvar (tinfIdent, (Tpointer ((Tstruct (threadInfIdent,
         noattr)), noattr)))), (Tstruct (threadInfIdent, noattr)))),
         limitIdent, (Tpointer (coq_val, { attr_volatile = false;
         attr_alignas = None })))))))), Sskip)), slots)

(** val make_case_switch :
    ident -> positive -> labeled_statements -> labeled_statements -> statement **)

let make_case_switch caseIdent x ls ls' =
  Sifthenelse ((isPtr caseIdent x), (Sswitch ((Ebinop (Oand, (Ederef
    ((add (Ecast ((Etempvar (x, coq_val)), (Tpointer (coq_val,
       { attr_volatile = false; attr_alignas = None }))))
       (c_int' (Zneg Coq_xH) coq_val)), coq_val)),
    (make_cint (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
      Coq_xH)))))))) coq_val), coq_val)), ls)), (Sswitch ((Ebinop (Oshr,
    (Etempvar (x, coq_val)), (make_cint (Zpos Coq_xH) coq_val), coq_val)),
    ls')))

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
    ident -> ident -> ident -> ident -> ident -> ident -> ident -> String.t
    -> ident -> ident -> ident -> ident -> ident -> ident -> ident -> nat ->
    prim_env -> ident -> ident -> ident -> ident -> ident -> ident -> ident
    -> Float64.t -> float **)

let to_float _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ f =
  coq_FF2B (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))) (Zpos
    (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO Coq_xH))))))))))) (model_to_ff (float64_to_model f))

(** val compile_float :
    ident -> ctor_env -> n_ind_env -> fun_env -> fun_info_env -> positive ->
    float -> statement **)

let compile_float allocIdent _ _ _ _ x f =
  let tag =
    c_int' (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
      (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))))))) (Tlong (Unsigned, noattr))
  in
  Ssequence ((Ssequence ((Ssequence ((Sset (x, (Ecast
  ((add (Etempvar (allocIdent, (Tpointer (coq_val, { attr_volatile = false;
     attr_alignas = None })))) (c_int' BinInt.Z.one coq_val)), coq_val)))),
  (Sset (allocIdent,
  (add (Etempvar (allocIdent, (Tpointer (coq_val, { attr_volatile = false;
    attr_alignas = None })))) (c_int' (Zpos (Coq_xO Coq_xH)) coq_val)))))),
  (Sassign ((Ederef
  ((add (Ecast ((Etempvar (x, coq_val)), (Tpointer (coq_val,
     { attr_volatile = false; attr_alignas = None }))))
     (c_int' (Zneg Coq_xH) coq_val)), coq_val)), tag)))), (Sassign ((Ederef
  ((Ecast ((Etempvar (x, coq_val)), (Tpointer (floatType, { attr_volatile =
  false; attr_alignas = None })))), coq_val)), (Econst_float (f,
  floatType)))))

(** val compile_primitive :
    ident -> ident -> ident -> ident -> ident -> ident -> ident -> String.t
    -> ident -> ident -> ident -> ident -> ident -> ident -> ident -> nat ->
    prim_env -> ident -> ident -> ident -> ident -> ident -> ident -> ident
    -> ctor_env -> n_ind_env -> fun_env -> fun_info_env -> positive ->
    primitive -> statement **)

let compile_primitive argsIdent allocIdent nallocIdent limitIdent gcIdent mainIdent bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent numArgsIdent isptrIdent caseIdent resultIdent nParam prims stackframeTIdent frameIdent rootIdent fpIdent nextFld rootFld prevFld cenv ienv fenv map0 x p =
  let i = projT2 p in
  (match projT1 p with
   | Coq_primInt ->
     Sset (x, (Econst_long ((to_int64 (Obj.magic i)), (Tlong (Unsigned,
       noattr)))))
   | Coq_primFloat ->
     compile_float allocIdent cenv ienv fenv map0 x
       (to_float argsIdent allocIdent nallocIdent limitIdent gcIdent
         mainIdent bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent
         numArgsIdent isptrIdent caseIdent resultIdent nParam prims
         stackframeTIdent frameIdent rootIdent fpIdent nextFld rootFld
         prevFld (Obj.magic i)))

(** val translate_body :
    ident -> ident -> ident -> ident -> ident -> ident -> ident -> String.t
    -> ident -> ident -> ident -> ident -> ident -> ident -> ident -> nat ->
    prim_env -> ident -> ident -> ident -> ident -> ident -> ident -> ident
    -> bool -> positive list -> coq_FVSet -> coq_N list -> name_env -> exp ->
    fun_env -> ctor_env -> n_ind_env -> fun_info_env -> coq_N ->
    (statement * coq_N) error **)

let rec translate_body argsIdent allocIdent nallocIdent limitIdent gcIdent mainIdent bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent numArgsIdent isptrIdent caseIdent resultIdent nParam prims stackframeTIdent frameIdent rootIdent fpIdent nextFld rootFld prevFld args_opt fun_vars loc_vars locs nenv e fenv cenv ienv map0 slots =
  match e with
  | Econstr (x, t0, vs, e') ->
    bind (Obj.magic coq_MonadError)
      (Obj.magic assignConstructorS allocIdent threadInfIdent nParam cenv
        ienv fenv map0 x t0 vs) (fun prog ->
      bind (Obj.magic coq_MonadError)
        (translate_body argsIdent allocIdent nallocIdent limitIdent gcIdent
          mainIdent bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent
          numArgsIdent isptrIdent caseIdent resultIdent nParam prims
          stackframeTIdent frameIdent rootIdent fpIdent nextFld rootFld
          prevFld args_opt fun_vars loc_vars locs nenv e' fenv cenv ienv map0
          slots) (fun progn ->
        ret (Obj.magic coq_MonadError) ((Ssequence (prog, (fst progn))),
          (snd progn))))
  | Ecase (x, cs) ->
    bind (Obj.magic coq_MonadError)
      (let rec makeCases = function
       | [] -> ret (Obj.magic coq_MonadError) ((LSnil, LSnil), slots)
       | p :: l' ->
         bind (Obj.magic coq_MonadError)
           (translate_body argsIdent allocIdent nallocIdent limitIdent
             gcIdent mainIdent bodyIdent bodyName threadInfIdent tinfIdent
             heapInfIdent numArgsIdent isptrIdent caseIdent resultIdent
             nParam prims stackframeTIdent frameIdent rootIdent fpIdent
             nextFld rootFld prevFld args_opt fun_vars loc_vars locs nenv
             (snd p) fenv cenv ienv map0 slots) (fun progn ->
           bind (Obj.magic coq_MonadError) (makeCases l') (fun pn ->
             let (prog, n) = progn in
             let (p0, n') = pn in
             let (ls, ls') = p0 in
             bind (Obj.magic coq_MonadError)
               (Obj.magic make_ctor_rep cenv (fst p)) (fun p1 ->
               match p1 with
               | Coq_enum t0 ->
                 let tag =
                   BinInt.Z.add
                     (BinInt.Z.shiftl (BinInt.Z.of_N t0) (Zpos Coq_xH)) (Zpos
                     Coq_xH)
                 in
                 (match ls' with
                  | LSnil ->
                    ret (Obj.magic coq_MonadError) ((ls, (LScons (None,
                      (Ssequence (prog, Sbreak)), ls'))), (N.max n n'))
                  | LScons (_, _, _) ->
                    ret (Obj.magic coq_MonadError) ((ls, (LScons ((Some
                      (BinInt.Z.shiftr tag (Zpos Coq_xH))), (Ssequence (prog,
                      Sbreak)), ls'))), (N.max n n')))
               | Coq_boxed (t0, a) ->
                 let tag =
                   BinInt.Z.add
                     (BinInt.Z.shiftl (BinInt.Z.of_N a) (Zpos (Coq_xO (Coq_xI
                       (Coq_xO Coq_xH))))) (BinInt.Z.of_N t0)
                 in
                 (match ls with
                  | LSnil ->
                    ret (Obj.magic coq_MonadError) (((LScons (None,
                      (Ssequence (prog, Sbreak)), ls)), ls'), (N.max n n'))
                  | LScons (_, _, _) ->
                    ret (Obj.magic coq_MonadError) (((LScons ((Some
                      (BinInt.Z.coq_land tag (Zpos (Coq_xI (Coq_xI (Coq_xI
                        (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))))))),
                      (Ssequence (prog, Sbreak)), ls)), ls'), (N.max n n'))))))
       in makeCases cs) (fun p ->
      let (y, slots') = p in
      let (ls, ls') = y in
      ret (Obj.magic coq_MonadError) ((make_case_switch caseIdent x ls ls'),
        slots'))
  | Eproj (x, _, n, v, e') ->
    bind (Obj.magic coq_MonadError)
      (translate_body argsIdent allocIdent nallocIdent limitIdent gcIdent
        mainIdent bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent
        numArgsIdent isptrIdent caseIdent resultIdent nParam prims
        stackframeTIdent frameIdent rootIdent fpIdent nextFld rootFld prevFld
        args_opt fun_vars loc_vars locs nenv e' fenv cenv ienv map0 slots)
      (fun progn -> Ret ((Ssequence ((Sset (x, (Ederef
      ((add (Ecast ((Etempvar (v, coq_val)), (Tpointer (coq_val,
         { attr_volatile = false; attr_alignas = None }))))
         (c_int' (BinInt.Z.of_N n) coq_val)), coq_val)))), (fst progn))),
      (snd progn)))
  | Eletapp (x, f, t0, vs, e') ->
    let fvs_post_call = PS.inter (exp_fv e') loc_vars in
    let fvs = PS.remove x fvs_post_call in
    let fvs_list = PS.elements fvs in
    let fv_gc = if PS.mem x fvs_post_call then x :: [] else [] in
    let (push, slots0) = push_live_vars rootIdent fvs_list in
    let make_stack = Ssequence ((Ssequence (push,
      (update_stack stackframeTIdent frameIdent rootIdent nextFld slots0))),
      (set_stack threadInfIdent tinfIdent stackframeTIdent frameIdent fpIdent
        slots0 false))
    in
    (match M.get t0 fenv with
     | Some inf ->
       let name0 =
         match M.get f nenv with
         | Some n -> show_name n
         | None ->
           String.String (Coq_x6e, (String.String (Coq_x6f, (String.String
             (Coq_x74, (String.String (Coq_x20, (String.String (Coq_x61,
             (String.String (Coq_x6e, (String.String (Coq_x20, (String.String
             (Coq_x65, (String.String (Coq_x6e, (String.String (Coq_x74,
             (String.String (Coq_x72, (String.String (Coq_x79,
             String.EmptyString)))))))))))))))))))))))
       in
       bind (Obj.magic coq_MonadError)
         (if args_opt
          then Obj.magic asgnAppVars_fast argsIdent threadInfIdent tinfIdent
                 nParam fun_vars vs locs (snd inf) fenv map0 name0
          else Obj.magic asgnAppVars argsIdent threadInfIdent tinfIdent
                 nParam vs (snd inf) fenv map0 name0) (fun asgn ->
         let f_var = makeVar threadInfIdent nParam f fenv map0 in
         let pnum = min (N.to_nat (fst inf)) nParam in
         bind (Obj.magic coq_MonadError)
           (Obj.magic mkCall threadInfIdent tinfIdent nParam (Some x) fenv
             map0 (Ecast (f_var, (Tpointer ((mkFunTy threadInfIdent pnum),
             noattr)))) pnum vs) (fun c ->
           let alloc = max_allocs e' in
           let (gc_call, slots_gc) =
             make_GC_call allocIdent nallocIdent limitIdent gcIdent
               threadInfIdent tinfIdent stackframeTIdent frameIdent rootIdent
               fpIdent nextFld prevFld alloc fv_gc slots0
           in
           let discard_stack = Ssequence ((pop_live_vars rootIdent fvs_list),
             (reset_stack threadInfIdent tinfIdent stackframeTIdent
               frameIdent fpIdent prevFld slots0 false))
           in
           bind (Obj.magic coq_MonadError)
             (translate_body argsIdent allocIdent nallocIdent limitIdent
               gcIdent mainIdent bodyIdent bodyName threadInfIdent tinfIdent
               heapInfIdent numArgsIdent isptrIdent caseIdent resultIdent
               nParam prims stackframeTIdent frameIdent rootIdent fpIdent
               nextFld rootFld prevFld args_opt fun_vars loc_vars locs nenv
               e' fenv cenv ienv map0 (N.max slots slots_gc)) (fun progn ->
             Ret ((Ssequence ((Ssequence ((Ssequence ((Ssequence ((Ssequence
             ((Ssequence ((Ssequence ((Ssequence ((Ssequence (asgn, (Sassign
             ((Efield ((Ederef ((Etempvar (tinfIdent, (Tpointer ((Tstruct
             (threadInfIdent, noattr)), noattr)))), (Tstruct (threadInfIdent,
             noattr)))), allocIdent, (Tpointer (coq_val, { attr_volatile =
             false; attr_alignas = None })))), (Etempvar (allocIdent,
             (Tpointer (coq_val, { attr_volatile = false; attr_alignas =
             None })))))))), (Sassign ((Efield ((Ederef ((Etempvar
             (tinfIdent, (Tpointer ((Tstruct (threadInfIdent, noattr)),
             noattr)))), (Tstruct (threadInfIdent, noattr)))), limitIdent,
             (Tpointer (coq_val, { attr_volatile = false; attr_alignas =
             None })))), (Etempvar (limitIdent, (Tpointer (coq_val,
             { attr_volatile = false; attr_alignas = None })))))))),
             make_stack)), c)), (Sset (allocIdent, (Efield ((Ederef
             ((Etempvar (tinfIdent, (Tpointer ((Tstruct (threadInfIdent,
             noattr)), noattr)))), (Tstruct (threadInfIdent, noattr)))),
             allocIdent, (Tpointer (coq_val, { attr_volatile = false;
             attr_alignas = None })))))))), (Sset (limitIdent, (Efield
             ((Ederef ((Etempvar (tinfIdent, (Tpointer ((Tstruct
             (threadInfIdent, noattr)), noattr)))), (Tstruct (threadInfIdent,
             noattr)))), limitIdent, (Tpointer (coq_val, { attr_volatile =
             false; attr_alignas = None })))))))), gc_call)),
             discard_stack)), (fst progn))), (snd progn)))))
     | None ->
       Err (String.String (Coq_x74, (String.String (Coq_x72, (String.String
         (Coq_x61, (String.String (Coq_x6e, (String.String (Coq_x73,
         (String.String (Coq_x6c, (String.String (Coq_x61, (String.String
         (Coq_x74, (String.String (Coq_x65, (String.String (Coq_x5f,
         (String.String (Coq_x62, (String.String (Coq_x6f, (String.String
         (Coq_x64, (String.String (Coq_x79, (String.String (Coq_x3a,
         (String.String (Coq_x20, (String.String (Coq_x55, (String.String
         (Coq_x6e, (String.String (Coq_x6b, (String.String (Coq_x6e,
         (String.String (Coq_x6f, (String.String (Coq_x77, (String.String
         (Coq_x6e, (String.String (Coq_x20, (String.String (Coq_x66,
         (String.String (Coq_x75, (String.String (Coq_x6e, (String.String
         (Coq_x63, (String.String (Coq_x74, (String.String (Coq_x69,
         (String.String (Coq_x6f, (String.String (Coq_x6e, (String.String
         (Coq_x20, (String.String (Coq_x61, (String.String (Coq_x70,
         (String.String (Coq_x70, (String.String (Coq_x6c, (String.String
         (Coq_x69, (String.String (Coq_x63, (String.String (Coq_x61,
         (String.String (Coq_x74, (String.String (Coq_x69, (String.String
         (Coq_x6f, (String.String (Coq_x6e, (String.String (Coq_x20,
         (String.String (Coq_x69, (String.String (Coq_x6e, (String.String
         (Coq_x20, (String.String (Coq_x45, (String.String (Coq_x6c,
         (String.String (Coq_x65, (String.String (Coq_x74, (String.String
         (Coq_x61, (String.String (Coq_x70, (String.String (Coq_x70,
         String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | Efun (_, _) ->
    Err (String.String (Coq_x74, (String.String (Coq_x72, (String.String
      (Coq_x61, (String.String (Coq_x6e, (String.String (Coq_x73,
      (String.String (Coq_x6c, (String.String (Coq_x61, (String.String
      (Coq_x74, (String.String (Coq_x65, (String.String (Coq_x5f,
      (String.String (Coq_x62, (String.String (Coq_x6f, (String.String
      (Coq_x64, (String.String (Coq_x79, (String.String (Coq_x3a,
      (String.String (Coq_x20, (String.String (Coq_x4e, (String.String
      (Coq_x65, (String.String (Coq_x73, (String.String (Coq_x74,
      (String.String (Coq_x65, (String.String (Coq_x64, (String.String
      (Coq_x20, (String.String (Coq_x66, (String.String (Coq_x75,
      (String.String (Coq_x6e, (String.String (Coq_x63, (String.String
      (Coq_x74, (String.String (Coq_x69, (String.String (Coq_x6f,
      (String.String (Coq_x6e, (String.String (Coq_x20, (String.String
      (Coq_x64, (String.String (Coq_x65, (String.String (Coq_x74,
      (String.String (Coq_x65, (String.String (Coq_x63, (String.String
      (Coq_x74, (String.String (Coq_x65, (String.String (Coq_x64,
      String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | Eapp (x, t0, vs) ->
    (match M.get t0 fenv with
     | Some inf ->
       let name0 =
         match M.get x nenv with
         | Some n -> show_name n
         | None ->
           String.String (Coq_x6e, (String.String (Coq_x6f, (String.String
             (Coq_x74, (String.String (Coq_x20, (String.String (Coq_x61,
             (String.String (Coq_x6e, (String.String (Coq_x20, (String.String
             (Coq_x65, (String.String (Coq_x6e, (String.String (Coq_x74,
             (String.String (Coq_x72, (String.String (Coq_x79,
             String.EmptyString)))))))))))))))))))))))
       in
       bind (Obj.magic coq_MonadError)
         (if args_opt
          then Obj.magic asgnAppVars_fast argsIdent threadInfIdent tinfIdent
                 nParam fun_vars vs locs (snd inf) fenv map0 name0
          else Obj.magic asgnAppVars argsIdent threadInfIdent tinfIdent
                 nParam vs (snd inf) fenv map0 name0) (fun asgn ->
         let f_var = makeVar threadInfIdent nParam x fenv map0 in
         let pnum = min (N.to_nat (fst inf)) nParam in
         bind (Obj.magic coq_MonadError)
           (Obj.magic mkCall threadInfIdent tinfIdent nParam (Some
             resultIdent) fenv map0 (Ecast (f_var, (Tpointer
             ((mkFunTy threadInfIdent pnum), noattr)))) pnum vs) (fun c ->
           ret (Obj.magic coq_MonadError) ((Ssequence ((Ssequence ((Ssequence
             ((Ssequence (asgn, (Sassign ((Efield ((Ederef ((Etempvar
             (tinfIdent, (Tpointer ((Tstruct (threadInfIdent, noattr)),
             noattr)))), (Tstruct (threadInfIdent, noattr)))), allocIdent,
             (Tpointer (coq_val, { attr_volatile = false; attr_alignas =
             None })))), (Etempvar (allocIdent, (Tpointer (coq_val,
             { attr_volatile = false; attr_alignas = None })))))))), (Sassign
             ((Efield ((Ederef ((Etempvar (tinfIdent, (Tpointer ((Tstruct
             (threadInfIdent, noattr)), noattr)))), (Tstruct (threadInfIdent,
             noattr)))), limitIdent, (Tpointer (coq_val, { attr_volatile =
             false; attr_alignas = None })))), (Etempvar (limitIdent,
             (Tpointer (coq_val, { attr_volatile = false; attr_alignas =
             None })))))))), c)), (Sreturn (Some
             (makeVar threadInfIdent nParam resultIdent fenv map0))))), slots)))
     | None ->
       Err (String.String (Coq_x74, (String.String (Coq_x72, (String.String
         (Coq_x61, (String.String (Coq_x6e, (String.String (Coq_x73,
         (String.String (Coq_x6c, (String.String (Coq_x61, (String.String
         (Coq_x74, (String.String (Coq_x65, (String.String (Coq_x5f,
         (String.String (Coq_x62, (String.String (Coq_x6f, (String.String
         (Coq_x64, (String.String (Coq_x79, (String.String (Coq_x3a,
         (String.String (Coq_x20, (String.String (Coq_x55, (String.String
         (Coq_x6e, (String.String (Coq_x6b, (String.String (Coq_x6e,
         (String.String (Coq_x6f, (String.String (Coq_x77, (String.String
         (Coq_x6e, (String.String (Coq_x20, (String.String (Coq_x66,
         (String.String (Coq_x75, (String.String (Coq_x6e, (String.String
         (Coq_x63, (String.String (Coq_x74, (String.String (Coq_x69,
         (String.String (Coq_x6f, (String.String (Coq_x6e, (String.String
         (Coq_x20, (String.String (Coq_x61, (String.String (Coq_x70,
         (String.String (Coq_x70, (String.String (Coq_x6c, (String.String
         (Coq_x69, (String.String (Coq_x63, (String.String (Coq_x61,
         (String.String (Coq_x74, (String.String (Coq_x69, (String.String
         (Coq_x6f, (String.String (Coq_x6e, (String.String (Coq_x20,
         (String.String (Coq_x69, (String.String (Coq_x6e, (String.String
         (Coq_x20, (String.String (Coq_x45, (String.String (Coq_x61,
         (String.String (Coq_x70, (String.String (Coq_x70,
         String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | Eprim_val (x, p, e') ->
    bind (Obj.magic coq_MonadError)
      (translate_body argsIdent allocIdent nallocIdent limitIdent gcIdent
        mainIdent bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent
        numArgsIdent isptrIdent caseIdent resultIdent nParam prims
        stackframeTIdent frameIdent rootIdent fpIdent nextFld rootFld prevFld
        args_opt fun_vars loc_vars locs nenv e' fenv cenv ienv map0 slots)
      (fun progn -> Ret ((Ssequence
      ((compile_primitive argsIdent allocIdent nallocIdent limitIdent gcIdent
         mainIdent bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent
         numArgsIdent isptrIdent caseIdent resultIdent nParam prims
         stackframeTIdent frameIdent rootIdent fpIdent nextFld rootFld
         prevFld cenv ienv fenv map0 x p), (fst progn))), (snd progn)))
  | Eprim (x, p, vs, e') ->
    (match PTree.get p prims with
     | Some p0 ->
       let (p1, _) = p0 in
       let (_, b) = p1 in
       if b
       then let fvs_post_call = PS.inter (exp_fv e') loc_vars in
            let fvs = PS.remove x fvs_post_call in
            let fvs_list = PS.elements fvs in
            let fv_gc = if PS.mem x fvs_post_call then x :: [] else [] in
            let (push, slots0) = push_live_vars rootIdent fvs_list in
            let make_stack = Ssequence ((Ssequence (push,
              (update_stack stackframeTIdent frameIdent rootIdent nextFld
                slots0))),
              (set_stack threadInfIdent tinfIdent stackframeTIdent frameIdent
                fpIdent slots0 false))
            in
            bind (Obj.magic coq_MonadError)
              (Obj.magic mkPrimCallTinfo threadInfIdent tinfIdent nParam x p
                (Datatypes.length vs) fenv map0 vs) (fun c ->
              let alloc = max_allocs e' in
              let (gc_call, slots_gc) =
                make_GC_call allocIdent nallocIdent limitIdent gcIdent
                  threadInfIdent tinfIdent stackframeTIdent frameIdent
                  rootIdent fpIdent nextFld prevFld alloc fv_gc slots0
              in
              let discard_stack = Ssequence
                ((pop_live_vars rootIdent fvs_list),
                (reset_stack threadInfIdent tinfIdent stackframeTIdent
                  frameIdent fpIdent prevFld slots0 false))
              in
              bind (Obj.magic coq_MonadError)
                (translate_body argsIdent allocIdent nallocIdent limitIdent
                  gcIdent mainIdent bodyIdent bodyName threadInfIdent
                  tinfIdent heapInfIdent numArgsIdent isptrIdent caseIdent
                  resultIdent nParam prims stackframeTIdent frameIdent
                  rootIdent fpIdent nextFld rootFld prevFld args_opt fun_vars
                  loc_vars locs nenv e' fenv cenv ienv map0
                  (N.max slots slots_gc)) (fun x0 ->
                let (prog, slots1) = x0 in
                Ret ((Ssequence ((Ssequence ((Ssequence ((Ssequence
                ((Ssequence ((Ssequence ((Ssequence ((Ssequence ((Sassign
                ((Efield ((Ederef ((Etempvar (tinfIdent, (Tpointer ((Tstruct
                (threadInfIdent, noattr)), noattr)))), (Tstruct
                (threadInfIdent, noattr)))), allocIdent, (Tpointer (coq_val,
                { attr_volatile = false; attr_alignas = None })))), (Etempvar
                (allocIdent, (Tpointer (coq_val, { attr_volatile = false;
                attr_alignas = None })))))), (Sassign ((Efield ((Ederef
                ((Etempvar (tinfIdent, (Tpointer ((Tstruct (threadInfIdent,
                noattr)), noattr)))), (Tstruct (threadInfIdent, noattr)))),
                limitIdent, (Tpointer (coq_val, { attr_volatile = false;
                attr_alignas = None })))), (Etempvar (limitIdent, (Tpointer
                (coq_val, { attr_volatile = false; attr_alignas =
                None })))))))), make_stack)), c)), (Sset (allocIdent, (Efield
                ((Ederef ((Etempvar (tinfIdent, (Tpointer ((Tstruct
                (threadInfIdent, noattr)), noattr)))), (Tstruct
                (threadInfIdent, noattr)))), allocIdent, (Tpointer (coq_val,
                { attr_volatile = false; attr_alignas = None })))))))), (Sset
                (limitIdent, (Efield ((Ederef ((Etempvar (tinfIdent,
                (Tpointer ((Tstruct (threadInfIdent, noattr)), noattr)))),
                (Tstruct (threadInfIdent, noattr)))), limitIdent, (Tpointer
                (coq_val, { attr_volatile = false; attr_alignas =
                None })))))))), gc_call)), discard_stack)), prog)), slots1)))
       else bind (Obj.magic coq_MonadError)
              (Obj.magic mkPrimCall threadInfIdent nParam x p
                (Datatypes.length vs) fenv map0 vs) (fun c ->
              bind (Obj.magic coq_MonadError)
                (translate_body argsIdent allocIdent nallocIdent limitIdent
                  gcIdent mainIdent bodyIdent bodyName threadInfIdent
                  tinfIdent heapInfIdent numArgsIdent isptrIdent caseIdent
                  resultIdent nParam prims stackframeTIdent frameIdent
                  rootIdent fpIdent nextFld rootFld prevFld args_opt fun_vars
                  loc_vars locs nenv e' fenv cenv ienv map0 slots) (fun x0 ->
                let (prog, slots0) = x0 in
                ret (Obj.magic coq_MonadError) ((Ssequence (c, prog)), slots0)))
     | None ->
       Err (String.String (Coq_x74, (String.String (Coq_x72, (String.String
         (Coq_x61, (String.String (Coq_x6e, (String.String (Coq_x73,
         (String.String (Coq_x6c, (String.String (Coq_x61, (String.String
         (Coq_x74, (String.String (Coq_x65, (String.String (Coq_x5f,
         (String.String (Coq_x62, (String.String (Coq_x6f, (String.String
         (Coq_x64, (String.String (Coq_x79, (String.String (Coq_x3a,
         (String.String (Coq_x20, (String.String (Coq_x55, (String.String
         (Coq_x6e, (String.String (Coq_x6b, (String.String (Coq_x6e,
         (String.String (Coq_x6f, (String.String (Coq_x77, (String.String
         (Coq_x6e, (String.String (Coq_x20, (String.String (Coq_x70,
         (String.String (Coq_x72, (String.String (Coq_x69, (String.String
         (Coq_x6d, (String.String (Coq_x69, (String.String (Coq_x74,
         (String.String (Coq_x69, (String.String (Coq_x76, (String.String
         (Coq_x65, (String.String (Coq_x20, (String.String (Coq_x69,
         (String.String (Coq_x64, (String.String (Coq_x65, (String.String
         (Coq_x6e, (String.String (Coq_x74, (String.String (Coq_x69,
         (String.String (Coq_x66, (String.String (Coq_x69, (String.String
         (Coq_x65, (String.String (Coq_x72,
         String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | Ehalt x ->
    ret (Obj.magic coq_MonadError) ((Ssequence ((Ssequence ((Sassign ((Efield
      ((Ederef ((Etempvar (tinfIdent, (Tpointer ((Tstruct (threadInfIdent,
      noattr)), noattr)))), (Tstruct (threadInfIdent, noattr)))), allocIdent,
      (Tpointer (coq_val, { attr_volatile = false; attr_alignas = None })))),
      (Etempvar (allocIdent, (Tpointer (coq_val, { attr_volatile = false;
      attr_alignas = None })))))), (Sassign ((Efield ((Ederef ((Etempvar
      (tinfIdent, (Tpointer ((Tstruct (threadInfIdent, noattr)), noattr)))),
      (Tstruct (threadInfIdent, noattr)))), limitIdent, (Tpointer (coq_val,
      { attr_volatile = false; attr_alignas = None })))), (Etempvar
      (limitIdent, (Tpointer (coq_val, { attr_volatile = false;
      attr_alignas = None })))))))), (Sreturn (Some
      (makeVar threadInfIdent nParam x fenv map0))))), slots)

(** val mkFun :
    ident -> ident -> ident -> ident -> ident -> ident -> ident -> nat ->
    ident -> ident -> ident -> coq_Z -> positive list -> positive list ->
    statement -> coq_function **)

let mkFun argsIdent allocIdent limitIdent threadInfIdent tinfIdent caseIdent resultIdent nParam stackframeTIdent frameIdent rootIdent root_size vs loc body =
  { fn_return = coq_val; fn_callconv = cc_default; fn_params = ((tinfIdent,
    (Tpointer ((Tstruct (threadInfIdent, noattr)),
    noattr))) :: (map (fun x -> (x, coq_val)) (firstn nParam vs))); fn_vars =
    (stack_decl stackframeTIdent frameIdent rootIdent root_size); fn_temps =
    (app (map (fun x -> (x, coq_val)) (app (skipn nParam vs) loc))
      ((allocIdent, (Tpointer (coq_val, { attr_volatile = false;
      attr_alignas = None }))) :: ((limitIdent, (Tpointer (coq_val,
      { attr_volatile = false; attr_alignas = None }))) :: ((argsIdent,
      (Tpointer (coq_val, { attr_volatile = false; attr_alignas =
      None }))) :: ((caseIdent, (Tint (IBool, Unsigned,
      noattr))) :: ((resultIdent, coq_val) :: [])))))); fn_body = body }

(** val translate_fundefs :
    ident -> ident -> ident -> ident -> ident -> ident -> ident -> String.t
    -> ident -> ident -> ident -> ident -> ident -> ident -> ident -> nat ->
    prim_env -> ident -> ident -> ident -> ident -> ident -> ident -> ident
    -> bool -> fundefs -> fun_env -> ctor_env -> n_ind_env -> fun_info_env ->
    name_env -> (positive * (Clight.fundef, coq_type) globdef) list error **)

let rec translate_fundefs argsIdent allocIdent nallocIdent limitIdent gcIdent mainIdent bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent numArgsIdent isptrIdent caseIdent resultIdent nParam prims stackframeTIdent frameIdent rootIdent fpIdent nextFld rootFld prevFld args_opt fnd fenv cenv ienv map0 nenv =
  match fnd with
  | Fcons (f, t0, vs, e, fnd') ->
    bind (Obj.magic coq_MonadError)
      (translate_fundefs argsIdent allocIdent nallocIdent limitIdent gcIdent
        mainIdent bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent
        numArgsIdent isptrIdent caseIdent resultIdent nParam prims
        stackframeTIdent frameIdent rootIdent fpIdent nextFld rootFld prevFld
        args_opt fnd' fenv cenv ienv map0 nenv) (fun rest ->
      match M.get t0 fenv with
      | Some inf ->
        let (_, locs) = inf in
        bind (Obj.magic coq_MonadError)
          (Obj.magic asgnFunVars argsIdent nParam vs locs) (fun asgn ->
          let num_allocs = max_allocs e in
          let loc_vars = get_locals e in
          let var_set = union_list PS.empty vs in
          let loc_ids = union_list var_set loc_vars in
          let live_vars = PS.elements (PS.inter (exp_fv e) var_set) in
          let (gc, _) =
            make_GC_call allocIdent nallocIdent limitIdent gcIdent
              threadInfIdent tinfIdent stackframeTIdent frameIdent rootIdent
              fpIdent nextFld prevFld num_allocs live_vars N0
          in
          bind (Obj.magic coq_MonadError)
            (Obj.magic translate_body argsIdent allocIdent nallocIdent
              limitIdent gcIdent mainIdent bodyIdent bodyName threadInfIdent
              tinfIdent heapInfIdent numArgsIdent isptrIdent caseIdent
              resultIdent nParam prims stackframeTIdent frameIdent rootIdent
              fpIdent nextFld rootFld prevFld args_opt vs loc_ids locs nenv e
              fenv cenv ienv map0 N0) (fun x ->
            let (body, stack_slots) = x in
            let stack_slots0 =
              N.max (N.of_nat (Datatypes.length live_vars)) stack_slots
            in
            Ret ((f, (Gfun (Internal
            (mkFun argsIdent allocIdent limitIdent threadInfIdent tinfIdent
              caseIdent resultIdent nParam stackframeTIdent frameIdent
              rootIdent (BinInt.Z.of_N stack_slots0) vs loc_vars (Ssequence
              ((Ssequence ((Ssequence ((Ssequence ((Ssequence ((Ssequence
              ((Sset (allocIdent, (Efield ((Ederef ((Etempvar (tinfIdent,
              (Tpointer ((Tstruct (threadInfIdent, noattr)), noattr)))),
              (Tstruct (threadInfIdent, noattr)))), allocIdent, (Tpointer
              (coq_val, { attr_volatile = false; attr_alignas = None })))))),
              (Sset (limitIdent, (Efield ((Ederef ((Etempvar (tinfIdent,
              (Tpointer ((Tstruct (threadInfIdent, noattr)), noattr)))),
              (Tstruct (threadInfIdent, noattr)))), limitIdent, (Tpointer
              (coq_val, { attr_volatile = false; attr_alignas =
              None })))))))), (Sset (argsIdent, (Efield ((Ederef ((Etempvar
              (tinfIdent, (Tpointer ((Tstruct (threadInfIdent, noattr)),
              noattr)))), (Tstruct (threadInfIdent, noattr)))), argsIdent,
              (Tarray (uval, maxArgs, noattr)))))))), asgn)),
              (init_stack threadInfIdent tinfIdent stackframeTIdent
                frameIdent rootIdent fpIdent nextFld rootFld prevFld))),
              gc)), body)))))) :: rest)))
      | None ->
        Err (String.String (Coq_x74, (String.String (Coq_x72, (String.String
          (Coq_x61, (String.String (Coq_x6e, (String.String (Coq_x73,
          (String.String (Coq_x6c, (String.String (Coq_x61, (String.String
          (Coq_x74, (String.String (Coq_x65, (String.String (Coq_x5f,
          (String.String (Coq_x66, (String.String (Coq_x75, (String.String
          (Coq_x6e, (String.String (Coq_x64, (String.String (Coq_x65,
          (String.String (Coq_x66, (String.String (Coq_x73, (String.String
          (Coq_x3a, (String.String (Coq_x20, (String.String (Coq_x55,
          (String.String (Coq_x6e, (String.String (Coq_x6b, (String.String
          (Coq_x6e, (String.String (Coq_x6f, (String.String (Coq_x77,
          (String.String (Coq_x6e, (String.String (Coq_x20, (String.String
          (Coq_x66, (String.String (Coq_x75, (String.String (Coq_x6e,
          (String.String (Coq_x63, (String.String (Coq_x74, (String.String
          (Coq_x69, (String.String (Coq_x6f, (String.String (Coq_x6e,
          (String.String (Coq_x20, (String.String (Coq_x74, (String.String
          (Coq_x61, (String.String (Coq_x67,
          String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | Fnil -> Ret []

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
  let params =
    type_of_params ((tinfIdent, (Tpointer ((Tstruct (threadInfIdent,
      noattr)), noattr))) :: [])
  in
  (bodyIdent, (Gfun (External ((EF_external ((String.to_string bodyName),
  (signature_of_type params coq_val cc_default))), params, coq_val,
  cc_default))))

(** val translate_program :
    ident -> ident -> ident -> ident -> ident -> ident -> ident -> String.t
    -> ident -> ident -> ident -> ident -> ident -> ident -> ident -> nat ->
    prim_env -> ident -> ident -> ident -> ident -> ident -> ident -> ident
    -> bool -> exp -> fun_env -> ctor_env -> n_ind_env -> fun_info_env ->
    name_env -> (positive * (Clight.fundef, coq_type) globdef) list error **)

let translate_program argsIdent allocIdent nallocIdent limitIdent gcIdent mainIdent bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent numArgsIdent isptrIdent caseIdent resultIdent nParam prims stackframeTIdent frameIdent rootIdent fpIdent nextFld rootFld prevFld args_opt e fenv cenv ienv fmap nenv =
  match e with
  | Efun (fnd, e0) ->
    let localVars = get_locals e0 in
    bind (Obj.magic coq_MonadError)
      (translate_fundefs argsIdent allocIdent nallocIdent limitIdent gcIdent
        mainIdent bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent
        numArgsIdent isptrIdent caseIdent resultIdent nParam prims
        stackframeTIdent frameIdent rootIdent fpIdent nextFld rootFld prevFld
        args_opt fnd fenv cenv ienv fmap nenv) (fun funs ->
      let allocs = max_allocs e0 in
      let (gc_call, _) =
        make_GC_call allocIdent nallocIdent limitIdent gcIdent threadInfIdent
          tinfIdent stackframeTIdent frameIdent rootIdent fpIdent nextFld
          prevFld allocs [] N0
      in
      bind (Obj.magic coq_MonadError)
        (Obj.magic translate_body argsIdent allocIdent nallocIdent limitIdent
          gcIdent mainIdent bodyIdent bodyName threadInfIdent tinfIdent
          heapInfIdent numArgsIdent isptrIdent caseIdent resultIdent nParam
          prims stackframeTIdent frameIdent rootIdent fpIdent nextFld rootFld
          prevFld args_opt [] (union_list PS.empty localVars) [] nenv e0 fenv
          cenv ienv fmap N0) (fun x ->
        let (body, slots) = x in
        ret (Obj.magic coq_MonadError) ((bodyIdent, (Gfun (Internal
          { fn_return = coq_val; fn_callconv = cc_default; fn_params =
          ((tinfIdent, (Tpointer ((Tstruct (threadInfIdent, noattr)),
          noattr))) :: []); fn_vars =
          (stack_decl stackframeTIdent frameIdent rootIdent
            (BinInt.Z.of_N slots)); fn_temps =
          (app (map (fun x0 -> (x0, coq_val)) localVars) ((allocIdent,
            (Tpointer (coq_val, { attr_volatile = false; attr_alignas =
            None }))) :: ((limitIdent, (Tpointer (coq_val, { attr_volatile =
            false; attr_alignas = None }))) :: ((argsIdent, (Tpointer
            (coq_val, { attr_volatile = false; attr_alignas =
            None }))) :: [])))); fn_body = (Ssequence ((Ssequence ((Ssequence
          ((Ssequence ((Ssequence ((Sset (allocIdent, (Efield ((Ederef
          ((Etempvar (tinfIdent, (Tpointer ((Tstruct (threadInfIdent,
          noattr)), noattr)))), (Tstruct (threadInfIdent, noattr)))),
          allocIdent, (Tpointer (coq_val, { attr_volatile = false;
          attr_alignas = None })))))), (Sset (limitIdent, (Efield ((Ederef
          ((Etempvar (tinfIdent, (Tpointer ((Tstruct (threadInfIdent,
          noattr)), noattr)))), (Tstruct (threadInfIdent, noattr)))),
          limitIdent, (Tpointer (coq_val, { attr_volatile = false;
          attr_alignas = None })))))))), (Sset (argsIdent, (Efield ((Ederef
          ((Etempvar (tinfIdent, (Tpointer ((Tstruct (threadInfIdent,
          noattr)), noattr)))), (Tstruct (threadInfIdent, noattr)))),
          argsIdent, (Tarray (uval, maxArgs, noattr)))))))),
          (init_stack threadInfIdent tinfIdent stackframeTIdent frameIdent
            rootIdent fpIdent nextFld rootFld prevFld))), gc_call)),
          body)) }))) :: funs)))
  | _ ->
    Err (String.String (Coq_x74, (String.String (Coq_x72, (String.String
      (Coq_x61, (String.String (Coq_x6e, (String.String (Coq_x73,
      (String.String (Coq_x6c, (String.String (Coq_x61, (String.String
      (Coq_x74, (String.String (Coq_x65, (String.String (Coq_x5f,
      (String.String (Coq_x70, (String.String (Coq_x72, (String.String
      (Coq_x6f, (String.String (Coq_x67, (String.String (Coq_x72,
      (String.String (Coq_x61, (String.String (Coq_x6d, (String.String
      (Coq_x3a, (String.String (Coq_x20, (String.String (Coq_x4d,
      (String.String (Coq_x69, (String.String (Coq_x73, (String.String
      (Coq_x73, (String.String (Coq_x69, (String.String (Coq_x6e,
      (String.String (Coq_x67, (String.String (Coq_x20, (String.String
      (Coq_x74, (String.String (Coq_x6f, (String.String (Coq_x70,
      (String.String (Coq_x6c, (String.String (Coq_x65, (String.String
      (Coq_x76, (String.String (Coq_x65, (String.String (Coq_x6c,
      (String.String (Coq_x20, (String.String (Coq_x66, (String.String
      (Coq_x75, (String.String (Coq_x6e, (String.String (Coq_x63,
      (String.String (Coq_x74, (String.String (Coq_x69, (String.String
      (Coq_x6f, (String.String (Coq_x6e, (String.String (Coq_x20,
      (String.String (Coq_x62, (String.String (Coq_x6c, (String.String
      (Coq_x6f, (String.String (Coq_x63, (String.String (Coq_x6b,
      String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

type 'a nState = (positive, 'a) compM'

(** val getName : positive nState **)

let getName =
  bind (coq_MonadErrorT CompM.coq_MonadState) get (fun x ->
    let (cd, n) = x in
    bind (coq_MonadErrorT CompM.coq_MonadState)
      (put (cd, (Pos.add n Coq_xH))) (fun _ ->
      ret (coq_MonadErrorT CompM.coq_MonadState) n))

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
    globdef) list * fun_info_env) * name_env) nState **)

let rec make_fundef_info fnd fenv nenv =
  match fnd with
  | Fcons (x, t0, _, e, fnd') ->
    (match M.get t0 fenv with
     | Some inf ->
       let (_, l) = inf in
       bind (coq_MonadErrorT CompM.coq_MonadState)
         (make_fundef_info fnd' fenv nenv) (fun rest ->
         let (p, nenv') = rest in
         let (defs, map0) = p in
         bind (coq_MonadErrorT CompM.coq_MonadState) getName
           (fun info_name ->
           let len = BinInt.Z.of_nat (Datatypes.length l) in
           let ind = { gvar_info = (Tarray (uval,
             (BinInt.Z.add len (Zpos (Coq_xO Coq_xH))), noattr)); gvar_init =
             ((coq_Init_int (BinInt.Z.of_nat (max_allocs e))) :: ((coq_Init_int
                                                                    len) :: 
             (make_ind_array l))); gvar_readonly = true; gvar_volatile =
             false }
           in
           ret (coq_MonadErrorT CompM.coq_MonadState) ((((info_name, (Gvar
             ind)) :: defs), (M.set x (info_name, t0) map0)),
             (update_name_env_fun_info x info_name nenv'))))
     | None ->
       failwith (String.String (Coq_x6d, (String.String (Coq_x61,
         (String.String (Coq_x6b, (String.String (Coq_x65, (String.String
         (Coq_x5f, (String.String (Coq_x66, (String.String (Coq_x75,
         (String.String (Coq_x6e, (String.String (Coq_x64, (String.String
         (Coq_x65, (String.String (Coq_x66, (String.String (Coq_x5f,
         (String.String (Coq_x69, (String.String (Coq_x6e, (String.String
         (Coq_x66, (String.String (Coq_x6f, (String.String (Coq_x3a,
         (String.String (Coq_x20, (String.String (Coq_x55, (String.String
         (Coq_x6e, (String.String (Coq_x6b, (String.String (Coq_x6e,
         (String.String (Coq_x6f, (String.String (Coq_x77, (String.String
         (Coq_x6e, (String.String (Coq_x20, (String.String (Coq_x74,
         (String.String (Coq_x61, (String.String (Coq_x67,
         String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | Fnil -> ret (coq_MonadErrorT CompM.coq_MonadState) (([], M.empty), nenv)

(** val add_bodyinfo :
    ident -> exp -> fun_env -> name_env -> fun_info_env ->
    (positive * (Clight.fundef, coq_type) globdef) list ->
    (((positive * (Clight.fundef, coq_type) globdef)
    list * (positive * positive) M.tree) * name M.tree) nState **)

let add_bodyinfo mainIdent e _ nenv map0 defs =
  bind (coq_MonadErrorT CompM.coq_MonadState) getName (fun info_name ->
    let ind = { gvar_info = (Tarray (uval, (Zpos (Coq_xO Coq_xH)), noattr));
      gvar_init =
      ((coq_Init_int (BinInt.Z.of_nat (max_allocs e))) :: ((coq_Init_int Z0) :: []));
      gvar_readonly = true; gvar_volatile = false }
    in
    ret (coq_MonadErrorT CompM.coq_MonadState) ((((info_name, (Gvar
      ind)) :: defs), (M.set mainIdent (info_name, Coq_xH) map0)),
      (M.set info_name (Coq_nNamed (String.String (Coq_x62, (String.String
        (Coq_x6f, (String.String (Coq_x64, (String.String (Coq_x79,
        (String.String (Coq_x5f, (String.String (Coq_x69, (String.String
        (Coq_x6e, (String.String (Coq_x66, (String.String (Coq_x6f,
        String.EmptyString))))))))))))))))))) nenv)))

(** val make_funinfo :
    ident -> exp -> fun_env -> name_env -> (((positive * (Clight.fundef,
    coq_type) globdef) list * fun_info_env) * name_env) nState **)

let make_funinfo mainIdent e fenv nenv =
  match e with
  | Efun (fnd, e') ->
    bind (coq_MonadErrorT CompM.coq_MonadState)
      (make_fundef_info fnd fenv nenv) (fun p ->
      let (p0, nenv') = p in
      let (defs, map0) = p0 in add_bodyinfo mainIdent e' fenv nenv' map0 defs)
  | _ ->
    failwith (String.String (Coq_x6d, (String.String (Coq_x61, (String.String
      (Coq_x6b, (String.String (Coq_x65, (String.String (Coq_x5f,
      (String.String (Coq_x66, (String.String (Coq_x75, (String.String
      (Coq_x6e, (String.String (Coq_x69, (String.String (Coq_x6e,
      (String.String (Coq_x66, (String.String (Coq_x6f, (String.String
      (Coq_x3a, (String.String (Coq_x20, (String.String (Coq_x46,
      (String.String (Coq_x75, (String.String (Coq_x6e, (String.String
      (Coq_x63, (String.String (Coq_x74, (String.String (Coq_x69,
      (String.String (Coq_x6f, (String.String (Coq_x6e, (String.String
      (Coq_x20, (String.String (Coq_x62, (String.String (Coq_x6c,
      (String.String (Coq_x6f, (String.String (Coq_x63, (String.String
      (Coq_x6b, (String.String (Coq_x20, (String.String (Coq_x65,
      (String.String (Coq_x78, (String.String (Coq_x70, (String.String
      (Coq_x65, (String.String (Coq_x63, (String.String (Coq_x74,
      (String.String (Coq_x65, (String.String (Coq_x64,
      String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val global_defs :
    exp -> (positive * (Clight.fundef, coq_type) globdef) list **)

let global_defs _ =
  []

(** val make_defs :
    ident -> ident -> ident -> ident -> ident -> ident -> ident -> String.t
    -> ident -> ident -> ident -> ident -> ident -> ident -> ident -> nat ->
    prim_env -> ident -> ident -> ident -> ident -> ident -> ident -> ident
    -> bool -> exp -> fun_env -> ctor_env -> n_ind_env -> name_env ->
    (name_env * (positive * (Clight.fundef, coq_type) globdef) list) nState **)

let make_defs argsIdent allocIdent nallocIdent limitIdent gcIdent mainIdent bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent numArgsIdent isptrIdent caseIdent resultIdent nParam prims stackframeTIdent frameIdent rootIdent fpIdent nextFld rootFld prevFld args_opt e fenv cenv ienv nenv =
  bind (coq_MonadErrorT CompM.coq_MonadState)
    (make_funinfo mainIdent e fenv nenv) (fun fun_inf' ->
    let (p, nenv') = fun_inf' in
    let (fun_inf, map0) = p in
    (match translate_program argsIdent allocIdent nallocIdent limitIdent
             gcIdent mainIdent bodyIdent bodyName threadInfIdent tinfIdent
             heapInfIdent numArgsIdent isptrIdent caseIdent resultIdent
             nParam prims stackframeTIdent frameIdent rootIdent fpIdent
             nextFld rootFld prevFld args_opt e fenv cenv ienv map0 nenv with
     | Err s -> failwith s
     | Ret fun_defs' ->
       let fun_defs = rev fun_defs' in
       ret (coq_MonadErrorT CompM.coq_MonadState) (nenv',
         (app (global_defs e) (app fun_inf fun_defs)))))

(** val composites : composite_definition list **)

let composites =
  []

(** val mk_prog_opt :
    ident -> (ident * (Clight.fundef, coq_type) globdef) list -> ident ->
    bool -> Clight.program error **)

let mk_prog_opt bodyIdent defs main add_comp =
  let composites0 = if add_comp then composites else [] in
  let res = make_program composites0 defs (bodyIdent :: []) main in
  (match res with
   | OK p -> ret (Obj.magic coq_MonadError) p
   | Error _ ->
     Err (String.String (Coq_x6d, (String.String (Coq_x6b, (String.String
       (Coq_x5f, (String.String (Coq_x70, (String.String (Coq_x72,
       (String.String (Coq_x6f, (String.String (Coq_x67, (String.String
       (Coq_x5f, (String.String (Coq_x6f, (String.String (Coq_x70,
       (String.String (Coq_x74, String.EmptyString)))))))))))))))))))))))

(** val wrap_in_fun : exp -> exp **)

let wrap_in_fun e = match e with
| Efun (_, _) -> e
| _ -> Efun (Fnil, e)

(** val inf_vars :
    ident -> ident -> ident -> ident -> ident -> ident -> ident -> String.t
    -> ident -> ident -> ident -> ident -> ident -> ident -> ident -> ident
    -> ident -> ident -> ident -> ident -> ident -> ident -> (ident * name)
    list **)

let inf_vars argsIdent allocIdent nallocIdent limitIdent gcIdent mainIdent bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent numArgsIdent isptrIdent caseIdent resultIdent stackframeTIdent frameIdent rootIdent fpIdent nextFld rootFld prevFld =
  (isptrIdent, (Coq_nNamed (String.String (Coq_x69, (String.String (Coq_x73,
    (String.String (Coq_x5f, (String.String (Coq_x70, (String.String
    (Coq_x74, (String.String (Coq_x72,
    String.EmptyString)))))))))))))) :: ((argsIdent, (Coq_nNamed
    (String.String (Coq_x61, (String.String (Coq_x72, (String.String
    (Coq_x67, (String.String (Coq_x73,
    String.EmptyString)))))))))) :: ((allocIdent, (Coq_nNamed (String.String
    (Coq_x61, (String.String (Coq_x6c, (String.String (Coq_x6c,
    (String.String (Coq_x6f, (String.String (Coq_x63,
    String.EmptyString)))))))))))) :: ((nallocIdent, (Coq_nNamed
    (String.String (Coq_x6e, (String.String (Coq_x61, (String.String
    (Coq_x6c, (String.String (Coq_x6c, (String.String (Coq_x6f,
    (String.String (Coq_x63,
    String.EmptyString)))))))))))))) :: ((limitIdent, (Coq_nNamed
    (String.String (Coq_x6c, (String.String (Coq_x69, (String.String
    (Coq_x6d, (String.String (Coq_x69, (String.String (Coq_x74,
    String.EmptyString)))))))))))) :: ((gcIdent, (Coq_nNamed (String.String
    (Coq_x67, (String.String (Coq_x61, (String.String (Coq_x72,
    (String.String (Coq_x62, (String.String (Coq_x61, (String.String
    (Coq_x67, (String.String (Coq_x65, (String.String (Coq_x5f,
    (String.String (Coq_x63, (String.String (Coq_x6f, (String.String
    (Coq_x6c, (String.String (Coq_x6c, (String.String (Coq_x65,
    (String.String (Coq_x63, (String.String (Coq_x74,
    String.EmptyString)))))))))))))))))))))))))))))))) :: ((mainIdent,
    (Coq_nNamed (String.String (Coq_x6d, (String.String (Coq_x61,
    (String.String (Coq_x69, (String.String (Coq_x6e,
    String.EmptyString)))))))))) :: ((bodyIdent, (Coq_nNamed
    bodyName)) :: ((threadInfIdent, (Coq_nNamed (String.String (Coq_x74,
    (String.String (Coq_x68, (String.String (Coq_x72, (String.String
    (Coq_x65, (String.String (Coq_x61, (String.String (Coq_x64,
    (String.String (Coq_x5f, (String.String (Coq_x69, (String.String
    (Coq_x6e, (String.String (Coq_x66, (String.String (Coq_x6f,
    String.EmptyString)))))))))))))))))))))))) :: ((tinfIdent, (Coq_nNamed
    (String.String (Coq_x74, (String.String (Coq_x69, (String.String
    (Coq_x6e, (String.String (Coq_x66, (String.String (Coq_x6f,
    String.EmptyString)))))))))))) :: ((heapInfIdent, (Coq_nNamed
    (String.String (Coq_x68, (String.String (Coq_x65, (String.String
    (Coq_x61, (String.String (Coq_x70,
    String.EmptyString)))))))))) :: ((caseIdent, (Coq_nNamed (String.String
    (Coq_x61, (String.String (Coq_x72, (String.String (Coq_x67,
    String.EmptyString)))))))) :: ((numArgsIdent, (Coq_nNamed (String.String
    (Coq_x6e, (String.String (Coq_x75, (String.String (Coq_x6d,
    (String.String (Coq_x5f, (String.String (Coq_x61, (String.String
    (Coq_x72, (String.String (Coq_x67, (String.String (Coq_x73,
    String.EmptyString)))))))))))))))))) :: ((stackframeTIdent, (Coq_nNamed
    (String.String (Coq_x73, (String.String (Coq_x74, (String.String
    (Coq_x61, (String.String (Coq_x63, (String.String (Coq_x6b,
    (String.String (Coq_x5f, (String.String (Coq_x66, (String.String
    (Coq_x72, (String.String (Coq_x61, (String.String (Coq_x6d,
    (String.String (Coq_x65,
    String.EmptyString)))))))))))))))))))))))) :: ((frameIdent, (Coq_nNamed
    (String.String (Coq_x66, (String.String (Coq_x72, (String.String
    (Coq_x61, (String.String (Coq_x6d, (String.String (Coq_x65,
    String.EmptyString)))))))))))) :: ((rootIdent, (Coq_nNamed (String.String
    (Coq_x72, (String.String (Coq_x6f, (String.String (Coq_x6f,
    (String.String (Coq_x74, (String.String (Coq_x73,
    String.EmptyString)))))))))))) :: ((fpIdent, (Coq_nNamed (String.String
    (Coq_x66, (String.String (Coq_x70, String.EmptyString)))))) :: ((nextFld,
    (Coq_nNamed (String.String (Coq_x6e, (String.String (Coq_x65,
    (String.String (Coq_x78, (String.String (Coq_x74,
    String.EmptyString)))))))))) :: ((rootFld, (Coq_nNamed (String.String
    (Coq_x72, (String.String (Coq_x6f, (String.String (Coq_x6f,
    (String.String (Coq_x74, String.EmptyString)))))))))) :: ((prevFld,
    (Coq_nNamed (String.String (Coq_x70, (String.String (Coq_x72,
    (String.String (Coq_x65, (String.String (Coq_x76,
    String.EmptyString)))))))))) :: ((resultIdent, (Coq_nNamed (String.String
    (Coq_x72, (String.String (Coq_x65, (String.String (Coq_x73,
    (String.String (Coq_x75, (String.String (Coq_x6c, (String.String
    (Coq_x74, String.EmptyString)))))))))))))) :: []))))))))))))))))))))

(** val add_inf_vars :
    ident -> ident -> ident -> ident -> ident -> ident -> ident -> String.t
    -> ident -> ident -> ident -> ident -> ident -> ident -> ident -> ident
    -> ident -> ident -> ident -> ident -> ident -> ident -> name_env ->
    name_env **)

let add_inf_vars argsIdent allocIdent nallocIdent limitIdent gcIdent mainIdent bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent numArgsIdent isptrIdent caseIdent resultIdent stackframeTIdent frameIdent rootIdent fpIdent nextFld rootFld prevFld nenv =
  fold_left (fun nenv0 inf -> M.set (fst inf) (snd inf) nenv0)
    (inf_vars argsIdent allocIdent nallocIdent limitIdent gcIdent mainIdent
      bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent numArgsIdent
      isptrIdent caseIdent resultIdent stackframeTIdent frameIdent rootIdent
      fpIdent nextFld rootFld prevFld) nenv

(** val ensure_unique : name_env -> name_env **)

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

(** val make_tinfo_rec :
    ident -> positive * (Clight.fundef, coq_type) globdef **)

let make_tinfo_rec threadInfIdent =
  (make_tinfoIdent, (Gfun (External ((EF_external ((String ((Ascii (true,
    false, true, true, false, true, true, false)), (String ((Ascii (true,
    false, false, false, false, true, true, false)), (String ((Ascii (true,
    true, false, true, false, true, true, false)), (String ((Ascii (true,
    false, true, false, false, true, true, false)), (String ((Ascii (true,
    true, true, true, true, false, true, false)), (String ((Ascii (false,
    false, true, false, true, true, true, false)), (String ((Ascii (true,
    false, false, true, false, true, true, false)), (String ((Ascii (false,
    true, true, true, false, true, true, false)), (String ((Ascii (false,
    true, true, false, false, true, true, false)), (String ((Ascii (true,
    true, true, true, false, true, true, false)),
    EmptyString)))))))))))))))))))), { sig_args = []; sig_res = (Tret
    val_typ); sig_cc = cc_default })), Tnil, (Tpointer ((Tstruct
    (threadInfIdent, noattr)), noattr)), cc_default))))

(** val compile :
    ident -> ident -> ident -> ident -> ident -> ident -> ident -> String.t
    -> ident -> ident -> ident -> ident -> ident -> ident -> ident -> nat ->
    prim_env -> ident -> ident -> ident -> ident -> ident -> ident -> ident
    -> bool -> exp -> ctor_env -> name_env ->
    ((name_env * Clight.program) * Clight.program) error * String.t **)

let compile argsIdent allocIdent nallocIdent limitIdent gcIdent mainIdent bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent numArgsIdent isptrIdent caseIdent resultIdent nParam prims stackframeTIdent frameIdent rootIdent fpIdent nextFld rootFld prevFld args_opt e cenv nenv0 =
  let e0 = wrap_in_fun e in
  let fenv = compute_fun_env nenv0 e0 in
  let ienv = compute_ind_env cenv in
  let p'' =
    make_defs argsIdent allocIdent nallocIdent limitIdent gcIdent mainIdent
      bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent numArgsIdent
      isptrIdent caseIdent resultIdent nParam prims stackframeTIdent
      frameIdent rootIdent fpIdent nextFld rootFld prevFld args_opt e0 fenv
      cenv ienv nenv0
  in
  let n =
    Pos.add
      (max_var e0 (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
        Coq_xH))))))) Coq_xH
  in
  let comp_d =
    pack_data Coq_xH Coq_xH Coq_xH Coq_xH cenv fenv nenv0 M.empty []
  in
  let err =
    let (res, _) = run_compM p'' comp_d n in
    bind coq_MonadError res (fun x ->
      let (nenv1, defs) = x in
      let nenv =
        add_inf_vars argsIdent allocIdent nallocIdent limitIdent gcIdent
          mainIdent bodyIdent bodyName threadInfIdent tinfIdent heapInfIdent
          numArgsIdent isptrIdent caseIdent resultIdent stackframeTIdent
          frameIdent rootIdent fpIdent nextFld rootFld prevFld
          (ensure_unique nenv1)
      in
      let forward_defs = make_extern_decls nenv defs false in
      bind coq_MonadError
        (Obj.magic mk_prog_opt bodyIdent
          ((body_external_decl bodyIdent bodyName threadInfIdent tinfIdent) :: [])
          mainIdent false) (fun body ->
        bind coq_MonadError
          (Obj.magic mk_prog_opt bodyIdent
            ((make_tinfo_rec threadInfIdent) :: (app forward_defs defs))
            mainIdent true) (fun head ->
          ret coq_MonadError
            (((M.set make_tinfoIdent (Coq_nNamed (String.String (Coq_x6d,
                (String.String (Coq_x61, (String.String (Coq_x6b,
                (String.String (Coq_x65, (String.String (Coq_x5f,
                (String.String (Coq_x74, (String.String (Coq_x69,
                (String.String (Coq_x6e, (String.String (Coq_x66,
                (String.String (Coq_x6f,
                String.EmptyString))))))))))))))))))))) nenv), body), head))))
  in
  ((Obj.magic err), String.EmptyString)

(** val empty_program : ident -> Clight.program **)

let empty_program mainIdent =
  { prog_defs = []; prog_public = []; prog_main = mainIdent; prog_types = [];
    prog_comp_env = PTree.empty }

(** val stripOption : ident -> Clight.program option -> Clight.program **)

let stripOption mainIdent = function
| Some p' -> p'
| None -> empty_program mainIdent
