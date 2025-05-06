open AST
open BinNums
open Cop
open Coqlib0
open Ctypes
open Datatypes
open Floats
open Globalenvs
open Integers
open List0
open Maps
open Memory
open Smallstep
open Values0

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

type expr =
| Econst_int of Int.int * coq_type
| Econst_float of float * coq_type
| Econst_single of float32 * coq_type
| Econst_long of Int64.int * coq_type
| Evar of ident * coq_type
| Etempvar of ident * coq_type
| Ederef of expr * coq_type
| Eaddrof of expr * coq_type
| Eunop of unary_operation * expr * coq_type
| Ebinop of binary_operation * expr * expr * coq_type
| Ecast of expr * coq_type
| Efield of expr * ident * coq_type
| Esizeof of coq_type * coq_type
| Ealignof of coq_type * coq_type

(** val expr_rect :
    (Int.int -> coq_type -> 'a1) -> (float -> coq_type -> 'a1) -> (float32 ->
    coq_type -> 'a1) -> (Int64.int -> coq_type -> 'a1) -> (ident -> coq_type
    -> 'a1) -> (ident -> coq_type -> 'a1) -> (expr -> 'a1 -> coq_type -> 'a1)
    -> (expr -> 'a1 -> coq_type -> 'a1) -> (unary_operation -> expr -> 'a1 ->
    coq_type -> 'a1) -> (binary_operation -> expr -> 'a1 -> expr -> 'a1 ->
    coq_type -> 'a1) -> (expr -> 'a1 -> coq_type -> 'a1) -> (expr -> 'a1 ->
    ident -> coq_type -> 'a1) -> (coq_type -> coq_type -> 'a1) -> (coq_type
    -> coq_type -> 'a1) -> expr -> 'a1 **)

let rec expr_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 = function
| Econst_int (i, t0) -> f i t0
| Econst_float (f13, t0) -> f0 f13 t0
| Econst_single (f13, t0) -> f1 f13 t0
| Econst_long (i, t0) -> f2 i t0
| Evar (i, t0) -> f3 i t0
| Etempvar (i, t0) -> f4 i t0
| Ederef (e0, t0) ->
  f5 e0 (expr_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 e0) t0
| Eaddrof (e0, t0) ->
  f6 e0 (expr_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 e0) t0
| Eunop (u, e0, t0) ->
  f7 u e0 (expr_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 e0) t0
| Ebinop (b, e0, e1, t0) ->
  f8 b e0 (expr_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 e0) e1
    (expr_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 e1) t0
| Ecast (e0, t0) ->
  f9 e0 (expr_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 e0) t0
| Efield (e0, i, t0) ->
  f10 e0 (expr_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 e0) i t0
| Esizeof (t0, t1) -> f11 t0 t1
| Ealignof (t0, t1) -> f12 t0 t1

(** val expr_rec :
    (Int.int -> coq_type -> 'a1) -> (float -> coq_type -> 'a1) -> (float32 ->
    coq_type -> 'a1) -> (Int64.int -> coq_type -> 'a1) -> (ident -> coq_type
    -> 'a1) -> (ident -> coq_type -> 'a1) -> (expr -> 'a1 -> coq_type -> 'a1)
    -> (expr -> 'a1 -> coq_type -> 'a1) -> (unary_operation -> expr -> 'a1 ->
    coq_type -> 'a1) -> (binary_operation -> expr -> 'a1 -> expr -> 'a1 ->
    coq_type -> 'a1) -> (expr -> 'a1 -> coq_type -> 'a1) -> (expr -> 'a1 ->
    ident -> coq_type -> 'a1) -> (coq_type -> coq_type -> 'a1) -> (coq_type
    -> coq_type -> 'a1) -> expr -> 'a1 **)

let rec expr_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 = function
| Econst_int (i, t0) -> f i t0
| Econst_float (f13, t0) -> f0 f13 t0
| Econst_single (f13, t0) -> f1 f13 t0
| Econst_long (i, t0) -> f2 i t0
| Evar (i, t0) -> f3 i t0
| Etempvar (i, t0) -> f4 i t0
| Ederef (e0, t0) ->
  f5 e0 (expr_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 e0) t0
| Eaddrof (e0, t0) ->
  f6 e0 (expr_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 e0) t0
| Eunop (u, e0, t0) ->
  f7 u e0 (expr_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 e0) t0
| Ebinop (b, e0, e1, t0) ->
  f8 b e0 (expr_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 e0) e1
    (expr_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 e1) t0
| Ecast (e0, t0) ->
  f9 e0 (expr_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 e0) t0
| Efield (e0, i, t0) ->
  f10 e0 (expr_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 e0) i t0
| Esizeof (t0, t1) -> f11 t0 t1
| Ealignof (t0, t1) -> f12 t0 t1

(** val typeof : expr -> coq_type **)

let typeof = function
| Econst_int (_, ty) -> ty
| Econst_float (_, ty) -> ty
| Econst_single (_, ty) -> ty
| Econst_long (_, ty) -> ty
| Evar (_, ty) -> ty
| Etempvar (_, ty) -> ty
| Ederef (_, ty) -> ty
| Eaddrof (_, ty) -> ty
| Eunop (_, _, ty) -> ty
| Ebinop (_, _, _, ty) -> ty
| Ecast (_, ty) -> ty
| Efield (_, _, ty) -> ty
| Esizeof (_, ty) -> ty
| Ealignof (_, ty) -> ty

type label = ident

type statement =
| Sskip
| Sassign of expr * expr
| Sset of ident * expr
| Scall of ident option * expr * expr list
| Sbuiltin of ident option * external_function * typelist * expr list
| Ssequence of statement * statement
| Sifthenelse of expr * statement * statement
| Sloop of statement * statement
| Sbreak
| Scontinue
| Sreturn of expr option
| Sswitch of expr * labeled_statements
| Slabel of label * statement
| Sgoto of label
and labeled_statements =
| LSnil
| LScons of coq_Z option * statement * labeled_statements

(** val statement_rect :
    'a1 -> (expr -> expr -> 'a1) -> (ident -> expr -> 'a1) -> (ident option
    -> expr -> expr list -> 'a1) -> (ident option -> external_function ->
    typelist -> expr list -> 'a1) -> (statement -> 'a1 -> statement -> 'a1 ->
    'a1) -> (expr -> statement -> 'a1 -> statement -> 'a1 -> 'a1) ->
    (statement -> 'a1 -> statement -> 'a1 -> 'a1) -> 'a1 -> 'a1 -> (expr
    option -> 'a1) -> (expr -> labeled_statements -> 'a1) -> (label ->
    statement -> 'a1 -> 'a1) -> (label -> 'a1) -> statement -> 'a1 **)

let rec statement_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 = function
| Sskip -> f
| Sassign (e, e0) -> f0 e e0
| Sset (i, e) -> f1 i e
| Scall (o, e, l) -> f2 o e l
| Sbuiltin (o, e, t0, l) -> f3 o e t0 l
| Ssequence (s0, s1) ->
  f4 s0 (statement_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 s0) s1
    (statement_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 s1)
| Sifthenelse (e, s0, s1) ->
  f5 e s0 (statement_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 s0) s1
    (statement_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 s1)
| Sloop (s0, s1) ->
  f6 s0 (statement_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 s0) s1
    (statement_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 s1)
| Sbreak -> f7
| Scontinue -> f8
| Sreturn o -> f9 o
| Sswitch (e, l) -> f10 e l
| Slabel (l, s0) ->
  f11 l s0 (statement_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 s0)
| Sgoto l -> f12 l

(** val statement_rec :
    'a1 -> (expr -> expr -> 'a1) -> (ident -> expr -> 'a1) -> (ident option
    -> expr -> expr list -> 'a1) -> (ident option -> external_function ->
    typelist -> expr list -> 'a1) -> (statement -> 'a1 -> statement -> 'a1 ->
    'a1) -> (expr -> statement -> 'a1 -> statement -> 'a1 -> 'a1) ->
    (statement -> 'a1 -> statement -> 'a1 -> 'a1) -> 'a1 -> 'a1 -> (expr
    option -> 'a1) -> (expr -> labeled_statements -> 'a1) -> (label ->
    statement -> 'a1 -> 'a1) -> (label -> 'a1) -> statement -> 'a1 **)

let rec statement_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 = function
| Sskip -> f
| Sassign (e, e0) -> f0 e e0
| Sset (i, e) -> f1 i e
| Scall (o, e, l) -> f2 o e l
| Sbuiltin (o, e, t0, l) -> f3 o e t0 l
| Ssequence (s0, s1) ->
  f4 s0 (statement_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 s0) s1
    (statement_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 s1)
| Sifthenelse (e, s0, s1) ->
  f5 e s0 (statement_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 s0) s1
    (statement_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 s1)
| Sloop (s0, s1) ->
  f6 s0 (statement_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 s0) s1
    (statement_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 s1)
| Sbreak -> f7
| Scontinue -> f8
| Sreturn o -> f9 o
| Sswitch (e, l) -> f10 e l
| Slabel (l, s0) ->
  f11 l s0 (statement_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 s0)
| Sgoto l -> f12 l

(** val labeled_statements_rect :
    'a1 -> (coq_Z option -> statement -> labeled_statements -> 'a1 -> 'a1) ->
    labeled_statements -> 'a1 **)

let rec labeled_statements_rect f f0 = function
| LSnil -> f
| LScons (o, s, l0) -> f0 o s l0 (labeled_statements_rect f f0 l0)

(** val labeled_statements_rec :
    'a1 -> (coq_Z option -> statement -> labeled_statements -> 'a1 -> 'a1) ->
    labeled_statements -> 'a1 **)

let rec labeled_statements_rec f f0 = function
| LSnil -> f
| LScons (o, s, l0) -> f0 o s l0 (labeled_statements_rec f f0 l0)

(** val coq_Swhile : expr -> statement -> statement **)

let coq_Swhile e s =
  Sloop ((Ssequence ((Sifthenelse (e, Sskip, Sbreak)), s)), Sskip)

(** val coq_Sdowhile : statement -> expr -> statement **)

let coq_Sdowhile s e =
  Sloop (s, (Sifthenelse (e, Sskip, Sbreak)))

(** val coq_Sfor :
    statement -> expr -> statement -> statement -> statement **)

let coq_Sfor s1 e2 s3 s4 =
  Ssequence (s1, (Sloop ((Ssequence ((Sifthenelse (e2, Sskip, Sbreak)), s3)),
    s4)))

type coq_function = { fn_return : coq_type; fn_callconv : calling_convention;
                      fn_params : (ident * coq_type) list;
                      fn_vars : (ident * coq_type) list;
                      fn_temps : (ident * coq_type) list; fn_body : statement }

(** val fn_return : coq_function -> coq_type **)

let fn_return f =
  f.fn_return

(** val fn_callconv : coq_function -> calling_convention **)

let fn_callconv f =
  f.fn_callconv

(** val fn_params : coq_function -> (ident * coq_type) list **)

let fn_params f =
  f.fn_params

(** val fn_vars : coq_function -> (ident * coq_type) list **)

let fn_vars f =
  f.fn_vars

(** val fn_temps : coq_function -> (ident * coq_type) list **)

let fn_temps f =
  f.fn_temps

(** val fn_body : coq_function -> statement **)

let fn_body f =
  f.fn_body

(** val var_names : (ident * coq_type) list -> ident list **)

let var_names vars =
  map fst vars

type fundef = coq_function Ctypes.fundef

(** val type_of_function : coq_function -> coq_type **)

let type_of_function f =
  Tfunction ((type_of_params f.fn_params), f.fn_return, f.fn_callconv)

(** val type_of_fundef : fundef -> coq_type **)

let type_of_fundef = function
| Internal fd -> type_of_function fd
| External (_, args, res, cc) -> Tfunction (args, res, cc)

type program = coq_function Ctypes.program

type genv = { genv_genv : (fundef, coq_type) Genv.t; genv_cenv : composite_env }

(** val genv_genv : genv -> (fundef, coq_type) Genv.t **)

let genv_genv g =
  g.genv_genv

(** val genv_cenv : genv -> composite_env **)

let genv_cenv g =
  g.genv_cenv

(** val globalenv : program -> genv **)

let globalenv p =
  { genv_genv = (Genv.globalenv (program_of_program p)); genv_cenv =
    p.prog_comp_env }

type env = (block * coq_type) PTree.t

(** val empty_env : env **)

let empty_env =
  PTree.empty

type temp_env = coq_val PTree.t

(** val create_undef_temps : (ident * coq_type) list -> temp_env **)

let rec create_undef_temps = function
| [] -> PTree.empty
| p :: temps' ->
  let (id, _) = p in PTree.set id Vundef (create_undef_temps temps')

(** val bind_parameter_temps :
    (ident * coq_type) list -> coq_val list -> temp_env -> temp_env option **)

let rec bind_parameter_temps formals args le =
  match formals with
  | [] -> (match args with
           | [] -> Some le
           | _ :: _ -> None)
  | p :: xl ->
    let (id, _) = p in
    (match args with
     | [] -> None
     | v :: vl -> bind_parameter_temps xl vl (PTree.set id v le))

(** val block_of_binding :
    genv -> (ident * (block * coq_type)) -> (block * coq_Z) * coq_Z **)

let block_of_binding ge = function
| (_, p) -> let (b, ty) = p in ((b, Z0), (sizeof ge.genv_cenv ty))

(** val blocks_of_env : genv -> env -> ((block * coq_Z) * coq_Z) list **)

let blocks_of_env ge e =
  map (block_of_binding ge) (PTree.elements e)

(** val set_opttemp :
    ident option -> coq_val -> temp_env -> coq_val PTree.tree **)

let set_opttemp optid v le =
  match optid with
  | Some id -> PTree.set id v le
  | None -> le

(** val select_switch_default : labeled_statements -> labeled_statements **)

let rec select_switch_default sl = match sl with
| LSnil -> sl
| LScons (o, _, sl') ->
  (match o with
   | Some _ -> select_switch_default sl'
   | None -> sl)

(** val select_switch_case :
    coq_Z -> labeled_statements -> labeled_statements option **)

let rec select_switch_case n sl = match sl with
| LSnil -> None
| LScons (o, _, sl') ->
  (match o with
   | Some c -> if zeq c n then Some sl else select_switch_case n sl'
   | None -> select_switch_case n sl')

(** val select_switch : coq_Z -> labeled_statements -> labeled_statements **)

let select_switch n sl =
  match select_switch_case n sl with
  | Some sl' -> sl'
  | None -> select_switch_default sl

(** val seq_of_labeled_statement : labeled_statements -> statement **)

let rec seq_of_labeled_statement = function
| LSnil -> Sskip
| LScons (_, s, sl') -> Ssequence (s, (seq_of_labeled_statement sl'))

type cont =
| Kstop
| Kseq of statement * cont
| Kloop1 of statement * statement * cont
| Kloop2 of statement * statement * cont
| Kswitch of cont
| Kcall of ident option * coq_function * env * temp_env * cont

(** val cont_rect :
    'a1 -> (statement -> cont -> 'a1 -> 'a1) -> (statement -> statement ->
    cont -> 'a1 -> 'a1) -> (statement -> statement -> cont -> 'a1 -> 'a1) ->
    (cont -> 'a1 -> 'a1) -> (ident option -> coq_function -> env -> temp_env
    -> cont -> 'a1 -> 'a1) -> cont -> 'a1 **)

let rec cont_rect f f0 f1 f2 f3 f4 = function
| Kstop -> f
| Kseq (s, c0) -> f0 s c0 (cont_rect f f0 f1 f2 f3 f4 c0)
| Kloop1 (s, s0, c0) -> f1 s s0 c0 (cont_rect f f0 f1 f2 f3 f4 c0)
| Kloop2 (s, s0, c0) -> f2 s s0 c0 (cont_rect f f0 f1 f2 f3 f4 c0)
| Kswitch c0 -> f3 c0 (cont_rect f f0 f1 f2 f3 f4 c0)
| Kcall (o, f5, e, t0, c0) -> f4 o f5 e t0 c0 (cont_rect f f0 f1 f2 f3 f4 c0)

(** val cont_rec :
    'a1 -> (statement -> cont -> 'a1 -> 'a1) -> (statement -> statement ->
    cont -> 'a1 -> 'a1) -> (statement -> statement -> cont -> 'a1 -> 'a1) ->
    (cont -> 'a1 -> 'a1) -> (ident option -> coq_function -> env -> temp_env
    -> cont -> 'a1 -> 'a1) -> cont -> 'a1 **)

let rec cont_rec f f0 f1 f2 f3 f4 = function
| Kstop -> f
| Kseq (s, c0) -> f0 s c0 (cont_rec f f0 f1 f2 f3 f4 c0)
| Kloop1 (s, s0, c0) -> f1 s s0 c0 (cont_rec f f0 f1 f2 f3 f4 c0)
| Kloop2 (s, s0, c0) -> f2 s s0 c0 (cont_rec f f0 f1 f2 f3 f4 c0)
| Kswitch c0 -> f3 c0 (cont_rec f f0 f1 f2 f3 f4 c0)
| Kcall (o, f5, e, t0, c0) -> f4 o f5 e t0 c0 (cont_rec f f0 f1 f2 f3 f4 c0)

(** val call_cont : cont -> cont **)

let rec call_cont k = match k with
| Kseq (_, k0) -> call_cont k0
| Kloop1 (_, _, k0) -> call_cont k0
| Kloop2 (_, _, k0) -> call_cont k0
| Kswitch k0 -> call_cont k0
| _ -> k

type state =
| State of coq_function * statement * cont * env * temp_env * Mem.mem
| Callstate of fundef * coq_val list * cont * Mem.mem
| Returnstate of coq_val * cont * Mem.mem

(** val state_rect :
    (coq_function -> statement -> cont -> env -> temp_env -> Mem.mem -> 'a1)
    -> (fundef -> coq_val list -> cont -> Mem.mem -> 'a1) -> (coq_val -> cont
    -> Mem.mem -> 'a1) -> state -> 'a1 **)

let state_rect f f0 f1 = function
| State (f2, s0, k, e, le, m) -> f f2 s0 k e le m
| Callstate (fd, args, k, m) -> f0 fd args k m
| Returnstate (res, k, m) -> f1 res k m

(** val state_rec :
    (coq_function -> statement -> cont -> env -> temp_env -> Mem.mem -> 'a1)
    -> (fundef -> coq_val list -> cont -> Mem.mem -> 'a1) -> (coq_val -> cont
    -> Mem.mem -> 'a1) -> state -> 'a1 **)

let state_rec f f0 f1 = function
| State (f2, s0, k, e, le, m) -> f f2 s0 k e le m
| Callstate (fd, args, k, m) -> f0 fd args k m
| Returnstate (res, k, m) -> f1 res k m

(** val find_label :
    label -> statement -> cont -> (statement * cont) option **)

let rec find_label lbl s k =
  match s with
  | Ssequence (s1, s2) ->
    (match find_label lbl s1 (Kseq (s2, k)) with
     | Some sk -> Some sk
     | None -> find_label lbl s2 k)
  | Sifthenelse (_, s1, s2) ->
    (match find_label lbl s1 k with
     | Some sk -> Some sk
     | None -> find_label lbl s2 k)
  | Sloop (s1, s2) ->
    (match find_label lbl s1 (Kloop1 (s1, s2, k)) with
     | Some sk -> Some sk
     | None -> find_label lbl s2 (Kloop2 (s1, s2, k)))
  | Sswitch (_, sl) -> find_label_ls lbl sl (Kswitch k)
  | Slabel (lbl', s') ->
    if ident_eq lbl lbl' then Some (s', k) else find_label lbl s' k
  | _ -> None

(** val find_label_ls :
    label -> labeled_statements -> cont -> (statement * cont) option **)

and find_label_ls lbl sl k =
  match sl with
  | LSnil -> None
  | LScons (_, s, sl') ->
    (match find_label lbl s (Kseq ((seq_of_labeled_statement sl'), k)) with
     | Some sk -> Some sk
     | None -> find_label_ls lbl sl' k)

(** val function_entry2_rect :
    genv -> coq_function -> coq_val list -> Mem.mem -> env -> temp_env ->
    Mem.mem -> (__ -> __ -> __ -> __ -> __ -> 'a1) -> 'a1 **)

let function_entry2_rect _ _ _ _ _ _ _ f0 =
  f0 __ __ __ __ __

(** val function_entry2_rec :
    genv -> coq_function -> coq_val list -> Mem.mem -> env -> temp_env ->
    Mem.mem -> (__ -> __ -> __ -> __ -> __ -> 'a1) -> 'a1 **)

let function_entry2_rec _ _ _ _ _ _ _ f0 =
  f0 __ __ __ __ __

(** val semantics1 : program -> semantics **)

let semantics1 p =
  let ge = globalenv p in
  { Smallstep.globalenv = (Obj.magic ge); symbolenv =
  (Genv.to_senv ge.genv_genv) }

(** val semantics2 : program -> semantics **)

let semantics2 p =
  let ge = globalenv p in
  { Smallstep.globalenv = (Obj.magic ge); symbolenv =
  (Genv.to_senv ge.genv_genv) }
