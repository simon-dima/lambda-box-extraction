open BasicAst
open BinNat
open BinNums
open BinPos
open Byte
open Datatypes
open List_util
open Monad0
open Bytestring
open CompM
open Cps
open Cps_show
open Cps_util

type comp_data = { next_var : var; nect_ctor_tag : ctor_tag;
                   next_ind_tag : ind_tag; next_fun_tag : fun_tag;
                   cenv : ctor_env; fenv : fun_env; nenv : Cps_show.name_env;
                   inline_map : nat M.tree; log : String.t list }

type ('s, 'a) compM' = (unit, comp_data * 's, 'a) compM

(** val get_name_env : unit -> ('a1, Cps_show.name_env) compM' **)

let get_name_env _ =
  bind (coq_MonadErrorT coq_MonadState) get (fun s ->
    ret (coq_MonadErrorT coq_MonadState) (fst s).nenv)

(** val get_name : var -> String.t -> ('a1, var) compM' **)

let get_name old_var suff =
  bind (coq_MonadErrorT coq_MonadState) get (fun p ->
    let (y, st) = p in
    let { next_var = n; nect_ctor_tag = c; next_ind_tag = i; next_fun_tag =
      f; cenv = e; fenv = fenv0; nenv = names; inline_map = imap; log =
      log0 } = y
    in
    let names' = add_entry names n old_var suff in
    bind (coq_MonadErrorT coq_MonadState)
      (put ({ next_var = (Pos.add n Coq_xH); nect_ctor_tag = c;
        next_ind_tag = i; next_fun_tag = f; cenv = e; fenv = fenv0; nenv =
        names'; inline_map = imap; log = log0 }, st)) (fun _ ->
      ret (coq_MonadErrorT coq_MonadState) n))

(** val add_entry_from_map :
    Cps_show.name_env -> Cps_show.name_env -> var -> var -> String.t ->
    Cps_show.name_env **)

let add_entry_from_map nenv0 nenv_old x x_origin suff =
  match M.get x_origin nenv_old with
  | Some n ->
    (match n with
     | Coq_nAnon ->
       M.set x (Coq_nNamed
         (String.append (String.String (Coq_x61, (String.String (Coq_x6e,
           (String.String (Coq_x6f, (String.String (Coq_x6e,
           String.EmptyString)))))))) suff)) nenv0
     | Coq_nNamed s -> M.set x (Coq_nNamed (String.append s suff)) nenv0)
  | None ->
    M.set x (Coq_nNamed
      (String.append (String.String (Coq_x61, (String.String (Coq_x6e,
        (String.String (Coq_x6f, (String.String (Coq_x6e,
        String.EmptyString)))))))) suff)) nenv0

(** val get_name' :
    var -> String.t -> Cps_show.name_env -> ('a1, var) compM' **)

let get_name' old_var suff nenv_old =
  bind (coq_MonadErrorT coq_MonadState) get (fun p ->
    let (y, st) = p in
    let { next_var = n; nect_ctor_tag = c; next_ind_tag = i; next_fun_tag =
      f; cenv = e; fenv = fenv0; nenv = nenv0; inline_map = imap; log =
      log0 } = y
    in
    let nenv' = add_entry_from_map nenv0 nenv_old n old_var suff in
    bind (coq_MonadErrorT coq_MonadState)
      (put ({ next_var = (Pos.add n Coq_xH); nect_ctor_tag = c;
        next_ind_tag = i; next_fun_tag = f; cenv = e; fenv = fenv0; nenv =
        nenv'; inline_map = imap; log = log0 }, st)) (fun _ ->
      ret (coq_MonadErrorT coq_MonadState) n))

(** val get_names_lst : var list -> String.t -> ('a1, var list) compM' **)

let get_names_lst old suff =
  mapM (coq_MonadErrorT coq_MonadState) (fun o -> get_name o suff) old

(** val get_names_lst' :
    var list -> String.t -> Cps_show.name_env -> ('a1, var list) compM' **)

let get_names_lst' old suff nenv_old =
  mapM (coq_MonadErrorT coq_MonadState) (fun o -> get_name' o suff nenv_old)
    old

(** val get_named : name -> ('a1, var) compM' **)

let get_named s =
  bind (coq_MonadErrorT coq_MonadState) get (fun p ->
    let (y, st) = p in
    let { next_var = n; nect_ctor_tag = c; next_ind_tag = i; next_fun_tag =
      f; cenv = e; fenv = fenv0; nenv = names; inline_map = imap; log =
      log0 } = y
    in
    let names' = M.set n s names in
    bind (coq_MonadErrorT coq_MonadState)
      (put ({ next_var = (Pos.add n Coq_xH); nect_ctor_tag = c;
        next_ind_tag = i; next_fun_tag = f; cenv = e; fenv = fenv0; nenv =
        names'; inline_map = imap; log = log0 }, st)) (fun _ ->
      ret (coq_MonadErrorT coq_MonadState) n))

(** val get_named_lst : name list -> ('a1, var list) compM' **)

let get_named_lst s =
  mapM (coq_MonadErrorT coq_MonadState) get_named s

(** val get_named_str : String.t -> ('a1, var) compM' **)

let get_named_str name0 =
  bind (coq_MonadErrorT coq_MonadState) get (fun p ->
    let (y, st) = p in
    let { next_var = n; nect_ctor_tag = c; next_ind_tag = i; next_fun_tag =
      f; cenv = e; fenv = fenv0; nenv = names; inline_map = imap; log =
      log0 } = y
    in
    let names' = add_entry_str names n name0 in
    bind (coq_MonadErrorT coq_MonadState)
      (put ({ next_var = (Pos.add n Coq_xH); nect_ctor_tag = c;
        next_ind_tag = i; next_fun_tag = f; cenv = e; fenv = fenv0; nenv =
        names'; inline_map = imap; log = log0 }, st)) (fun _ ->
      ret (coq_MonadErrorT coq_MonadState) n))

(** val make_record_ctor_tag : coq_N -> ('a1, ctor_tag) compM' **)

let make_record_ctor_tag n =
  bind (coq_MonadErrorT coq_MonadState) get (fun p ->
    let (y, st) = p in
    let { next_var = x; nect_ctor_tag = c; next_ind_tag = i; next_fun_tag =
      f; cenv = cenv0; fenv = fenv0; nenv = names; inline_map = imap; log =
      log0 } = y
    in
    let inf = { ctor_name = Coq_nAnon; ctor_ind_name = Coq_nAnon;
      ctor_ind_tag = i; ctor_arity = n; ctor_ordinal = N0 }
    in
    let cenv' = M.set c inf cenv0 in
    bind (coq_MonadErrorT coq_MonadState)
      (put ({ next_var = x; nect_ctor_tag = (Pos.add c Coq_xH);
        next_ind_tag = (Pos.add i Coq_xH); next_fun_tag = f; cenv = cenv';
        fenv = fenv0; nenv = names; inline_map = imap; log = log0 }, st))
      (fun _ -> ret (coq_MonadErrorT coq_MonadState) c))

(** val get_pp_name : var -> ('a1, String.t) compM' **)

let get_pp_name x =
  bind (coq_MonadErrorT coq_MonadState) (get_name_env ()) (fun nenv0 ->
    ret (coq_MonadErrorT coq_MonadState) (show_tree (show_var nenv0 x)))

(** val add_log : String.t -> comp_data -> comp_data **)

let add_log msg c =
  let { next_var = x; nect_ctor_tag = c0; next_ind_tag = i; next_fun_tag = f;
    cenv = e; fenv = fenv0; nenv = names; inline_map = imap; log = log0 } = c
  in
  { next_var = x; nect_ctor_tag = c0; next_ind_tag = i; next_fun_tag = f;
  cenv = e; fenv = fenv0; nenv = names; inline_map = imap; log =
  (msg :: log0) }

(** val get_state : unit -> ('a1, 'a1) compM' **)

let get_state _ =
  bind (coq_MonadErrorT coq_MonadState) get (fun s ->
    ret (coq_MonadErrorT coq_MonadState) (snd s))

(** val put_state : 'a1 -> ('a1, unit) compM' **)

let put_state st =
  bind (coq_MonadErrorT coq_MonadState) get (fun s -> put ((fst s), st))

(** val get_ftag : coq_N -> ('a1, fun_tag) compM' **)

let get_ftag arity =
  bind (coq_MonadErrorT coq_MonadState) get (fun p ->
    let (y, st) = p in
    let { next_var = x; nect_ctor_tag = c; next_ind_tag = i; next_fun_tag =
      f; cenv = e; fenv = fenv0; nenv = names; inline_map = imap; log =
      log0 } = y
    in
    bind (coq_MonadErrorT coq_MonadState)
      (put ({ next_var = x; nect_ctor_tag = c; next_ind_tag = i;
        next_fun_tag = (Pos.add f Coq_xH); cenv = e; fenv =
        (M.set f (arity, (fromN N0 (N.to_nat arity))) fenv0); nenv = names;
        inline_map = imap; log = log0 }, st)) (fun _ ->
      ret (coq_MonadErrorT coq_MonadState) f))

(** val run_compM :
    ('a1, 'a2) compM' -> comp_data -> 'a1 -> 'a2 error * (comp_data * 'a1) **)

let run_compM m st s =
  Obj.magic m () (st, s)

(** val pack_data :
    var -> ctor_tag -> ind_tag -> fun_tag -> ctor_env -> fun_env ->
    Cps_show.name_env -> nat M.tree -> String.t list -> comp_data **)

let pack_data x x0 x1 x2 x3 x4 x5 x6 x7 =
  { next_var = x; nect_ctor_tag = x0; next_ind_tag = x1; next_fun_tag = x2;
    cenv = x3; fenv = x4; nenv = x5; inline_map = x6; log = x7 }

(** val put_ctor_env : ctor_env -> comp_data -> comp_data **)

let put_ctor_env cenv0 c =
  let { next_var = next; nect_ctor_tag = ctag; next_ind_tag = itag;
    next_fun_tag = ftag; cenv = _; fenv = fenv0; nenv = names; inline_map =
    imap; log = log0 } = c
  in
  pack_data next ctag itag ftag cenv0 fenv0 names imap log0
