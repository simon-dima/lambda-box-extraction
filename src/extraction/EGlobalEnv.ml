open Datatypes
open EAst
open ELiftSubst
open Kernames
open List0
open MCOption
open MCProd
open Monad_utils

(** val lookup_env : global_declarations -> kername -> global_decl option **)

let rec lookup_env _UU03a3_ id =
  match _UU03a3_ with
  | [] -> None
  | hd :: tl ->
    if Kername.reflect_kername id (fst hd)
    then Some (snd hd)
    else lookup_env tl id

(** val lookup_constant :
    global_declarations -> kername -> constant_body option **)

let lookup_constant _UU03a3_ kn =
  bind (Obj.magic option_monad) (Obj.magic lookup_env _UU03a3_ kn)
    (fun decl ->
    match decl with
    | ConstantDecl cdecl -> ret (Obj.magic option_monad) cdecl
    | InductiveDecl _ -> None)

(** val lookup_minductive :
    global_declarations -> kername -> mutual_inductive_body option **)

let lookup_minductive _UU03a3_ kn =
  bind (Obj.magic option_monad) (Obj.magic lookup_env _UU03a3_ kn)
    (fun decl ->
    match decl with
    | ConstantDecl _ -> None
    | InductiveDecl mdecl -> ret (Obj.magic option_monad) mdecl)

(** val lookup_inductive :
    global_declarations -> inductive ->
    (mutual_inductive_body * one_inductive_body) option **)

let lookup_inductive _UU03a3_ kn =
  bind (Obj.magic option_monad)
    (Obj.magic lookup_minductive _UU03a3_ kn.inductive_mind) (fun mdecl ->
    bind (Obj.magic option_monad)
      (nth_error (Obj.magic mdecl.ind_bodies) kn.inductive_ind) (fun idecl ->
      ret (Obj.magic option_monad) (mdecl, idecl)))

(** val lookup_constructor :
    global_declarations -> inductive -> nat ->
    ((mutual_inductive_body * one_inductive_body) * constructor_body) option **)

let lookup_constructor _UU03a3_ kn c =
  bind (Obj.magic option_monad) (Obj.magic lookup_inductive _UU03a3_ kn)
    (fun x ->
    let (mdecl, idecl) = x in
    bind (Obj.magic option_monad) (nth_error (Obj.magic idecl.ind_ctors) c)
      (fun cdecl -> ret (Obj.magic option_monad) ((mdecl, idecl), cdecl)))

(** val lookup_constructor_pars_args :
    global_declarations -> inductive -> nat -> (nat * nat) option **)

let lookup_constructor_pars_args _UU03a3_ kn c =
  bind (Obj.magic option_monad) (Obj.magic lookup_constructor _UU03a3_ kn c)
    (fun x ->
    let (y, cdecl) = x in
    let (mdecl, _) = y in
    ret (Obj.magic option_monad) (mdecl.ind_npars, cdecl.cstr_nargs))

(** val lookup_projection :
    global_declarations -> projection ->
    (((mutual_inductive_body * one_inductive_body) * constructor_body) * projection_body)
    option **)

let lookup_projection _UU03a3_ p =
  bind (Obj.magic option_monad)
    (Obj.magic lookup_constructor _UU03a3_ p.proj_ind O) (fun x ->
    let (y, cdecl) = x in
    let (mdecl, idecl) = y in
    bind (Obj.magic option_monad)
      (nth_error (Obj.magic idecl.ind_projs) p.proj_arg) (fun pdecl ->
      ret (Obj.magic option_monad) (((mdecl, idecl), cdecl), pdecl)))

(** val closed_decl : global_decl -> bool **)

let closed_decl = function
| ConstantDecl cb -> option_default (closedn O) cb true
| InductiveDecl _ -> true

(** val closed_env : global_declarations -> bool **)

let closed_env _UU03a3_ =
  forallb (test_snd closed_decl) _UU03a3_
