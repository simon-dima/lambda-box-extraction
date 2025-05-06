open AstCommon
open BinNums
open Byte
open Datatypes
open EAst
open Erasure0
open Kernames
open Monad0
open Nat0
open Pipeline_utils
open Bytestring
open CompM
open Compile0
open Cps_show
open Pipeline
open Toplevel0
open Toplevel

(** val make_opts : bool -> bool -> coq_Options **)

let make_opts cps debug0 =
  { erasure_config = default_erasure_config; inductives_mapping = [];
    direct = (negb cps); c_args = (S (S (S (S (S O))))); anf_conf = O;
    show_anf = false; o_level = O; Pipeline_utils.time = false;
    Pipeline_utils.time_anf = false; debug = debug0; dev = O; prefix =
    String.EmptyString; body_name = (String.String (Coq_x62, (String.String
    (Coq_x6f, (String.String (Coq_x64, (String.String (Coq_x79,
    String.EmptyString)))))))); prims = [] }

(** val find_arity : term -> nat **)

let rec find_arity = function
| Coq_tLambda (_, body) -> add (S O) (find_arity body)
| _ -> O

(** val find_global_decl_arity : global_decl -> nat error **)

let find_global_decl_arity = function
| ConstantDecl bd ->
  (match bd with
   | Some bd0 -> Ret (find_arity bd0)
   | None ->
     Err (String.String (Coq_x46, (String.String (Coq_x6f, (String.String
       (Coq_x75, (String.String (Coq_x6e, (String.String (Coq_x64,
       (String.String (Coq_x20, (String.String (Coq_x65, (String.String
       (Coq_x6d, (String.String (Coq_x70, (String.String (Coq_x74,
       (String.String (Coq_x79, (String.String (Coq_x20, (String.String
       (Coq_x43, (String.String (Coq_x6f, (String.String (Coq_x6e,
       (String.String (Coq_x73, (String.String (Coq_x74, (String.String
       (Coq_x61, (String.String (Coq_x6e, (String.String (Coq_x74,
       (String.String (Coq_x44, (String.String (Coq_x65, (String.String
       (Coq_x63, (String.String (Coq_x6c, (String.String (Coq_x20,
       (String.String (Coq_x62, (String.String (Coq_x6f, (String.String
       (Coq_x64, (String.String (Coq_x79,
       String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
| InductiveDecl _ ->
  Err (String.String (Coq_x45, (String.String (Coq_x78, (String.String
    (Coq_x70, (String.String (Coq_x65, (String.String (Coq_x63,
    (String.String (Coq_x74, (String.String (Coq_x65, (String.String
    (Coq_x64, (String.String (Coq_x20, (String.String (Coq_x43,
    (String.String (Coq_x6f, (String.String (Coq_x6e, (String.String
    (Coq_x73, (String.String (Coq_x74, (String.String (Coq_x61,
    (String.String (Coq_x6e, (String.String (Coq_x74, (String.String
    (Coq_x44, (String.String (Coq_x65, (String.String (Coq_x63,
    (String.String (Coq_x6c, (String.String (Coq_x20, (String.String
    (Coq_x62, (String.String (Coq_x75, (String.String (Coq_x74,
    (String.String (Coq_x20, (String.String (Coq_x66, (String.String
    (Coq_x6f, (String.String (Coq_x75, (String.String (Coq_x6e,
    (String.String (Coq_x64, (String.String (Coq_x20, (String.String
    (Coq_x49, (String.String (Coq_x6e, (String.String (Coq_x64,
    (String.String (Coq_x75, (String.String (Coq_x63, (String.String
    (Coq_x74, (String.String (Coq_x69, (String.String (Coq_x76,
    (String.String (Coq_x65, (String.String (Coq_x44, (String.String
    (Coq_x65, (String.String (Coq_x63, (String.String (Coq_x6c,
    String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val find_prim_arity : global_declarations -> kername -> nat error **)

let rec find_prim_arity env0 pr =
  match env0 with
  | [] ->
    Err
      (String.append (String.String (Coq_x43, (String.String (Coq_x6f,
        (String.String (Coq_x6e, (String.String (Coq_x73, (String.String
        (Coq_x74, (String.String (Coq_x61, (String.String (Coq_x6e,
        (String.String (Coq_x74, (String.String (Coq_x20,
        String.EmptyString))))))))))))))))))
        (String.append (string_of_kername pr) (String.String (Coq_x20,
          (String.String (Coq_x6e, (String.String (Coq_x6f, (String.String
          (Coq_x74, (String.String (Coq_x20, (String.String (Coq_x66,
          (String.String (Coq_x6f, (String.String (Coq_x75, (String.String
          (Coq_x6e, (String.String (Coq_x64, (String.String (Coq_x20,
          (String.String (Coq_x69, (String.String (Coq_x6e, (String.String
          (Coq_x20, (String.String (Coq_x65, (String.String (Coq_x6e,
          (String.String (Coq_x76, (String.String (Coq_x69, (String.String
          (Coq_x72, (String.String (Coq_x6f, (String.String (Coq_x6e,
          (String.String (Coq_x6d, (String.String (Coq_x65, (String.String
          (Coq_x6e, (String.String (Coq_x74,
          String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))
  | p :: env1 ->
    let (n, gd) = p in
    if Kername.reflect_kername pr n
    then find_global_decl_arity gd
    else find_prim_arity env1 pr

(** val find_prim_arities :
    global_declarations -> ((kername * String.t) * bool) list ->
    ((((kername * String.t) * bool) * nat) * positive) list error **)

let rec find_prim_arities env0 = function
| [] -> Ret []
| p :: prs0 ->
  let (p0, b) = p in
  let (pr, s) = p0 in
  (match find_prim_arity env0 pr with
   | Err _ ->
     bind (Obj.magic coq_MonadError) (find_prim_arities env0 prs0)
       (fun prs' -> Ret prs')
   | Ret arity ->
     bind (Obj.magic coq_MonadError) (find_prim_arities env0 prs0)
       (fun prs' -> Ret (((((pr, s), b), arity), Coq_xH) :: prs')))

(** val register_prims :
    positive -> global_declarations ->
    (((((kername * String.t) * bool) * nat) * positive) list * positive)
    pipelineM **)

let register_prims id env0 =
  bind (coq_MonadErrorT CompM.coq_MonadState) get_options (fun o ->
    match find_prim_arities env0 o.prims with
    | Err s -> failwith s
    | Ret prs ->
      ret (coq_MonadErrorT CompM.coq_MonadState) (pick_prim_ident id prs))

(** val anf_pipeline :
    program -> ((((kername * String.t) * bool) * nat) * positive) list ->
    positive -> coq_LambdaANF_FullTerm pipelineM **)

let anf_pipeline p prs next_id =
  bind (coq_MonadErrorT CompM.coq_MonadState) get_options (fun o ->
    let p_mut = { main = (compile (snd p)); env = (compile_ctx (fst p)) } in
    bind (coq_MonadErrorT CompM.coq_MonadState) (check_axioms prs p_mut)
      (fun _ ->
      bind (coq_MonadErrorT CompM.coq_MonadState)
        (compile_LambdaBoxLocal prs p_mut) (fun p_local ->
        let local_to_anf_trans =
          if o.direct then compile_LambdaANF_ANF else compile_LambdaANF_CPS
        in
        bind (coq_MonadErrorT CompM.coq_MonadState)
          (local_to_anf_trans next_id prs p_local) (fun p_anf ->
          let anf_trans =
            if o.debug then compile_LambdaANF_debug else compile_LambdaANF
          in
          bind (coq_MonadErrorT CompM.coq_MonadState)
            (anf_trans next_id p_anf) (fun p_anf0 ->
            ret (coq_MonadErrorT CompM.coq_MonadState) p_anf0)))))

(** val show_IR : coq_Options -> program -> String.t error * String.t **)

let show_IR opts p =
  let next_id = Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH))))) in
  let genv = fst p in
  let ir_term = fun p0 ->
    bind (coq_MonadErrorT CompM.coq_MonadState) get_options (fun _ ->
      bind (coq_MonadErrorT CompM.coq_MonadState)
        (register_prims next_id genv) (fun x ->
        let (prs, next_id0) = x in anf_pipeline p0 prs next_id0))
  in
  let (perr, log) = run_pipeline opts p ir_term in
  (match perr with
   | Err s -> ((Err s), log)
   | Ret p0 ->
     let (l, e) = p0 in
     let (p1, _) = l in
     let (p2, _) = p1 in
     let (p3, nenv) = p2 in
     let (p4, _) = p3 in
     let (p5, _) = p4 in
     let (_, cenv) = p5 in ((Ret (show_exp nenv cenv true e)), log))
