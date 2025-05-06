open BasicAst
open Byte
open Classes1
open Common2
open Datatypes
open EAst
open EPrimitive
open ExAst
open Extraction
open Kernames
open List0
open MCOption
open MCString
open Nat0
open PrettyPrinterMonad0
open ResultMonad
open StringExtra0
open Universes0
open Bytestring
open Monad_utils

type __ = Obj.t

module E = EAst

module Ex = ExAst

(** val indent_size : nat **)

let indent_size =
  S (S O)

type coq_ElmPrintConfig = { term_box_symbol : String.t;
                            type_box_symbol : String.t;
                            any_type_symbol : String.t;
                            false_elim_def : String.t; print_full_names : 
                            bool }

(** val elm_false_rec : String.t **)

let elm_false_rec =
  String.concat Common2.nl ((String.String (Coq_x66, (String.String (Coq_x61,
    (String.String (Coq_x6c, (String.String (Coq_x73, (String.String
    (Coq_x65, (String.String (Coq_x5f, (String.String (Coq_x72,
    (String.String (Coq_x65, (String.String (Coq_x63, (String.String
    (Coq_x20, (String.String (Coq_x3a, (String.String (Coq_x20,
    (String.String (Coq_x28, (String.String (Coq_x29, (String.String
    (Coq_x20, (String.String (Coq_x2d, (String.String (Coq_x3e,
    (String.String (Coq_x20, (String.String (Coq_x61,
    String.EmptyString)))))))))))))))))))))))))))))))))))))) :: ((String.String
    (Coq_x66, (String.String (Coq_x61, (String.String (Coq_x6c,
    (String.String (Coq_x73, (String.String (Coq_x65, (String.String
    (Coq_x5f, (String.String (Coq_x72, (String.String (Coq_x65,
    (String.String (Coq_x63, (String.String (Coq_x20, (String.String
    (Coq_x5f, (String.String (Coq_x20, (String.String (Coq_x3d,
    (String.String (Coq_x20, (String.String (Coq_x66, (String.String
    (Coq_x61, (String.String (Coq_x6c, (String.String (Coq_x73,
    (String.String (Coq_x65, (String.String (Coq_x5f, (String.String
    (Coq_x72, (String.String (Coq_x65, (String.String (Coq_x63,
    (String.String (Coq_x20, (String.String (Coq_x28, (String.String
    (Coq_x29,
    String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))) :: []))

(** val get_fun_name :
    (kername -> String.t option) -> coq_ElmPrintConfig -> kername -> String.t **)

let get_fun_name translate h name0 =
  let get_name = fun _ ->
    if h.print_full_names
    then replace_char Coq_x2e Coq_x5f (string_of_kername name0)
    else snd name0
  in
  option_get (uncapitalize (get_name name0)) (translate name0)

(** val get_ty_name : (kername -> String.t option) -> kername -> String.t **)

let get_ty_name translate name0 =
  option_get (capitalize (replace_char Coq_x2e Coq_x5f (snd name0)))
    (translate name0)

(** val get_ctor_name :
    (kername -> String.t option) -> kername -> String.t **)

let get_ctor_name translate name0 =
  option_get (capitalize (snd name0)) (translate name0)

(** val get_ident_name : ident -> ident **)

let get_ident_name name0 =
  uncapitalize (remove_char Coq_x27 (replace_char Coq_x2e Coq_x5f name0))

(** val get_ty_arg_name : ident -> ident **)

let get_ty_arg_name =
  uncapitalize

(** val lookup_ind_decl :
    Ex.global_env -> inductive -> (Ex.one_inductive_body, String.t) result **)

let lookup_ind_decl _UU03a3_ ind =
  match Ex.lookup_env _UU03a3_ ind.inductive_mind with
  | Some g ->
    (match g with
     | Ex.InductiveDecl m ->
       let { Ex.ind_finite = _; Ex.ind_npars = _; Ex.ind_bodies = oibs } = m
       in
       (match nth_error oibs ind.inductive_ind with
        | Some body -> ret (Obj.magic coq_Monad_result) body
        | None ->
          Err
            (String.append (String.String (Coq_x43, (String.String (Coq_x6f,
              (String.String (Coq_x75, (String.String (Coq_x6c,
              (String.String (Coq_x64, (String.String (Coq_x20,
              (String.String (Coq_x6e, (String.String (Coq_x6f,
              (String.String (Coq_x74, (String.String (Coq_x20,
              (String.String (Coq_x66, (String.String (Coq_x69,
              (String.String (Coq_x6e, (String.String (Coq_x64,
              (String.String (Coq_x20, (String.String (Coq_x69,
              (String.String (Coq_x6e, (String.String (Coq_x64,
              (String.String (Coq_x75, (String.String (Coq_x63,
              (String.String (Coq_x74, (String.String (Coq_x69,
              (String.String (Coq_x76, (String.String (Coq_x65,
              (String.String (Coq_x20,
              String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))
              (String.append (string_of_nat ind.inductive_ind)
                (String.append (String.String (Coq_x20, (String.String
                  (Coq_x69, (String.String (Coq_x6e, (String.String (Coq_x20,
                  (String.String (Coq_x6d, (String.String (Coq_x75,
                  (String.String (Coq_x74, (String.String (Coq_x75,
                  (String.String (Coq_x61, (String.String (Coq_x6c,
                  (String.String (Coq_x20, (String.String (Coq_x69,
                  (String.String (Coq_x6e, (String.String (Coq_x64,
                  (String.String (Coq_x75, (String.String (Coq_x63,
                  (String.String (Coq_x74, (String.String (Coq_x69,
                  (String.String (Coq_x76, (String.String (Coq_x65,
                  (String.String (Coq_x20,
                  String.EmptyString))))))))))))))))))))))))))))))))))))))))))
                  (string_of_kername ind.inductive_mind)))))
     | _ ->
       Err
         (String.append (String.String (Coq_x43, (String.String (Coq_x6f,
           (String.String (Coq_x75, (String.String (Coq_x6c, (String.String
           (Coq_x64, (String.String (Coq_x20, (String.String (Coq_x6e,
           (String.String (Coq_x6f, (String.String (Coq_x74, (String.String
           (Coq_x20, (String.String (Coq_x66, (String.String (Coq_x69,
           (String.String (Coq_x6e, (String.String (Coq_x64, (String.String
           (Coq_x20, (String.String (Coq_x69, (String.String (Coq_x6e,
           (String.String (Coq_x64, (String.String (Coq_x75, (String.String
           (Coq_x63, (String.String (Coq_x74, (String.String (Coq_x69,
           (String.String (Coq_x76, (String.String (Coq_x65, (String.String
           (Coq_x20,
           String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))
           (String.append (string_of_kername ind.inductive_mind)
             (String.String (Coq_x20, (String.String (Coq_x69, (String.String
             (Coq_x6e, (String.String (Coq_x20, (String.String (Coq_x65,
             (String.String (Coq_x6e, (String.String (Coq_x76, (String.String
             (Coq_x69, (String.String (Coq_x72, (String.String (Coq_x6f,
             (String.String (Coq_x6e, (String.String (Coq_x6d, (String.String
             (Coq_x65, (String.String (Coq_x6e, (String.String (Coq_x74,
             String.EmptyString)))))))))))))))))))))))))))))))))
  | None ->
    Err
      (String.append (String.String (Coq_x43, (String.String (Coq_x6f,
        (String.String (Coq_x75, (String.String (Coq_x6c, (String.String
        (Coq_x64, (String.String (Coq_x20, (String.String (Coq_x6e,
        (String.String (Coq_x6f, (String.String (Coq_x74, (String.String
        (Coq_x20, (String.String (Coq_x66, (String.String (Coq_x69,
        (String.String (Coq_x6e, (String.String (Coq_x64, (String.String
        (Coq_x20, (String.String (Coq_x69, (String.String (Coq_x6e,
        (String.String (Coq_x64, (String.String (Coq_x75, (String.String
        (Coq_x63, (String.String (Coq_x74, (String.String (Coq_x69,
        (String.String (Coq_x76, (String.String (Coq_x65, (String.String
        (Coq_x20,
        String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))
        (String.append (string_of_kername ind.inductive_mind) (String.String
          (Coq_x20, (String.String (Coq_x69, (String.String (Coq_x6e,
          (String.String (Coq_x20, (String.String (Coq_x65, (String.String
          (Coq_x6e, (String.String (Coq_x76, (String.String (Coq_x69,
          (String.String (Coq_x72, (String.String (Coq_x6f, (String.String
          (Coq_x6e, (String.String (Coq_x6d, (String.String (Coq_x65,
          (String.String (Coq_x6e, (String.String (Coq_x74,
          String.EmptyString))))))))))))))))))))))))))))))))

(** val print_ind :
    Ex.global_env -> (kername -> String.t option) -> inductive -> unit
    coq_PrettyPrinter **)

let print_ind _UU03a3_ translate ind =
  bind (Obj.magic coq_Monad_PrettyPrinter)
    (wrap_result (Obj.magic lookup_ind_decl _UU03a3_ ind) (Obj.magic id))
    (fun oib ->
    let kn = ((fst ind.inductive_mind), (Ex.ind_name oib)) in
    append (get_ty_name translate kn))

(** val print_ind_ctor :
    Ex.global_env -> (kername -> String.t option) -> inductive -> nat -> unit
    coq_PrettyPrinter **)

let print_ind_ctor _UU03a3_ translate ind i =
  bind (Obj.magic coq_Monad_PrettyPrinter)
    (wrap_result (Obj.magic lookup_ind_decl _UU03a3_ ind) (Obj.magic id))
    (fun oib ->
    match nth_error (Ex.ind_ctors oib) i with
    | Some p ->
      let (p0, _) = p in
      let (name0, _) = p0 in
      let kn = ((fst ind.inductive_mind), name0) in
      append (get_ctor_name translate kn)
    | None ->
      printer_fail
        (String.append (Ex.ind_name oib)
          (String.append (String.String (Coq_x20, (String.String (Coq_x64,
            (String.String (Coq_x6f, (String.String (Coq_x65, (String.String
            (Coq_x73, (String.String (Coq_x20, (String.String (Coq_x6e,
            (String.String (Coq_x6f, (String.String (Coq_x74, (String.String
            (Coq_x20, (String.String (Coq_x68, (String.String (Coq_x61,
            (String.String (Coq_x76, (String.String (Coq_x65, (String.String
            (Coq_x20, (String.String (Coq_x61, (String.String (Coq_x20,
            (String.String (Coq_x63, (String.String (Coq_x74, (String.String
            (Coq_x6f, (String.String (Coq_x72, (String.String (Coq_x20,
            String.EmptyString))))))))))))))))))))))))))))))))))))))))))))
            (string_of_nat i))))

(** val print_parenthesized :
    bool -> unit coq_PrettyPrinter -> unit coq_PrettyPrinter **)

let print_parenthesized parenthesize print =
  if parenthesize
  then bind (Obj.magic coq_Monad_PrettyPrinter)
         (append (String.String (Coq_x28, String.EmptyString))) (fun _ ->
         bind (Obj.magic coq_Monad_PrettyPrinter) print (fun _ ->
           append (String.String (Coq_x29, String.EmptyString))))
  else print

(** val parenthesize_app_head : term -> bool **)

let parenthesize_app_head = function
| Coq_tLambda (_, _) -> true
| Coq_tLetIn (_, _, _) -> true
| Coq_tCase (_, _, _) -> true
| Coq_tFix (_, _) -> true
| _ -> false

(** val parenthesize_app_arg : term -> bool **)

let parenthesize_app_arg = function
| Coq_tLambda (_, _) -> true
| Coq_tLetIn (_, _, _) -> true
| Coq_tApp (_, _) -> true
| Coq_tCase (_, _, _) -> true
| Coq_tProj (_, _) -> true
| Coq_tFix (_, _) -> true
| _ -> false

(** val parenthesize_case_discriminee : term -> bool **)

let parenthesize_case_discriminee = function
| Coq_tLetIn (_, _, _) -> true
| Coq_tCase (_, _, _) -> true
| _ -> false

(** val parenthesize_case_branch : term -> bool **)

let parenthesize_case_branch _ =
  false

(** val fresh : ident -> ident list -> ident **)

let fresh name0 used =
  if existsb (String.eqb name0) used
  then let rec f n i =
         match n with
         | O ->
           String.String (Coq_x75, (String.String (Coq_x6e, (String.String
             (Coq_x72, (String.String (Coq_x65, (String.String (Coq_x61,
             (String.String (Coq_x63, (String.String (Coq_x68, (String.String
             (Coq_x61, (String.String (Coq_x62, (String.String (Coq_x6c,
             (String.String (Coq_x65, String.EmptyString)))))))))))))))))))))
         | S n0 ->
           let numbered_name = String.append name0 (MCString.string_of_nat i)
           in
           if existsb (String.eqb numbered_name) used
           then f n0 (S i)
           else numbered_name
       in f (S (length used)) (S (S O))
  else name0

(** val fresh_ident : name -> ident list -> ident coq_PrettyPrinter **)

let fresh_ident name0 _UU0393_ =
  bind (Obj.magic coq_Monad_PrettyPrinter) (Obj.magic get_used_names)
    (fun used_names ->
    match name0 with
    | Coq_nAnon ->
      ret (Obj.magic coq_Monad_PrettyPrinter)
        (fresh (String.String (Coq_x78, String.EmptyString))
          (app _UU0393_ used_names))
    | Coq_nNamed name1 ->
      ret (Obj.magic coq_Monad_PrettyPrinter)
        (fresh (get_ident_name name1) (app _UU0393_ used_names)))

(** val fresh_ty_arg_name : name -> ident list -> ident coq_PrettyPrinter **)

let fresh_ty_arg_name name0 _UU0393_ =
  bind (Obj.magic coq_Monad_PrettyPrinter) (Obj.magic get_used_names)
    (fun used_names ->
    match name0 with
    | Coq_nAnon ->
      ret (Obj.magic coq_Monad_PrettyPrinter)
        (fresh (String.String (Coq_x61, String.EmptyString))
          (app _UU0393_ used_names))
    | Coq_nNamed name1 ->
      ret (Obj.magic coq_Monad_PrettyPrinter)
        (fresh (get_ty_arg_name name1) (app _UU0393_ used_names)))

(** val parenthesize_prod_domain : box_type -> bool **)

let parenthesize_prod_domain = function
| TArr (_, _) -> true
| _ -> false

(** val parenthesize_ty_app_arg : box_type -> bool **)

let parenthesize_ty_app_arg = function
| TApp (_, _) -> true
| _ -> false

(** val print_type :
    Ex.global_env -> (kername -> String.t option) -> coq_ElmPrintConfig ->
    ident list -> box_type -> unit coq_PrettyPrinter **)

let rec print_type _UU03a3_ translate h _UU0393_ = function
| TBox -> append h.type_box_symbol
| TAny -> append h.any_type_symbol
| TArr (dom, cod) ->
  bind (Obj.magic coq_Monad_PrettyPrinter)
    (print_parenthesized (parenthesize_prod_domain dom)
      (print_type _UU03a3_ translate h _UU0393_ dom)) (fun _ ->
    bind (Obj.magic coq_Monad_PrettyPrinter)
      (append (String.String (Coq_x20, (String.String (Coq_x2d,
        (String.String (Coq_x3e, (String.String (Coq_x20,
        String.EmptyString))))))))) (fun _ ->
      print_type _UU03a3_ translate h _UU0393_ cod))
| TApp (head, arg) ->
  bind (Obj.magic coq_Monad_PrettyPrinter)
    (print_type _UU03a3_ translate h _UU0393_ head) (fun _ ->
    bind (Obj.magic coq_Monad_PrettyPrinter)
      (append (String.String (Coq_x20, String.EmptyString))) (fun _ ->
      print_parenthesized (parenthesize_ty_app_arg arg)
        (print_type _UU03a3_ translate h _UU0393_ arg)))
| TVar n ->
  (match nth_error _UU0393_ n with
   | Some name0 -> append name0
   | None ->
     printer_fail
       (String.append (String.String (Coq_x75, (String.String (Coq_x6e,
         (String.String (Coq_x62, (String.String (Coq_x6f, (String.String
         (Coq_x75, (String.String (Coq_x6e, (String.String (Coq_x64,
         (String.String (Coq_x20, (String.String (Coq_x54, (String.String
         (Coq_x56, (String.String (Coq_x61, (String.String (Coq_x72,
         (String.String (Coq_x20,
         String.EmptyString)))))))))))))))))))))))))) (string_of_nat n)))
| TInd ind -> print_ind _UU03a3_ translate ind
| TConst name0 -> append (get_ty_name translate name0)

(** val print_define_term :
    ident list -> ident -> term -> (ident list -> term -> unit
    coq_PrettyPrinter) -> unit coq_PrettyPrinter **)

let print_define_term _UU0393_ name0 t0 print_term0 =
  bind (Obj.magic coq_Monad_PrettyPrinter)
    (Obj.magic get_current_line_length) (fun name_col ->
    bind (Obj.magic coq_Monad_PrettyPrinter) (append name0) (fun _ ->
      let print_decompose =
        let rec print_decompose _UU0393_0 t1 = match t1 with
        | Coq_tLambda (arg_name, t2) ->
          bind (Obj.magic coq_Monad_PrettyPrinter)
            (Obj.magic fresh_ident arg_name _UU0393_0) (fun arg_name0 ->
            bind (Obj.magic coq_Monad_PrettyPrinter)
              (append
                (String.append (String.String (Coq_x20, String.EmptyString))
                  arg_name0)) (fun _ ->
              print_decompose (arg_name0 :: _UU0393_0) t2))
        | _ ->
          bind (Obj.magic coq_Monad_PrettyPrinter)
            (append (String.String (Coq_x20, (String.String (Coq_x3d,
              String.EmptyString))))) (fun _ ->
            bind (Obj.magic coq_Monad_PrettyPrinter)
              (push_indent (add name_col indent_size)) (fun _ ->
              bind (Obj.magic coq_Monad_PrettyPrinter) append_nl (fun _ ->
                bind (Obj.magic coq_Monad_PrettyPrinter)
                  (print_term0 _UU0393_0 t1) (fun _ -> pop_indent))))
        in print_decompose
      in
      (match t0 with
       | Coq_tFix (mfix, idx) ->
         (match mfix with
          | [] -> print_decompose _UU0393_ t0
          | d :: l ->
            (match l with
             | [] ->
               (match idx with
                | O -> print_decompose (name0 :: _UU0393_) d.dbody
                | S _ -> print_decompose _UU0393_ t0)
             | _ :: _ -> print_decompose _UU0393_ t0))
       | _ -> print_decompose _UU0393_ t0)))

(** val get_infix : String.t -> String.t option **)

let get_infix s =
  let len = String.length s in
  let begins = substring_count (S O) s in
  let ends = substring_from (sub len (S O)) s in
  if (&&) (String.eqb begins (String.String (Coq_x28, String.EmptyString)))
       (String.eqb ends (String.String (Coq_x29, String.EmptyString)))
  then Some (String.substring (S O) (sub len (S (S O))) s)
  else None

(** val print_infix_match_branch :
    (ident list -> term -> unit coq_PrettyPrinter) -> String.t -> ident list
    -> name list -> term -> unit coq_PrettyPrinter **)

let print_infix_match_branch print infix_op _UU0393_ br_ctx br =
  match br_ctx with
  | [] ->
    printer_fail (String.String (Coq_x63, (String.String (Coq_x6f,
      (String.String (Coq_x75, (String.String (Coq_x6c, (String.String
      (Coq_x64, (String.String (Coq_x20, (String.String (Coq_x6e,
      (String.String (Coq_x6f, (String.String (Coq_x74, (String.String
      (Coq_x20, (String.String (Coq_x64, (String.String (Coq_x65,
      (String.String (Coq_x63, (String.String (Coq_x6f, (String.String
      (Coq_x6d, (String.String (Coq_x70, (String.String (Coq_x6f,
      (String.String (Coq_x73, (String.String (Coq_x65, (String.String
      (Coq_x20, (String.String (Coq_x62, (String.String (Coq_x72,
      (String.String (Coq_x61, (String.String (Coq_x6e, (String.String
      (Coq_x63, (String.String (Coq_x68, (String.String (Coq_x20,
      (String.String (Coq_x66, (String.String (Coq_x6f, (String.String
      (Coq_x72, (String.String (Coq_x20, (String.String (Coq_x69,
      (String.String (Coq_x6e, (String.String (Coq_x66, (String.String
      (Coq_x69, (String.String (Coq_x78, (String.String (Coq_x20,
      (String.String (Coq_x63, (String.String (Coq_x6f, (String.String
      (Coq_x6e, (String.String (Coq_x73, (String.String (Coq_x74,
      (String.String (Coq_x72, (String.String (Coq_x75, (String.String
      (Coq_x63, (String.String (Coq_x74, (String.String (Coq_x6f,
      (String.String (Coq_x72,
      String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | name2 :: l ->
    (match l with
     | [] ->
       printer_fail (String.String (Coq_x63, (String.String (Coq_x6f,
         (String.String (Coq_x75, (String.String (Coq_x6c, (String.String
         (Coq_x64, (String.String (Coq_x20, (String.String (Coq_x6e,
         (String.String (Coq_x6f, (String.String (Coq_x74, (String.String
         (Coq_x20, (String.String (Coq_x64, (String.String (Coq_x65,
         (String.String (Coq_x63, (String.String (Coq_x6f, (String.String
         (Coq_x6d, (String.String (Coq_x70, (String.String (Coq_x6f,
         (String.String (Coq_x73, (String.String (Coq_x65, (String.String
         (Coq_x20, (String.String (Coq_x62, (String.String (Coq_x72,
         (String.String (Coq_x61, (String.String (Coq_x6e, (String.String
         (Coq_x63, (String.String (Coq_x68, (String.String (Coq_x20,
         (String.String (Coq_x66, (String.String (Coq_x6f, (String.String
         (Coq_x72, (String.String (Coq_x20, (String.String (Coq_x69,
         (String.String (Coq_x6e, (String.String (Coq_x66, (String.String
         (Coq_x69, (String.String (Coq_x78, (String.String (Coq_x20,
         (String.String (Coq_x63, (String.String (Coq_x6f, (String.String
         (Coq_x6e, (String.String (Coq_x73, (String.String (Coq_x74,
         (String.String (Coq_x72, (String.String (Coq_x75, (String.String
         (Coq_x63, (String.String (Coq_x74, (String.String (Coq_x6f,
         (String.String (Coq_x72,
         String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
     | name1 :: l0 ->
       (match l0 with
        | [] ->
          bind (Obj.magic coq_Monad_PrettyPrinter)
            (Obj.magic fresh_ident name1 _UU0393_) (fun name3 ->
            let _UU0393_0 = name3 :: _UU0393_ in
            bind (Obj.magic coq_Monad_PrettyPrinter)
              (Obj.magic fresh_ident name2 _UU0393_0) (fun name4 ->
              bind (Obj.magic coq_Monad_PrettyPrinter)
                (append
                  (String.append name3 (String.String (Coq_x20,
                    String.EmptyString)))) (fun _ ->
                bind (Obj.magic coq_Monad_PrettyPrinter) (append infix_op)
                  (fun _ ->
                  bind (Obj.magic coq_Monad_PrettyPrinter)
                    (append
                      (String.append (String.String (Coq_x20,
                        String.EmptyString)) name4)) (fun _ ->
                    bind (Obj.magic coq_Monad_PrettyPrinter)
                      (append (String.String (Coq_x20, (String.String
                        (Coq_x2d, (String.String (Coq_x3e,
                        String.EmptyString))))))) (fun _ ->
                      bind (Obj.magic coq_Monad_PrettyPrinter) append_nl
                        (fun _ ->
                        print_parenthesized (parenthesize_case_branch br)
                          (print (name4 :: _UU0393_0) br))))))))
        | _ :: _ ->
          printer_fail (String.String (Coq_x63, (String.String (Coq_x6f,
            (String.String (Coq_x75, (String.String (Coq_x6c, (String.String
            (Coq_x64, (String.String (Coq_x20, (String.String (Coq_x6e,
            (String.String (Coq_x6f, (String.String (Coq_x74, (String.String
            (Coq_x20, (String.String (Coq_x64, (String.String (Coq_x65,
            (String.String (Coq_x63, (String.String (Coq_x6f, (String.String
            (Coq_x6d, (String.String (Coq_x70, (String.String (Coq_x6f,
            (String.String (Coq_x73, (String.String (Coq_x65, (String.String
            (Coq_x20, (String.String (Coq_x62, (String.String (Coq_x72,
            (String.String (Coq_x61, (String.String (Coq_x6e, (String.String
            (Coq_x63, (String.String (Coq_x68, (String.String (Coq_x20,
            (String.String (Coq_x66, (String.String (Coq_x6f, (String.String
            (Coq_x72, (String.String (Coq_x20, (String.String (Coq_x69,
            (String.String (Coq_x6e, (String.String (Coq_x66, (String.String
            (Coq_x69, (String.String (Coq_x78, (String.String (Coq_x20,
            (String.String (Coq_x63, (String.String (Coq_x6f, (String.String
            (Coq_x6e, (String.String (Coq_x73, (String.String (Coq_x74,
            (String.String (Coq_x72, (String.String (Coq_x75, (String.String
            (Coq_x63, (String.String (Coq_x74, (String.String (Coq_x6f,
            (String.String (Coq_x72,
            String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val print_term :
    Ex.global_env -> (kername -> String.t option) -> coq_ElmPrintConfig ->
    ident list -> term -> unit coq_PrettyPrinter **)

let rec print_term _UU03a3_ translate h _UU0393_ = function
| Coq_tBox -> append h.term_box_symbol
| Coq_tRel n ->
  (match nth_error _UU0393_ n with
   | Some name0 -> append name0
   | None ->
     printer_fail
       (String.append (String.String (Coq_x75, (String.String (Coq_x6e,
         (String.String (Coq_x62, (String.String (Coq_x6f, (String.String
         (Coq_x75, (String.String (Coq_x6e, (String.String (Coq_x64,
         (String.String (Coq_x20, (String.String (Coq_x74, (String.String
         (Coq_x52, (String.String (Coq_x65, (String.String (Coq_x6c,
         (String.String (Coq_x20,
         String.EmptyString)))))))))))))))))))))))))) (string_of_nat n)))
| Coq_tVar ident0 ->
  printer_fail
    (String.append (String.String (Coq_x74, (String.String (Coq_x56,
      (String.String (Coq_x61, (String.String (Coq_x72, (String.String
      (Coq_x20, String.EmptyString)))))))))) ident0)
| Coq_tEvar (_, _) ->
  printer_fail (String.String (Coq_x75, (String.String (Coq_x6e,
    (String.String (Coq_x65, (String.String (Coq_x78, (String.String
    (Coq_x70, (String.String (Coq_x65, (String.String (Coq_x63,
    (String.String (Coq_x74, (String.String (Coq_x65, (String.String
    (Coq_x64, (String.String (Coq_x20, (String.String (Coq_x65,
    (String.String (Coq_x76, (String.String (Coq_x61, (String.String
    (Coq_x72, String.EmptyString))))))))))))))))))))))))))))))
| Coq_tLambda (name0, t1) ->
  bind (Obj.magic coq_Monad_PrettyPrinter)
    (append (String.String (Coq_x5c, String.EmptyString))) (fun _ ->
    let rec f _UU0393_0 name1 body =
      bind (Obj.magic coq_Monad_PrettyPrinter)
        (Obj.magic fresh_ident name1 _UU0393_0) (fun name2 ->
        bind (Obj.magic coq_Monad_PrettyPrinter)
          (append
            (String.append name2 (String.String (Coq_x20,
              String.EmptyString)))) (fun _ ->
          let _UU0393_1 = name2 :: _UU0393_0 in
          (match body with
           | Coq_tLambda (name3, t2) -> f _UU0393_1 name3 t2
           | _ ->
             bind (Obj.magic coq_Monad_PrettyPrinter)
               (append (String.String (Coq_x2d, (String.String (Coq_x3e,
                 (String.String (Coq_x20, String.EmptyString))))))) (fun _ ->
               print_term _UU03a3_ translate h _UU0393_1 body))))
    in f _UU0393_ name0 t1)
| Coq_tLetIn (name0, value, body) ->
  bind (Obj.magic coq_Monad_PrettyPrinter)
    (Obj.magic get_current_line_length) (fun let_col ->
    bind (Obj.magic coq_Monad_PrettyPrinter) (push_indent let_col) (fun _ ->
      bind (Obj.magic coq_Monad_PrettyPrinter)
        (append (String.String (Coq_x6c, (String.String (Coq_x65,
          (String.String (Coq_x74, String.EmptyString))))))) (fun _ ->
        let print_and_add_one = fun _UU0393_0 name1 value0 ->
          bind coq_Monad_PrettyPrinter (Obj.magic append_nl) (fun _ ->
            bind coq_Monad_PrettyPrinter
              (Obj.magic fresh_ident name1 _UU0393_0) (fun name2 ->
              bind coq_Monad_PrettyPrinter (Obj.magic push_use name2)
                (fun _ ->
                bind coq_Monad_PrettyPrinter
                  (Obj.magic print_define_term _UU0393_0 name2 value0
                    (print_term _UU03a3_ translate h)) (fun _ ->
                  ret coq_Monad_PrettyPrinter (name2 :: _UU0393_0)))))
        in
        bind (Obj.magic coq_Monad_PrettyPrinter)
          (push_indent (add let_col indent_size)) (fun _ ->
          bind (Obj.magic coq_Monad_PrettyPrinter)
            (Obj.magic print_and_add_one _UU0393_ name0 value)
            (fun _UU0393_0 ->
            bind (Obj.magic coq_Monad_PrettyPrinter) pop_indent (fun _ ->
              bind (Obj.magic coq_Monad_PrettyPrinter) append_nl (fun _ ->
                bind (Obj.magic coq_Monad_PrettyPrinter)
                  (append (String.String (Coq_x69, (String.String (Coq_x6e,
                    String.EmptyString))))) (fun _ ->
                  bind (Obj.magic coq_Monad_PrettyPrinter) append_nl
                    (fun _ ->
                    bind (Obj.magic coq_Monad_PrettyPrinter)
                      (print_term _UU03a3_ translate h _UU0393_0 body)
                      (fun _ ->
                      bind (Obj.magic coq_Monad_PrettyPrinter) pop_indent
                        (fun _ -> pop_use)))))))))))
| Coq_tApp (head, arg) ->
  bind (Obj.magic coq_Monad_PrettyPrinter)
    (print_parenthesized (parenthesize_app_head head)
      (print_term _UU03a3_ translate h _UU0393_ head)) (fun _ ->
    bind (Obj.magic coq_Monad_PrettyPrinter)
      (append (String.String (Coq_x20, String.EmptyString))) (fun _ ->
      print_parenthesized (parenthesize_app_arg arg)
        (print_term _UU03a3_ translate h _UU0393_ arg)))
| Coq_tConst name0 -> append (get_fun_name translate h name0)
| Coq_tConstruct (ind, i, args) ->
  (match args with
   | [] -> print_ind_ctor _UU03a3_ translate ind i
   | _ :: _ ->
     printer_fail
       (String.append (String.String (Coq_x43, (String.String (Coq_x6f,
         (String.String (Coq_x73, (String.String (Coq_x74, (String.String
         (Coq_x72, (String.String (Coq_x75, (String.String (Coq_x63,
         (String.String (Coq_x74, (String.String (Coq_x6f, (String.String
         (Coq_x72, (String.String (Coq_x73, (String.String (Coq_x2d,
         (String.String (Coq_x61, (String.String (Coq_x73, (String.String
         (Coq_x2d, (String.String (Coq_x62, (String.String (Coq_x6c,
         (String.String (Coq_x6f, (String.String (Coq_x63, (String.String
         (Coq_x6b, (String.String (Coq_x73, (String.String (Coq_x20,
         (String.String (Coq_x69, (String.String (Coq_x73, (String.String
         (Coq_x20, (String.String (Coq_x6e, (String.String (Coq_x6f,
         (String.String (Coq_x74, (String.String (Coq_x20, (String.String
         (Coq_x73, (String.String (Coq_x75, (String.String (Coq_x70,
         (String.String (Coq_x70, (String.String (Coq_x6f, (String.String
         (Coq_x72, (String.String (Coq_x74, (String.String (Coq_x65,
         (String.String (Coq_x64, (String.String (Coq_x3a, (String.String
         (Coq_x20,
         String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
         (string_of_kername ind.inductive_mind)))
| Coq_tCase (indn, discriminee, branches) ->
  let (ind, npars) = indn in
  (match branches with
   | [] -> append h.false_elim_def
   | _ :: _ ->
     bind (Obj.magic coq_Monad_PrettyPrinter)
       (Obj.magic get_current_line_length) (fun case_col ->
       bind (Obj.magic coq_Monad_PrettyPrinter)
         (append (String.String (Coq_x63, (String.String (Coq_x61,
           (String.String (Coq_x73, (String.String (Coq_x65, (String.String
           (Coq_x20, String.EmptyString))))))))))) (fun _ ->
         bind (Obj.magic coq_Monad_PrettyPrinter)
           (print_parenthesized (parenthesize_case_discriminee discriminee)
             (print_term _UU03a3_ translate h _UU0393_ discriminee))
           (fun _ ->
           bind (Obj.magic coq_Monad_PrettyPrinter)
             (append (String.String (Coq_x20, (String.String (Coq_x6f,
               (String.String (Coq_x66, String.EmptyString))))))) (fun _ ->
             bind (Obj.magic coq_Monad_PrettyPrinter)
               (push_indent (add case_col indent_size)) (fun _ ->
               bind (Obj.magic coq_Monad_PrettyPrinter)
                 (wrap_result (Obj.magic lookup_ind_decl _UU03a3_ ind)
                   (Obj.magic id)) (fun oib ->
                 bind (Obj.magic coq_Monad_PrettyPrinter)
                   (let rec print_branches branches0 ctors =
                      match branches0 with
                      | [] ->
                        (match ctors with
                         | [] -> ret (Obj.magic coq_Monad_PrettyPrinter) ()
                         | _ :: _ ->
                           printer_fail (String.String (Coq_x77,
                             (String.String (Coq_x72, (String.String
                             (Coq_x6f, (String.String (Coq_x6e,
                             (String.String (Coq_x67, (String.String
                             (Coq_x20, (String.String (Coq_x6e,
                             (String.String (Coq_x75, (String.String
                             (Coq_x6d, (String.String (Coq_x62,
                             (String.String (Coq_x65, (String.String
                             (Coq_x72, (String.String (Coq_x20,
                             (String.String (Coq_x6f, (String.String
                             (Coq_x66, (String.String (Coq_x20,
                             (String.String (Coq_x63, (String.String
                             (Coq_x61, (String.String (Coq_x73,
                             (String.String (Coq_x65, (String.String
                             (Coq_x20, (String.String (Coq_x62,
                             (String.String (Coq_x72, (String.String
                             (Coq_x61, (String.String (Coq_x6e,
                             (String.String (Coq_x63, (String.String
                             (Coq_x68, (String.String (Coq_x65,
                             (String.String (Coq_x73, (String.String
                             (Coq_x20, (String.String (Coq_x63,
                             (String.String (Coq_x6f, (String.String
                             (Coq_x6d, (String.String (Coq_x70,
                             (String.String (Coq_x61, (String.String
                             (Coq_x72, (String.String (Coq_x65,
                             (String.String (Coq_x64, (String.String
                             (Coq_x20, (String.String (Coq_x74,
                             (String.String (Coq_x6f, (String.String
                             (Coq_x20, (String.String (Coq_x69,
                             (String.String (Coq_x6e, (String.String
                             (Coq_x64, (String.String (Coq_x75,
                             (String.String (Coq_x63, (String.String
                             (Coq_x74, (String.String (Coq_x69,
                             (String.String (Coq_x76, (String.String
                             (Coq_x65,
                             String.EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                      | p :: branches1 ->
                        let (bctx, t1) = p in
                        (match ctors with
                         | [] ->
                           printer_fail (String.String (Coq_x77,
                             (String.String (Coq_x72, (String.String
                             (Coq_x6f, (String.String (Coq_x6e,
                             (String.String (Coq_x67, (String.String
                             (Coq_x20, (String.String (Coq_x6e,
                             (String.String (Coq_x75, (String.String
                             (Coq_x6d, (String.String (Coq_x62,
                             (String.String (Coq_x65, (String.String
                             (Coq_x72, (String.String (Coq_x20,
                             (String.String (Coq_x6f, (String.String
                             (Coq_x66, (String.String (Coq_x20,
                             (String.String (Coq_x63, (String.String
                             (Coq_x61, (String.String (Coq_x73,
                             (String.String (Coq_x65, (String.String
                             (Coq_x20, (String.String (Coq_x62,
                             (String.String (Coq_x72, (String.String
                             (Coq_x61, (String.String (Coq_x6e,
                             (String.String (Coq_x63, (String.String
                             (Coq_x68, (String.String (Coq_x65,
                             (String.String (Coq_x73, (String.String
                             (Coq_x20, (String.String (Coq_x63,
                             (String.String (Coq_x6f, (String.String
                             (Coq_x6d, (String.String (Coq_x70,
                             (String.String (Coq_x61, (String.String
                             (Coq_x72, (String.String (Coq_x65,
                             (String.String (Coq_x64, (String.String
                             (Coq_x20, (String.String (Coq_x74,
                             (String.String (Coq_x6f, (String.String
                             (Coq_x20, (String.String (Coq_x69,
                             (String.String (Coq_x6e, (String.String
                             (Coq_x64, (String.String (Coq_x75,
                             (String.String (Coq_x63, (String.String
                             (Coq_x74, (String.String (Coq_x69,
                             (String.String (Coq_x76, (String.String
                             (Coq_x65,
                             String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                         | p0 :: ctors0 ->
                           let (p1, _) = p0 in
                           let (ctor_name, _) = p1 in
                           bind (Obj.magic coq_Monad_PrettyPrinter) append_nl
                             (fun _ ->
                             bind (Obj.magic coq_Monad_PrettyPrinter)
                               (Obj.magic get_indent) (fun ctor_indent ->
                               bind (Obj.magic coq_Monad_PrettyPrinter)
                                 (push_indent (add ctor_indent indent_size))
                                 (fun _ ->
                                 let ctor_name0 =
                                   get_ctor_name translate
                                     ((fst ind.inductive_mind), ctor_name)
                                 in
                                 bind (Obj.magic coq_Monad_PrettyPrinter)
                                   (match get_infix ctor_name0 with
                                    | Some op ->
                                      print_infix_match_branch
                                        (print_term _UU03a3_ translate h) op
                                        _UU0393_ bctx t1
                                    | None ->
                                      bind
                                        (Obj.magic coq_Monad_PrettyPrinter)
                                        (append
                                          (get_ctor_name translate
                                            ((fst ind.inductive_mind),
                                            ctor_name0))) (fun _ ->
                                        bind
                                          (Obj.magic coq_Monad_PrettyPrinter)
                                          (append
                                            (String.concat String.EmptyString
                                              (map (fun _ -> String.String
                                                (Coq_x20, (String.String
                                                (Coq_x5f,
                                                String.EmptyString))))
                                                (seq O npars)))) (fun _ ->
                                          let rec print_branch bctx0 args _UU0393_0 =
                                            match bctx0 with
                                            | [] ->
                                              bind
                                                (Obj.magic
                                                  coq_Monad_PrettyPrinter)
                                                (append (String.String
                                                  (Coq_x20, (String.String
                                                  (Coq_x2d, (String.String
                                                  (Coq_x3e,
                                                  String.EmptyString)))))))
                                                (fun _ ->
                                                bind
                                                  (Obj.magic
                                                    coq_Monad_PrettyPrinter)
                                                  append_nl (fun _ ->
                                                  print_parenthesized
                                                    (parenthesize_case_branch
                                                      t1)
                                                    (print_term _UU03a3_
                                                      translate h _UU0393_0
                                                      t1)))
                                            | name0 :: bctx1 ->
                                              bind
                                                (Obj.magic
                                                  coq_Monad_PrettyPrinter)
                                                (Obj.magic fresh_ident name0
                                                  _UU0393_0) (fun name1 ->
                                                bind
                                                  (Obj.magic
                                                    coq_Monad_PrettyPrinter)
                                                  (append
                                                    (String.append
                                                      (String.String
                                                      (Coq_x20,
                                                      String.EmptyString))
                                                      name1)) (fun _ ->
                                                  print_branch bctx1
                                                    (name1 :: args)
                                                    (name1 :: _UU0393_0)))
                                          in print_branch (rev bctx) []
                                               _UU0393_))) (fun _ ->
                                   bind (Obj.magic coq_Monad_PrettyPrinter)
                                     pop_indent (fun _ ->
                                     print_branches branches1 ctors0))))))
                    in print_branches branches (Ex.ind_ctors oib)) (fun _ ->
                   pop_indent))))))))
| Coq_tProj (_, _) ->
  printer_fail (String.String (Coq_x74, (String.String (Coq_x50,
    (String.String (Coq_x72, (String.String (Coq_x6f, (String.String
    (Coq_x6a, String.EmptyString))))))))))
| Coq_tFix (mfix, nfix) ->
  bind (Obj.magic coq_Monad_PrettyPrinter)
    (Obj.magic get_current_line_length) (fun let_col ->
    bind (Obj.magic coq_Monad_PrettyPrinter) (push_indent let_col) (fun _ ->
      bind (Obj.magic coq_Monad_PrettyPrinter)
        (monad_fold_left (Obj.magic coq_Monad_PrettyPrinter)
          (fun _UU0393_0 d ->
          bind (Obj.magic coq_Monad_PrettyPrinter)
            (Obj.magic fresh_ident d.dname _UU0393_0) (fun name0 ->
            ret (Obj.magic coq_Monad_PrettyPrinter) (name0 :: _UU0393_0)))
          mfix _UU0393_) (fun _UU0393_0 ->
        let names = rev (firstn (length mfix) _UU0393_0) in
        bind (Obj.magic coq_Monad_PrettyPrinter)
          (append (String.String (Coq_x6c, (String.String (Coq_x65,
            (String.String (Coq_x74, String.EmptyString))))))) (fun _ ->
          bind (Obj.magic coq_Monad_PrettyPrinter)
            (push_indent (add let_col indent_size)) (fun _ ->
            bind (Obj.magic coq_Monad_PrettyPrinter)
              (let rec print_fixes ds names0 =
                 match ds with
                 | [] -> ret (Obj.magic coq_Monad_PrettyPrinter) ()
                 | d :: ds0 ->
                   (match names0 with
                    | [] ->
                      printer_fail (String.String (Coq_x75, (String.String
                        (Coq_x6e, (String.String (Coq_x72, (String.String
                        (Coq_x65, (String.String (Coq_x61, (String.String
                        (Coq_x63, (String.String (Coq_x68, (String.String
                        (Coq_x61, (String.String (Coq_x62, (String.String
                        (Coq_x6c, (String.String (Coq_x65,
                        String.EmptyString))))))))))))))))))))))
                    | name0 :: names1 ->
                      bind (Obj.magic coq_Monad_PrettyPrinter) append_nl
                        (fun _ ->
                        bind (Obj.magic coq_Monad_PrettyPrinter)
                          (print_define_term _UU0393_0 name0 d.dbody
                            (print_term _UU03a3_ translate h)) (fun _ ->
                          print_fixes ds0 names1)))
               in print_fixes mfix names) (fun _ ->
              bind (Obj.magic coq_Monad_PrettyPrinter) pop_indent (fun _ ->
                bind (Obj.magic coq_Monad_PrettyPrinter) append_nl (fun _ ->
                  bind (Obj.magic coq_Monad_PrettyPrinter)
                    (append (String.String (Coq_x69, (String.String (Coq_x6e,
                      String.EmptyString))))) (fun _ ->
                    bind (Obj.magic coq_Monad_PrettyPrinter) append_nl
                      (fun _ ->
                      bind (Obj.magic coq_Monad_PrettyPrinter)
                        (match nth_error names nfix with
                         | Some n -> append n
                         | None ->
                           printer_fail (String.String (Coq_x69,
                             (String.String (Coq_x6e, (String.String
                             (Coq_x76, (String.String (Coq_x61,
                             (String.String (Coq_x6c, (String.String
                             (Coq_x69, (String.String (Coq_x64,
                             (String.String (Coq_x20, (String.String
                             (Coq_x66, (String.String (Coq_x69,
                             (String.String (Coq_x78, (String.String
                             (Coq_x20, (String.String (Coq_x69,
                             (String.String (Coq_x6e, (String.String
                             (Coq_x64, (String.String (Coq_x65,
                             (String.String (Coq_x78,
                             String.EmptyString)))))))))))))))))))))))))))))))))))
                        (fun _ -> pop_indent)))))))))))
| Coq_tCoFix (_, _) ->
  printer_fail (String.String (Coq_x43, (String.String (Coq_x61,
    (String.String (Coq_x6e, (String.String (Coq_x6e, (String.String
    (Coq_x6f, (String.String (Coq_x74, (String.String (Coq_x20,
    (String.String (Coq_x68, (String.String (Coq_x61, (String.String
    (Coq_x6e, (String.String (Coq_x64, (String.String (Coq_x6c,
    (String.String (Coq_x65, (String.String (Coq_x20, (String.String
    (Coq_x63, (String.String (Coq_x6f, (String.String (Coq_x66,
    (String.String (Coq_x69, (String.String (Coq_x78,
    String.EmptyString))))))))))))))))))))))))))))))))))))))
| Coq_tPrim _ ->
  printer_fail (String.String (Coq_x43, (String.String (Coq_x61,
    (String.String (Coq_x6e, (String.String (Coq_x6e, (String.String
    (Coq_x6f, (String.String (Coq_x74, (String.String (Coq_x20,
    (String.String (Coq_x68, (String.String (Coq_x61, (String.String
    (Coq_x6e, (String.String (Coq_x64, (String.String (Coq_x6c,
    (String.String (Coq_x65, (String.String (Coq_x20, (String.String
    (Coq_x43, (String.String (Coq_x6f, (String.String (Coq_x71,
    (String.String (Coq_x20, (String.String (Coq_x70, (String.String
    (Coq_x72, (String.String (Coq_x69, (String.String (Coq_x6d,
    (String.String (Coq_x69, (String.String (Coq_x74, (String.String
    (Coq_x69, (String.String (Coq_x76, (String.String (Coq_x65,
    (String.String (Coq_x20, (String.String (Coq_x74, (String.String
    (Coq_x79, (String.String (Coq_x70, (String.String (Coq_x65,
    (String.String (Coq_x73,
    String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
| Coq_tLazy _ ->
  printer_fail (String.String (Coq_x43, (String.String (Coq_x61,
    (String.String (Coq_x6e, (String.String (Coq_x6e, (String.String
    (Coq_x6f, (String.String (Coq_x74, (String.String (Coq_x20,
    (String.String (Coq_x68, (String.String (Coq_x61, (String.String
    (Coq_x6e, (String.String (Coq_x64, (String.String (Coq_x6c,
    (String.String (Coq_x65, (String.String (Coq_x20, (String.String
    (Coq_x6c, (String.String (Coq_x61, (String.String (Coq_x7a,
    (String.String (Coq_x79,
    String.EmptyString))))))))))))))))))))))))))))))))))))
| Coq_tForce _ ->
  printer_fail (String.String (Coq_x43, (String.String (Coq_x61,
    (String.String (Coq_x6e, (String.String (Coq_x6e, (String.String
    (Coq_x6f, (String.String (Coq_x74, (String.String (Coq_x20,
    (String.String (Coq_x68, (String.String (Coq_x61, (String.String
    (Coq_x6e, (String.String (Coq_x64, (String.String (Coq_x6c,
    (String.String (Coq_x65, (String.String (Coq_x20, (String.String
    (Coq_x66, (String.String (Coq_x6f, (String.String (Coq_x72,
    (String.String (Coq_x63, (String.String (Coq_x65,
    String.EmptyString))))))))))))))))))))))))))))))))))))))

(** val print_constant :
    Ex.global_env -> (kername -> String.t option) -> coq_ElmPrintConfig ->
    kername -> (name list * box_type) -> E.term -> String.t coq_PrettyPrinter **)

let print_constant _UU03a3_ translate h kn type0 body =
  let ml_name = get_fun_name translate h kn in
  bind (Obj.magic coq_Monad_PrettyPrinter)
    (Obj.magic get_current_line_length) (fun name_col ->
    bind (Obj.magic coq_Monad_PrettyPrinter) (Obj.magic push_indent name_col)
      (fun _ ->
      bind (Obj.magic coq_Monad_PrettyPrinter)
        (let (type_vars, ty) = type0 in
         bind (Obj.magic coq_Monad_PrettyPrinter) (Obj.magic append ml_name)
           (fun _ ->
           bind (Obj.magic coq_Monad_PrettyPrinter)
             (Obj.magic append (String.String (Coq_x20, (String.String
               (Coq_x3a, (String.String (Coq_x20, String.EmptyString)))))))
             (fun _ ->
             bind (Obj.magic coq_Monad_PrettyPrinter)
               (monad_fold_left (Obj.magic coq_Monad_PrettyPrinter)
                 (fun _UU0393_ name0 ->
                 bind (Obj.magic coq_Monad_PrettyPrinter)
                   (fresh_ty_arg_name name0 _UU0393_) (fun name1 ->
                   ret (Obj.magic coq_Monad_PrettyPrinter) (name1 :: _UU0393_)))
                 type_vars []) (fun _UU0393_rev ->
               bind (Obj.magic coq_Monad_PrettyPrinter)
                 (Obj.magic print_type _UU03a3_ translate h (rev _UU0393_rev)
                   ty) (fun _ -> Obj.magic append_nl))))) (fun _ ->
        bind (Obj.magic coq_Monad_PrettyPrinter)
          (Obj.magic print_define_term [] ml_name body
            (print_term _UU03a3_ translate h)) (fun _ ->
          bind (Obj.magic coq_Monad_PrettyPrinter) (Obj.magic pop_indent)
            (fun _ -> ret (Obj.magic coq_Monad_PrettyPrinter) ml_name)))))

(** val parenthesize_ind_ctor_ty : box_type -> bool **)

let parenthesize_ind_ctor_ty = function
| TArr (_, _) -> true
| TApp (_, _) -> true
| _ -> false

(** val print_ind_ctor_definition :
    Ex.global_env -> (kername -> String.t option) -> coq_ElmPrintConfig ->
    ident list -> kername -> (name * box_type) list -> unit coq_PrettyPrinter **)

let print_ind_ctor_definition _UU03a3_ translate h _UU0393_ ctor_name data =
  bind (Obj.magic coq_Monad_PrettyPrinter)
    (append (get_ctor_name translate ctor_name)) (fun _ ->
    monad_fold_left (Obj.magic coq_Monad_PrettyPrinter) (fun _ pat ->
      let (_, bty) = pat in
      bind (Obj.magic coq_Monad_PrettyPrinter)
        (append (String.String (Coq_x20, String.EmptyString))) (fun _ ->
        print_parenthesized (parenthesize_ind_ctor_ty bty)
          (print_type _UU03a3_ translate h _UU0393_ bty))) data ())

(** val print_mutual_inductive_body :
    Ex.global_env -> (kername -> String.t option) -> coq_ElmPrintConfig ->
    kername -> mutual_inductive_body -> (kername * String.t) list
    coq_PrettyPrinter **)

let print_mutual_inductive_body _UU03a3_ translate h kn mib =
  bind (Obj.magic coq_Monad_PrettyPrinter)
    (Obj.magic get_current_line_length) (fun col ->
    bind (Obj.magic coq_Monad_PrettyPrinter) (Obj.magic push_indent col)
      (fun _ ->
      bind (Obj.magic coq_Monad_PrettyPrinter)
        (let rec print_ind_bodies l first names =
           match l with
           | [] -> ret (Obj.magic coq_Monad_PrettyPrinter) names
           | oib :: l0 ->
             bind (Obj.magic coq_Monad_PrettyPrinter)
               (if first
                then ret (Obj.magic coq_Monad_PrettyPrinter) ()
                else Obj.magic append_nl) (fun _ ->
               bind (Obj.magic coq_Monad_PrettyPrinter)
                 (monad_fold_left (Obj.magic coq_Monad_PrettyPrinter)
                   (fun _UU0393_ name0 ->
                   bind (Obj.magic coq_Monad_PrettyPrinter)
                     (Obj.magic fresh_ty_arg_name name0.tvar_name _UU0393_)
                     (fun name1 ->
                     ret (Obj.magic coq_Monad_PrettyPrinter)
                       (app _UU0393_ (name1 :: [])))) (Ex.ind_type_vars oib)
                   []) (fun _UU0393_ ->
                 bind (Obj.magic coq_Monad_PrettyPrinter)
                   (Obj.magic append (String.String (Coq_x74, (String.String
                     (Coq_x79, (String.String (Coq_x70, (String.String
                     (Coq_x65, (String.String (Coq_x20,
                     String.EmptyString))))))))))) (fun _ ->
                   let ind_name0 = ((fst kn), oib.ind_name) in
                   let ind_ml_name = get_ty_name translate ind_name0 in
                   bind (Obj.magic coq_Monad_PrettyPrinter)
                     (Obj.magic append ind_ml_name) (fun _ ->
                     bind (Obj.magic coq_Monad_PrettyPrinter)
                       (monad_fold_left (Obj.magic coq_Monad_PrettyPrinter)
                         (fun _ name0 ->
                         Obj.magic append
                           (String.append (String.String (Coq_x20,
                             String.EmptyString)) name0)) _UU0393_ ())
                       (fun _ ->
                       bind (Obj.magic coq_Monad_PrettyPrinter)
                         (Obj.magic push_indent (add col indent_size))
                         (fun _ ->
                         bind (Obj.magic coq_Monad_PrettyPrinter)
                           (let rec print_ind_ctors ctors prefix =
                              match ctors with
                              | [] ->
                                ret (Obj.magic coq_Monad_PrettyPrinter) ()
                              | p :: ctors0 ->
                                let (p0, _) = p in
                                let (name0, data) = p0 in
                                bind (Obj.magic coq_Monad_PrettyPrinter)
                                  (Obj.magic append_nl) (fun _ ->
                                  bind (Obj.magic coq_Monad_PrettyPrinter)
                                    (Obj.magic append
                                      (String.append prefix (String.String
                                        (Coq_x20, String.EmptyString))))
                                    (fun _ ->
                                    bind (Obj.magic coq_Monad_PrettyPrinter)
                                      (Obj.magic print_ind_ctor_definition
                                        _UU03a3_ translate h _UU0393_
                                        ((fst kn), name0) data) (fun _ ->
                                      print_ind_ctors ctors0 (String.String
                                        (Coq_x7c, String.EmptyString)))))
                            in print_ind_ctors oib.ind_ctors (String.String
                                 (Coq_x3d, String.EmptyString))) (fun _ ->
                           bind (Obj.magic coq_Monad_PrettyPrinter)
                             (Obj.magic pop_indent) (fun _ ->
                             print_ind_bodies l0 false ((ind_name0,
                               ind_ml_name) :: names)))))))))
         in print_ind_bodies mib.ind_bodies true []) (fun names ->
        bind (Obj.magic coq_Monad_PrettyPrinter) (Obj.magic pop_indent)
          (fun _ -> ret (Obj.magic coq_Monad_PrettyPrinter) names))))

(** val print_type_alias :
    Ex.global_env -> (kername -> String.t option) -> coq_ElmPrintConfig ->
    kername -> type_var_info list -> box_type -> String.t coq_PrettyPrinter **)

let print_type_alias _UU03a3_ translate h nm tvars bt =
  bind (Obj.magic coq_Monad_PrettyPrinter)
    (Obj.magic append (String.String (Coq_x74, (String.String (Coq_x79,
      (String.String (Coq_x70, (String.String (Coq_x65, (String.String
      (Coq_x20, (String.String (Coq_x61, (String.String (Coq_x6c,
      (String.String (Coq_x69, (String.String (Coq_x61, (String.String
      (Coq_x73, (String.String (Coq_x20,
      String.EmptyString))))))))))))))))))))))) (fun _ ->
    let ty_ml_name = get_ty_name translate nm in
    bind (Obj.magic coq_Monad_PrettyPrinter) (Obj.magic append ty_ml_name)
      (fun _ ->
      bind (Obj.magic coq_Monad_PrettyPrinter)
        (monad_fold_left (Obj.magic coq_Monad_PrettyPrinter)
          (fun _UU0393_ tvar ->
          bind (Obj.magic coq_Monad_PrettyPrinter)
            (fresh_ty_arg_name tvar.tvar_name _UU0393_) (fun name0 ->
            ret (Obj.magic coq_Monad_PrettyPrinter) (name0 :: _UU0393_)))
          tvars []) (fun _UU0393_rev ->
        let _UU0393_ = rev _UU0393_rev in
        bind (Obj.magic coq_Monad_PrettyPrinter)
          (Obj.magic append
            (String.concat String.EmptyString
              (map (fun x ->
                String.append (String.String (Coq_x20, String.EmptyString)) x)
                _UU0393_))) (fun _ ->
          bind (Obj.magic coq_Monad_PrettyPrinter)
            (Obj.magic append (String.String (Coq_x20, (String.String
              (Coq_x3d, (String.String (Coq_x20, String.EmptyString)))))))
            (fun _ ->
            bind (Obj.magic coq_Monad_PrettyPrinter)
              (Obj.magic print_type _UU03a3_ translate h _UU0393_ bt)
              (fun _ -> ret (Obj.magic coq_Monad_PrettyPrinter) ty_ml_name))))))

(** val print_env :
    Ex.global_env -> (kername -> String.t option) -> coq_ElmPrintConfig ->
    (kername * String.t) list coq_PrettyPrinter **)

let print_env _UU03a3_ translate h =
  bind (Obj.magic coq_Monad_PrettyPrinter)
    (monad_iter (Obj.magic coq_Monad_PrettyPrinter) (Obj.magic push_use)
      (map (fun pat ->
        let (y, _) = pat in let (kn, _) = y in get_fun_name translate h kn)
        _UU03a3_)) (fun _ ->
    bind (Obj.magic coq_Monad_PrettyPrinter)
      (Obj.magic get_current_line_length) (fun sig_col ->
      bind (Obj.magic coq_Monad_PrettyPrinter)
        (Obj.magic push_indent sig_col) (fun _ ->
        let _UU03a3_0 =
          filter (fun pat -> let (_, d) = pat in negb (is_empty_type_decl d))
            _UU03a3_
        in
        bind (Obj.magic coq_Monad_PrettyPrinter)
          (let rec f l prefix names =
             match l with
             | [] -> ret (Obj.magic coq_Monad_PrettyPrinter) names
             | p :: l0 ->
               let (p0, decl) = p in
               let (kn, has_deps) = p0 in
               bind (Obj.magic coq_Monad_PrettyPrinter)
                 (if has_deps
                  then (match decl with
                        | ConstantDecl c ->
                          let { cst_type = type0; cst_body = cst_body0 } = c
                          in
                          (match cst_body0 with
                           | Some body ->
                             bind (Obj.magic coq_Monad_PrettyPrinter)
                               (Obj.magic prefix) (fun _ ->
                               bind (Obj.magic coq_Monad_PrettyPrinter)
                                 (Obj.magic print_constant _UU03a3_ translate
                                   h kn type0 body) (fun name0 ->
                                 ret (Obj.magic coq_Monad_PrettyPrinter)
                                   ((kn, name0) :: [])))
                           | None ->
                             ret (Obj.magic coq_Monad_PrettyPrinter) [])
                        | InductiveDecl mib ->
                          bind (Obj.magic coq_Monad_PrettyPrinter)
                            (Obj.magic prefix) (fun _ ->
                            print_mutual_inductive_body _UU03a3_ translate h
                              kn mib)
                        | TypeAliasDecl o ->
                          (match o with
                           | Some p1 ->
                             let (tvars, bt) = p1 in
                             bind (Obj.magic coq_Monad_PrettyPrinter)
                               (Obj.magic prefix) (fun _ ->
                               bind (Obj.magic coq_Monad_PrettyPrinter)
                                 (Obj.magic print_type_alias _UU03a3_
                                   translate h kn tvars bt) (fun name0 ->
                                 ret (Obj.magic coq_Monad_PrettyPrinter)
                                   ((kn, name0) :: [])))
                           | None ->
                             ret (Obj.magic coq_Monad_PrettyPrinter) []))
                  else ret (Obj.magic coq_Monad_PrettyPrinter) [])
                 (fun new_names ->
                 Obj.magic f l0
                   (bind (Obj.magic coq_Monad_PrettyPrinter) append_nl
                     (fun _ -> append_nl)) (app new_names names))
           in f (rev _UU03a3_0) (ret coq_Monad_PrettyPrinter ()) [])
          (fun names ->
          bind (Obj.magic coq_Monad_PrettyPrinter) (Obj.magic pop_indent)
            (fun _ -> ret (Obj.magic coq_Monad_PrettyPrinter) names)))))
