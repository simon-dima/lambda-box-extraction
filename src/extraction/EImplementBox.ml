open BasicAst
open Byte
open Datatypes
open EAst
open ELiftSubst
open EPrimitive
open EProgram
open Extract
open Kernames
open List0
open MCList
open MCProd
open Bytestring

(** val iBox : term **)

let iBox =
  Coq_tFix (({ E.dname = (Coq_nNamed (String.String (Coq_x72, (String.String
    (Coq_x65, (String.String (Coq_x63, (String.String (Coq_x63,
    (String.String (Coq_x61, (String.String (Coq_x6c, (String.String
    (Coq_x6c, String.EmptyString))))))))))))))); E.dbody = (Coq_tLambda
    (Coq_nAnon, (Coq_tRel (S O)))); E.rarg = O } :: []), O)

(** val implement_box : term -> term **)

let rec implement_box = function
| Coq_tBox -> iBox
| Coq_tEvar (n, l) ->
  Coq_tEvar (n, (map_InP l (fun x0 _ -> implement_box x0)))
| Coq_tLambda (na, t) -> Coq_tLambda (na, (implement_box t))
| Coq_tLetIn (na, b, t) ->
  Coq_tLetIn (na, (implement_box b), (implement_box t))
| Coq_tApp (u, v) -> Coq_tApp ((implement_box u), (implement_box v))
| Coq_tConstruct (ind, n, args) ->
  Coq_tConstruct (ind, n, (map_InP args (fun d _ -> implement_box d)))
| Coq_tCase (indn, c, brs) ->
  let brs' =
    map_InP brs (fun x0 _ -> ((fst x0),
      (lift (S O) (length (fst x0)) (implement_box (snd x0)))))
  in
  Coq_tLetIn ((Coq_nNamed (String.String (Coq_x64, (String.String (Coq_x69,
  (String.String (Coq_x73, (String.String (Coq_x63, (String.String (Coq_x72,
  String.EmptyString))))))))))), (implement_box c), (Coq_tCase (((fst indn),
  O), (Coq_tRel O), brs')))
| Coq_tProj (p, c) ->
  Coq_tProj ({ proj_ind = p.proj_ind; proj_npars = O; proj_arg =
    p.proj_arg }, (implement_box c))
| Coq_tFix (mfix, idx) ->
  let mfix' =
    map_InP mfix (fun d _ -> { E.dname = d.dname; E.dbody =
      (implement_box d.dbody); E.rarg = d.rarg })
  in
  Coq_tFix (mfix', idx)
| Coq_tCoFix (mfix, idx) ->
  let mfix' =
    map_InP mfix (fun d _ -> { E.dname = d.dname; E.dbody =
      (implement_box d.dbody); E.rarg = d.rarg })
  in
  Coq_tCoFix (mfix', idx)
| Coq_tPrim prim -> Coq_tPrim (map_primIn prim (fun x0 _ -> implement_box x0))
| Coq_tLazy t -> Coq_tLazy (implement_box t)
| Coq_tForce t -> Coq_tForce (implement_box t)
| x0 -> x0

(** val implement_box_constant_decl : constant_body -> E.constant_body **)

let implement_box_constant_decl cb =
  option_map implement_box cb

(** val implement_box_decl : global_decl -> global_decl **)

let implement_box_decl = function
| ConstantDecl cb -> ConstantDecl (implement_box_constant_decl cb)
| InductiveDecl idecl -> InductiveDecl idecl

(** val implement_box_env :
    global_declarations -> (kername * global_decl) list **)

let implement_box_env _UU03a3_ =
  map (on_snd implement_box_decl) _UU03a3_

(** val implement_box_program :
    eprogram -> (kername * global_decl) list * term **)

let implement_box_program p =
  ((implement_box_env (fst p)), (implement_box (snd p)))
