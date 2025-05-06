open BasicAst
open Byte
open Datatypes
open EAst
open EPrimitive
open Kernames
open List0
open MCList
open MCString
open Nat0
open ReflectEq
open Bytestring
open Ssrbool

type value =
| Coq_vClos of ident * term * (ident * value) list
| Coq_vConstruct of inductive * nat * value list
| Coq_vRecClos of (ident * term) list * nat * (ident * value) list
| Coq_vPrim of value prim_val
| Coq_vLazy of term * (ident * value) list

(** val gen_fresh_aux : ident -> String.t list -> nat -> ident **)

let rec gen_fresh_aux na _UU0393_ i = match i with
| O -> na
| S i' ->
  let na' = String.append na (string_of_nat (sub (length _UU0393_) i)) in
  if is_left
       (in_dec (coq_ReflectEq_EqDec IdentOT.reflect_eq_string) na' _UU0393_)
  then gen_fresh_aux na _UU0393_ i'
  else na'

(** val gen_fresh : String.t -> String.t list -> ident **)

let gen_fresh na _UU0393_ =
  gen_fresh_aux na _UU0393_ (length _UU0393_)

(** val gen_many_fresh : String.t list -> name list -> ident list **)

let rec gen_many_fresh _UU0393_ = function
| [] -> []
| y :: nms0 ->
  (match y with
   | Coq_nAnon ->
     let na' =
       gen_fresh (String.String (Coq_x77, (String.String (Coq_x69,
         (String.String (Coq_x6c, (String.String (Coq_x64, (String.String
         (Coq_x63, (String.String (Coq_x61, (String.String (Coq_x72,
         (String.String (Coq_x64, String.EmptyString)))))))))))))))) _UU0393_
     in
     na' :: (gen_many_fresh (na' :: _UU0393_) nms0)
   | Coq_nNamed na ->
     let na' =
       if is_left
            (in_dec (coq_ReflectEq_EqDec IdentOT.reflect_eq_string) na
              _UU0393_)
       then gen_fresh na _UU0393_
       else na
     in
     na' :: (gen_many_fresh (na' :: _UU0393_) nms0))

(** val map_def_name :
    (name -> name) -> ('a1 -> 'a1) -> 'a1 def -> 'a1 def **)

let map_def_name g f d =
  { dname = (g d.dname); dbody = (f d.dbody); rarg = d.rarg }

(** val annotate : ident list -> term -> term **)

let rec annotate s u = match u with
| Coq_tRel n ->
  (match nth_error s n with
   | Some na -> Coq_tVar na
   | None -> Coq_tRel n)
| Coq_tEvar (ev, args) -> Coq_tEvar (ev, (map (annotate s) args))
| Coq_tLambda (na, m) ->
  let na' =
    match na with
    | Coq_nAnon ->
      gen_fresh (String.String (Coq_x77, (String.String (Coq_x69,
        (String.String (Coq_x6c, (String.String (Coq_x64, (String.String
        (Coq_x63, (String.String (Coq_x61, (String.String (Coq_x72,
        (String.String (Coq_x64, String.EmptyString)))))))))))))))) s
    | Coq_nNamed na0 ->
      if is_left
           (in_dec (coq_ReflectEq_EqDec IdentOT.reflect_eq_string) na0 s)
      then gen_fresh na0 s
      else na0
  in
  Coq_tLambda ((Coq_nNamed na'), (annotate (na' :: s) m))
| Coq_tLetIn (na, b, b') ->
  let na' =
    match na with
    | Coq_nAnon ->
      gen_fresh (String.String (Coq_x77, (String.String (Coq_x69,
        (String.String (Coq_x6c, (String.String (Coq_x64, (String.String
        (Coq_x63, (String.String (Coq_x61, (String.String (Coq_x72,
        (String.String (Coq_x64, String.EmptyString)))))))))))))))) s
    | Coq_nNamed na0 ->
      if is_left
           (in_dec (coq_ReflectEq_EqDec IdentOT.reflect_eq_string) na0 s)
      then gen_fresh na0 s
      else na0
  in
  Coq_tLetIn ((Coq_nNamed na'), (annotate s b), (annotate (na' :: s) b'))
| Coq_tApp (u0, v) -> Coq_tApp ((annotate s u0), (annotate s v))
| Coq_tConstruct (ind, i, args) ->
  Coq_tConstruct (ind, i, (map (annotate s) args))
| Coq_tCase (ind, c, brs) ->
  let brs' =
    map (fun br ->
      let nms = gen_many_fresh s (fst br) in
      ((map (fun x -> Coq_nNamed x) nms), (annotate (app nms s) (snd br))))
      brs
  in
  Coq_tCase (ind, (annotate s c), brs')
| Coq_tProj (p, c) -> Coq_tProj (p, (annotate s c))
| Coq_tFix (mfix, idx) ->
  let nms = gen_many_fresh s (map (fun d -> d.dname) mfix) in
  let mfix' =
    map2 (fun d na ->
      map_def_name (fun _ -> Coq_nNamed na)
        (annotate
          (app (List0.rev (gen_many_fresh s (map (fun d0 -> d0.dname) mfix)))
            s)) d) mfix nms
  in
  Coq_tFix (mfix', idx)
| Coq_tCoFix (mfix, idx) ->
  let nms = gen_many_fresh s (map (fun d -> d.dname) mfix) in
  let mfix' =
    map2 (fun d na ->
      map_def_name (fun _ -> Coq_nNamed na)
        (annotate
          (app (List0.rev (gen_many_fresh s (map (fun d0 -> d0.dname) mfix)))
            s)) d) mfix nms
  in
  Coq_tCoFix (mfix', idx)
| Coq_tPrim p -> Coq_tPrim (map_prim (annotate s) p)
| Coq_tLazy t0 -> Coq_tLazy (annotate s t0)
| Coq_tForce t0 -> Coq_tForce (annotate s t0)
| _ -> u

(** val annotate_env :
    ident list -> global_declarations -> (kername * global_decl) list **)

let rec annotate_env _UU0393_ = function
| [] -> []
| d :: _UU03a3_0 ->
  let (na, g) = d in
  (match g with
   | ConstantDecl c ->
     (match c with
      | Some b ->
        (na, (ConstantDecl (Some
          (annotate _UU0393_ b)))) :: (annotate_env _UU0393_ _UU03a3_0)
      | None -> d :: (annotate_env _UU0393_ _UU03a3_0))
   | InductiveDecl _ -> d :: (annotate_env _UU0393_ _UU03a3_0))
