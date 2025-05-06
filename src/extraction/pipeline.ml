open AstCommon
open BinNums
open BinPos
open Byte
open Datatypes
open Kernames
open List0
open MCString
open Monad0
open Pipeline_utils
open Bytestring
open CompM
open Compile0

(** val pick_prim_ident :
    positive -> ((((kername * String.t) * bool) * nat) * positive) list ->
    ((((kername * String.t) * bool) * nat) * positive) list * positive **)

let rec pick_prim_ident id = function
| [] -> ([], id)
| p :: prs0 ->
  let (p0, _) = p in
  let next_id0 = Pos.add id Coq_xH in
  let (prs', id') = pick_prim_ident next_id0 prs0 in (((p0, id) :: prs'), id')

(** val find_axioms :
    ((((kername * String.t) * bool) * nat) * positive) list -> kername list
    -> 'a1 environ -> kername list **)

let rec find_axioms prims acc = function
| [] -> acc
| p :: decls ->
  let (kn, d) = p in
  (match d with
   | Coq_ecTrm _ -> find_axioms prims acc decls
   | Coq_ecTyp (n, i) ->
     (match n with
      | O ->
        (match i with
         | [] ->
           (match find (fun prim ->
                    Kername.reflect_kername kn (fst (fst (fst (fst prim)))))
                    prims with
            | Some _ -> find_axioms prims acc decls
            | None -> find_axioms prims (kn :: acc) decls)
         | _ :: _ -> find_axioms prims acc decls)
      | S _ -> find_axioms prims acc decls))

(** val check_axioms :
    ((((kername * String.t) * bool) * nat) * positive) list -> coq_Term
    coq_Program -> unit pipelineM **)

let check_axioms prims p =
  match find_axioms prims [] p.env with
  | [] -> ret (coq_MonadErrorT coq_MonadState) ()
  | k :: l0 ->
    failwith
      (String.append (String.String (Coq_x41, (String.String (Coq_x78,
        (String.String (Coq_x69, (String.String (Coq_x6f, (String.String
        (Coq_x6d, (String.String (Coq_x73, (String.String (Coq_x20,
        (String.String (Coq_x66, (String.String (Coq_x6f, (String.String
        (Coq_x75, (String.String (Coq_x6e, (String.String (Coq_x64,
        (String.String (Coq_x2c, (String.String (Coq_x20, (String.String
        (Coq_x75, (String.String (Coq_x73, (String.String (Coq_x65,
        (String.String (Coq_x20, (String.String (Coq_x45, (String.String
        (Coq_x78, (String.String (Coq_x74, (String.String (Coq_x72,
        (String.String (Coq_x61, (String.String (Coq_x63, (String.String
        (Coq_x74, (String.String (Coq_x20, (String.String (Coq_x43,
        (String.String (Coq_x6f, (String.String (Coq_x6e, (String.String
        (Coq_x73, (String.String (Coq_x74, (String.String (Coq_x61,
        (String.String (Coq_x6e, (String.String (Coq_x74, (String.String
        (Coq_x20, (String.String (Coq_x74, (String.String (Coq_x6f,
        (String.String (Coq_x20, (String.String (Coq_x72, (String.String
        (Coq_x65, (String.String (Coq_x61, (String.String (Coq_x6c,
        (String.String (Coq_x69, (String.String (Coq_x7a, (String.String
        (Coq_x65, (String.String (Coq_x20, (String.String (Coq_x74,
        (String.String (Coq_x68, (String.String (Coq_x65, (String.String
        (Coq_x6d, (String.String (Coq_x20, (String.String (Coq_x69,
        (String.String (Coq_x6e, (String.String (Coq_x20, (String.String
        (Coq_x43, (String.String (Coq_x3a, (String.String (Coq_x20,
        String.EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
        (String.append newline
          (print_list string_of_kername (String.String (Coq_x2c,
            (String.String (Coq_x20, String.EmptyString)))) (k :: l0))))

(** val next_id : positive **)

let next_id =
  Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))
