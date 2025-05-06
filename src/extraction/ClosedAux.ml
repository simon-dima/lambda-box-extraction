open Datatypes
open EAst
open ELiftSubst
open List0

(** val decl_closed : global_decl -> bool **)

let decl_closed = function
| ConstantDecl cst ->
  (match cst with
   | Some body -> closedn O body
   | None -> true)
| InductiveDecl _ -> true

(** val env_closed : global_declarations -> bool **)

let env_closed _UU03a3_ =
  forallb (fun x -> decl_closed (snd x)) _UU03a3_
