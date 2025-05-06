open BinNums
open CeresParserInternal
open CeresParserUtils
open CeresS
open Datatypes
open List0
open String0

(** val parse_sexp : string -> (error, atom sexp_) sum **)

let parse_sexp s =
  let (p0, i) = parse_sexps_ initial_state N0 s in
  let (e, p) = p0 in
  (match rev' i.parser_done with
   | [] ->
     (match e with
      | Some e0 -> Coq_inl e0
      | None ->
        (match eof i p with
         | Coq_inl e0 -> Coq_inl e0
         | Coq_inr l ->
           (match l with
            | [] -> Coq_inl EmptyInput
            | r :: _ -> Coq_inr r)))
   | r :: _ -> Coq_inr r)
