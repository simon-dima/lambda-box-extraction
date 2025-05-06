open Datatypes

(** val _find_or :
    ('a1 -> 'a1 -> bool) -> 'a1 -> ('a1 * 'a2) list -> ('a2 -> 'a3) -> 'a3 ->
    'a3 **)

let rec _find_or eqb a xs f b =
  match xs with
  | [] -> b
  | p :: xs0 ->
    let (x, y) = p in if eqb a x then f y else _find_or eqb a xs0 f b

(** val _bind_sum :
    ('a1, 'a2) sum -> ('a2 -> ('a1, 'a3) sum) -> ('a1, 'a3) sum **)

let _bind_sum x f =
  match x with
  | Coq_inl a -> Coq_inl a
  | Coq_inr b -> f b
