open ExAst
open ResultMonad
open Bytestring

type 'a coq_Transform = 'a -> ('a, String.t) result

type coq_ExtractTransform = global_env coq_Transform

(** val compose_transforms : 'a1 coq_Transform list -> 'a1 coq_Transform **)

let rec compose_transforms transforms x =
  match transforms with
  | [] -> Ok x
  | t0 :: transforms0 ->
    (match t0 x with
     | Ok _UU03a3_opt -> compose_transforms transforms0 _UU03a3_opt
     | Err e -> Err e)
