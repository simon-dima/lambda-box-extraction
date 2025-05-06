open ExAst
open ResultMonad
open Bytestring

type 'a coq_Transform = 'a -> ('a, String.t) result

type coq_ExtractTransform = global_env coq_Transform

val compose_transforms : 'a1 coq_Transform list -> 'a1 coq_Transform
