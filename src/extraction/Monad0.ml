
type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

type 'm coq_Monad = { ret : (__ -> __ -> 'm);
                      bind : (__ -> __ -> 'm -> (__ -> 'm) -> 'm) }

(** val ret : 'a1 coq_Monad -> 'a2 -> 'a1 **)

let ret monad x =
  Obj.magic monad.ret __ x

(** val bind : 'a1 coq_Monad -> 'a1 -> ('a2 -> 'a1) -> 'a1 **)

let bind monad x x0 =
  Obj.magic monad.bind __ __ x x0
