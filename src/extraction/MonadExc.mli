
type __ = Obj.t

type ('e, 'm) coq_MonadExc = { raise : (__ -> 'e -> 'm);
                               catch : (__ -> 'm -> ('e -> 'm) -> 'm) }

val raise : ('a1, 'a2) coq_MonadExc -> 'a1 -> 'a2
