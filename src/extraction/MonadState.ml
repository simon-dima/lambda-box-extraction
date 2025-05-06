
type ('t, 'm) coq_MonadState = { get : 'm; put : ('t -> 'm) }
