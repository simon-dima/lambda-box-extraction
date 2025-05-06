open Monad0
open MonadState

type __ = Obj.t

type ('s, 't) state =
  's -> 't * 's
  (* singleton inductive, whose constructor was mkState *)

val coq_Monad_state : ('a1, __) state coq_Monad

val coq_MonadState_state : ('a1, ('a1, __) state) coq_MonadState
