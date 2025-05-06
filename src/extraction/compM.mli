open Monad0
open Bytestring

type __ = Obj.t

val mapM : 'a1 coq_Monad -> ('a2 -> 'a1) -> 'a2 list -> 'a1

val sequence : 'a1 coq_Monad -> 'a1 list -> 'a1

type 'a error =
| Err of String.t
| Ret of 'a

val coq_MonadError : __ error coq_Monad

type ('m, 'a) errorT = 'm

val coq_MonadErrorT : 'a1 coq_Monad -> ('a1, __) errorT coq_Monad

type ('r, 'w, 'a) state =
  'r -> 'w -> 'a * 'w
  (* singleton inductive, whose constructor was State *)

val coq_MonadState : ('a1, 'a2, __) state coq_Monad

type ('r, 'w, 'a) compM = (('r, 'w, __) state, 'a) errorT

val get : ('a1, 'a2, 'a2) compM

val put : 'a2 -> ('a1, 'a2, unit) compM

val failwith : String.t -> ('a1, 'a2, 'a3) compM

val ask : ('a1, 'a2, 'a1) compM
