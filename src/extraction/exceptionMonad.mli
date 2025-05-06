open Monad0
open MonadExc
open Bytestring

type __ = Obj.t

type 'a coq_exception =
| Exc of String.t
| Ret of 'a

val ret : 'a1 -> 'a1 coq_exception

val raise : String.t -> 'a1 coq_exception

val bind :
  'a1 coq_exception -> ('a1 -> 'a2 coq_exception) -> 'a2 coq_exception

val exn_monad : __ coq_exception coq_Monad

val catch :
  'a1 coq_exception -> (String.t -> 'a1 coq_exception) -> 'a1 coq_exception

val exn_monad_exc : (String.t, __ coq_exception) coq_MonadExc
