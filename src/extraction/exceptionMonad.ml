open Monad0
open MonadExc
open Bytestring

type __ = Obj.t

type 'a coq_exception =
| Exc of String.t
| Ret of 'a

(** val ret : 'a1 -> 'a1 coq_exception **)

let ret x =
  Ret x

(** val raise : String.t -> 'a1 coq_exception **)

let raise str =
  Exc str

(** val bind :
    'a1 coq_exception -> ('a1 -> 'a2 coq_exception) -> 'a2 coq_exception **)

let bind a f =
  match a with
  | Exc str -> Exc str
  | Ret x -> f x

(** val exn_monad : __ coq_exception coq_Monad **)

let exn_monad =
  { Monad0.ret = (fun _ -> ret); Monad0.bind = (fun _ _ -> bind) }

(** val catch :
    'a1 coq_exception -> (String.t -> 'a1 coq_exception) -> 'a1 coq_exception **)

let catch e f =
  match e with
  | Exc s -> f s
  | Ret x -> Ret x

(** val exn_monad_exc : (String.t, __ coq_exception) coq_MonadExc **)

let exn_monad_exc =
  { MonadExc.raise = (fun _ -> raise); MonadExc.catch = (fun _ -> catch) }
