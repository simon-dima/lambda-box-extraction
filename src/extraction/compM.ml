open Monad0
open Bytestring

type __ = Obj.t

(** val mapM : 'a1 coq_Monad -> ('a2 -> 'a1) -> 'a2 list -> 'a1 **)

let rec mapM h f = function
| [] -> ret h []
| x :: xs ->
  let sx' = f x in
  bind h sx' (fun x' -> bind h (mapM h f xs) (fun xs' -> ret h (x' :: xs')))

(** val sequence : 'a1 coq_Monad -> 'a1 list -> 'a1 **)

let rec sequence h = function
| [] -> ret h []
| x :: xs ->
  bind h x (fun x' -> bind h (sequence h xs) (fun xs' -> ret h (x' :: xs')))

type 'a error =
| Err of String.t
| Ret of 'a

(** val coq_MonadError : __ error coq_Monad **)

let coq_MonadError =
  { ret = (fun _ t0 -> Ret t0); bind = (fun _ _ x m2 ->
    match x with
    | Err t0 -> Err t0
    | Ret t0 -> m2 t0) }

type ('m, 'a) errorT = 'm

(** val coq_MonadErrorT : 'a1 coq_Monad -> ('a1, __) errorT coq_Monad **)

let coq_MonadErrorT hM =
  { ret = (fun _ x -> ret hM (Ret x)); bind = (fun _ _ m f ->
    bind hM m (fun r -> match r with
                        | Err s -> ret hM (Err s)
                        | Ret a -> f a)) }

type ('r, 'w, 'a) state =
  'r -> 'w -> 'a * 'w
  (* singleton inductive, whose constructor was State *)

(** val coq_MonadState : ('a1, 'a2, __) state coq_Monad **)

let coq_MonadState =
  { ret = (fun _ x _ w -> (x, w)); bind = (fun _ _ m f r w ->
    let (a, w') = m r w in f a r w') }

type ('r, 'w, 'a) compM = (('r, 'w, __) state, 'a) errorT

(** val get : ('a1, 'a2, 'a2) compM **)

let get _ w =
  ((Obj.magic (Ret w)), w)

(** val put : 'a2 -> ('a1, 'a2, unit) compM **)

let put w _ _ =
  ((Obj.magic (Ret ())), w)

(** val failwith : String.t -> ('a1, 'a2, 'a3) compM **)

let failwith s _ w =
  ((Obj.magic (Err s)), w)

(** val ask : ('a1, 'a2, 'a1) compM **)

let ask r w =
  ((Obj.magic (Ret r)), w)
