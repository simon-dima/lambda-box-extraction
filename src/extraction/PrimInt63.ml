open Datatypes

(** val coq_lsl : Uint63.t -> Uint63.t -> Uint63.t **)

let coq_lsl = Uint63.l_sl

(** val coq_lsr : Uint63.t -> Uint63.t -> Uint63.t **)

let coq_lsr = Uint63.l_sr

(** val coq_land : Uint63.t -> Uint63.t -> Uint63.t **)

let coq_land = Uint63.l_and

(** val coq_lor : Uint63.t -> Uint63.t -> Uint63.t **)

let coq_lor = Uint63.l_or

(** val coq_lxor : Uint63.t -> Uint63.t -> Uint63.t **)

let coq_lxor = Uint63.l_xor

(** val add : Uint63.t -> Uint63.t -> Uint63.t **)

let add = Uint63.add

(** val sub : Uint63.t -> Uint63.t -> Uint63.t **)

let sub = Uint63.sub

(** val mulc : Uint63.t -> Uint63.t -> Uint63.t * Uint63.t **)

let mulc = Uint63.mulc

(** val div : Uint63.t -> Uint63.t -> Uint63.t **)

let div = Uint63.div

(** val coq_mod : Uint63.t -> Uint63.t -> Uint63.t **)

let coq_mod = Uint63.rem

(** val eqb : Uint63.t -> Uint63.t -> bool **)

let eqb = Uint63.equal

(** val ltb : Uint63.t -> Uint63.t -> bool **)

let ltb = Uint63.lt

(** val leb : Uint63.t -> Uint63.t -> bool **)

let leb = Uint63.le

(** val addc : Uint63.t -> Uint63.t -> Uint63.t Uint63.carry **)

let addc = Uint63.addc

(** val subc : Uint63.t -> Uint63.t -> Uint63.t Uint63.carry **)

let subc = Uint63.subc

(** val diveucl_21 :
    Uint63.t -> Uint63.t -> Uint63.t -> Uint63.t * Uint63.t **)

let diveucl_21 = Uint63.div21

(** val compare : Uint63.t -> Uint63.t -> comparison **)

let compare = (fun x y -> let c = Uint63.compare x y in if c = 0 then Eq else if c < 0 then Lt else Gt)
