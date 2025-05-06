open Datatypes

val coq_lsl : Uint63.t -> Uint63.t -> Uint63.t

val coq_lsr : Uint63.t -> Uint63.t -> Uint63.t

val coq_land : Uint63.t -> Uint63.t -> Uint63.t

val coq_lor : Uint63.t -> Uint63.t -> Uint63.t

val coq_lxor : Uint63.t -> Uint63.t -> Uint63.t

val add : Uint63.t -> Uint63.t -> Uint63.t

val sub : Uint63.t -> Uint63.t -> Uint63.t

val mulc : Uint63.t -> Uint63.t -> Uint63.t * Uint63.t

val div : Uint63.t -> Uint63.t -> Uint63.t

val coq_mod : Uint63.t -> Uint63.t -> Uint63.t

val eqb : Uint63.t -> Uint63.t -> bool

val ltb : Uint63.t -> Uint63.t -> bool

val leb : Uint63.t -> Uint63.t -> bool

val addc : Uint63.t -> Uint63.t -> Uint63.t Uint63.carry

val subc : Uint63.t -> Uint63.t -> Uint63.t Uint63.carry

val diveucl_21 : Uint63.t -> Uint63.t -> Uint63.t -> Uint63.t * Uint63.t

val compare : Uint63.t -> Uint63.t -> comparison
