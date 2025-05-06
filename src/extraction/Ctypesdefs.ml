open BinNums
open Ctypes

(** val tvoid : coq_type **)

let tvoid =
  Tvoid

(** val tptr : coq_type -> coq_type **)

let tptr t =
  Tpointer (t, noattr)

(** val tattr : attr -> coq_type -> coq_type **)

let tattr a = function
| Tint (sz, si, _) -> Tint (sz, si, a)
| Tlong (si, _) -> Tlong (si, a)
| Tfloat (sz, _) -> Tfloat (sz, a)
| Tpointer (elt, _) -> Tpointer (elt, a)
| Tarray (elt, sz, _) -> Tarray (elt, sz, a)
| Tstruct (id, _) -> Tstruct (id, a)
| Tunion (id, _) -> Tunion (id, a)
| x -> x

(** val talignas : coq_N -> coq_type -> coq_type **)

let talignas n ty =
  tattr { attr_volatile = false; attr_alignas = (Some n) } ty
