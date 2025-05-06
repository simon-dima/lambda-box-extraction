open Bool
open Eqtype

let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val eq_dec_Equality_axiom :
    ('a1 -> 'a1 -> bool) -> 'a1 Equality.axiom **)

let eq_dec_Equality_axiom eq_dec x y =
  let _evar_0_ = fun _ -> ReflectT in
  let _evar_0_0 = fun _ -> ReflectF in
  if eq_dec x y then _evar_0_ __ else _evar_0_0 __
