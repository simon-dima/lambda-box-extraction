open BinNums
open SpecFloat

type full_float =
| F754_zero of bool
| F754_infinity of bool
| F754_nan of bool * positive
| F754_finite of bool * positive * coq_Z

(** val coq_SF2FF : spec_float -> full_float **)

let coq_SF2FF = function
| S754_zero s -> F754_zero s
| S754_infinity s -> F754_infinity s
| S754_nan -> F754_nan (false, Coq_xH)
| S754_finite (s, m, e) -> F754_finite (s, m, e)

type binary_float =
| B754_zero of bool
| B754_infinity of bool
| B754_nan of bool * positive
| B754_finite of bool * positive * coq_Z

(** val coq_FF2B : coq_Z -> coq_Z -> full_float -> binary_float **)

let coq_FF2B _ _ = function
| F754_zero s -> B754_zero s
| F754_infinity s -> B754_infinity s
| F754_nan (b, pl) -> B754_nan (b, pl)
| F754_finite (s, m, e) -> B754_finite (s, m, e)
