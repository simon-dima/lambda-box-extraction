open BinNums
open SpecFloat

type full_float =
| F754_zero of bool
| F754_infinity of bool
| F754_nan of bool * positive
| F754_finite of bool * positive * coq_Z

val coq_SF2FF : spec_float -> full_float

type binary_float =
| B754_zero of bool
| B754_infinity of bool
| B754_nan of bool * positive
| B754_finite of bool * positive * coq_Z

val coq_FF2B : coq_Z -> coq_Z -> full_float -> binary_float
