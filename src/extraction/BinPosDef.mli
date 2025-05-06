open BinNums

module Pos :
 sig
  type mask =
  | IsNul
  | IsPos of positive
  | IsNeg

  val eqb : positive -> positive -> bool
 end
