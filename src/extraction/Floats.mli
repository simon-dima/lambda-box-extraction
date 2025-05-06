open Bits
open Integers

type float = binary64

type float32 = binary32

module Float :
 sig
  val to_bits : float -> Int64.int

  val of_bits : Int64.int -> float
 end

module Float32 :
 sig
  val to_bits : float32 -> Int.int

  val of_bits : Int.int -> float32
 end
