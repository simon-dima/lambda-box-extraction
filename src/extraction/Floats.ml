open Bits
open Integers

type float = binary64

type float32 = binary32

module Float =
 struct
  (** val to_bits : float -> Int64.int **)

  let to_bits f =
    Int64.repr (bits_of_b64 f)

  (** val of_bits : Int64.int -> float **)

  let of_bits b =
    b64_of_bits (Int64.unsigned b)
 end

module Float32 =
 struct
  (** val to_bits : float32 -> Int.int **)

  let to_bits f =
    Int.repr (bits_of_b32 f)

  (** val of_bits : Int.int -> float32 **)

  let of_bits b =
    b32_of_bits (Int.unsigned b)
 end
