open BinNums
open Coqlib0
open Floats
open Integers

type block = positive

val eq_block : positive -> positive -> bool

type coq_val =
| Vundef
| Vint of Int.int
| Vlong of Int64.int
| Vfloat of float
| Vsingle of float32
| Vptr of block * Ptrofs.int
