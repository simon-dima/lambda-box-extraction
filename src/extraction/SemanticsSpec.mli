open BinNums
open Datatypes
open Malfunction
open Bytestring

type __ = Obj.t

type coq_Pointer =
| Build_Pointer

type pointer = __

type coq_Heap =
| Build_Heap

val coq_CanonicalPointer : coq_Pointer

val coq_CanonicalHeap : coq_Heap

type rec_value =
| RFunc of (Ident.t * t)
| RThunk of pointer
| Bad_recursive_value

type value =
| Block of (Uint63.t * value list)
| Vec of (vector_type * pointer)
| Func of ((value Ident.Map.t * Ident.t) * t)
| RClos of (((value Ident.Map.t * Ident.t list) * rec_value list) * nat)
| Lazy of (value Ident.Map.t * t)
| Coq_value_Int of (inttype * coq_Z)
| Float of Float64.t
| Thunk of pointer
| Coq_fail of String.t
| Coq_not_evaluated
