open Archi
open BinNums
open Coqlib0
open Floats
open Integers
open String0

type ident = positive

val ident_eq : positive -> positive -> bool

type typ =
| Tint
| Tfloat
| Tlong
| Tsingle
| Tany32
| Tany64

val coq_Tptr : typ

type rettype =
| Tret of typ
| Tint8signed
| Tint8unsigned
| Tint16signed
| Tint16unsigned
| Tvoid

type calling_convention = { cc_vararg : coq_Z option; cc_unproto : bool;
                            cc_structret : bool }

val cc_default : calling_convention

type signature = { sig_args : typ list; sig_res : rettype;
                   sig_cc : calling_convention }

type memory_chunk =
| Mint8signed
| Mint8unsigned
| Mint16signed
| Mint16unsigned
| Mint32
| Mint64
| Mfloat32
| Mfloat64
| Many32
| Many64

type init_data =
| Init_int8 of Int.int
| Init_int16 of Int.int
| Init_int32 of Int.int
| Init_int64 of Int64.int
| Init_float32 of float32
| Init_float64 of float
| Init_space of coq_Z
| Init_addrof of ident * Ptrofs.int

type 'v globvar = { gvar_info : 'v; gvar_init : init_data list;
                    gvar_readonly : bool; gvar_volatile : bool }

type ('f, 'v) globdef =
| Gfun of 'f
| Gvar of 'v globvar

type ('f, 'v) program = { prog_defs : (ident * ('f, 'v) globdef) list;
                          prog_public : ident list; prog_main : ident }

type external_function =
| EF_external of string * signature
| EF_builtin of string * signature
| EF_runtime of string * signature
| EF_vload of memory_chunk
| EF_vstore of memory_chunk
| EF_malloc
| EF_free
| EF_memcpy of coq_Z * coq_Z
| EF_annot of positive * string * typ list
| EF_annot_val of positive * string * typ
| EF_inline_asm of string * signature * string list
| EF_debug of positive * ident * typ list
