open Ascii
open BinInt
open BinNums
open BinPos
open Byte
open Byte0
open ByteCompareSpec
open CeresS
open CeresSerialize
open CeresString
open Datatypes
open DecimalString
open FloatOps
open HexadecimalString
open Kernames
open List0
open MCString
open Malfunction
open Nat0
open PeanoNat
open PrimInt63
open Sint63
open SpecFloat
open String0
open Bytestring

val _escape_ident : String.t -> String.t -> String.t

val coq_Serialize_Ident : Ident.t coq_Serialize

val sint_to_Z : Uint63.t -> coq_Z

val coq_Serialize_int : Uint63.t coq_Serialize

val string_of_specfloat : spec_float -> string

val coq_Serialize_numconst : numconst coq_Serialize

val coq_Cons : atom sexp_ -> atom sexp_ -> atom sexp_

val coq_App : atom sexp_ -> atom sexp_ -> atom sexp_

val rawapp : atom sexp_ -> string -> atom sexp_

val coq_Serialize_case : case coq_Serialize

val coq_Serialize_unary_num_op : unary_num_op coq_Serialize

val numtype_to_string : numtype -> string

val vector_type_to_string : vector_type -> string

val coq_Serialize_binary_arith_op : binary_arith_op coq_Serialize

val coq_Serialize_binary_bitwise_op : binary_bitwise_op coq_Serialize

val coq_Serialize_binary_comparison : binary_comparison coq_Serialize

val coq_Serialize_binary_num_op : binary_num_op coq_Serialize

val to_sexp_t : t -> atom sexp_

val to_sexp_binding : binding -> atom sexp_

val coq_Serialize_t : t coq_Serialize

val uncapitalize_char : byte -> byte

val uncapitalize : String.t -> String.t

val encode_name : String.t -> String.t

val bytestring_atom : String.t -> string

val find_prim : Ident.t -> primitives -> string prim_def option

val add_suffix : String.t -> nat -> String.t

val binders : String.t -> nat -> String.t list -> String.t list

val mk_eta_exp : nat -> atom -> atom sexp_

val global_serializer : primitives -> (Ident.t * t option) coq_Serialize

val filter_erased_prims :
  primitives -> (Ident.t * t option) list -> (Ident.t * t option) list

val thename : byte list -> String.t -> String.t

type program_type =
| Standalone
| Shared_lib of String.t * String.t

val shared_lib_register :
  String.t -> String.t -> (String.t * String.t) -> atom sexp_

val coq_Serialize_module :
  primitives -> program_type -> String.t list -> program coq_Serialize
