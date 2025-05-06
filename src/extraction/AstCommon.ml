open BasicAst
open BinNat
open BinNums
open Byte
open Classes1
open Datatypes
open EAst
open FloatOps
open Kernames
open List0
open MCString
open RandyPrelude
open ReflectEq
open Show
open SpecFloat
open Specif
open Bytestring
open Classes2

type __ = Obj.t

(** val print_name : name -> String.t **)

let print_name = function
| Coq_nAnon ->
  String.String (Coq_x20, (String.String (Coq_x5f, (String.String (Coq_x20,
    String.EmptyString)))))
| Coq_nNamed str -> str

(** val print_inductive : inductive -> String.t **)

let print_inductive i =
  let { inductive_mind = str; inductive_ind = n } = i in
  String.append (String.String (Coq_x28, (String.String (Coq_x69,
    (String.String (Coq_x6e, (String.String (Coq_x64, (String.String
    (Coq_x75, (String.String (Coq_x63, (String.String (Coq_x74,
    (String.String (Coq_x69, (String.String (Coq_x76, (String.String
    (Coq_x65, (String.String (Coq_x3a,
    String.EmptyString))))))))))))))))))))))
    (String.append (string_of_kername str)
      (String.append (String.String (Coq_x3a, String.EmptyString))
        (String.append (nat_to_string n) (String.String (Coq_x29,
          String.EmptyString)))))

(** val inductive_dec : inductive -> inductive -> bool **)

let inductive_dec =
  eq_dec (coq_ReflectEq_EqDec reflect_eq_inductive)

(** val coq_NEq : coq_N coq_Eq **)

let coq_NEq =
  N.eq_dec

type coq_Cnstr = { coq_CnstrNm : String.t; coq_CnstrArity : nat }

type ityp = { itypNm : String.t; itypCnstrs : coq_Cnstr list }

type itypPack = ityp list

type 'trm envClass =
| Coq_ecTrm of 'trm
| Coq_ecTyp of nat * itypPack

(** val ecAx : 'a1 envClass **)

let ecAx =
  Coq_ecTyp (O, [])

type 'trm environ = (kername * 'trm envClass) list

(** val cnstr_Cnstr : constructor_body -> coq_Cnstr **)

let cnstr_Cnstr c =
  { coq_CnstrNm = c.cstr_name; coq_CnstrArity = c.cstr_nargs }

(** val ibody_ityp : one_inductive_body -> ityp **)

let ibody_ityp iib =
  let ctors = map cnstr_Cnstr iib.ind_ctors in
  { itypNm = iib.ind_name; itypCnstrs = ctors }

(** val ibodies_itypPack : one_inductive_body list -> itypPack **)

let ibodies_itypPack ibs =
  map ibody_ityp ibs

type 'trm coq_Program = { main : 'trm; env : 'trm environ }

(** val lookup : kername -> 'a1 environ -> 'a1 envClass option **)

let rec lookup nm = function
| [] -> None
| p0 :: p1 ->
  let (s, ec) = p0 in
  if Kername.reflect_kername nm s then Some ec else lookup nm p1

(** val timePhase : String.t -> ('a1 -> 'a2) -> 'a1 -> 'a2 **)

let timePhase _ f =
  f

type prim_tag =
| Coq_primInt
| Coq_primFloat

type prim_value = __

type primitive = (prim_tag, prim_value) sigT

(** val string_of_specfloat : spec_float -> String.t **)

let string_of_specfloat = function
| S754_zero sign ->
  if sign
  then String.String (Coq_x2d, (String.String (Coq_x30, String.EmptyString)))
  else String.String (Coq_x30, String.EmptyString)
| S754_infinity sign ->
  if sign
  then String.String (Coq_x2d, (String.String (Coq_x49, (String.String
         (Coq_x4e, (String.String (Coq_x46, (String.String (Coq_x49,
         (String.String (Coq_x4e, (String.String (Coq_x49, (String.String
         (Coq_x54, (String.String (Coq_x59,
         String.EmptyString)))))))))))))))))
  else String.String (Coq_x49, (String.String (Coq_x4e, (String.String
         (Coq_x46, (String.String (Coq_x49, (String.String (Coq_x4e,
         (String.String (Coq_x49, (String.String (Coq_x54, (String.String
         (Coq_x59, String.EmptyString)))))))))))))))
| S754_nan ->
  String.String (Coq_x4e, (String.String (Coq_x41, (String.String (Coq_x4e,
    String.EmptyString)))))
| S754_finite (sign, p, z) ->
  let num =
    String.append (string_of_positive p)
      (String.append (String.String (Coq_x70, String.EmptyString))
        (string_of_Z z))
  in
  if sign
  then String.append (String.String (Coq_x2d, String.EmptyString)) num
  else num

(** val string_of_float : Float64.t -> String.t **)

let string_of_float f =
  string_of_specfloat (coq_Prim2SF f)

(** val string_of_prim : primitive -> String.t **)

let string_of_prim p =
  match projT1 p with
  | Coq_primInt -> Obj.magic string_of_prim_int (projT2 p)
  | Coq_primFloat -> Obj.magic string_of_float (projT2 p)
