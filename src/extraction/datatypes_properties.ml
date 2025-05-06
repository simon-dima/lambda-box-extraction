open Common0
open Datatypes0
open Eqtype
open Ssrbool

(** val number_type_beq : number_type -> number_type -> bool **)

let number_type_beq x y =
  match x with
  | T_i32 -> (match y with
              | T_i32 -> true
              | _ -> false)
  | T_i64 -> (match y with
              | T_i64 -> true
              | _ -> false)
  | T_f32 -> (match y with
              | T_f32 -> true
              | _ -> false)
  | T_f64 -> (match y with
              | T_f64 -> true
              | _ -> false)

(** val reference_type_beq : reference_type -> reference_type -> bool **)

let reference_type_beq x y =
  match x with
  | T_funcref -> (match y with
                  | T_funcref -> true
                  | T_externref -> false)
  | T_externref -> (match y with
                    | T_funcref -> false
                    | T_externref -> true)

(** val reference_type_eq_dec : reference_type -> reference_type -> bool **)

let reference_type_eq_dec x y =
  let b = reference_type_beq x y in if b then true else false

(** val reference_type_eqb : reference_type -> reference_type -> bool **)

let reference_type_eqb v1 v2 =
  is_left (reference_type_eq_dec v1 v2)

(** val eqreference_typeP : reference_type Equality.axiom **)

let eqreference_typeP =
  eq_dec_Equality_axiom reference_type_eq_dec

(** val reference_type_eqMixin : reference_type Equality.mixin_of **)

let reference_type_eqMixin =
  { Equality.op = reference_type_eqb; Equality.mixin_of__1 =
    eqreference_typeP }

(** val reference_type_eqType : Equality.coq_type **)

let reference_type_eqType =
  Obj.magic reference_type_eqMixin

(** val internal_vector_type_beq : vector_type -> vector_type -> bool **)

let internal_vector_type_beq _ _ =
  true

(** val value_type_beq : value_type -> value_type -> bool **)

let value_type_beq x y =
  match x with
  | T_num x0 -> (match y with
                 | T_num x1 -> number_type_beq x0 x1
                 | _ -> false)
  | T_vec x0 ->
    (match y with
     | T_vec x1 -> internal_vector_type_beq x0 x1
     | _ -> false)
  | T_ref x0 ->
    (match y with
     | T_ref x1 -> reference_type_beq x0 x1
     | _ -> false)
  | T_bot -> (match y with
              | T_bot -> true
              | _ -> false)

(** val value_type_eq_dec : value_type -> value_type -> bool **)

let value_type_eq_dec x y =
  let b = value_type_beq x y in if b then true else false

(** val value_type_eqb : value_type -> value_type -> bool **)

let value_type_eqb v1 v2 =
  is_left (value_type_eq_dec v1 v2)

(** val eqvalue_typeP : value_type Equality.axiom **)

let eqvalue_typeP =
  eq_dec_Equality_axiom value_type_eq_dec

(** val value_type_eqMixin : value_type Equality.mixin_of **)

let value_type_eqMixin =
  { Equality.op = value_type_eqb; Equality.mixin_of__1 = eqvalue_typeP }

(** val value_type_eqType : Equality.coq_type **)

let value_type_eqType =
  Obj.magic value_type_eqMixin
