open Common0
open Datatypes0
open Eqtype
open Ssrbool

val number_type_beq : number_type -> number_type -> bool

val reference_type_beq : reference_type -> reference_type -> bool

val reference_type_eq_dec : reference_type -> reference_type -> bool

val reference_type_eqb : reference_type -> reference_type -> bool

val eqreference_typeP : reference_type Equality.axiom

val reference_type_eqMixin : reference_type Equality.mixin_of

val reference_type_eqType : Equality.coq_type

val internal_vector_type_beq : vector_type -> vector_type -> bool

val value_type_beq : value_type -> value_type -> bool

val value_type_eq_dec : value_type -> value_type -> bool

val value_type_eqb : value_type -> value_type -> bool

val eqvalue_typeP : value_type Equality.axiom

val value_type_eqMixin : value_type Equality.mixin_of

val value_type_eqType : Equality.coq_type
