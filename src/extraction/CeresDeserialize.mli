open Ascii
open BinInt
open BinNums
open CeresParser
open CeresParserUtils
open CeresS
open CeresString
open CeresUtils
open Datatypes
open List0
open Nat0
open String0

type loc = nat list

type message =
| MsgApp of message * message
| MsgStr of string
| MsgSexp of atom sexp_

val type_error : string -> message -> message

type error =
| ParseError of CeresParserUtils.error
| DeserError of loc * message

type 'a coq_FromSexp = loc -> atom sexp_ -> (error, 'a) sum

type 'a coq_Deserialize = 'a coq_FromSexp

val _from_sexp : 'a1 coq_Deserialize -> 'a1 coq_FromSexp

val from_sexp : 'a1 coq_Deserialize -> atom sexp_ -> (error, 'a1) sum

val from_string : 'a1 coq_Deserialize -> string -> (error, 'a1) sum

type 'a coq_FromSexpList =
  loc -> (message -> message) -> atom sexp_ list -> (error, 'a) sum

type 'a coq_FromSexpListN =
  'a coq_FromSexpList
  (* singleton inductive, whose constructor was Build_FromSexpListN *)

module Deser :
 sig
  val _con :
    string -> (string -> loc -> (error, 'a1) sum) -> (string -> 'a1
    coq_FromSexpList) -> 'a1 coq_FromSexp

  val match_con :
    string -> (string * 'a1) list -> (string * 'a1 coq_FromSexpList) list ->
    'a1 coq_FromSexp

  val fields : nat -> 'a1 coq_FromSexpListN -> 'a1 coq_FromSexpList

  val ret : 'a1 -> nat -> 'a1 coq_FromSexpListN

  val bind_field :
    'a1 coq_FromSexp -> nat -> nat -> ('a1 -> 'a2 coq_FromSexpListN) -> 'a2
    coq_FromSexpListN

  val con1 : ('a1 -> 'a2) -> 'a1 coq_FromSexp -> 'a2 coq_FromSexpList

  val con2 :
    ('a1 -> 'a2 -> 'a3) -> 'a1 coq_FromSexp -> 'a2 coq_FromSexp -> 'a3
    coq_FromSexpList

  val con3 :
    ('a1 -> 'a2 -> 'a3 -> 'a4) -> 'a1 coq_FromSexp -> 'a2 coq_FromSexp -> 'a3
    coq_FromSexp -> 'a4 coq_FromSexpList

  val con4 :
    ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5) -> 'a1 coq_FromSexp -> 'a2 coq_FromSexp
    -> 'a3 coq_FromSexp -> 'a4 coq_FromSexp -> 'a5 coq_FromSexpList

  val con5 :
    ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6) -> 'a1 coq_FromSexp -> 'a2
    coq_FromSexp -> 'a3 coq_FromSexp -> 'a4 coq_FromSexp -> 'a5 coq_FromSexp
    -> 'a6 coq_FromSexpList

  val con1_ : ('a1 -> 'a2) -> 'a1 coq_Deserialize -> 'a2 coq_FromSexpList

  val con2_ :
    ('a1 -> 'a2 -> 'a3) -> 'a1 coq_Deserialize -> 'a2 coq_Deserialize -> 'a3
    coq_FromSexpList

  val con3_ :
    ('a1 -> 'a2 -> 'a3 -> 'a4) -> 'a1 coq_Deserialize -> 'a2 coq_Deserialize
    -> 'a3 coq_Deserialize -> 'a4 coq_FromSexpList

  val con4_ :
    ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5) -> 'a1 coq_Deserialize -> 'a2
    coq_Deserialize -> 'a3 coq_Deserialize -> 'a4 coq_Deserialize -> 'a5
    coq_FromSexpList

  val con5_ :
    ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6) -> 'a1 coq_Deserialize -> 'a2
    coq_Deserialize -> 'a3 coq_Deserialize -> 'a4 coq_Deserialize -> 'a5
    coq_Deserialize -> 'a6 coq_FromSexpList
 end

type 'a coq_SemiIntegral = coq_Z -> 'a option

val from_Z : 'a1 coq_SemiIntegral -> coq_Z -> 'a1 option

val coq_Deserialize_SemiIntegral : 'a1 coq_SemiIntegral -> 'a1 coq_Deserialize

val coq_SemiIntegral_nat : nat coq_SemiIntegral

val coq_Deserialize_bool : bool coq_Deserialize

val coq_Deserialize_option : 'a1 coq_Deserialize -> 'a1 option coq_Deserialize

val coq_Deserialize_prod :
  'a1 coq_Deserialize -> 'a2 coq_Deserialize -> ('a1 * 'a2) coq_Deserialize

val _sexp_to_list :
  'a1 coq_FromSexp -> 'a1 list -> nat -> loc -> atom sexp_ list -> (error,
  'a1 list) sum

val coq_Deserialize_list : 'a1 coq_Deserialize -> 'a1 list coq_Deserialize
