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

(** val type_error : string -> message -> message **)

let type_error tyname msg =
  MsgApp ((MsgStr (String ((Ascii (true, true, false, false, false, true,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), (String ((Ascii (true, false, true, false, true, true,
    true, false)), (String ((Ascii (false, false, true, true, false, true,
    true, false)), (String ((Ascii (false, false, true, false, false, true,
    true, false)), (String ((Ascii (false, false, false, false, false, true,
    false, false)), (String ((Ascii (false, true, true, true, false, true,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), (String ((Ascii (false, false, true, false, true, true,
    true, false)), (String ((Ascii (false, false, false, false, false, true,
    false, false)), (String ((Ascii (false, true, false, false, true, true,
    true, false)), (String ((Ascii (true, false, true, false, false, true,
    true, false)), (String ((Ascii (true, false, false, false, false, true,
    true, false)), (String ((Ascii (false, false, true, false, false, true,
    true, false)), (String ((Ascii (false, false, false, false, false, true,
    false, false)), (String ((Ascii (false, false, true, false, true, true,
    true, false)), (String ((Ascii (true, false, false, true, true, true,
    true, false)), (String ((Ascii (false, false, false, false, true, true,
    true, false)), (String ((Ascii (true, false, true, false, false, true,
    true, false)), (String ((Ascii (false, false, false, false, false, true,
    false, false)), (String ((Ascii (true, true, true, false, false, true,
    false, false)), EmptyString))))))))))))))))))))))))))))))))))))))))))),
    (MsgApp ((MsgStr tyname), (MsgApp ((MsgStr (String ((Ascii (true, true,
    true, false, false, true, false, false)), (String ((Ascii (false, false,
    true, true, false, true, false, false)), (String ((Ascii (false, false,
    false, false, false, true, false, false)), EmptyString))))))), msg)))))

type error =
| ParseError of CeresParserUtils.error
| DeserError of loc * message

type 'a coq_FromSexp = loc -> atom sexp_ -> (error, 'a) sum

type 'a coq_Deserialize = 'a coq_FromSexp

(** val _from_sexp : 'a1 coq_Deserialize -> 'a1 coq_FromSexp **)

let _from_sexp deserialize =
  deserialize

(** val from_sexp : 'a1 coq_Deserialize -> atom sexp_ -> (error, 'a1) sum **)

let from_sexp h =
  _from_sexp h []

(** val from_string : 'a1 coq_Deserialize -> string -> (error, 'a1) sum **)

let from_string h s =
  match parse_sexp s with
  | Coq_inl e -> Coq_inl (ParseError e)
  | Coq_inr x -> from_sexp h x

type 'a coq_FromSexpList =
  loc -> (message -> message) -> atom sexp_ list -> (error, 'a) sum

type 'a coq_FromSexpListN =
  'a coq_FromSexpList
  (* singleton inductive, whose constructor was Build_FromSexpListN *)

module Deser =
 struct
  (** val _con :
      string -> (string -> loc -> (error, 'a1) sum) -> (string -> 'a1
      coq_FromSexpList) -> 'a1 coq_FromSexp **)

  let _con tyname g f l = function
  | Atom_ a ->
    (match a with
     | Raw c -> g c l
     | _ ->
       Coq_inl (DeserError (l,
         (type_error tyname (MsgStr (String ((Ascii (true, false, true,
           false, true, true, true, false)), (String ((Ascii (false, true,
           true, true, false, true, true, false)), (String ((Ascii (true,
           false, true, false, false, true, true, false)), (String ((Ascii
           (false, false, false, true, true, true, true, false)), (String
           ((Ascii (false, false, false, false, true, true, true, false)),
           (String ((Ascii (true, false, true, false, false, true, true,
           false)), (String ((Ascii (true, true, false, false, false, true,
           true, false)), (String ((Ascii (false, false, true, false, true,
           true, true, false)), (String ((Ascii (true, false, true, false,
           false, true, true, false)), (String ((Ascii (false, false, true,
           false, false, true, true, false)), (String ((Ascii (false, false,
           false, false, false, true, false, false)), (String ((Ascii (true,
           false, false, false, false, true, true, false)), (String ((Ascii
           (false, false, true, false, true, true, true, false)), (String
           ((Ascii (true, true, true, true, false, true, true, false)),
           (String ((Ascii (true, false, true, true, false, true, true,
           false)), (String ((Ascii (false, false, false, false, false, true,
           false, false)), (String ((Ascii (false, false, false, true, false,
           true, false, false)), (String ((Ascii (true, false, true, false,
           false, true, true, false)), (String ((Ascii (false, false, false,
           true, true, true, true, false)), (String ((Ascii (false, false,
           false, false, true, true, true, false)), (String ((Ascii (true,
           false, true, false, false, true, true, false)), (String ((Ascii
           (true, true, false, false, false, true, true, false)), (String
           ((Ascii (false, false, true, false, true, true, true, false)),
           (String ((Ascii (true, false, true, false, false, true, true,
           false)), (String ((Ascii (false, false, true, false, false, true,
           true, false)), (String ((Ascii (false, false, false, false, false,
           true, false, false)), (String ((Ascii (false, false, true, true,
           false, true, true, false)), (String ((Ascii (true, false, false,
           true, false, true, true, false)), (String ((Ascii (true, true,
           false, false, true, true, true, false)), (String ((Ascii (false,
           false, true, false, true, true, true, false)), (String ((Ascii
           (false, false, false, false, false, true, false, false)), (String
           ((Ascii (true, true, true, true, false, true, true, false)),
           (String ((Ascii (false, true, false, false, true, true, true,
           false)), (String ((Ascii (false, false, false, false, false, true,
           false, false)), (String ((Ascii (false, true, true, true, false,
           true, true, false)), (String ((Ascii (true, false, true, false,
           true, true, true, false)), (String ((Ascii (false, false, true,
           true, false, true, true, false)), (String ((Ascii (false, false,
           true, true, false, true, true, false)), (String ((Ascii (true,
           false, false, false, false, true, true, false)), (String ((Ascii
           (false, true, false, false, true, true, true, false)), (String
           ((Ascii (true, false, false, true, true, true, true, false)),
           (String ((Ascii (false, false, false, false, false, true, false,
           false)), (String ((Ascii (true, true, false, false, false, true,
           true, false)), (String ((Ascii (true, true, true, true, false,
           true, true, false)), (String ((Ascii (false, true, true, true,
           false, true, true, false)), (String ((Ascii (true, true, false,
           false, true, true, true, false)), (String ((Ascii (false, false,
           true, false, true, true, true, false)), (String ((Ascii (false,
           true, false, false, true, true, true, false)), (String ((Ascii
           (true, false, true, false, true, true, true, false)), (String
           ((Ascii (true, true, false, false, false, true, true, false)),
           (String ((Ascii (false, false, true, false, true, true, true,
           false)), (String ((Ascii (true, true, true, true, false, true,
           true, false)), (String ((Ascii (false, true, false, false, true,
           true, true, false)), (String ((Ascii (false, false, false, false,
           false, true, false, false)), (String ((Ascii (false, true, true,
           true, false, true, true, false)), (String ((Ascii (true, false,
           false, false, false, true, true, false)), (String ((Ascii (true,
           false, true, true, false, true, true, false)), (String ((Ascii
           (true, false, true, false, false, true, true, false)), (String
           ((Ascii (true, false, false, true, false, true, false, false)),
           EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | List xs ->
    (match xs with
     | [] ->
       Coq_inl (DeserError (l,
         (type_error tyname (MsgStr (String ((Ascii (true, false, true,
           false, true, true, true, false)), (String ((Ascii (false, true,
           true, true, false, true, true, false)), (String ((Ascii (true,
           false, true, false, false, true, true, false)), (String ((Ascii
           (false, false, false, true, true, true, true, false)), (String
           ((Ascii (false, false, false, false, true, true, true, false)),
           (String ((Ascii (true, false, true, false, false, true, true,
           false)), (String ((Ascii (true, true, false, false, false, true,
           true, false)), (String ((Ascii (false, false, true, false, true,
           true, true, false)), (String ((Ascii (true, false, true, false,
           false, true, true, false)), (String ((Ascii (false, false, true,
           false, false, true, true, false)), (String ((Ascii (false, false,
           false, false, false, true, false, false)), (String ((Ascii (true,
           false, true, false, false, true, true, false)), (String ((Ascii
           (true, false, true, true, false, true, true, false)), (String
           ((Ascii (false, false, false, false, true, true, true, false)),
           (String ((Ascii (false, false, true, false, true, true, true,
           false)), (String ((Ascii (true, false, false, true, true, true,
           true, false)), (String ((Ascii (false, false, false, false, false,
           true, false, false)), (String ((Ascii (false, false, true, true,
           false, true, true, false)), (String ((Ascii (true, false, false,
           true, false, true, true, false)), (String ((Ascii (true, true,
           false, false, true, true, true, false)), (String ((Ascii (false,
           false, true, false, true, true, true, false)),
           EmptyString))))))))))))))))))))))))))))))))))))))))))))))
     | s :: es ->
       (match s with
        | Atom_ a ->
          (match a with
           | Raw c -> f c l (type_error tyname) es
           | _ ->
             Coq_inl (DeserError ((O :: l),
               (type_error tyname (MsgStr (String ((Ascii (true, false, true,
                 false, true, true, true, false)), (String ((Ascii (false,
                 true, true, true, false, true, true, false)), (String
                 ((Ascii (true, false, true, false, false, true, true,
                 false)), (String ((Ascii (false, false, false, true, true,
                 true, true, false)), (String ((Ascii (false, false, false,
                 false, true, true, true, false)), (String ((Ascii (true,
                 false, true, false, false, true, true, false)), (String
                 ((Ascii (true, true, false, false, false, true, true,
                 false)), (String ((Ascii (false, false, true, false, true,
                 true, true, false)), (String ((Ascii (true, false, true,
                 false, false, true, true, false)), (String ((Ascii (false,
                 false, true, false, false, true, true, false)), (String
                 ((Ascii (false, false, false, false, false, true, false,
                 false)), (String ((Ascii (true, false, false, false, false,
                 true, true, false)), (String ((Ascii (false, false, true,
                 false, true, true, true, false)), (String ((Ascii (true,
                 true, true, true, false, true, true, false)), (String
                 ((Ascii (true, false, true, true, false, true, true,
                 false)), (String ((Ascii (false, false, false, false, false,
                 true, false, false)), (String ((Ascii (false, false, false,
                 true, false, true, false, false)), (String ((Ascii (true,
                 false, true, false, false, true, true, false)), (String
                 ((Ascii (false, false, false, true, true, true, true,
                 false)), (String ((Ascii (false, false, false, false, true,
                 true, true, false)), (String ((Ascii (true, false, true,
                 false, false, true, true, false)), (String ((Ascii (true,
                 true, false, false, false, true, true, false)), (String
                 ((Ascii (false, false, true, false, true, true, true,
                 false)), (String ((Ascii (true, false, true, false, false,
                 true, true, false)), (String ((Ascii (false, false, true,
                 false, false, true, true, false)), (String ((Ascii (false,
                 false, false, false, false, true, false, false)), (String
                 ((Ascii (true, true, false, false, false, true, true,
                 false)), (String ((Ascii (true, true, true, true, false,
                 true, true, false)), (String ((Ascii (false, true, true,
                 true, false, true, true, false)), (String ((Ascii (true,
                 true, false, false, true, true, true, false)), (String
                 ((Ascii (false, false, true, false, true, true, true,
                 false)), (String ((Ascii (false, true, false, false, true,
                 true, true, false)), (String ((Ascii (true, false, true,
                 false, true, true, true, false)), (String ((Ascii (true,
                 true, false, false, false, true, true, false)), (String
                 ((Ascii (false, false, true, false, true, true, true,
                 false)), (String ((Ascii (true, true, true, true, false,
                 true, true, false)), (String ((Ascii (false, true, false,
                 false, true, true, true, false)), (String ((Ascii (false,
                 false, false, false, false, true, false, false)), (String
                 ((Ascii (false, true, true, true, false, true, true,
                 false)), (String ((Ascii (true, false, false, false, false,
                 true, true, false)), (String ((Ascii (true, false, true,
                 true, false, true, true, false)), (String ((Ascii (true,
                 false, true, false, false, true, true, false)), (String
                 ((Ascii (true, false, false, true, false, true, false,
                 false)),
                 EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
        | List _ ->
          Coq_inl (DeserError ((O :: l),
            (type_error tyname (MsgStr (String ((Ascii (true, false, true,
              false, true, true, true, false)), (String ((Ascii (false, true,
              true, true, false, true, true, false)), (String ((Ascii (true,
              false, true, false, false, true, true, false)), (String ((Ascii
              (false, false, false, true, true, true, true, false)), (String
              ((Ascii (false, false, false, false, true, true, true, false)),
              (String ((Ascii (true, false, true, false, false, true, true,
              false)), (String ((Ascii (true, true, false, false, false,
              true, true, false)), (String ((Ascii (false, false, true,
              false, true, true, true, false)), (String ((Ascii (true, false,
              true, false, false, true, true, false)), (String ((Ascii
              (false, false, true, false, false, true, true, false)), (String
              ((Ascii (false, false, false, false, false, true, false,
              false)), (String ((Ascii (true, false, false, false, false,
              true, true, false)), (String ((Ascii (false, false, true,
              false, true, true, true, false)), (String ((Ascii (true, true,
              true, true, false, true, true, false)), (String ((Ascii (true,
              false, true, true, false, true, true, false)), (String ((Ascii
              (false, false, false, false, false, true, false, false)),
              (String ((Ascii (false, false, false, true, false, true, false,
              false)), (String ((Ascii (true, false, true, false, false,
              true, true, false)), (String ((Ascii (false, false, false,
              true, true, true, true, false)), (String ((Ascii (false, false,
              false, false, true, true, true, false)), (String ((Ascii (true,
              false, true, false, false, true, true, false)), (String ((Ascii
              (true, true, false, false, false, true, true, false)), (String
              ((Ascii (false, false, true, false, true, true, true, false)),
              (String ((Ascii (true, false, true, false, false, true, true,
              false)), (String ((Ascii (false, false, true, false, false,
              true, true, false)), (String ((Ascii (false, false, false,
              false, false, true, false, false)), (String ((Ascii (true,
              true, false, false, false, true, true, false)), (String ((Ascii
              (true, true, true, true, false, true, true, false)), (String
              ((Ascii (false, true, true, true, false, true, true, false)),
              (String ((Ascii (true, true, false, false, true, true, true,
              false)), (String ((Ascii (false, false, true, false, true,
              true, true, false)), (String ((Ascii (false, true, false,
              false, true, true, true, false)), (String ((Ascii (true, false,
              true, false, true, true, true, false)), (String ((Ascii (true,
              true, false, false, false, true, true, false)), (String ((Ascii
              (false, false, true, false, true, true, true, false)), (String
              ((Ascii (true, true, true, true, false, true, true, false)),
              (String ((Ascii (false, true, false, false, true, true, true,
              false)), (String ((Ascii (false, false, false, false, false,
              true, false, false)), (String ((Ascii (false, true, true, true,
              false, true, true, false)), (String ((Ascii (true, false,
              false, false, false, true, true, false)), (String ((Ascii
              (true, false, true, true, false, true, true, false)), (String
              ((Ascii (true, false, true, false, false, true, true, false)),
              (String ((Ascii (true, false, false, true, false, true, false,
              false)),
              EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

  (** val match_con :
      string -> (string * 'a1) list -> (string * 'a1 coq_FromSexpList) list
      -> 'a1 coq_FromSexp **)

  let match_con tyname c0 c1 =
    _con tyname (fun c l ->
      let all_con = map fst c0 in
      _find_or eqb_string c c0 (fun x -> Coq_inr x)
        (let msg =
           match all_con with
           | [] ->
             MsgStr (String ((Ascii (true, false, true, false, true, true,
               true, false)), (String ((Ascii (false, true, true, true,
               false, true, true, false)), (String ((Ascii (true, false,
               true, false, false, true, true, false)), (String ((Ascii
               (false, false, false, true, true, true, true, false)), (String
               ((Ascii (false, false, false, false, true, true, true,
               false)), (String ((Ascii (true, false, true, false, false,
               true, true, false)), (String ((Ascii (true, true, false,
               false, false, true, true, false)), (String ((Ascii (false,
               false, true, false, true, true, true, false)), (String ((Ascii
               (true, false, true, false, false, true, true, false)), (String
               ((Ascii (false, false, true, false, false, true, true,
               false)), (String ((Ascii (false, false, false, false, false,
               true, false, false)), (String ((Ascii (true, false, false,
               false, false, true, true, false)), (String ((Ascii (false,
               false, true, false, true, true, true, false)), (String ((Ascii
               (true, true, true, true, false, true, true, false)), (String
               ((Ascii (true, false, true, true, false, true, true, false)),
               (String ((Ascii (false, false, false, false, false, true,
               false, false)), (String ((Ascii (false, false, false, true,
               false, true, false, false)), (String ((Ascii (true, false,
               true, false, false, true, true, false)), (String ((Ascii
               (false, false, false, true, true, true, true, false)), (String
               ((Ascii (false, false, false, false, true, true, true,
               false)), (String ((Ascii (true, false, true, false, false,
               true, true, false)), (String ((Ascii (true, true, false,
               false, false, true, true, false)), (String ((Ascii (false,
               false, true, false, true, true, true, false)), (String ((Ascii
               (true, false, true, false, false, true, true, false)), (String
               ((Ascii (false, false, true, false, false, true, true,
               false)), (String ((Ascii (false, false, false, false, false,
               true, false, false)), (String ((Ascii (false, false, true,
               true, false, true, true, false)), (String ((Ascii (true,
               false, false, true, false, true, true, false)), (String
               ((Ascii (true, true, false, false, true, true, true, false)),
               (String ((Ascii (false, false, true, false, true, true, true,
               false)), (String ((Ascii (true, false, false, true, false,
               true, false, false)),
               EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
           | _ :: _ ->
             MsgApp ((MsgStr (String ((Ascii (true, false, true, false,
               false, true, true, false)), (String ((Ascii (false, false,
               false, true, true, true, true, false)), (String ((Ascii
               (false, false, false, false, true, true, true, false)),
               (String ((Ascii (true, false, true, false, false, true, true,
               false)), (String ((Ascii (true, true, false, false, false,
               true, true, false)), (String ((Ascii (false, false, true,
               false, true, true, true, false)), (String ((Ascii (true,
               false, true, false, false, true, true, false)), (String
               ((Ascii (false, false, true, false, false, true, true,
               false)), (String ((Ascii (false, false, false, false, false,
               true, false, false)), (String ((Ascii (false, true, true,
               true, false, true, true, false)), (String ((Ascii (true,
               false, true, false, true, true, true, false)), (String ((Ascii
               (false, false, true, true, false, true, true, false)), (String
               ((Ascii (false, false, true, true, false, true, true, false)),
               (String ((Ascii (true, false, false, false, false, true, true,
               false)), (String ((Ascii (false, true, false, false, true,
               true, true, false)), (String ((Ascii (true, false, false,
               true, true, true, true, false)), (String ((Ascii (false,
               false, false, false, false, true, false, false)), (String
               ((Ascii (true, true, false, false, false, true, true, false)),
               (String ((Ascii (true, true, true, true, false, true, true,
               false)), (String ((Ascii (false, true, true, true, false,
               true, true, false)), (String ((Ascii (true, true, false,
               false, true, true, true, false)), (String ((Ascii (false,
               false, true, false, true, true, true, false)), (String ((Ascii
               (false, true, false, false, true, true, true, false)), (String
               ((Ascii (true, false, true, false, true, true, true, false)),
               (String ((Ascii (true, true, false, false, false, true, true,
               false)), (String ((Ascii (false, false, true, false, true,
               true, true, false)), (String ((Ascii (true, true, true, true,
               false, true, true, false)), (String ((Ascii (false, true,
               false, false, true, true, true, false)), (String ((Ascii
               (false, false, false, false, false, true, false, false)),
               (String ((Ascii (false, true, true, true, false, true, true,
               false)), (String ((Ascii (true, false, false, false, false,
               true, true, false)), (String ((Ascii (true, false, true, true,
               false, true, true, false)), (String ((Ascii (true, false,
               true, false, false, true, true, false)), (String ((Ascii
               (false, false, true, true, false, true, false, false)),
               (String ((Ascii (false, false, false, false, false, true,
               false, false)), (String ((Ascii (true, true, true, true,
               false, true, true, false)), (String ((Ascii (false, true,
               true, true, false, true, true, false)), (String ((Ascii (true,
               false, true, false, false, true, true, false)), (String
               ((Ascii (false, false, false, false, false, true, false,
               false)), (String ((Ascii (true, true, true, true, false, true,
               true, false)), (String ((Ascii (false, true, true, false,
               false, true, true, false)), (String ((Ascii (false, false,
               false, false, false, true, false, false)),
               EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))),
               (MsgApp ((MsgStr (comma_sep all_con)), (MsgApp ((MsgStr
               (String ((Ascii (false, false, true, true, false, true, false,
               false)), (String ((Ascii (false, false, false, false, false,
               true, false, false)), (String ((Ascii (false, true, true,
               false, false, true, true, false)), (String ((Ascii (true,
               true, true, true, false, true, true, false)), (String ((Ascii
               (true, false, true, false, true, true, true, false)), (String
               ((Ascii (false, true, true, true, false, true, true, false)),
               (String ((Ascii (false, false, true, false, false, true, true,
               false)), (String ((Ascii (false, false, false, false, false,
               true, false, false)), EmptyString))))))))))))))))), (MsgStr
               c))))))
         in
         Coq_inl (DeserError (l, (type_error tyname msg)))))
      (fun c l err es ->
      let all_con = map fst c1 in
      _find_or eqb_string c c1 (fun x _ -> x l err es) (fun _ ->
        let msg =
          match all_con with
          | [] ->
            MsgStr (String ((Ascii (true, false, true, false, true, true,
              true, false)), (String ((Ascii (false, true, true, true, false,
              true, true, false)), (String ((Ascii (true, false, true, false,
              false, true, true, false)), (String ((Ascii (false, false,
              false, true, true, true, true, false)), (String ((Ascii (false,
              false, false, false, true, true, true, false)), (String ((Ascii
              (true, false, true, false, false, true, true, false)), (String
              ((Ascii (true, true, false, false, false, true, true, false)),
              (String ((Ascii (false, false, true, false, true, true, true,
              false)), (String ((Ascii (true, false, true, false, false,
              true, true, false)), (String ((Ascii (false, false, true,
              false, false, true, true, false)), (String ((Ascii (false,
              false, false, false, false, true, false, false)), (String
              ((Ascii (true, false, false, false, false, true, true, false)),
              (String ((Ascii (false, false, true, false, true, true, true,
              false)), (String ((Ascii (true, true, true, true, false, true,
              true, false)), (String ((Ascii (true, false, true, true, false,
              true, true, false)), EmptyString))))))))))))))))))))))))))))))
          | _ :: _ ->
            MsgApp ((MsgStr (String ((Ascii (true, false, true, false, false,
              true, true, false)), (String ((Ascii (false, false, false,
              true, true, true, true, false)), (String ((Ascii (false, false,
              false, false, true, true, true, false)), (String ((Ascii (true,
              false, true, false, false, true, true, false)), (String ((Ascii
              (true, true, false, false, false, true, true, false)), (String
              ((Ascii (false, false, true, false, true, true, true, false)),
              (String ((Ascii (true, false, true, false, false, true, true,
              false)), (String ((Ascii (false, false, true, false, false,
              true, true, false)), (String ((Ascii (false, false, false,
              false, false, true, false, false)), (String ((Ascii (true,
              true, false, false, false, true, true, false)), (String ((Ascii
              (true, true, true, true, false, true, true, false)), (String
              ((Ascii (false, true, true, true, false, true, true, false)),
              (String ((Ascii (true, true, false, false, true, true, true,
              false)), (String ((Ascii (false, false, true, false, true,
              true, true, false)), (String ((Ascii (false, true, false,
              false, true, true, true, false)), (String ((Ascii (true, false,
              true, false, true, true, true, false)), (String ((Ascii (true,
              true, false, false, false, true, true, false)), (String ((Ascii
              (false, false, true, false, true, true, true, false)), (String
              ((Ascii (true, true, true, true, false, true, true, false)),
              (String ((Ascii (false, true, false, false, true, true, true,
              false)), (String ((Ascii (false, false, false, false, false,
              true, false, false)), (String ((Ascii (false, true, true, true,
              false, true, true, false)), (String ((Ascii (true, false,
              false, false, false, true, true, false)), (String ((Ascii
              (true, false, true, true, false, true, true, false)), (String
              ((Ascii (true, false, true, false, false, true, true, false)),
              (String ((Ascii (false, false, true, true, false, true, false,
              false)), (String ((Ascii (false, false, false, false, false,
              true, false, false)), (String ((Ascii (true, true, true, true,
              false, true, true, false)), (String ((Ascii (false, true, true,
              true, false, true, true, false)), (String ((Ascii (true, false,
              true, false, false, true, true, false)), (String ((Ascii
              (false, false, false, false, false, true, false, false)),
              (String ((Ascii (true, true, true, true, false, true, true,
              false)), (String ((Ascii (false, true, true, false, false,
              true, true, false)), (String ((Ascii (false, false, false,
              false, false, true, false, false)),
              EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))),
              (MsgApp ((MsgStr (comma_sep all_con)), (MsgApp ((MsgStr (String
              ((Ascii (false, false, true, true, false, true, false, false)),
              (String ((Ascii (false, false, false, false, false, true,
              false, false)), (String ((Ascii (false, true, true, false,
              false, true, true, false)), (String ((Ascii (true, true, true,
              true, false, true, true, false)), (String ((Ascii (true, false,
              true, false, true, true, true, false)), (String ((Ascii (false,
              true, true, true, false, true, true, false)), (String ((Ascii
              (false, false, true, false, false, true, true, false)), (String
              ((Ascii (false, false, false, false, false, true, false,
              false)), EmptyString))))))))))))))))), (MsgStr c))))))
        in
        Coq_inl (DeserError (l, (type_error tyname msg)))) ())

  (** val fields : nat -> 'a1 coq_FromSexpListN -> 'a1 coq_FromSexpList **)

  let fields _ p =
    p

  (** val ret : 'a1 -> nat -> 'a1 coq_FromSexpListN **)

  let ret r n l mk_error es = match es with
  | [] -> Coq_inr r
  | _ :: _ ->
    let msg = MsgApp ((MsgStr (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, false, true,
      true, false, true, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (true, false, false,
      true, true, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, true, true,
      false, false, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false, true,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      false, false, true, true, false)), (String ((Ascii (true, true, false,
      false, true, true, true, false)), (String ((Ascii (false, false, true,
      true, false, true, false, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, true, true, true, true, false)), (String ((Ascii (false,
      false, false, false, true, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (true,
      true, false, false, false, true, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))),
      (MsgApp ((MsgStr (string_of_nat n)), (MsgApp ((MsgStr (String ((Ascii
      (false, false, true, true, false, true, false, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, true, true, false, false, true, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      EmptyString))))))))))))), (MsgStr
      (string_of_nat (add n (Datatypes.length es)))))))))
    in
    Coq_inl (DeserError (l, (mk_error msg)))

  (** val bind_field :
      'a1 coq_FromSexp -> nat -> nat -> ('a1 -> 'a2 coq_FromSexpListN) -> 'a2
      coq_FromSexpListN **)

  let bind_field pa n m f l mk_error = function
  | [] ->
    let msg = MsgApp ((MsgStr (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (true, false, true, false,
      true, true, true, false)), (String ((Ascii (true, true, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false, true,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, true, true,
      false, false, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false, true,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      false, false, true, true, false)), (String ((Ascii (true, true, false,
      false, true, true, true, false)), (String ((Ascii (false, false, true,
      true, false, true, false, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, true, true, true, true, false)), (String ((Ascii (false,
      false, false, false, true, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (true,
      true, false, false, false, true, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))),
      (MsgApp ((MsgStr (string_of_nat m)), (MsgApp ((MsgStr (String ((Ascii
      (false, false, true, true, false, true, false, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, true, true, false, false, true, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      (String ((Ascii (false, true, true, true, false, true, true, false)),
      (String ((Ascii (false, false, true, true, false, true, true, false)),
      (String ((Ascii (true, false, false, true, true, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), EmptyString))))))))))))))))))))))), (MsgStr
      (string_of_nat n)))))))
    in
    Coq_inl (DeserError (l, (mk_error msg)))
  | e :: es0 -> _bind_sum (pa (n :: l) e) (fun a -> f a l mk_error es0)

  (** val con1 : ('a1 -> 'a2) -> 'a1 coq_FromSexp -> 'a2 coq_FromSexpList **)

  let con1 f pa =
    fields (S O) (bind_field pa O (S O) (fun a -> ret (f a) (S O)))

  (** val con2 :
      ('a1 -> 'a2 -> 'a3) -> 'a1 coq_FromSexp -> 'a2 coq_FromSexp -> 'a3
      coq_FromSexpList **)

  let con2 f pa pb =
    fields (S (S O))
      (bind_field pa O (S (S O)) (fun a ->
        bind_field pb (S O) (S (S O)) (fun b -> ret (f a b) (S (S O)))))

  (** val con3 :
      ('a1 -> 'a2 -> 'a3 -> 'a4) -> 'a1 coq_FromSexp -> 'a2 coq_FromSexp ->
      'a3 coq_FromSexp -> 'a4 coq_FromSexpList **)

  let con3 f pa pb pc =
    fields (S (S (S O)))
      (bind_field pa O (S (S (S O))) (fun a ->
        bind_field pb (S O) (S (S (S O))) (fun b ->
          bind_field pc (S (S O)) (S (S (S O))) (fun c ->
            ret (f a b c) (S (S (S O)))))))

  (** val con4 :
      ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5) -> 'a1 coq_FromSexp -> 'a2
      coq_FromSexp -> 'a3 coq_FromSexp -> 'a4 coq_FromSexp -> 'a5
      coq_FromSexpList **)

  let con4 f pa pb pc pd =
    fields (S (S (S (S O))))
      (bind_field pa O (S (S (S (S O)))) (fun a ->
        bind_field pb (S O) (S (S (S (S O)))) (fun b ->
          bind_field pc (S (S O)) (S (S (S (S O)))) (fun c ->
            bind_field pd (S (S (S O))) (S (S (S (S O)))) (fun d ->
              ret (f a b c d) (S (S (S (S O)))))))))

  (** val con5 :
      ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6) -> 'a1 coq_FromSexp -> 'a2
      coq_FromSexp -> 'a3 coq_FromSexp -> 'a4 coq_FromSexp -> 'a5
      coq_FromSexp -> 'a6 coq_FromSexpList **)

  let con5 f pa pb pc pd pe =
    fields (S (S (S (S (S O)))))
      (bind_field pa O (S (S (S (S (S O))))) (fun a ->
        bind_field pb (S O) (S (S (S (S (S O))))) (fun b ->
          bind_field pc (S (S O)) (S (S (S (S (S O))))) (fun c ->
            bind_field pd (S (S (S O))) (S (S (S (S (S O))))) (fun d ->
              bind_field pe (S (S (S (S O)))) (S (S (S (S (S O))))) (fun e ->
                ret (f a b c d e) (S (S (S (S (S O)))))))))))

  (** val con1_ :
      ('a1 -> 'a2) -> 'a1 coq_Deserialize -> 'a2 coq_FromSexpList **)

  let con1_ f h =
    con1 f (_from_sexp h)

  (** val con2_ :
      ('a1 -> 'a2 -> 'a3) -> 'a1 coq_Deserialize -> 'a2 coq_Deserialize ->
      'a3 coq_FromSexpList **)

  let con2_ f h h0 =
    con2 f (_from_sexp h) (_from_sexp h0)

  (** val con3_ :
      ('a1 -> 'a2 -> 'a3 -> 'a4) -> 'a1 coq_Deserialize -> 'a2
      coq_Deserialize -> 'a3 coq_Deserialize -> 'a4 coq_FromSexpList **)

  let con3_ f h h0 h1 =
    con3 f (_from_sexp h) (_from_sexp h0) (_from_sexp h1)

  (** val con4_ :
      ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5) -> 'a1 coq_Deserialize -> 'a2
      coq_Deserialize -> 'a3 coq_Deserialize -> 'a4 coq_Deserialize -> 'a5
      coq_FromSexpList **)

  let con4_ f h h0 h1 h2 =
    con4 f (_from_sexp h) (_from_sexp h0) (_from_sexp h1) (_from_sexp h2)

  (** val con5_ :
      ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6) -> 'a1 coq_Deserialize -> 'a2
      coq_Deserialize -> 'a3 coq_Deserialize -> 'a4 coq_Deserialize -> 'a5
      coq_Deserialize -> 'a6 coq_FromSexpList **)

  let con5_ f h h0 h1 h2 h3 =
    con5 f (_from_sexp h) (_from_sexp h0) (_from_sexp h1) (_from_sexp h2)
      (_from_sexp h3)
 end

type 'a coq_SemiIntegral = coq_Z -> 'a option

(** val from_Z : 'a1 coq_SemiIntegral -> coq_Z -> 'a1 option **)

let from_Z semiIntegral =
  semiIntegral

(** val coq_Deserialize_SemiIntegral :
    'a1 coq_SemiIntegral -> 'a1 coq_Deserialize **)

let coq_Deserialize_SemiIntegral h l e = match e with
| Atom_ a ->
  (match a with
   | Num n ->
     (match from_Z h n with
      | Some a0 -> Coq_inr a0
      | None ->
        Coq_inl (DeserError (l, (MsgApp ((MsgStr (String ((Ascii (true, true,
          false, false, false, true, true, false)), (String ((Ascii (true,
          true, true, true, false, true, true, false)), (String ((Ascii
          (true, false, true, false, true, true, true, false)), (String
          ((Ascii (false, false, true, true, false, true, true, false)),
          (String ((Ascii (false, false, true, false, false, true, true,
          false)), (String ((Ascii (false, false, false, false, false, true,
          false, false)), (String ((Ascii (false, true, true, true, false,
          true, true, false)), (String ((Ascii (true, true, true, true,
          false, true, true, false)), (String ((Ascii (false, false, true,
          false, true, true, true, false)), (String ((Ascii (false, false,
          false, false, false, true, false, false)), (String ((Ascii (false,
          true, false, false, true, true, true, false)), (String ((Ascii
          (true, false, true, false, false, true, true, false)), (String
          ((Ascii (true, false, false, false, false, true, true, false)),
          (String ((Ascii (false, false, true, false, false, true, true,
          false)), (String ((Ascii (false, false, false, false, false, true,
          false, false)), (String ((Ascii (true, false, false, true, false,
          true, true, false)), (String ((Ascii (false, true, true, true,
          false, true, true, false)), (String ((Ascii (false, false, true,
          false, true, true, true, false)), (String ((Ascii (true, false,
          true, false, false, true, true, false)), (String ((Ascii (true,
          true, true, false, false, true, true, false)), (String ((Ascii
          (false, true, false, false, true, true, true, false)), (String
          ((Ascii (true, false, false, false, false, true, true, false)),
          (String ((Ascii (false, false, true, true, false, true, true,
          false)), (String ((Ascii (false, false, false, false, false, true,
          false, false)), (String ((Ascii (false, false, true, false, true,
          true, true, false)), (String ((Ascii (true, false, false, true,
          true, true, true, false)), (String ((Ascii (false, false, false,
          false, true, true, true, false)), (String ((Ascii (true, false,
          true, false, false, true, true, false)), (String ((Ascii (false,
          false, true, true, false, true, false, false)), (String ((Ascii
          (false, false, false, false, false, true, false, false)), (String
          ((Ascii (true, false, false, true, false, true, true, false)),
          (String ((Ascii (false, true, true, true, false, true, true,
          false)), (String ((Ascii (false, true, true, false, true, true,
          true, false)), (String ((Ascii (true, false, false, false, false,
          true, true, false)), (String ((Ascii (false, false, true, true,
          false, true, true, false)), (String ((Ascii (true, false, false,
          true, false, true, true, false)), (String ((Ascii (false, false,
          true, false, false, true, true, false)), (String ((Ascii (false,
          false, false, false, false, true, false, false)), (String ((Ascii
          (false, true, true, false, true, true, true, false)), (String
          ((Ascii (true, false, false, false, false, true, true, false)),
          (String ((Ascii (false, false, true, true, false, true, true,
          false)), (String ((Ascii (true, false, true, false, true, true,
          true, false)), (String ((Ascii (true, false, true, false, false,
          true, true, false)), (String ((Ascii (false, false, false, false,
          false, true, false, false)),
          EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))),
          (MsgSexp e))))))
   | _ ->
     Coq_inl (DeserError (l, (MsgApp ((MsgStr (String ((Ascii (true, true,
       false, false, false, true, true, false)), (String ((Ascii (true, true,
       true, true, false, true, true, false)), (String ((Ascii (true, false,
       true, false, true, true, true, false)), (String ((Ascii (false, false,
       true, true, false, true, true, false)), (String ((Ascii (false, false,
       true, false, false, true, true, false)), (String ((Ascii (false,
       false, false, false, false, true, false, false)), (String ((Ascii
       (false, true, true, true, false, true, true, false)), (String ((Ascii
       (true, true, true, true, false, true, true, false)), (String ((Ascii
       (false, false, true, false, true, true, true, false)), (String ((Ascii
       (false, false, false, false, false, true, false, false)), (String
       ((Ascii (false, true, false, false, true, true, true, false)), (String
       ((Ascii (true, false, true, false, false, true, true, false)), (String
       ((Ascii (true, false, false, false, false, true, true, false)),
       (String ((Ascii (false, false, true, false, false, true, true,
       false)), (String ((Ascii (false, false, false, false, false, true,
       false, false)), (String ((Ascii (true, false, false, true, false,
       true, true, false)), (String ((Ascii (false, true, true, true, false,
       true, true, false)), (String ((Ascii (false, false, true, false, true,
       true, true, false)), (String ((Ascii (true, false, true, false, false,
       true, true, false)), (String ((Ascii (true, true, true, false, false,
       true, true, false)), (String ((Ascii (false, true, false, false, true,
       true, true, false)), (String ((Ascii (true, false, false, false,
       false, true, true, false)), (String ((Ascii (false, false, true, true,
       false, true, true, false)), (String ((Ascii (false, false, false,
       false, false, true, false, false)), (String ((Ascii (false, false,
       true, false, true, true, true, false)), (String ((Ascii (true, false,
       false, true, true, true, true, false)), (String ((Ascii (false, false,
       false, false, true, true, true, false)), (String ((Ascii (true, false,
       true, false, false, true, true, false)), (String ((Ascii (false,
       false, true, true, false, true, false, false)), (String ((Ascii
       (false, false, false, false, false, true, false, false)), (String
       ((Ascii (true, true, true, false, false, true, true, false)), (String
       ((Ascii (true, true, true, true, false, true, true, false)), (String
       ((Ascii (false, false, true, false, true, true, true, false)), (String
       ((Ascii (false, false, false, false, false, true, false, false)),
       (String ((Ascii (true, false, false, false, false, true, true,
       false)), (String ((Ascii (false, false, false, false, false, true,
       false, false)), (String ((Ascii (false, true, true, true, false, true,
       true, false)), (String ((Ascii (true, true, true, true, false, true,
       true, false)), (String ((Ascii (false, true, true, true, false, true,
       true, false)), (String ((Ascii (true, false, true, true, false, true,
       false, false)), (String ((Ascii (false, true, true, true, false,
       false, true, false)), (String ((Ascii (true, false, true, false, true,
       true, true, false)), (String ((Ascii (true, false, true, true, false,
       true, true, false)), (String ((Ascii (false, false, false, false,
       false, true, false, false)), (String ((Ascii (true, false, false,
       false, false, true, true, false)), (String ((Ascii (false, false,
       true, false, true, true, true, false)), (String ((Ascii (true, true,
       true, true, false, true, true, false)), (String ((Ascii (true, false,
       true, true, false, true, true, false)), (String ((Ascii (false, false,
       false, false, false, true, false, false)),
       EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))),
       (MsgSexp e))))))
| List _ ->
  Coq_inl (DeserError (l, (MsgStr (String ((Ascii (true, true, false, false,
    false, true, true, false)), (String ((Ascii (true, true, true, true,
    false, true, true, false)), (String ((Ascii (true, false, true, false,
    true, true, true, false)), (String ((Ascii (false, false, true, true,
    false, true, true, false)), (String ((Ascii (false, false, true, false,
    false, true, true, false)), (String ((Ascii (false, false, false, false,
    false, true, false, false)), (String ((Ascii (false, true, true, true,
    false, true, true, false)), (String ((Ascii (true, true, true, true,
    false, true, true, false)), (String ((Ascii (false, false, true, false,
    true, true, true, false)), (String ((Ascii (false, false, false, false,
    false, true, false, false)), (String ((Ascii (false, true, false, false,
    true, true, true, false)), (String ((Ascii (true, false, true, false,
    false, true, true, false)), (String ((Ascii (true, false, false, false,
    false, true, true, false)), (String ((Ascii (false, false, true, false,
    false, true, true, false)), (String ((Ascii (false, false, false, false,
    false, true, false, false)), (String ((Ascii (true, false, false, true,
    false, true, true, false)), (String ((Ascii (false, true, true, true,
    false, true, true, false)), (String ((Ascii (false, false, true, false,
    true, true, true, false)), (String ((Ascii (true, false, true, false,
    false, true, true, false)), (String ((Ascii (true, true, true, false,
    false, true, true, false)), (String ((Ascii (false, true, false, false,
    true, true, true, false)), (String ((Ascii (true, false, false, false,
    false, true, true, false)), (String ((Ascii (false, false, true, true,
    false, true, true, false)), (String ((Ascii (false, false, false, false,
    false, true, false, false)), (String ((Ascii (false, false, true, false,
    true, true, true, false)), (String ((Ascii (true, false, false, true,
    true, true, true, false)), (String ((Ascii (false, false, false, false,
    true, true, true, false)), (String ((Ascii (true, false, true, false,
    false, true, true, false)), (String ((Ascii (false, false, true, true,
    false, true, false, false)), (String ((Ascii (false, false, false, false,
    false, true, false, false)), (String ((Ascii (true, true, true, false,
    false, true, true, false)), (String ((Ascii (true, true, true, true,
    false, true, true, false)), (String ((Ascii (false, false, true, false,
    true, true, true, false)), (String ((Ascii (false, false, false, false,
    false, true, false, false)), (String ((Ascii (true, false, false, false,
    false, true, true, false)), (String ((Ascii (false, false, false, false,
    false, true, false, false)), (String ((Ascii (false, false, true, true,
    false, true, true, false)), (String ((Ascii (true, false, false, true,
    false, true, true, false)), (String ((Ascii (true, true, false, false,
    true, true, true, false)), (String ((Ascii (false, false, true, false,
    true, true, true, false)),
    EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val coq_SemiIntegral_nat : nat coq_SemiIntegral **)

let coq_SemiIntegral_nat n =
  if Z.ltb n Z0 then None else Some (Z.to_nat n)

(** val coq_Deserialize_bool : bool coq_Deserialize **)

let coq_Deserialize_bool =
  Deser.match_con (String ((Ascii (false, true, false, false, false, true,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), (String ((Ascii (false, false, true, true, false, true,
    true, false)), EmptyString)))))))) (((String ((Ascii (false, true, true,
    false, false, true, true, false)), (String ((Ascii (true, false, false,
    false, false, true, true, false)), (String ((Ascii (false, false, true,
    true, false, true, true, false)), (String ((Ascii (true, true, false,
    false, true, true, true, false)), (String ((Ascii (true, false, true,
    false, false, true, true, false)), EmptyString)))))))))),
    false) :: (((String ((Ascii (false, false, true, false, true, true, true,
    false)), (String ((Ascii (false, true, false, false, true, true, true,
    false)), (String ((Ascii (true, false, true, false, true, true, true,
    false)), (String ((Ascii (true, false, true, false, false, true, true,
    false)), EmptyString)))))))), true) :: [])) []

(** val coq_Deserialize_option :
    'a1 coq_Deserialize -> 'a1 option coq_Deserialize **)

let coq_Deserialize_option h =
  Deser.match_con (String ((Ascii (true, true, true, true, false, true, true,
    false)), (String ((Ascii (false, false, false, false, true, true, true,
    false)), (String ((Ascii (false, false, true, false, true, true, true,
    false)), (String ((Ascii (true, false, false, true, false, true, true,
    false)), (String ((Ascii (true, true, true, true, false, true, true,
    false)), (String ((Ascii (false, true, true, true, false, true, true,
    false)), EmptyString)))))))))))) (((String ((Ascii (false, true, true,
    true, false, false, true, false)), (String ((Ascii (true, true, true,
    true, false, true, true, false)), (String ((Ascii (false, true, true,
    true, false, true, true, false)), (String ((Ascii (true, false, true,
    false, false, true, true, false)), EmptyString)))))))), None) :: [])
    (((String ((Ascii (true, true, false, false, true, false, true, false)),
    (String ((Ascii (true, true, true, true, false, true, true, false)),
    (String ((Ascii (true, false, true, true, false, true, true, false)),
    (String ((Ascii (true, false, true, false, false, true, true, false)),
    EmptyString)))))))), (Deser.con1_ (fun x -> Some x) h)) :: [])

(** val coq_Deserialize_prod :
    'a1 coq_Deserialize -> 'a2 coq_Deserialize -> ('a1 * 'a2) coq_Deserialize **)

let coq_Deserialize_prod h h0 l = function
| Atom_ _ ->
  Coq_inl (DeserError (l, (MsgStr (String ((Ascii (true, true, false, false,
    false, true, true, false)), (String ((Ascii (true, true, true, true,
    false, true, true, false)), (String ((Ascii (true, false, true, false,
    true, true, true, false)), (String ((Ascii (false, false, true, true,
    false, true, true, false)), (String ((Ascii (false, false, true, false,
    false, true, true, false)), (String ((Ascii (false, false, false, false,
    false, true, false, false)), (String ((Ascii (false, true, true, true,
    false, true, true, false)), (String ((Ascii (true, true, true, true,
    false, true, true, false)), (String ((Ascii (false, false, true, false,
    true, true, true, false)), (String ((Ascii (false, false, false, false,
    false, true, false, false)), (String ((Ascii (false, true, false, false,
    true, true, true, false)), (String ((Ascii (true, false, true, false,
    false, true, true, false)), (String ((Ascii (true, false, false, false,
    false, true, true, false)), (String ((Ascii (false, false, true, false,
    false, true, true, false)), (String ((Ascii (false, false, false, false,
    false, true, false, false)), (String ((Ascii (true, true, true, false,
    false, true, false, false)), (String ((Ascii (false, false, false, false,
    true, true, true, false)), (String ((Ascii (false, true, false, false,
    true, true, true, false)), (String ((Ascii (true, true, true, true,
    false, true, true, false)), (String ((Ascii (false, false, true, false,
    false, true, true, false)), (String ((Ascii (true, true, true, false,
    false, true, false, false)), (String ((Ascii (false, false, true, true,
    false, true, false, false)), (String ((Ascii (false, false, false, false,
    false, true, false, false)), (String ((Ascii (true, false, true, false,
    false, true, true, false)), (String ((Ascii (false, false, false, true,
    true, true, true, false)), (String ((Ascii (false, false, false, false,
    true, true, true, false)), (String ((Ascii (true, false, true, false,
    false, true, true, false)), (String ((Ascii (true, true, false, false,
    false, true, true, false)), (String ((Ascii (false, false, true, false,
    true, true, true, false)), (String ((Ascii (true, false, true, false,
    false, true, true, false)), (String ((Ascii (false, false, true, false,
    false, true, true, false)), (String ((Ascii (false, false, false, false,
    false, true, false, false)), (String ((Ascii (false, false, true, true,
    false, true, true, false)), (String ((Ascii (true, false, false, true,
    false, true, true, false)), (String ((Ascii (true, true, false, false,
    true, true, true, false)), (String ((Ascii (false, false, true, false,
    true, true, true, false)), (String ((Ascii (false, false, false, false,
    false, true, false, false)), (String ((Ascii (true, true, true, true,
    false, true, true, false)), (String ((Ascii (false, true, true, false,
    false, true, true, false)), (String ((Ascii (false, false, false, false,
    false, true, false, false)), (String ((Ascii (false, false, true, true,
    false, true, true, false)), (String ((Ascii (true, false, true, false,
    false, true, true, false)), (String ((Ascii (false, true, true, true,
    false, true, true, false)), (String ((Ascii (true, true, true, false,
    false, true, true, false)), (String ((Ascii (false, false, true, false,
    true, true, true, false)), (String ((Ascii (false, false, false, true,
    false, true, true, false)), (String ((Ascii (false, false, false, false,
    false, true, false, false)), (String ((Ascii (false, true, false, false,
    true, true, false, false)), (String ((Ascii (false, false, true, true,
    false, true, false, false)), (String ((Ascii (false, false, false, false,
    false, true, false, false)), (String ((Ascii (true, true, true, false,
    false, true, true, false)), (String ((Ascii (true, true, true, true,
    false, true, true, false)), (String ((Ascii (false, false, true, false,
    true, true, true, false)), (String ((Ascii (false, false, false, false,
    false, true, false, false)), (String ((Ascii (true, false, false, false,
    false, true, true, false)), (String ((Ascii (false, false, true, false,
    true, true, true, false)), (String ((Ascii (true, true, true, true,
    false, true, true, false)), (String ((Ascii (true, false, true, true,
    false, true, true, false)),
    EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
| List xs ->
  (match xs with
   | [] ->
     Coq_inl (DeserError (l, (MsgStr (String ((Ascii (true, true, false,
       false, false, true, true, false)), (String ((Ascii (true, true, true,
       true, false, true, true, false)), (String ((Ascii (true, false, true,
       false, true, true, true, false)), (String ((Ascii (false, false, true,
       true, false, true, true, false)), (String ((Ascii (false, false, true,
       false, false, true, true, false)), (String ((Ascii (false, false,
       false, false, false, true, false, false)), (String ((Ascii (false,
       true, true, true, false, true, true, false)), (String ((Ascii (true,
       true, true, true, false, true, true, false)), (String ((Ascii (false,
       false, true, false, true, true, true, false)), (String ((Ascii (false,
       false, false, false, false, true, false, false)), (String ((Ascii
       (false, true, false, false, true, true, true, false)), (String ((Ascii
       (true, false, true, false, false, true, true, false)), (String ((Ascii
       (true, false, false, false, false, true, true, false)), (String
       ((Ascii (false, false, true, false, false, true, true, false)),
       (String ((Ascii (false, false, false, false, false, true, false,
       false)), (String ((Ascii (true, true, true, false, false, true, false,
       false)), (String ((Ascii (false, false, false, false, true, true,
       true, false)), (String ((Ascii (false, true, false, false, true, true,
       true, false)), (String ((Ascii (true, true, true, true, false, true,
       true, false)), (String ((Ascii (false, false, true, false, false,
       true, true, false)), (String ((Ascii (true, true, true, false, false,
       true, false, false)), (String ((Ascii (false, false, true, true,
       false, true, false, false)), (String ((Ascii (false, false, false,
       false, false, true, false, false)), (String ((Ascii (true, false,
       true, false, false, true, true, false)), (String ((Ascii (false,
       false, false, true, true, true, true, false)), (String ((Ascii (false,
       false, false, false, true, true, true, false)), (String ((Ascii (true,
       false, true, false, false, true, true, false)), (String ((Ascii (true,
       true, false, false, false, true, true, false)), (String ((Ascii
       (false, false, true, false, true, true, true, false)), (String ((Ascii
       (true, false, true, false, false, true, true, false)), (String ((Ascii
       (false, false, true, false, false, true, true, false)), (String
       ((Ascii (false, false, false, false, false, true, false, false)),
       (String ((Ascii (false, false, true, true, false, true, true, false)),
       (String ((Ascii (true, false, false, true, false, true, true, false)),
       (String ((Ascii (true, true, false, false, true, true, true, false)),
       (String ((Ascii (false, false, true, false, true, true, true, false)),
       (String ((Ascii (false, false, false, false, false, true, false,
       false)), (String ((Ascii (true, true, true, true, false, true, true,
       false)), (String ((Ascii (false, true, true, false, false, true, true,
       false)), (String ((Ascii (false, false, false, false, false, true,
       false, false)), (String ((Ascii (false, false, true, true, false,
       true, true, false)), (String ((Ascii (true, false, true, false, false,
       true, true, false)), (String ((Ascii (false, true, true, true, false,
       true, true, false)), (String ((Ascii (true, true, true, false, false,
       true, true, false)), (String ((Ascii (false, false, true, false, true,
       true, true, false)), (String ((Ascii (false, false, false, true,
       false, true, true, false)), (String ((Ascii (false, false, false,
       false, false, true, false, false)), (String ((Ascii (false, true,
       false, false, true, true, false, false)), (String ((Ascii (false,
       false, true, true, false, true, false, false)), (String ((Ascii
       (false, false, false, false, false, true, false, false)), (String
       ((Ascii (true, true, true, false, false, true, true, false)), (String
       ((Ascii (true, true, true, true, false, true, true, false)), (String
       ((Ascii (false, false, true, false, true, true, true, false)), (String
       ((Ascii (false, false, false, false, false, true, false, false)),
       (String ((Ascii (false, false, true, true, false, true, true, false)),
       (String ((Ascii (true, false, false, true, false, true, true, false)),
       (String ((Ascii (true, true, false, false, true, true, true, false)),
       (String ((Ascii (false, false, true, false, true, true, true, false)),
       (String ((Ascii (false, false, false, false, false, true, false,
       false)), (String ((Ascii (true, true, true, true, false, true, true,
       false)), (String ((Ascii (false, true, true, false, false, true, true,
       false)), (String ((Ascii (false, false, false, false, false, true,
       false, false)), (String ((Ascii (true, false, false, false, false,
       true, true, false)), (String ((Ascii (false, false, false, false,
       false, true, false, false)), (String ((Ascii (false, false, true,
       false, false, true, true, false)), (String ((Ascii (true, false,
       false, true, false, true, true, false)), (String ((Ascii (false, true,
       true, false, false, true, true, false)), (String ((Ascii (false, true,
       true, false, false, true, true, false)), (String ((Ascii (true, false,
       true, false, false, true, true, false)), (String ((Ascii (false, true,
       false, false, true, true, true, false)), (String ((Ascii (true, false,
       true, false, false, true, true, false)), (String ((Ascii (false, true,
       true, true, false, true, true, false)), (String ((Ascii (false, false,
       true, false, true, true, true, false)), (String ((Ascii (false, false,
       false, false, false, true, false, false)), (String ((Ascii (false,
       false, true, true, false, true, true, false)), (String ((Ascii (true,
       false, true, false, false, true, true, false)), (String ((Ascii
       (false, true, true, true, false, true, true, false)), (String ((Ascii
       (true, true, true, false, false, true, true, false)), (String ((Ascii
       (false, false, true, false, true, true, true, false)), (String ((Ascii
       (false, false, false, true, false, true, true, false)),
       EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
   | e1 :: l0 ->
     (match l0 with
      | [] ->
        Coq_inl (DeserError (l, (MsgStr (String ((Ascii (true, true, false,
          false, false, true, true, false)), (String ((Ascii (true, true,
          true, true, false, true, true, false)), (String ((Ascii (true,
          false, true, false, true, true, true, false)), (String ((Ascii
          (false, false, true, true, false, true, true, false)), (String
          ((Ascii (false, false, true, false, false, true, true, false)),
          (String ((Ascii (false, false, false, false, false, true, false,
          false)), (String ((Ascii (false, true, true, true, false, true,
          true, false)), (String ((Ascii (true, true, true, true, false,
          true, true, false)), (String ((Ascii (false, false, true, false,
          true, true, true, false)), (String ((Ascii (false, false, false,
          false, false, true, false, false)), (String ((Ascii (false, true,
          false, false, true, true, true, false)), (String ((Ascii (true,
          false, true, false, false, true, true, false)), (String ((Ascii
          (true, false, false, false, false, true, true, false)), (String
          ((Ascii (false, false, true, false, false, true, true, false)),
          (String ((Ascii (false, false, false, false, false, true, false,
          false)), (String ((Ascii (true, true, true, false, false, true,
          false, false)), (String ((Ascii (false, false, false, false, true,
          true, true, false)), (String ((Ascii (false, true, false, false,
          true, true, true, false)), (String ((Ascii (true, true, true, true,
          false, true, true, false)), (String ((Ascii (false, false, true,
          false, false, true, true, false)), (String ((Ascii (true, true,
          true, false, false, true, false, false)), (String ((Ascii (false,
          false, true, true, false, true, false, false)), (String ((Ascii
          (false, false, false, false, false, true, false, false)), (String
          ((Ascii (true, false, true, false, false, true, true, false)),
          (String ((Ascii (false, false, false, true, true, true, true,
          false)), (String ((Ascii (false, false, false, false, true, true,
          true, false)), (String ((Ascii (true, false, true, false, false,
          true, true, false)), (String ((Ascii (true, true, false, false,
          false, true, true, false)), (String ((Ascii (false, false, true,
          false, true, true, true, false)), (String ((Ascii (true, false,
          true, false, false, true, true, false)), (String ((Ascii (false,
          false, true, false, false, true, true, false)), (String ((Ascii
          (false, false, false, false, false, true, false, false)), (String
          ((Ascii (false, false, true, true, false, true, true, false)),
          (String ((Ascii (true, false, false, true, false, true, true,
          false)), (String ((Ascii (true, true, false, false, true, true,
          true, false)), (String ((Ascii (false, false, true, false, true,
          true, true, false)), (String ((Ascii (false, false, false, false,
          false, true, false, false)), (String ((Ascii (true, true, true,
          true, false, true, true, false)), (String ((Ascii (false, true,
          true, false, false, true, true, false)), (String ((Ascii (false,
          false, false, false, false, true, false, false)), (String ((Ascii
          (false, false, true, true, false, true, true, false)), (String
          ((Ascii (true, false, true, false, false, true, true, false)),
          (String ((Ascii (false, true, true, true, false, true, true,
          false)), (String ((Ascii (true, true, true, false, false, true,
          true, false)), (String ((Ascii (false, false, true, false, true,
          true, true, false)), (String ((Ascii (false, false, false, true,
          false, true, true, false)), (String ((Ascii (false, false, false,
          false, false, true, false, false)), (String ((Ascii (false, true,
          false, false, true, true, false, false)), (String ((Ascii (false,
          false, true, true, false, true, false, false)), (String ((Ascii
          (false, false, false, false, false, true, false, false)), (String
          ((Ascii (true, true, true, false, false, true, true, false)),
          (String ((Ascii (true, true, true, true, false, true, true,
          false)), (String ((Ascii (false, false, true, false, true, true,
          true, false)), (String ((Ascii (false, false, false, false, false,
          true, false, false)), (String ((Ascii (false, false, true, true,
          false, true, true, false)), (String ((Ascii (true, false, false,
          true, false, true, true, false)), (String ((Ascii (true, true,
          false, false, true, true, true, false)), (String ((Ascii (false,
          false, true, false, true, true, true, false)), (String ((Ascii
          (false, false, false, false, false, true, false, false)), (String
          ((Ascii (true, true, true, true, false, true, true, false)),
          (String ((Ascii (false, true, true, false, false, true, true,
          false)), (String ((Ascii (false, false, false, false, false, true,
          false, false)), (String ((Ascii (true, false, false, false, false,
          true, true, false)), (String ((Ascii (false, false, false, false,
          false, true, false, false)), (String ((Ascii (false, false, true,
          false, false, true, true, false)), (String ((Ascii (true, false,
          false, true, false, true, true, false)), (String ((Ascii (false,
          true, true, false, false, true, true, false)), (String ((Ascii
          (false, true, true, false, false, true, true, false)), (String
          ((Ascii (true, false, true, false, false, true, true, false)),
          (String ((Ascii (false, true, false, false, true, true, true,
          false)), (String ((Ascii (true, false, true, false, false, true,
          true, false)), (String ((Ascii (false, true, true, true, false,
          true, true, false)), (String ((Ascii (false, false, true, false,
          true, true, true, false)), (String ((Ascii (false, false, false,
          false, false, true, false, false)), (String ((Ascii (false, false,
          true, true, false, true, true, false)), (String ((Ascii (true,
          false, true, false, false, true, true, false)), (String ((Ascii
          (false, true, true, true, false, true, true, false)), (String
          ((Ascii (true, true, true, false, false, true, true, false)),
          (String ((Ascii (false, false, true, false, true, true, true,
          false)), (String ((Ascii (false, false, false, true, false, true,
          true, false)),
          EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      | e2 :: l1 ->
        (match l1 with
         | [] ->
           _bind_sum (_from_sexp h (O :: l) e1) (fun a ->
             _bind_sum (_from_sexp h0 ((S O) :: l) e2) (fun b -> Coq_inr (a,
               b)))
         | _ :: _ ->
           Coq_inl (DeserError (l, (MsgStr (String ((Ascii (true, true,
             false, false, false, true, true, false)), (String ((Ascii (true,
             true, true, true, false, true, true, false)), (String ((Ascii
             (true, false, true, false, true, true, true, false)), (String
             ((Ascii (false, false, true, true, false, true, true, false)),
             (String ((Ascii (false, false, true, false, false, true, true,
             false)), (String ((Ascii (false, false, false, false, false,
             true, false, false)), (String ((Ascii (false, true, true, true,
             false, true, true, false)), (String ((Ascii (true, true, true,
             true, false, true, true, false)), (String ((Ascii (false, false,
             true, false, true, true, true, false)), (String ((Ascii (false,
             false, false, false, false, true, false, false)), (String
             ((Ascii (false, true, false, false, true, true, true, false)),
             (String ((Ascii (true, false, true, false, false, true, true,
             false)), (String ((Ascii (true, false, false, false, false,
             true, true, false)), (String ((Ascii (false, false, true, false,
             false, true, true, false)), (String ((Ascii (false, false,
             false, false, false, true, false, false)), (String ((Ascii
             (true, true, true, false, false, true, false, false)), (String
             ((Ascii (false, false, false, false, true, true, true, false)),
             (String ((Ascii (false, true, false, false, true, true, true,
             false)), (String ((Ascii (true, true, true, true, false, true,
             true, false)), (String ((Ascii (false, false, true, false,
             false, true, true, false)), (String ((Ascii (true, true, true,
             false, false, true, false, false)), (String ((Ascii (false,
             false, true, true, false, true, false, false)), (String ((Ascii
             (false, false, false, false, false, true, false, false)),
             (String ((Ascii (true, false, true, false, false, true, true,
             false)), (String ((Ascii (false, false, false, true, true, true,
             true, false)), (String ((Ascii (false, false, false, false,
             true, true, true, false)), (String ((Ascii (true, false, true,
             false, false, true, true, false)), (String ((Ascii (true, true,
             false, false, false, true, true, false)), (String ((Ascii
             (false, false, true, false, true, true, true, false)), (String
             ((Ascii (true, false, true, false, false, true, true, false)),
             (String ((Ascii (false, false, true, false, false, true, true,
             false)), (String ((Ascii (false, false, false, false, false,
             true, false, false)), (String ((Ascii (false, false, true, true,
             false, true, true, false)), (String ((Ascii (true, false, false,
             true, false, true, true, false)), (String ((Ascii (true, true,
             false, false, true, true, true, false)), (String ((Ascii (false,
             false, true, false, true, true, true, false)), (String ((Ascii
             (false, false, false, false, false, true, false, false)),
             (String ((Ascii (true, true, true, true, false, true, true,
             false)), (String ((Ascii (false, true, true, false, false, true,
             true, false)), (String ((Ascii (false, false, false, false,
             false, true, false, false)), (String ((Ascii (false, false,
             true, true, false, true, true, false)), (String ((Ascii (true,
             false, true, false, false, true, true, false)), (String ((Ascii
             (false, true, true, true, false, true, true, false)), (String
             ((Ascii (true, true, true, false, false, true, true, false)),
             (String ((Ascii (false, false, true, false, true, true, true,
             false)), (String ((Ascii (false, false, false, true, false,
             true, true, false)), (String ((Ascii (false, false, false,
             false, false, true, false, false)), (String ((Ascii (false,
             true, false, false, true, true, false, false)), (String ((Ascii
             (false, false, true, true, false, true, false, false)), (String
             ((Ascii (false, false, false, false, false, true, false,
             false)), (String ((Ascii (true, true, true, false, false, true,
             true, false)), (String ((Ascii (true, true, true, true, false,
             true, true, false)), (String ((Ascii (false, false, true, false,
             true, true, true, false)), (String ((Ascii (false, false, false,
             false, false, true, false, false)), (String ((Ascii (false,
             false, true, true, false, true, true, false)), (String ((Ascii
             (true, false, false, true, false, true, true, false)), (String
             ((Ascii (true, true, false, false, true, true, true, false)),
             (String ((Ascii (false, false, true, false, true, true, true,
             false)), (String ((Ascii (false, false, false, false, false,
             true, false, false)), (String ((Ascii (true, true, true, true,
             false, true, true, false)), (String ((Ascii (false, true, true,
             false, false, true, true, false)), (String ((Ascii (false,
             false, false, false, false, true, false, false)), (String
             ((Ascii (true, false, false, false, false, true, true, false)),
             (String ((Ascii (false, false, false, false, false, true, false,
             false)), (String ((Ascii (false, false, true, false, false,
             true, true, false)), (String ((Ascii (true, false, false, true,
             false, true, true, false)), (String ((Ascii (false, true, true,
             false, false, true, true, false)), (String ((Ascii (false, true,
             true, false, false, true, true, false)), (String ((Ascii (true,
             false, true, false, false, true, true, false)), (String ((Ascii
             (false, true, false, false, true, true, true, false)), (String
             ((Ascii (true, false, true, false, false, true, true, false)),
             (String ((Ascii (false, true, true, true, false, true, true,
             false)), (String ((Ascii (false, false, true, false, true, true,
             true, false)), (String ((Ascii (false, false, false, false,
             false, true, false, false)), (String ((Ascii (false, false,
             true, true, false, true, true, false)), (String ((Ascii (true,
             false, true, false, false, true, true, false)), (String ((Ascii
             (false, true, true, true, false, true, true, false)), (String
             ((Ascii (true, true, true, false, false, true, true, false)),
             (String ((Ascii (false, false, true, false, true, true, true,
             false)), (String ((Ascii (false, false, false, true, false,
             true, true, false)),
             EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val _sexp_to_list :
    'a1 coq_FromSexp -> 'a1 list -> nat -> loc -> atom sexp_ list -> (error,
    'a1 list) sum **)

let rec _sexp_to_list pa xs n l = function
| [] -> Coq_inr (rev' xs)
| y :: ys0 ->
  (match pa (n :: l) y with
   | Coq_inl e -> Coq_inl e
   | Coq_inr x -> _sexp_to_list pa (x :: xs) (S n) l ys0)

(** val coq_Deserialize_list :
    'a1 coq_Deserialize -> 'a1 list coq_Deserialize **)

let coq_Deserialize_list h l = function
| Atom_ _ ->
  Coq_inl (DeserError (l, (MsgStr (String ((Ascii (true, true, false, false,
    false, true, true, false)), (String ((Ascii (true, true, true, true,
    false, true, true, false)), (String ((Ascii (true, false, true, false,
    true, true, true, false)), (String ((Ascii (false, false, true, true,
    false, true, true, false)), (String ((Ascii (false, false, true, false,
    false, true, true, false)), (String ((Ascii (false, false, false, false,
    false, true, false, false)), (String ((Ascii (false, true, true, true,
    false, true, true, false)), (String ((Ascii (true, true, true, true,
    false, true, true, false)), (String ((Ascii (false, false, true, false,
    true, true, true, false)), (String ((Ascii (false, false, false, false,
    false, true, false, false)), (String ((Ascii (false, true, false, false,
    true, true, true, false)), (String ((Ascii (true, false, true, false,
    false, true, true, false)), (String ((Ascii (true, false, false, false,
    false, true, true, false)), (String ((Ascii (false, false, true, false,
    false, true, true, false)), (String ((Ascii (false, false, false, false,
    false, true, false, false)), (String ((Ascii (true, true, true, false,
    false, true, false, false)), (String ((Ascii (false, false, true, true,
    false, true, true, false)), (String ((Ascii (true, false, false, true,
    false, true, true, false)), (String ((Ascii (true, true, false, false,
    true, true, true, false)), (String ((Ascii (false, false, true, false,
    true, true, true, false)), (String ((Ascii (true, true, true, false,
    false, true, false, false)), (String ((Ascii (false, false, true, true,
    false, true, false, false)), (String ((Ascii (false, false, false, false,
    false, true, false, false)), (String ((Ascii (true, true, true, false,
    false, true, true, false)), (String ((Ascii (true, true, true, true,
    false, true, true, false)), (String ((Ascii (false, false, true, false,
    true, true, true, false)), (String ((Ascii (false, false, false, false,
    false, true, false, false)), (String ((Ascii (true, false, false, false,
    false, true, true, false)), (String ((Ascii (false, false, true, false,
    true, true, true, false)), (String ((Ascii (true, true, true, true,
    false, true, true, false)), (String ((Ascii (true, false, true, true,
    false, true, true, false)),
    EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
| List es -> _sexp_to_list (_from_sexp h) [] O l es
