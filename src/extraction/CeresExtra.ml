open Ascii
open CeresDeserialize
open CeresFormat0
open CeresParserUtils
open CeresString
open Datatypes
open List0
open String0

(** val con6 :
    ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7) -> 'a1 coq_FromSexp ->
    'a2 coq_FromSexp -> 'a3 coq_FromSexp -> 'a4 coq_FromSexp -> 'a5
    coq_FromSexp -> 'a6 coq_FromSexp -> 'a7 coq_FromSexpList **)

let con6 f pa pb pc pd pe pf =
  Deser.fields (S (S (S (S (S (S O))))))
    (Deser.bind_field pa O (S (S (S (S (S (S O)))))) (fun a ->
      Deser.bind_field pb (S O) (S (S (S (S (S (S O)))))) (fun b ->
        Deser.bind_field pc (S (S O)) (S (S (S (S (S (S O)))))) (fun c ->
          Deser.bind_field pd (S (S (S O))) (S (S (S (S (S (S O))))))
            (fun d ->
            Deser.bind_field pe (S (S (S (S O)))) (S (S (S (S (S (S O))))))
              (fun e ->
              Deser.bind_field pf (S (S (S (S (S O))))) (S (S (S (S (S (S
                O)))))) (fun f' ->
                Deser.ret (f a b c d e f') (S (S (S (S (S (S O)))))))))))))

(** val con6_ :
    ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7) -> 'a1 coq_Deserialize ->
    'a2 coq_Deserialize -> 'a3 coq_Deserialize -> 'a4 coq_Deserialize -> 'a5
    coq_Deserialize -> 'a6 coq_Deserialize -> 'a7 coq_FromSexpList **)

let con6_ f h h0 h1 h2 h3 h4 =
  con6 f (_from_sexp h) (_from_sexp h0) (_from_sexp h1) (_from_sexp h2)
    (_from_sexp h3) (_from_sexp h4)

(** val string_of_loc : CeresDeserialize.loc -> string **)

let string_of_loc l =
  comma_sep (map string_of_nat l)

(** val string_of_message : bool -> message -> string **)

let rec string_of_message print_sexp = function
| MsgApp (m1, m2) ->
  let m1_str = string_of_message print_sexp m1 in
  let m2_str = string_of_message print_sexp m2 in append m1_str m2_str
| MsgStr s -> s
| MsgSexp e -> if print_sexp then string_of_sexp e else EmptyString

(** val string_of_error : bool -> bool -> CeresDeserialize.error -> string **)

let string_of_error print_loc print_sexp = function
| ParseError e0 -> pretty_error e0
| DeserError (l, m) ->
  let msg_str = string_of_message print_sexp m in
  if print_loc
  then append msg_str
         (append (String ((Ascii (false, false, false, false, false, true,
           false, false)), (String ((Ascii (true, false, false, false, false,
           true, true, false)), (String ((Ascii (false, false, true, false,
           true, true, true, false)), (String ((Ascii (false, false, false,
           false, false, true, false, false)), (String ((Ascii (false, false,
           true, true, false, true, true, false)), (String ((Ascii (true,
           true, true, true, false, true, true, false)), (String ((Ascii
           (true, true, false, false, false, true, true, false)), (String
           ((Ascii (true, false, false, false, false, true, true, false)),
           (String ((Ascii (false, false, true, false, true, true, true,
           false)), (String ((Ascii (true, false, false, true, false, true,
           true, false)), (String ((Ascii (true, true, true, true, false,
           true, true, false)), (String ((Ascii (false, true, true, true,
           false, true, true, false)), (String ((Ascii (false, false, false,
           false, false, true, false, false)),
           EmptyString)))))))))))))))))))))))))) (string_of_loc l))
  else msg_str
