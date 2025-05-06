open Ascii
open Byte
open Datatypes
open List0

type string =
| EmptyString
| String of ascii * string

(** val append : string -> string -> string **)

let rec append s1 s2 =
  match s1 with
  | EmptyString -> s2
  | String (c, s1') -> String (c, (append s1' s2))

(** val length : string -> nat **)

let rec length = function
| EmptyString -> O
| String (_, s') -> S (length s')

(** val string_of_list_ascii : ascii list -> string **)

let rec string_of_list_ascii = function
| [] -> EmptyString
| ch :: s0 -> String (ch, (string_of_list_ascii s0))

(** val string_of_list_byte : byte list -> string **)

let string_of_list_byte s =
  string_of_list_ascii (map ascii_of_byte s)
