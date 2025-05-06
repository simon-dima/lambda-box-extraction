open Ascii
open BinInt
open BinNat
open CeresParserUtils
open CeresS
open CeresString
open Datatypes
open DecimalString
open List0
open String0

type symbol =
| Open of loc
| Exp of atom sexp_

type escape =
| EscBackslash
| EscNone

type partial_token =
| NoToken
| SimpleToken of loc * string
| StrToken of loc * string * escape
| Comment

type 't parser_state_ = { parser_done : atom sexp_ list;
                          parser_stack : symbol list; parser_cur_token : 
                          't }

val set_cur_token : 'a1 parser_state_ -> 'a2 -> 'a2 parser_state_

type parser_state = partial_token parser_state_

val initial_state : parser_state

val new_sexp :
  atom sexp_ list -> symbol list -> atom sexp_ -> 'a1 -> 'a1 parser_state_

val next_str :
  parser_state -> loc -> string -> escape -> loc -> ascii -> (error,
  parser_state) sum

val _fold_stack :
  atom sexp_ list -> loc -> atom sexp_ list -> symbol list -> (error,
  parser_state) sum

val next' : 'a1 parser_state_ -> loc -> ascii -> (error, parser_state) sum

val next_comment : parser_state -> ascii -> (error, parser_state) sum

val raw_or_num : string -> atom

val next : parser_state -> loc -> ascii -> (error, parser_state) sum

val _done_or_fail :
  atom sexp_ list -> symbol list -> (error, atom sexp_ list) sum

val eof : parser_state -> loc -> (error, atom sexp_ list) sum

val parse_sexps_ :
  parser_state -> loc -> string -> (error option * loc) * parser_state
