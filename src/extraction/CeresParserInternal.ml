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

(** val set_cur_token : 'a1 parser_state_ -> 'a2 -> 'a2 parser_state_ **)

let set_cur_token i u =
  { parser_done = i.parser_done; parser_stack = i.parser_stack;
    parser_cur_token = u }

type parser_state = partial_token parser_state_

(** val initial_state : parser_state **)

let initial_state =
  { parser_done = []; parser_stack = []; parser_cur_token = NoToken }

(** val new_sexp :
    atom sexp_ list -> symbol list -> atom sexp_ -> 'a1 -> 'a1 parser_state_ **)

let new_sexp d s e t =
  match s with
  | [] -> { parser_done = (e :: d); parser_stack = []; parser_cur_token = t }
  | _ :: _ ->
    { parser_done = d; parser_stack = ((Exp e) :: s); parser_cur_token = t }

(** val next_str :
    parser_state -> loc -> string -> escape -> loc -> ascii -> (error,
    parser_state) sum **)

let next_str i p0 tok e p c =
  let { parser_done = d; parser_stack = s; parser_cur_token = _ } = i in
  let ret = fun tok' e' -> Coq_inr { parser_done = d; parser_stack = s;
    parser_cur_token = (StrToken (p0, tok', e')) }
  in
  (match e with
   | EscBackslash ->
     if eqb_ascii (Ascii (false, true, true, true, false, true, true, false))
          c
     then ret (String ((Ascii (false, true, false, true, false, false, false,
            false)), tok)) EscNone
     else if eqb_ascii (Ascii (false, false, true, true, true, false, true,
               false)) c
          then ret (String ((Ascii (false, false, true, true, true, false,
                 true, false)), tok)) EscNone
          else if eqb_ascii (Ascii (false, true, false, false, false, true,
                    false, false)) c
               then ret (String ((Ascii (false, true, false, false, false,
                      true, false, false)), tok)) EscNone
               else Coq_inl (UnknownEscape (p, c))
   | EscNone ->
     if eqb_ascii (Ascii (false, false, true, true, true, false, true,
          false)) c
     then ret tok EscBackslash
     else if eqb_ascii (Ascii (false, true, false, false, false, true, false,
               false)) c
          then Coq_inr
                 (new_sexp d s (Atom_ (Str (string_reverse tok))) NoToken)
          else if is_printable c
               then ret (String (c, tok)) EscNone
               else Coq_inl (InvalidStringChar (c, p)))

(** val _fold_stack :
    atom sexp_ list -> loc -> atom sexp_ list -> symbol list -> (error,
    parser_state) sum **)

let rec _fold_stack d p r = function
| [] -> Coq_inl (UnmatchedClose p)
| s0 :: s1 ->
  (match s0 with
   | Open _ -> Coq_inr (new_sexp d s1 (List r) NoToken)
   | Exp e -> _fold_stack d p (e :: r) s1)

(** val next' :
    'a1 parser_state_ -> loc -> ascii -> (error, parser_state) sum **)

let next' i p c =
  if eqb_ascii (Ascii (false, false, false, true, false, true, false, false))
       c
  then Coq_inr { parser_done = i.parser_done; parser_stack = ((Open
         p) :: i.parser_stack); parser_cur_token = NoToken }
  else if eqb_ascii (Ascii (true, false, false, true, false, true, false,
            false)) c
       then _fold_stack i.parser_done p [] i.parser_stack
       else if eqb_ascii (Ascii (false, true, false, false, false, true,
                 false, false)) c
            then Coq_inr
                   (set_cur_token i (StrToken (p, EmptyString, EscNone)))
            else if eqb_ascii (Ascii (true, true, false, true, true, true,
                      false, false)) c
                 then Coq_inr (set_cur_token i Comment)
                 else if is_whitespace c
                      then Coq_inr (set_cur_token i NoToken)
                      else Coq_inl (InvalidChar (c, p))

(** val next_comment : parser_state -> ascii -> (error, parser_state) sum **)

let next_comment i c =
  if eqb_ascii (Ascii (false, true, false, true, false, false, false, false))
       c
  then Coq_inr { parser_done = i.parser_done; parser_stack = i.parser_stack;
         parser_cur_token = NoToken }
  else Coq_inr i

(** val raw_or_num : string -> atom **)

let raw_or_num s =
  let s0 = string_reverse s in
  (match NilZero.int_of_string s0 with
   | Some n -> Num (Z.of_int n)
   | None -> Raw s0)

(** val next : parser_state -> loc -> ascii -> (error, parser_state) sum **)

let next i p c =
  match i.parser_cur_token with
  | NoToken ->
    if is_atom_char c
    then Coq_inr
           (set_cur_token i (SimpleToken (p, (String (c, EmptyString)))))
    else next' i p c
  | SimpleToken (_, tok) ->
    if is_atom_char c
    then Coq_inr (set_cur_token i (SimpleToken (p, (String (c, tok)))))
    else let i' =
           new_sexp i.parser_done i.parser_stack (Atom_ (raw_or_num tok)) ()
         in
         next' i' p c
  | StrToken (p0, tok, e) -> next_str i p0 tok e p c
  | Comment -> next_comment i c

(** val _done_or_fail :
    atom sexp_ list -> symbol list -> (error, atom sexp_ list) sum **)

let rec _done_or_fail r = function
| [] -> Coq_inr (rev' r)
| s0 :: s1 ->
  (match s0 with
   | Open p -> Coq_inl (UnmatchedOpen p)
   | Exp _ -> _done_or_fail r s1)

(** val eof : parser_state -> loc -> (error, atom sexp_ list) sum **)

let eof i _ =
  match i.parser_cur_token with
  | SimpleToken (_, tok) ->
    let i0 = new_sexp i.parser_done i.parser_stack (Atom_ (raw_or_num tok)) ()
    in
    _done_or_fail i0.parser_done i0.parser_stack
  | StrToken (p0, _, _) -> Coq_inl (UnterminatedString p0)
  | _ -> _done_or_fail i.parser_done i.parser_stack

(** val parse_sexps_ :
    parser_state -> loc -> string -> (error option * loc) * parser_state **)

let rec parse_sexps_ i p = function
| EmptyString -> ((None, p), i)
| String (c, s0) ->
  (match next i p c with
   | Coq_inl e -> (((Some e), p), i)
   | Coq_inr i0 -> parse_sexps_ i0 (N.succ p) s0)
