open Byte
open Common1
open Datatypes
open Kernames
open List0
open MCList
open Nat0
open ResultMonad
open Bytestring
open Monad_utils

type __ = Obj.t

type coq_PrettyPrinterState = { indent_stack : nat list;
                                used_names : ident list;
                                output_lines : (nat * String.t) list;
                                cur_output_line : (nat * String.t) }

type 'a coq_PrettyPrinter =
  coq_PrettyPrinterState -> ('a * coq_PrettyPrinterState, String.t) result

val coq_Monad_PrettyPrinter : __ coq_PrettyPrinter coq_Monad

val map_pps :
  (nat list -> nat list) -> (ident list -> ident list) -> ((nat * String.t)
  list -> (nat * String.t) list) -> ((nat * String.t) -> nat * String.t) ->
  unit coq_PrettyPrinter

val prefix_spaces : nat -> String.t -> String.t

val collect_output_lines : coq_PrettyPrinterState -> String.t list

type 'a result_string_err = ('a, String.t) result

val finish_print_lines :
  'a1 coq_PrettyPrinter -> ('a1 * String.t list) result_string_err

val collect_output : coq_PrettyPrinterState -> String.t

val printer_fail : String.t -> 'a1 coq_PrettyPrinter

val map_indent_stack : (nat list -> nat list) -> unit coq_PrettyPrinter

val push_indent : nat -> unit coq_PrettyPrinter

val pop_indent : unit coq_PrettyPrinter

val get_used_names : ident list coq_PrettyPrinter

val map_used_names : (ident list -> ident list) -> unit coq_PrettyPrinter

val push_use : ident -> unit coq_PrettyPrinter

val get_current_line_length : nat coq_PrettyPrinter

val map_cur_output_line :
  ((nat * String.t) -> nat * String.t) -> unit coq_PrettyPrinter

val append : String.t -> unit coq_PrettyPrinter

val append_nl : unit coq_PrettyPrinter

val monad_append_join :
  unit coq_PrettyPrinter -> unit coq_PrettyPrinter list -> unit
  coq_PrettyPrinter

val append_join : String.t -> String.t list -> unit coq_PrettyPrinter

val monad_append_concat :
  unit coq_PrettyPrinter list -> unit coq_PrettyPrinter

val append_concat : String.t list -> unit coq_PrettyPrinter

val wrap_option : 'a1 option -> String.t -> 'a1 coq_PrettyPrinter

val wrap_result :
  ('a1, 'a2) result -> ('a2 -> String.t) -> 'a1 coq_PrettyPrinter
