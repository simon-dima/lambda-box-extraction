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

(** val coq_Monad_PrettyPrinter : __ coq_PrettyPrinter coq_Monad **)

let coq_Monad_PrettyPrinter =
  { ret = (fun _ a pps -> Ok (a, pps)); bind = (fun _ _ m f pps ->
    match m pps with
    | Ok t0 -> let (a, pps0) = t0 in f a pps0
    | Err err -> Err err) }

(** val map_pps :
    (nat list -> nat list) -> (ident list -> ident list) -> ((nat * String.t)
    list -> (nat * String.t) list) -> ((nat * String.t) -> nat * String.t) ->
    unit coq_PrettyPrinter **)

let map_pps f g h i pps =
  Ok ((),
    (let { indent_stack = a; used_names = b; output_lines = c;
       cur_output_line = d } = pps
     in
     { indent_stack = (f a); used_names = (g b); output_lines = (h c);
     cur_output_line = (i d) }))

(** val prefix_spaces : nat -> String.t -> String.t **)

let rec prefix_spaces n s =
  match n with
  | O -> s
  | S n0 -> prefix_spaces n0 (String.String (Coq_x20, s))

(** val collect_output_lines : coq_PrettyPrinterState -> String.t list **)

let collect_output_lines pps =
  rev_map (fun pat -> let (n, l) = pat in prefix_spaces n l)
    (pps.cur_output_line :: pps.output_lines)

type 'a result_string_err = ('a, String.t) result

(** val finish_print_lines :
    'a1 coq_PrettyPrinter -> ('a1 * String.t list) result_string_err **)

let finish_print_lines pp =
  bind (Obj.magic coq_Monad_result)
    (Obj.magic pp { indent_stack = []; used_names = []; output_lines = [];
      cur_output_line = (O, String.EmptyString) }) (fun x ->
    let (a, pps) = x in
    ret (Obj.magic coq_Monad_result) (a, (collect_output_lines pps)))

(** val collect_output : coq_PrettyPrinterState -> String.t **)

let collect_output pps =
  String.concat nl (collect_output_lines pps)

(** val printer_fail : String.t -> 'a1 coq_PrettyPrinter **)

let printer_fail str pps =
  Err
    (String.append str
      (String.append nl
        (String.append (String.String (Coq_x66, (String.String (Coq_x61,
          (String.String (Coq_x69, (String.String (Coq_x6c, (String.String
          (Coq_x65, (String.String (Coq_x64, (String.String (Coq_x20,
          (String.String (Coq_x61, (String.String (Coq_x66, (String.String
          (Coq_x74, (String.String (Coq_x65, (String.String (Coq_x72,
          (String.String (Coq_x20, (String.String (Coq_x70, (String.String
          (Coq_x72, (String.String (Coq_x69, (String.String (Coq_x6e,
          (String.String (Coq_x74, (String.String (Coq_x69, (String.String
          (Coq_x6e, (String.String (Coq_x67,
          String.EmptyString))))))))))))))))))))))))))))))))))))))))))
          (String.append nl (collect_output pps)))))

(** val map_indent_stack :
    (nat list -> nat list) -> unit coq_PrettyPrinter **)

let map_indent_stack f =
  map_pps f (Obj.magic id) (Obj.magic id) (Obj.magic id)

(** val push_indent : nat -> unit coq_PrettyPrinter **)

let push_indent n =
  map_indent_stack (fun x -> n :: x)

(** val pop_indent : unit coq_PrettyPrinter **)

let pop_indent =
  map_indent_stack tl

(** val get_used_names : ident list coq_PrettyPrinter **)

let get_used_names pps =
  Ok (pps.used_names, pps)

(** val map_used_names :
    (ident list -> ident list) -> unit coq_PrettyPrinter **)

let map_used_names f =
  map_pps (Obj.magic id) f (Obj.magic id) (Obj.magic id)

(** val push_use : ident -> unit coq_PrettyPrinter **)

let push_use n =
  map_used_names (fun x -> n :: x)

(** val get_current_line_length : nat coq_PrettyPrinter **)

let get_current_line_length pps =
  Ok
    ((add (fst pps.cur_output_line) (String.length (snd pps.cur_output_line))),
    pps)

(** val map_cur_output_line :
    ((nat * String.t) -> nat * String.t) -> unit coq_PrettyPrinter **)

let map_cur_output_line f =
  map_pps (Obj.magic id) (Obj.magic id) (Obj.magic id) f

(** val append : String.t -> unit coq_PrettyPrinter **)

let append s =
  map_cur_output_line (fun pat ->
    let (n, prev) = pat in (n, (String.append prev s)))

(** val append_nl : unit coq_PrettyPrinter **)

let append_nl pps =
  Ok ((), { indent_stack = pps.indent_stack; used_names = pps.used_names;
    output_lines = (pps.cur_output_line :: pps.output_lines);
    cur_output_line = ((hd O pps.indent_stack), String.EmptyString) })

(** val monad_append_join :
    unit coq_PrettyPrinter -> unit coq_PrettyPrinter list -> unit
    coq_PrettyPrinter **)

let monad_append_join sep xs =
  bind (Obj.magic coq_Monad_PrettyPrinter)
    (monad_fold_left (Obj.magic coq_Monad_PrettyPrinter) (fun sep' x ->
      bind (Obj.magic coq_Monad_PrettyPrinter) (Obj.magic sep') (fun _ ->
        bind (Obj.magic coq_Monad_PrettyPrinter) x (fun _ ->
          ret (Obj.magic coq_Monad_PrettyPrinter) sep))) xs
      (ret coq_Monad_PrettyPrinter ())) (fun _ ->
    ret (Obj.magic coq_Monad_PrettyPrinter) ())

(** val append_join : String.t -> String.t list -> unit coq_PrettyPrinter **)

let append_join sep s =
  monad_append_join (append sep) (map append s)

(** val monad_append_concat :
    unit coq_PrettyPrinter list -> unit coq_PrettyPrinter **)

let monad_append_concat xs =
  bind (Obj.magic coq_Monad_PrettyPrinter)
    (monad_map (Obj.magic coq_Monad_PrettyPrinter) (Obj.magic id) xs)
    (fun _ -> ret (Obj.magic coq_Monad_PrettyPrinter) ())

(** val append_concat : String.t list -> unit coq_PrettyPrinter **)

let append_concat xs =
  monad_append_concat (map append xs)

(** val wrap_option : 'a1 option -> String.t -> 'a1 coq_PrettyPrinter **)

let wrap_option o err =
  match o with
  | Some a -> ret (Obj.magic coq_Monad_PrettyPrinter) a
  | None -> printer_fail err

(** val wrap_result :
    ('a1, 'a2) result -> ('a2 -> String.t) -> 'a1 coq_PrettyPrinter **)

let wrap_result r err_string =
  match r with
  | Ok t0 -> ret (Obj.magic coq_Monad_PrettyPrinter) t0
  | Err e -> printer_fail (err_string e)
