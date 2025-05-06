open Datatypes
open List0
open Nat0

(** val those_aux : 'a1 list option -> 'a1 option list -> 'a1 list option **)

let rec those_aux acc l =
  match acc with
  | Some ys_rev ->
    (match l with
     | [] -> Some ys_rev
     | o :: xs ->
       (match o with
        | Some y -> those_aux (Some (y :: ys_rev)) xs
        | None -> None))
  | None -> None

(** val those : 'a1 option list -> 'a1 list option **)

let those l =
  match those_aux (Some []) l with
  | Some l0 -> Some (rev l0)
  | None -> None

(** val mapi_aux :
    (nat * 'a2 list) -> (nat -> 'a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec mapi_aux acc f xs =
  let (i, ys_rev) = acc in
  (match xs with
   | [] -> rev ys_rev
   | x :: xs' ->
     let y = f i x in mapi_aux ((add i (S O)), (y :: ys_rev)) f xs')

(** val mapi : (nat -> 'a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let mapi f xs =
  mapi_aux (O, []) f xs
