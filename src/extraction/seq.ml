
(** val cat : 'a1 list -> 'a1 list -> 'a1 list **)

let rec cat s1 s2 =
  match s1 with
  | [] -> s2
  | x :: s1' -> x :: (cat s1' s2)

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec map f = function
| [] -> []
| x :: s' -> (f x) :: (map f s')
