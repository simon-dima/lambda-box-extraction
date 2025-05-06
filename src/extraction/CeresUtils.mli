open Datatypes

val _find_or :
  ('a1 -> 'a1 -> bool) -> 'a1 -> ('a1 * 'a2) list -> ('a2 -> 'a3) -> 'a3 ->
  'a3

val _bind_sum : ('a1, 'a2) sum -> ('a2 -> ('a1, 'a3) sum) -> ('a1, 'a3) sum
