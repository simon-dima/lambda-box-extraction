
(** val is_left : bool -> bool **)

let is_left = function
| true -> true
| false -> false

type 't pred = 't -> bool

type 't rel = 't -> 't pred
