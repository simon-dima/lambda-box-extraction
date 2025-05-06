open Frame

type __ = Obj.t

(** val coq_Rec : 'a1 -> 'a1 **)

let coq_Rec rhs =
  rhs

type ('u, 'd) coq_Delay = 'd

type ('u, 'r) coq_Param = 'r

type ('u, 's) coq_State = 's

type ('u, 'd) coq_Delayed = { delayD : ('u -> 'u univD -> ('u, 'd) coq_Delay
                                       -> 'u univD);
                              delay_id : ('u -> 'u univD -> ('u, 'd)
                                         coq_Delay) }

type ('u, 'r) coq_Preserves_R =
  'u -> 'u -> __ -> __ -> 'u frame_t -> ('u, 'r) coq_Param -> ('u, 'r)
  coq_Param

type ('u, 's) coq_Preserves_S_up =
  'u -> 'u -> __ -> __ -> 'u frame_t -> 'u univD -> ('u, 's) coq_State ->
  ('u, 's) coq_State

type ('u, 's) coq_Preserves_S_dn =
  'u -> 'u -> __ -> __ -> 'u frame_t -> 'u univD -> ('u, 's) coq_State ->
  ('u, 's) coq_State

type coq_Fuel = __

type 'u coq_Metric = __

type ('u, 's) result = { resTree : 'u univD; resState : ('u, 's) coq_State }

type ('u, 'r, 's) rw_for =
  __ -> __ -> ('u, 'r) coq_Param -> ('u, 's) coq_State -> __ -> ('u, 's)
  result

type ('u, 'd, 'r, 's) rewriter' =
  coq_Fuel -> 'u -> __ -> __ -> 'u univD -> ('u, 'd) coq_Delay -> ('u, 'r,
  's) rw_for
