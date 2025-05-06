open Globalenvs

type __ = Obj.t

type semantics = { globalenv : __; symbolenv : Senv.t }
