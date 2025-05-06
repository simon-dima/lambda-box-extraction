open AST
open BinNums
open BinPos
open Datatypes
open List0
open Maps
open Values0

module Senv =
 struct
  type t = { find_symbol : (ident -> block option);
             public_symbol : (ident -> bool);
             invert_symbol : (block -> ident option);
             block_is_volatile : (block -> bool); nextblock : block }
 end

module Genv =
 struct
  type ('f, 'v) t = { genv_public : ident list; genv_symb : block PTree.t;
                      genv_defs : ('f, 'v) globdef PTree.t; genv_next : 
                      block }

  (** val genv_public : ('a1, 'a2) t -> ident list **)

  let genv_public t0 =
    t0.genv_public

  (** val genv_symb : ('a1, 'a2) t -> block PTree.t **)

  let genv_symb t0 =
    t0.genv_symb

  (** val genv_defs : ('a1, 'a2) t -> ('a1, 'a2) globdef PTree.t **)

  let genv_defs t0 =
    t0.genv_defs

  (** val genv_next : ('a1, 'a2) t -> block **)

  let genv_next t0 =
    t0.genv_next

  (** val find_symbol : ('a1, 'a2) t -> ident -> block option **)

  let find_symbol ge id =
    PTree.get id ge.genv_symb

  (** val public_symbol : ('a1, 'a2) t -> ident -> bool **)

  let public_symbol ge id =
    match find_symbol ge id with
    | Some _ -> (fun x -> x) (in_dec ident_eq id ge.genv_public)
    | None -> false

  (** val find_def : ('a1, 'a2) t -> block -> ('a1, 'a2) globdef option **)

  let find_def ge b =
    PTree.get b ge.genv_defs

  (** val invert_symbol : ('a1, 'a2) t -> block -> ident option **)

  let invert_symbol ge b =
    PTree.fold (fun res id b' -> if eq_block b b' then Some id else res)
      ge.genv_symb None

  (** val find_var_info : ('a1, 'a2) t -> block -> 'a2 globvar option **)

  let find_var_info ge b =
    match find_def ge b with
    | Some g -> (match g with
                 | Gfun _ -> None
                 | Gvar v -> Some v)
    | None -> None

  (** val block_is_volatile : ('a1, 'a2) t -> block -> bool **)

  let block_is_volatile ge b =
    match find_var_info ge b with
    | Some gv -> gv.gvar_volatile
    | None -> false

  (** val add_global :
      ('a1, 'a2) t -> (ident * ('a1, 'a2) globdef) -> ('a1, 'a2) t **)

  let add_global ge idg =
    { genv_public = ge.genv_public; genv_symb =
      (PTree.set (fst idg) ge.genv_next ge.genv_symb); genv_defs =
      (PTree.set ge.genv_next (snd idg) ge.genv_defs); genv_next =
      (Pos.succ ge.genv_next) }

  (** val add_globals :
      ('a1, 'a2) t -> (ident * ('a1, 'a2) globdef) list -> ('a1, 'a2) t **)

  let add_globals ge gl =
    fold_left add_global gl ge

  (** val empty_genv : ident list -> ('a1, 'a2) t **)

  let empty_genv pub =
    { genv_public = pub; genv_symb = PTree.empty; genv_defs = PTree.empty;
      genv_next = Coq_xH }

  (** val globalenv : ('a1, 'a2) program -> ('a1, 'a2) t **)

  let globalenv p =
    add_globals (empty_genv p.prog_public) p.prog_defs

  (** val to_senv : ('a1, 'a2) t -> Senv.t **)

  let to_senv ge =
    { Senv.find_symbol = (find_symbol ge); Senv.public_symbol =
      (public_symbol ge); Senv.invert_symbol = (invert_symbol ge);
      Senv.block_is_volatile = (block_is_volatile ge); Senv.nextblock =
      ge.genv_next }
 end
