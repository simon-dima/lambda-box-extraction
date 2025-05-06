open AST
open BinNums
open BinPos
open Datatypes
open List0
open Maps
open Values0

module Senv :
 sig
  type t = { find_symbol : (ident -> block option);
             public_symbol : (ident -> bool);
             invert_symbol : (block -> ident option);
             block_is_volatile : (block -> bool); nextblock : block }
 end

module Genv :
 sig
  type ('f, 'v) t = { genv_public : ident list; genv_symb : block PTree.t;
                      genv_defs : ('f, 'v) globdef PTree.t; genv_next : 
                      block }

  val genv_public : ('a1, 'a2) t -> ident list

  val genv_symb : ('a1, 'a2) t -> block PTree.t

  val genv_defs : ('a1, 'a2) t -> ('a1, 'a2) globdef PTree.t

  val genv_next : ('a1, 'a2) t -> block

  val find_symbol : ('a1, 'a2) t -> ident -> block option

  val public_symbol : ('a1, 'a2) t -> ident -> bool

  val find_def : ('a1, 'a2) t -> block -> ('a1, 'a2) globdef option

  val invert_symbol : ('a1, 'a2) t -> block -> ident option

  val find_var_info : ('a1, 'a2) t -> block -> 'a2 globvar option

  val block_is_volatile : ('a1, 'a2) t -> block -> bool

  val add_global :
    ('a1, 'a2) t -> (ident * ('a1, 'a2) globdef) -> ('a1, 'a2) t

  val add_globals :
    ('a1, 'a2) t -> (ident * ('a1, 'a2) globdef) list -> ('a1, 'a2) t

  val empty_genv : ident list -> ('a1, 'a2) t

  val globalenv : ('a1, 'a2) program -> ('a1, 'a2) t

  val to_senv : ('a1, 'a2) t -> Senv.t
 end
