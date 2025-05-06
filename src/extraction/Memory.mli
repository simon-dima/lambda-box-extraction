open BinNums
open Maps
open Memdata
open Memtype
open Values0

module Mem :
 sig
  type mem' = { mem_contents : memval ZMap.t PMap.t;
                mem_access : (coq_Z -> perm_kind -> permission option) PMap.t;
                nextblock : block }

  type mem = mem'
 end
