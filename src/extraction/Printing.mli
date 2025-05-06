open Kernames
open Bytestring

type remapped_inductive = { re_ind_name : String.t;
                            re_ind_ctors : String.t list;
                            re_ind_match : String.t option }

type remaps = { remap_inductive : (inductive -> remapped_inductive option);
                remap_constant : (kername -> String.t option);
                remap_inline_constant : (kername -> String.t option) }

val no_remaps : remaps
