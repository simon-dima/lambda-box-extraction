open BinNums
open Datatypes
open List0
open MSetRBT
open POrderedType

module PS :
 sig
  module Raw :
   sig
    type elt = positive

    type tree =
    | Leaf
    | Node of Color.t * tree * positive * tree

    val empty : tree

    val is_empty : tree -> bool

    val mem : positive -> tree -> bool

    val min_elt : tree -> elt option

    val max_elt : tree -> elt option

    val choose : tree -> elt option

    val fold : (elt -> 'a1 -> 'a1) -> tree -> 'a1 -> 'a1

    val elements_aux : positive list -> tree -> positive list

    val elements : tree -> positive list

    val rev_elements_aux : positive list -> tree -> positive list

    val rev_elements : tree -> positive list

    val cardinal : tree -> nat

    val maxdepth : tree -> nat

    val mindepth : tree -> nat

    val for_all : (elt -> bool) -> tree -> bool

    val exists_ : (elt -> bool) -> tree -> bool

    type enumeration =
    | End
    | More of elt * tree * enumeration

    val cons : tree -> enumeration -> enumeration

    val compare_more :
      positive -> (enumeration -> comparison) -> enumeration -> comparison

    val compare_cont :
      tree -> (enumeration -> comparison) -> enumeration -> comparison

    val compare_end : enumeration -> comparison

    val compare : tree -> tree -> comparison

    val equal : tree -> tree -> bool

    val subsetl : (tree -> bool) -> positive -> tree -> bool

    val subsetr : (tree -> bool) -> positive -> tree -> bool

    val subset : tree -> tree -> bool

    type t = tree

    val singleton : elt -> tree

    val makeBlack : tree -> tree

    val makeRed : tree -> tree

    val lbal : tree -> positive -> tree -> tree

    val rbal : tree -> positive -> tree -> tree

    val rbal' : tree -> positive -> tree -> tree

    val lbalS : tree -> positive -> tree -> tree

    val rbalS : tree -> positive -> tree -> tree

    val ins : positive -> tree -> tree

    val add : positive -> tree -> tree

    val append : tree -> tree -> tree

    val del : positive -> tree -> tree

    val remove : positive -> tree -> tree

    val delmin : tree -> elt -> tree -> elt * tree

    val remove_min : tree -> (elt * tree) option

    val bogus : tree * elt list

    val treeify_zero : elt list -> tree * elt list

    val treeify_one : elt list -> tree * elt list

    val treeify_cont :
      (elt list -> tree * elt list) -> (elt list -> tree * elt list) -> elt
      list -> tree * elt list

    val treeify_aux : bool -> positive -> elt list -> tree * elt list

    val plength_aux : elt list -> positive -> positive

    val plength : elt list -> positive

    val treeify : elt list -> tree

    val filter_aux : (elt -> bool) -> tree -> positive list -> positive list

    val filter : (elt -> bool) -> t -> t

    val partition_aux :
      (elt -> bool) -> tree -> positive list -> positive list -> positive
      list * positive list

    val partition : (elt -> bool) -> t -> t * t

    val union_list : elt list -> elt list -> elt list -> elt list

    val linear_union : tree -> tree -> tree

    val inter_list : positive list -> elt list -> elt list -> elt list

    val linear_inter : tree -> tree -> tree

    val diff_list : elt list -> elt list -> elt list -> elt list

    val linear_diff : tree -> tree -> tree

    val skip_red : tree -> tree

    val skip_black : tree -> tree

    val compare_height : tree -> tree -> tree -> tree -> comparison

    val union : t -> t -> t

    val diff : t -> t -> t

    val inter : t -> t -> t

    val ltb_tree : positive -> tree -> bool

    val gtb_tree : positive -> tree -> bool

    val isok : tree -> bool

    module MX :
     sig
      module OrderTac :
       sig
        module OTF :
         sig
          type t = positive

          val compare : positive -> positive -> comparison

          val eq_dec : positive -> positive -> bool
         end

        module TO :
         sig
          type t = positive

          val compare : positive -> positive -> comparison

          val eq_dec : positive -> positive -> bool
         end
       end

      val eq_dec : positive -> positive -> bool

      val lt_dec : positive -> positive -> bool

      val eqb : positive -> positive -> bool
     end

    module L :
     sig
      module MO :
       sig
        module OrderTac :
         sig
          module OTF :
           sig
            type t = positive

            val compare : positive -> positive -> comparison

            val eq_dec : positive -> positive -> bool
           end

          module TO :
           sig
            type t = positive

            val compare : positive -> positive -> comparison

            val eq_dec : positive -> positive -> bool
           end
         end

        val eq_dec : positive -> positive -> bool

        val lt_dec : positive -> positive -> bool

        val eqb : positive -> positive -> bool
       end
     end

    val flatten_e : enumeration -> elt list

    val rcase :
      (tree -> positive -> tree -> 'a1) -> (tree -> 'a1) -> tree -> 'a1

    val rrcase :
      (tree -> positive -> tree -> positive -> tree -> 'a1) -> (tree -> 'a1)
      -> tree -> 'a1

    val rrcase' :
      (tree -> positive -> tree -> positive -> tree -> 'a1) -> (tree -> 'a1)
      -> tree -> 'a1
   end

  module E :
   sig
    type t = positive

    val compare : positive -> positive -> comparison

    val eq_dec : positive -> positive -> bool
   end

  type elt = positive

  type t_ = Raw.t
    (* singleton inductive, whose constructor was Mkt *)

  val this : t_ -> Raw.t

  type t = t_

  val mem : elt -> t -> bool

  val add : elt -> t -> t

  val remove : elt -> t -> t

  val singleton : elt -> t

  val union : t -> t -> t

  val inter : t -> t -> t

  val diff : t -> t -> t

  val equal : t -> t -> bool

  val subset : t -> t -> bool

  val empty : t

  val is_empty : t -> bool

  val elements : t -> elt list

  val choose : t -> elt option

  val fold : (elt -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1

  val cardinal : t -> nat

  val filter : (elt -> bool) -> t -> t

  val for_all : (elt -> bool) -> t -> bool

  val exists_ : (elt -> bool) -> t -> bool

  val partition : (elt -> bool) -> t -> t * t

  val eq_dec : t -> t -> bool

  val compare : t -> t -> comparison

  val min_elt : t -> elt option

  val max_elt : t -> elt option

  val mk_opt_t : (elt * Raw.t) option -> (elt * t) option

  val remove_min : t_ -> (elt * t) option
 end

val union_list : PS.t -> PS.elt list -> PS.t
