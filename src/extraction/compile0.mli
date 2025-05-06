open Ast0
open AstCommon
open BasicAst
open Byte
open Classes1
open Datatypes
open EAst
open EPrimitive
open EProgram
open ESpineView
open Erasure0
open ExtractionCorrectness
open Kernames
open List0
open MCList
open Nat0
open PeanoNat
open Primitive
open Specif
open Bytestring

type __ = Obj.t

type projection = inductive * nat

val project_dec : projection -> projection -> bool

type coq_Term =
| TRel of nat
| TProof
| TLambda of name * coq_Term
| TLetIn of name * coq_Term * coq_Term
| TApp of coq_Term * coq_Term
| TConst of kername
| TConstruct of inductive * nat * coq_Terms
| TCase of inductive * coq_Term * coq_Brs
| TFix of coq_Defs * nat
| TPrim of primitive
| TWrong of String.t
and coq_Terms =
| Coq_tnil
| Coq_tcons of coq_Term * coq_Terms
and coq_Brs =
| Coq_bnil
| Coq_bcons of name list * coq_Term * coq_Brs
and coq_Defs =
| Coq_dnil
| Coq_dcons of name * coq_Term * nat * coq_Defs

val coq_Term_rect :
  (nat -> 'a1) -> 'a1 -> (name -> coq_Term -> 'a1 -> 'a1) -> (name ->
  coq_Term -> 'a1 -> coq_Term -> 'a1 -> 'a1) -> (coq_Term -> 'a1 -> coq_Term
  -> 'a1 -> 'a1) -> (kername -> 'a1) -> (inductive -> nat -> coq_Terms ->
  'a1) -> (inductive -> coq_Term -> 'a1 -> coq_Brs -> 'a1) -> (coq_Defs ->
  nat -> 'a1) -> (primitive -> 'a1) -> (String.t -> 'a1) -> coq_Term -> 'a1

val coq_Term_rec :
  (nat -> 'a1) -> 'a1 -> (name -> coq_Term -> 'a1 -> 'a1) -> (name ->
  coq_Term -> 'a1 -> coq_Term -> 'a1 -> 'a1) -> (coq_Term -> 'a1 -> coq_Term
  -> 'a1 -> 'a1) -> (kername -> 'a1) -> (inductive -> nat -> coq_Terms ->
  'a1) -> (inductive -> coq_Term -> 'a1 -> coq_Brs -> 'a1) -> (coq_Defs ->
  nat -> 'a1) -> (primitive -> 'a1) -> (String.t -> 'a1) -> coq_Term -> 'a1

val coq_Terms_rect :
  'a1 -> (coq_Term -> coq_Terms -> 'a1 -> 'a1) -> coq_Terms -> 'a1

val coq_Terms_rec :
  'a1 -> (coq_Term -> coq_Terms -> 'a1 -> 'a1) -> coq_Terms -> 'a1

val coq_Brs_rect :
  'a1 -> (name list -> coq_Term -> coq_Brs -> 'a1 -> 'a1) -> coq_Brs -> 'a1

val coq_Brs_rec :
  'a1 -> (name list -> coq_Term -> coq_Brs -> 'a1 -> 'a1) -> coq_Brs -> 'a1

val coq_Defs_rect :
  'a1 -> (name -> coq_Term -> nat -> coq_Defs -> 'a1 -> 'a1) -> coq_Defs ->
  'a1

val coq_Defs_rec :
  'a1 -> (name -> coq_Term -> nat -> coq_Defs -> 'a1 -> 'a1) -> coq_Defs ->
  'a1

val coq_Terms_list : coq_Terms -> coq_Term list

val tlength : coq_Terms -> nat

type coq_R_tlength =
| R_tlength_0 of coq_Terms
| R_tlength_1 of coq_Terms * coq_Term * coq_Terms * nat * coq_R_tlength

val coq_R_tlength_rect :
  (coq_Terms -> __ -> 'a1) -> (coq_Terms -> coq_Term -> coq_Terms -> __ ->
  nat -> coq_R_tlength -> 'a1 -> 'a1) -> coq_Terms -> nat -> coq_R_tlength ->
  'a1

val coq_R_tlength_rec :
  (coq_Terms -> __ -> 'a1) -> (coq_Terms -> coq_Term -> coq_Terms -> __ ->
  nat -> coq_R_tlength -> 'a1 -> 'a1) -> coq_Terms -> nat -> coq_R_tlength ->
  'a1

val tlength_rect :
  (coq_Terms -> __ -> 'a1) -> (coq_Terms -> coq_Term -> coq_Terms -> __ ->
  'a1 -> 'a1) -> coq_Terms -> 'a1

val tlength_rec :
  (coq_Terms -> __ -> 'a1) -> (coq_Terms -> coq_Term -> coq_Terms -> __ ->
  'a1 -> 'a1) -> coq_Terms -> 'a1

val coq_R_tlength_correct : coq_Terms -> nat -> coq_R_tlength

val blength : coq_Brs -> nat

type coq_R_blength =
| R_blength_0 of coq_Brs
| R_blength_1 of coq_Brs * name list * coq_Term * coq_Brs * nat
   * coq_R_blength

val coq_R_blength_rect :
  (coq_Brs -> __ -> 'a1) -> (coq_Brs -> name list -> coq_Term -> coq_Brs ->
  __ -> nat -> coq_R_blength -> 'a1 -> 'a1) -> coq_Brs -> nat ->
  coq_R_blength -> 'a1

val coq_R_blength_rec :
  (coq_Brs -> __ -> 'a1) -> (coq_Brs -> name list -> coq_Term -> coq_Brs ->
  __ -> nat -> coq_R_blength -> 'a1 -> 'a1) -> coq_Brs -> nat ->
  coq_R_blength -> 'a1

val blength_rect :
  (coq_Brs -> __ -> 'a1) -> (coq_Brs -> name list -> coq_Term -> coq_Brs ->
  __ -> 'a1 -> 'a1) -> coq_Brs -> 'a1

val blength_rec :
  (coq_Brs -> __ -> 'a1) -> (coq_Brs -> name list -> coq_Term -> coq_Brs ->
  __ -> 'a1 -> 'a1) -> coq_Brs -> 'a1

val coq_R_blength_correct : coq_Brs -> nat -> coq_R_blength

val tappend : coq_Terms -> coq_Terms -> coq_Terms

type coq_R_tappend =
| R_tappend_0 of coq_Terms * coq_Terms
| R_tappend_1 of coq_Terms * coq_Terms * coq_Term * coq_Terms * coq_Terms
   * coq_R_tappend

val coq_R_tappend_rect :
  (coq_Terms -> coq_Terms -> __ -> 'a1) -> (coq_Terms -> coq_Terms ->
  coq_Term -> coq_Terms -> __ -> coq_Terms -> coq_R_tappend -> 'a1 -> 'a1) ->
  coq_Terms -> coq_Terms -> coq_Terms -> coq_R_tappend -> 'a1

val coq_R_tappend_rec :
  (coq_Terms -> coq_Terms -> __ -> 'a1) -> (coq_Terms -> coq_Terms ->
  coq_Term -> coq_Terms -> __ -> coq_Terms -> coq_R_tappend -> 'a1 -> 'a1) ->
  coq_Terms -> coq_Terms -> coq_Terms -> coq_R_tappend -> 'a1

val tappend_rect :
  (coq_Terms -> coq_Terms -> __ -> 'a1) -> (coq_Terms -> coq_Terms ->
  coq_Term -> coq_Terms -> __ -> 'a1 -> 'a1) -> coq_Terms -> coq_Terms -> 'a1

val tappend_rec :
  (coq_Terms -> coq_Terms -> __ -> 'a1) -> (coq_Terms -> coq_Terms ->
  coq_Term -> coq_Terms -> __ -> 'a1 -> 'a1) -> coq_Terms -> coq_Terms -> 'a1

val coq_R_tappend_correct :
  coq_Terms -> coq_Terms -> coq_Terms -> coq_R_tappend

val tdrop : nat -> coq_Terms -> coq_Terms

val treverse : coq_Terms -> coq_Terms

val dlength : coq_Defs -> nat

val isApp_dec : coq_Term -> bool

val lift : nat -> coq_Term -> coq_Term

val lifts : nat -> coq_Terms -> coq_Terms

val liftBs : nat -> coq_Brs -> coq_Brs

val liftDs : nat -> coq_Defs -> coq_Defs

type coq_R_lift =
| R_lift_0 of nat * coq_Term * nat
| R_lift_1 of nat * coq_Term * nat * comparison
| R_lift_2 of nat * coq_Term
| R_lift_3 of nat * coq_Term * name * coq_Term * coq_Term * coq_R_lift
| R_lift_4 of nat * coq_Term * name * coq_Term * coq_Term * coq_Term
   * coq_R_lift * coq_Term * coq_R_lift
| R_lift_5 of nat * coq_Term * coq_Term * coq_Term * coq_Term * coq_R_lift
   * coq_Term * coq_R_lift
| R_lift_6 of nat * coq_Term * inductive * nat * coq_Terms * coq_Terms
   * coq_R_lifts
| R_lift_7 of nat * coq_Term * inductive * coq_Term * coq_Brs * coq_Term
   * coq_R_lift * coq_Brs * coq_R_liftBs
| R_lift_8 of nat * coq_Term * coq_Defs * nat * coq_Defs * coq_R_liftDs
| R_lift_9 of nat * coq_Term * coq_Term
and coq_R_lifts =
| R_lifts_0 of nat * coq_Terms
| R_lifts_1 of nat * coq_Terms * coq_Term * coq_Terms * coq_Term * coq_R_lift
   * coq_Terms * coq_R_lifts
and coq_R_liftBs =
| R_liftBs_0 of nat * coq_Brs
| R_liftBs_1 of nat * coq_Brs * name list * coq_Term * coq_Brs * coq_Term
   * coq_R_lift * coq_Brs * coq_R_liftBs
and coq_R_liftDs =
| R_liftDs_0 of nat * coq_Defs
| R_liftDs_1 of nat * coq_Defs * name * coq_Term * nat * coq_Defs * coq_Term
   * coq_R_lift * coq_Defs * coq_R_liftDs

val coq_R_lift_rect :
  (nat -> coq_Term -> nat -> __ -> __ -> 'a1) -> (nat -> coq_Term -> nat ->
  __ -> comparison -> __ -> __ -> 'a1) -> (nat -> coq_Term -> __ -> 'a1) ->
  (nat -> coq_Term -> name -> coq_Term -> __ -> coq_Term -> coq_R_lift -> 'a1
  -> 'a1) -> (nat -> coq_Term -> name -> coq_Term -> coq_Term -> __ ->
  coq_Term -> coq_R_lift -> 'a1 -> coq_Term -> coq_R_lift -> 'a1 -> 'a1) ->
  (nat -> coq_Term -> coq_Term -> coq_Term -> __ -> coq_Term -> coq_R_lift ->
  'a1 -> coq_Term -> coq_R_lift -> 'a1 -> 'a1) -> (nat -> coq_Term ->
  inductive -> nat -> coq_Terms -> __ -> coq_Terms -> coq_R_lifts -> 'a1) ->
  (nat -> coq_Term -> inductive -> coq_Term -> coq_Brs -> __ -> coq_Term ->
  coq_R_lift -> 'a1 -> coq_Brs -> coq_R_liftBs -> 'a1) -> (nat -> coq_Term ->
  coq_Defs -> nat -> __ -> coq_Defs -> coq_R_liftDs -> 'a1) -> (nat ->
  coq_Term -> coq_Term -> __ -> __ -> 'a1) -> nat -> coq_Term -> coq_Term ->
  coq_R_lift -> 'a1

val coq_R_lift_rec :
  (nat -> coq_Term -> nat -> __ -> __ -> 'a1) -> (nat -> coq_Term -> nat ->
  __ -> comparison -> __ -> __ -> 'a1) -> (nat -> coq_Term -> __ -> 'a1) ->
  (nat -> coq_Term -> name -> coq_Term -> __ -> coq_Term -> coq_R_lift -> 'a1
  -> 'a1) -> (nat -> coq_Term -> name -> coq_Term -> coq_Term -> __ ->
  coq_Term -> coq_R_lift -> 'a1 -> coq_Term -> coq_R_lift -> 'a1 -> 'a1) ->
  (nat -> coq_Term -> coq_Term -> coq_Term -> __ -> coq_Term -> coq_R_lift ->
  'a1 -> coq_Term -> coq_R_lift -> 'a1 -> 'a1) -> (nat -> coq_Term ->
  inductive -> nat -> coq_Terms -> __ -> coq_Terms -> coq_R_lifts -> 'a1) ->
  (nat -> coq_Term -> inductive -> coq_Term -> coq_Brs -> __ -> coq_Term ->
  coq_R_lift -> 'a1 -> coq_Brs -> coq_R_liftBs -> 'a1) -> (nat -> coq_Term ->
  coq_Defs -> nat -> __ -> coq_Defs -> coq_R_liftDs -> 'a1) -> (nat ->
  coq_Term -> coq_Term -> __ -> __ -> 'a1) -> nat -> coq_Term -> coq_Term ->
  coq_R_lift -> 'a1

val coq_R_lifts_rect :
  (nat -> coq_Terms -> __ -> 'a1) -> (nat -> coq_Terms -> coq_Term ->
  coq_Terms -> __ -> coq_Term -> coq_R_lift -> coq_Terms -> coq_R_lifts ->
  'a1 -> 'a1) -> nat -> coq_Terms -> coq_Terms -> coq_R_lifts -> 'a1

val coq_R_lifts_rec :
  (nat -> coq_Terms -> __ -> 'a1) -> (nat -> coq_Terms -> coq_Term ->
  coq_Terms -> __ -> coq_Term -> coq_R_lift -> coq_Terms -> coq_R_lifts ->
  'a1 -> 'a1) -> nat -> coq_Terms -> coq_Terms -> coq_R_lifts -> 'a1

val coq_R_liftBs_rect :
  (nat -> coq_Brs -> __ -> 'a1) -> (nat -> coq_Brs -> name list -> coq_Term
  -> coq_Brs -> __ -> coq_Term -> coq_R_lift -> coq_Brs -> coq_R_liftBs ->
  'a1 -> 'a1) -> nat -> coq_Brs -> coq_Brs -> coq_R_liftBs -> 'a1

val coq_R_liftBs_rec :
  (nat -> coq_Brs -> __ -> 'a1) -> (nat -> coq_Brs -> name list -> coq_Term
  -> coq_Brs -> __ -> coq_Term -> coq_R_lift -> coq_Brs -> coq_R_liftBs ->
  'a1 -> 'a1) -> nat -> coq_Brs -> coq_Brs -> coq_R_liftBs -> 'a1

val coq_R_liftDs_rect :
  (nat -> coq_Defs -> __ -> 'a1) -> (nat -> coq_Defs -> name -> coq_Term ->
  nat -> coq_Defs -> __ -> coq_Term -> coq_R_lift -> coq_Defs -> coq_R_liftDs
  -> 'a1 -> 'a1) -> nat -> coq_Defs -> coq_Defs -> coq_R_liftDs -> 'a1

val coq_R_liftDs_rec :
  (nat -> coq_Defs -> __ -> 'a1) -> (nat -> coq_Defs -> name -> coq_Term ->
  nat -> coq_Defs -> __ -> coq_Term -> coq_R_lift -> coq_Defs -> coq_R_liftDs
  -> 'a1 -> 'a1) -> nat -> coq_Defs -> coq_Defs -> coq_R_liftDs -> 'a1

val lift_rect :
  (nat -> coq_Term -> nat -> __ -> __ -> 'a1) -> (nat -> coq_Term -> nat ->
  __ -> comparison -> __ -> __ -> 'a1) -> (nat -> coq_Term -> __ -> 'a1) ->
  (nat -> coq_Term -> name -> coq_Term -> __ -> 'a1 -> 'a1) -> (nat ->
  coq_Term -> name -> coq_Term -> coq_Term -> __ -> 'a1 -> 'a1 -> 'a1) ->
  (nat -> coq_Term -> coq_Term -> coq_Term -> __ -> 'a1 -> 'a1 -> 'a1) ->
  (nat -> coq_Term -> inductive -> nat -> coq_Terms -> __ -> 'a1) -> (nat ->
  coq_Term -> inductive -> coq_Term -> coq_Brs -> __ -> 'a1 -> 'a1) -> (nat
  -> coq_Term -> coq_Defs -> nat -> __ -> 'a1) -> (nat -> coq_Term ->
  coq_Term -> __ -> __ -> 'a1) -> nat -> coq_Term -> 'a1

val lift_rec :
  (nat -> coq_Term -> nat -> __ -> __ -> 'a1) -> (nat -> coq_Term -> nat ->
  __ -> comparison -> __ -> __ -> 'a1) -> (nat -> coq_Term -> __ -> 'a1) ->
  (nat -> coq_Term -> name -> coq_Term -> __ -> 'a1 -> 'a1) -> (nat ->
  coq_Term -> name -> coq_Term -> coq_Term -> __ -> 'a1 -> 'a1 -> 'a1) ->
  (nat -> coq_Term -> coq_Term -> coq_Term -> __ -> 'a1 -> 'a1 -> 'a1) ->
  (nat -> coq_Term -> inductive -> nat -> coq_Terms -> __ -> 'a1) -> (nat ->
  coq_Term -> inductive -> coq_Term -> coq_Brs -> __ -> 'a1 -> 'a1) -> (nat
  -> coq_Term -> coq_Defs -> nat -> __ -> 'a1) -> (nat -> coq_Term ->
  coq_Term -> __ -> __ -> 'a1) -> nat -> coq_Term -> 'a1

val lifts_rect :
  (nat -> coq_Terms -> __ -> 'a1) -> (nat -> coq_Terms -> coq_Term ->
  coq_Terms -> __ -> 'a1 -> 'a1) -> nat -> coq_Terms -> 'a1

val lifts_rec :
  (nat -> coq_Terms -> __ -> 'a1) -> (nat -> coq_Terms -> coq_Term ->
  coq_Terms -> __ -> 'a1 -> 'a1) -> nat -> coq_Terms -> 'a1

val liftBs_rect :
  (nat -> coq_Brs -> __ -> 'a1) -> (nat -> coq_Brs -> name list -> coq_Term
  -> coq_Brs -> __ -> 'a1 -> 'a1) -> nat -> coq_Brs -> 'a1

val liftBs_rec :
  (nat -> coq_Brs -> __ -> 'a1) -> (nat -> coq_Brs -> name list -> coq_Term
  -> coq_Brs -> __ -> 'a1 -> 'a1) -> nat -> coq_Brs -> 'a1

val liftDs_rect :
  (nat -> coq_Defs -> __ -> 'a1) -> (nat -> coq_Defs -> name -> coq_Term ->
  nat -> coq_Defs -> __ -> 'a1 -> 'a1) -> nat -> coq_Defs -> 'a1

val liftDs_rec :
  (nat -> coq_Defs -> __ -> 'a1) -> (nat -> coq_Defs -> name -> coq_Term ->
  nat -> coq_Defs -> __ -> 'a1 -> 'a1) -> nat -> coq_Defs -> 'a1

val coq_R_lift_correct : nat -> coq_Term -> coq_Term -> coq_R_lift

val coq_R_lifts_correct : nat -> coq_Terms -> coq_Terms -> coq_R_lifts

val coq_R_liftBs_correct : nat -> coq_Brs -> coq_Brs -> coq_R_liftBs

val coq_R_liftDs_correct : nat -> coq_Defs -> coq_Defs -> coq_R_liftDs

val coq_TmkApps : coq_Term -> coq_Terms -> coq_Term

val list_terms : coq_Term list -> coq_Terms

val list_Brs : (name list * coq_Term) list -> coq_Brs

val list_Defs : coq_Term def list -> coq_Defs

val trans_prim_val : 'a1 prim_val -> primitive option

type 't trans_prim_val_graph =
| Coq_trans_prim_val_graph_equation_1 of Uint63.t
| Coq_trans_prim_val_graph_equation_2 of Float64.t
| Coq_trans_prim_val_graph_equation_3 of 't prim_model

val trans_prim_val_graph_rect :
  (__ -> Uint63.t -> 'a1) -> (__ -> Float64.t -> 'a1) -> (__ -> __ prim_model
  -> 'a1) -> 'a2 prim_val -> primitive option -> 'a2 trans_prim_val_graph ->
  'a1

val trans_prim_val_graph_correct : 'a1 prim_val -> 'a1 trans_prim_val_graph

val trans_prim_val_elim :
  (__ -> Uint63.t -> 'a1) -> (__ -> Float64.t -> 'a1) -> (__ -> __ prim_model
  -> 'a1) -> 'a2 prim_val -> 'a1

val coq_FunctionalElimination_trans_prim_val :
  (__ -> Uint63.t -> __) -> (__ -> Float64.t -> __) -> (__ -> __ prim_model
  -> __) -> __ prim_val -> __

val coq_FunctionalInduction_trans_prim_val :
  (__ -> __ prim_val -> primitive option) coq_FunctionalInduction

val compile_clause_1_clause_12 :
  term prim_val -> primitive option -> (term -> __ -> coq_Term) -> coq_Term

val compile_clause_1 :
  term -> TermSpineView.t -> (term -> __ -> coq_Term) -> coq_Term

val compile_functional : term -> (term -> __ -> coq_Term) -> coq_Term

val compile : term -> coq_Term

val compile_unfold_clause_1_clause_12 :
  term prim_val -> primitive option -> coq_Term

val compile_unfold_clause_1 : term -> TermSpineView.t -> coq_Term

val compile_unfold : term -> coq_Term

type compile_graph =
| Coq_compile_graph_refinement_1 of term * compile_clause_1_graph
and compile_clause_1_graph =
| Coq_compile_clause_1_graph_equation_1
| Coq_compile_clause_1_graph_equation_2 of nat
| Coq_compile_clause_1_graph_equation_3 of ident
| Coq_compile_clause_1_graph_equation_4 of nat * term list
| Coq_compile_clause_1_graph_equation_5 of name * term * compile_graph
| Coq_compile_clause_1_graph_equation_6 of name * term * term * compile_graph
   * compile_graph
| Coq_compile_clause_1_graph_equation_7 of term * term list * compile_graph
   * (term -> __ -> compile_graph)
| Coq_compile_clause_1_graph_equation_8 of kername
| Coq_compile_clause_1_graph_equation_9 of inductive * nat * term list
   * (term -> __ -> compile_graph)
| Coq_compile_clause_1_graph_equation_10 of (inductive * nat) * term
   * (name list * term) list * ((name list * term) -> __ -> compile_graph)
   * compile_graph
| Coq_compile_clause_1_graph_equation_11 of Kernames.projection * term
| Coq_compile_clause_1_graph_equation_12 of term mfixpoint * nat
   * (term def -> __ -> compile_graph)
| Coq_compile_clause_1_graph_equation_13 of term mfixpoint * nat
| Coq_compile_clause_1_graph_refinement_14 of term prim_val
   * compile_clause_1_clause_12_graph
| Coq_compile_clause_1_graph_equation_15 of term * compile_graph
| Coq_compile_clause_1_graph_equation_16 of term * compile_graph
and compile_clause_1_clause_12_graph =
| Coq_compile_clause_1_clause_12_graph_equation_1 of term prim_val * primitive
| Coq_compile_clause_1_clause_12_graph_equation_2 of term prim_val

val compile_clause_1_clause_12_graph_mut :
  (term -> compile_clause_1_graph -> 'a2 -> 'a1) -> 'a2 -> (nat -> 'a2) ->
  (ident -> 'a2) -> (nat -> term list -> 'a2) -> (name -> term ->
  compile_graph -> 'a1 -> 'a2) -> (name -> term -> term -> compile_graph ->
  'a1 -> compile_graph -> 'a1 -> 'a2) -> (term -> term list -> __ -> __ ->
  compile_graph -> 'a1 -> (term -> __ -> compile_graph) -> (term -> __ ->
  'a1) -> 'a2) -> (kername -> 'a2) -> (inductive -> nat -> term list -> (term
  -> __ -> compile_graph) -> (term -> __ -> 'a1) -> 'a2) ->
  ((inductive * nat) -> term -> (name list * term) list -> ((name
  list * term) -> __ -> compile_graph) -> ((name list * term) -> __ -> 'a1)
  -> compile_graph -> 'a1 -> 'a2) -> (Kernames.projection -> term -> 'a2) ->
  (term mfixpoint -> nat -> (term def -> __ -> compile_graph) -> (term def ->
  __ -> 'a1) -> 'a2) -> (term mfixpoint -> nat -> 'a2) -> (term prim_val ->
  compile_clause_1_clause_12_graph -> 'a3 -> 'a2) -> (term -> compile_graph
  -> 'a1 -> 'a2) -> (term -> compile_graph -> 'a1 -> 'a2) -> (term prim_val
  -> primitive -> 'a3) -> (term prim_val -> 'a3) -> term prim_val ->
  primitive option -> coq_Term -> compile_clause_1_clause_12_graph -> 'a3

val compile_clause_1_graph_mut :
  (term -> compile_clause_1_graph -> 'a2 -> 'a1) -> 'a2 -> (nat -> 'a2) ->
  (ident -> 'a2) -> (nat -> term list -> 'a2) -> (name -> term ->
  compile_graph -> 'a1 -> 'a2) -> (name -> term -> term -> compile_graph ->
  'a1 -> compile_graph -> 'a1 -> 'a2) -> (term -> term list -> __ -> __ ->
  compile_graph -> 'a1 -> (term -> __ -> compile_graph) -> (term -> __ ->
  'a1) -> 'a2) -> (kername -> 'a2) -> (inductive -> nat -> term list -> (term
  -> __ -> compile_graph) -> (term -> __ -> 'a1) -> 'a2) ->
  ((inductive * nat) -> term -> (name list * term) list -> ((name
  list * term) -> __ -> compile_graph) -> ((name list * term) -> __ -> 'a1)
  -> compile_graph -> 'a1 -> 'a2) -> (Kernames.projection -> term -> 'a2) ->
  (term mfixpoint -> nat -> (term def -> __ -> compile_graph) -> (term def ->
  __ -> 'a1) -> 'a2) -> (term mfixpoint -> nat -> 'a2) -> (term prim_val ->
  compile_clause_1_clause_12_graph -> 'a3 -> 'a2) -> (term -> compile_graph
  -> 'a1 -> 'a2) -> (term -> compile_graph -> 'a1 -> 'a2) -> (term prim_val
  -> primitive -> 'a3) -> (term prim_val -> 'a3) -> term -> TermSpineView.t
  -> coq_Term -> compile_clause_1_graph -> 'a2

val compile_graph_mut :
  (term -> compile_clause_1_graph -> 'a2 -> 'a1) -> 'a2 -> (nat -> 'a2) ->
  (ident -> 'a2) -> (nat -> term list -> 'a2) -> (name -> term ->
  compile_graph -> 'a1 -> 'a2) -> (name -> term -> term -> compile_graph ->
  'a1 -> compile_graph -> 'a1 -> 'a2) -> (term -> term list -> __ -> __ ->
  compile_graph -> 'a1 -> (term -> __ -> compile_graph) -> (term -> __ ->
  'a1) -> 'a2) -> (kername -> 'a2) -> (inductive -> nat -> term list -> (term
  -> __ -> compile_graph) -> (term -> __ -> 'a1) -> 'a2) ->
  ((inductive * nat) -> term -> (name list * term) list -> ((name
  list * term) -> __ -> compile_graph) -> ((name list * term) -> __ -> 'a1)
  -> compile_graph -> 'a1 -> 'a2) -> (Kernames.projection -> term -> 'a2) ->
  (term mfixpoint -> nat -> (term def -> __ -> compile_graph) -> (term def ->
  __ -> 'a1) -> 'a2) -> (term mfixpoint -> nat -> 'a2) -> (term prim_val ->
  compile_clause_1_clause_12_graph -> 'a3 -> 'a2) -> (term -> compile_graph
  -> 'a1 -> 'a2) -> (term -> compile_graph -> 'a1 -> 'a2) -> (term prim_val
  -> primitive -> 'a3) -> (term prim_val -> 'a3) -> term -> coq_Term ->
  compile_graph -> 'a1

val compile_graph_rect :
  (term -> compile_clause_1_graph -> 'a2 -> 'a1) -> 'a2 -> (nat -> 'a2) ->
  (ident -> 'a2) -> (nat -> term list -> 'a2) -> (name -> term ->
  compile_graph -> 'a1 -> 'a2) -> (name -> term -> term -> compile_graph ->
  'a1 -> compile_graph -> 'a1 -> 'a2) -> (term -> term list -> __ -> __ ->
  compile_graph -> 'a1 -> (term -> __ -> compile_graph) -> (term -> __ ->
  'a1) -> 'a2) -> (kername -> 'a2) -> (inductive -> nat -> term list -> (term
  -> __ -> compile_graph) -> (term -> __ -> 'a1) -> 'a2) ->
  ((inductive * nat) -> term -> (name list * term) list -> ((name
  list * term) -> __ -> compile_graph) -> ((name list * term) -> __ -> 'a1)
  -> compile_graph -> 'a1 -> 'a2) -> (Kernames.projection -> term -> 'a2) ->
  (term mfixpoint -> nat -> (term def -> __ -> compile_graph) -> (term def ->
  __ -> 'a1) -> 'a2) -> (term mfixpoint -> nat -> 'a2) -> (term prim_val ->
  compile_clause_1_clause_12_graph -> 'a3 -> 'a2) -> (term -> compile_graph
  -> 'a1 -> 'a2) -> (term -> compile_graph -> 'a1 -> 'a2) -> (term prim_val
  -> primitive -> 'a3) -> (term prim_val -> 'a3) -> term -> coq_Term ->
  compile_graph -> 'a1

val compile_graph_correct : term -> compile_graph

val compile_elim :
  (__ -> 'a1) -> (nat -> __ -> 'a1) -> (ident -> __ -> 'a1) -> (nat -> term
  list -> __ -> 'a1) -> (name -> term -> 'a1 -> __ -> 'a1) -> (name -> term
  -> term -> 'a1 -> 'a1 -> __ -> 'a1) -> (term -> term list -> __ -> __ ->
  'a1 -> (term -> __ -> 'a1) -> __ -> 'a1) -> (kername -> __ -> 'a1) ->
  (inductive -> nat -> term list -> (term -> __ -> 'a1) -> __ -> 'a1) ->
  ((inductive * nat) -> term -> (name list * term) list -> ((name
  list * term) -> __ -> 'a1) -> 'a1 -> __ -> 'a1) -> (Kernames.projection ->
  term -> __ -> 'a1) -> (term mfixpoint -> nat -> (term def -> __ -> 'a1) ->
  __ -> 'a1) -> (term mfixpoint -> nat -> __ -> 'a1) -> (term -> 'a1 -> __ ->
  'a1) -> (term -> 'a1 -> __ -> 'a1) -> (term prim_val -> primitive -> __ ->
  __ -> 'a1) -> (term prim_val -> __ -> __ -> 'a1) -> term -> 'a1

val coq_FunctionalElimination_compile :
  (__ -> __) -> (nat -> __ -> __) -> (ident -> __ -> __) -> (nat -> term list
  -> __ -> __) -> (name -> term -> __ -> __ -> __) -> (name -> term -> term
  -> __ -> __ -> __ -> __) -> (term -> term list -> __ -> __ -> __ -> (term
  -> __ -> __) -> __ -> __) -> (kername -> __ -> __) -> (inductive -> nat ->
  term list -> (term -> __ -> __) -> __ -> __) -> ((inductive * nat) -> term
  -> (name list * term) list -> ((name list * term) -> __ -> __) -> __ -> __
  -> __) -> (Kernames.projection -> term -> __ -> __) -> (term mfixpoint ->
  nat -> (term def -> __ -> __) -> __ -> __) -> (term mfixpoint -> nat -> __
  -> __) -> (term -> __ -> __ -> __) -> (term -> __ -> __ -> __) -> (term
  prim_val -> primitive -> __ -> __ -> __) -> (term prim_val -> __ -> __ ->
  __) -> term -> __

val coq_FunctionalInduction_compile :
  (term -> coq_Term) coq_FunctionalInduction

val compile_global_decl : global_decl -> coq_Term envClass

val compile_ctx : global_declarations -> (kername * coq_Term envClass) list

val compile_program :
  erasure_configuration -> inductives_mapping -> Env.program -> coq_Term
  coq_Program

val program_Program :
  erasure_configuration -> inductives_mapping -> Env.program -> coq_Term
  coq_Program
