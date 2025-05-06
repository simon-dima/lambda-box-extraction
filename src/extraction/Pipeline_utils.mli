open AstCommon
open Byte
open Datatypes
open EProgram
open Erasure0
open Kernames
open List0
open Monad0
open Bytestring
open CompM

type coq_Options = { erasure_config : erasure_configuration;
                     inductives_mapping : inductives_mapping; direct : 
                     bool; c_args : nat; anf_conf : nat; show_anf : bool;
                     o_level : nat; time : bool; time_anf : bool;
                     debug : bool; dev : nat; prefix : String.t;
                     body_name : String.t;
                     prims : ((kername * String.t) * bool) list }

type coq_CompInfo = { time_log : String.t list; log : String.t list;
                      debug_log : String.t list }

type 'a pipelineM = (coq_Options, coq_CompInfo, 'a) compM

type ('src, 'dst) coq_CertiCoqTrans = 'src -> 'dst pipelineM

val get_options : coq_Options pipelineM

val debug_msg : String.t -> unit pipelineM

val chr_newline : byte

val newline : String.t

val log_to_string : String.t list -> String.t

val run_pipeline :
  coq_Options -> 'a1 -> ('a1, 'a2) coq_CertiCoqTrans -> 'a2 error * String.t

val timePhase_opt : coq_Options -> String.t -> (unit -> 'a1) -> 'a1

val coq_LiftCertiCoqTrans :
  String.t -> ('a1 -> 'a2) -> ('a1, 'a2) coq_CertiCoqTrans

val coq_LiftErrorCertiCoqTrans :
  String.t -> ('a1 -> 'a2 error) -> ('a1, 'a2) coq_CertiCoqTrans

val coq_LiftErrorLogCertiCoqTrans :
  String.t -> ('a1 -> 'a2 error * String.t) -> ('a1, 'a2) coq_CertiCoqTrans
