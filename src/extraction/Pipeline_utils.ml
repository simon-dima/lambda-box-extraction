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

(** val get_options : coq_Options pipelineM **)

let get_options =
  ask

(** val debug_msg : String.t -> unit pipelineM **)

let debug_msg s =
  bind (coq_MonadErrorT coq_MonadState) get_options (fun o ->
    if o.debug
    then bind (coq_MonadErrorT coq_MonadState) get (fun x ->
           let { time_log = tm; log = log0; debug_log = dbg } = x in
           put { time_log = tm; log = log0; debug_log = (s :: dbg) })
    else ret (coq_MonadErrorT coq_MonadState) ())

(** val chr_newline : byte **)

let chr_newline =
  Coq_x0a

(** val newline : String.t **)

let newline =
  String.String (chr_newline, String.EmptyString)

(** val log_to_string : String.t list -> String.t **)

let log_to_string log0 =
  String.concat newline ((String.String (Coq_x44, (String.String (Coq_x65,
    (String.String (Coq_x62, (String.String (Coq_x75, (String.String
    (Coq_x67, (String.String (Coq_x20, (String.String (Coq_x6d,
    (String.String (Coq_x65, (String.String (Coq_x73, (String.String
    (Coq_x73, (String.String (Coq_x61, (String.String (Coq_x67,
    (String.String (Coq_x65, (String.String (Coq_x73,
    String.EmptyString)))))))))))))))))))))))))))) :: (rev log0))

(** val run_pipeline :
    coq_Options -> 'a1 -> ('a1, 'a2) coq_CertiCoqTrans -> 'a2 error * String.t **)

let run_pipeline o src m =
  let w = { time_log = []; log = []; debug_log = [] } in
  let (res, c_info) = m src o w in
  ((Obj.magic res), (log_to_string c_info.debug_log))

(** val timePhase_opt : coq_Options -> String.t -> (unit -> 'a1) -> 'a1 **)

let timePhase_opt o s f =
  if o.time then timePhase s f () else f ()

(** val coq_LiftCertiCoqTrans :
    String.t -> ('a1 -> 'a2) -> ('a1, 'a2) coq_CertiCoqTrans **)

let coq_LiftCertiCoqTrans name f s =
  bind (coq_MonadErrorT coq_MonadState) get_options (fun o ->
    ret (coq_MonadErrorT coq_MonadState) (timePhase_opt o name (fun _ -> f s)))

(** val coq_LiftErrorCertiCoqTrans :
    String.t -> ('a1 -> 'a2 error) -> ('a1, 'a2) coq_CertiCoqTrans **)

let coq_LiftErrorCertiCoqTrans name f s o inf =
  timePhase_opt o name (fun _ -> ((Obj.magic f s), inf))

(** val coq_LiftErrorLogCertiCoqTrans :
    String.t -> ('a1 -> 'a2 error * String.t) -> ('a1, 'a2) coq_CertiCoqTrans **)

let coq_LiftErrorLogCertiCoqTrans name f s o inf =
  timePhase_opt o name (fun _ ->
    let (res, dbg) = f s in
    let inf' = { time_log = inf.time_log; log = inf.log; debug_log =
      (dbg :: inf.debug_log) }
    in
    ((Obj.magic res), inf'))
