open Ascii
open ExAst
open Extraction
open Optimize
open OptimizePropDiscr
open ResultMonad
open String0
open Transform0
open Utils
open Bytestring

(** val mk_params : bool -> bool -> extract_pcuic_params **)

let mk_params opt dearg =
  { optimize_prop_discr = opt; extract_transforms =
    (if dearg
     then (dearg_transform (fun _ -> None) true true true true false) :: []
     else []) }

(** val typed_transfoms :
    extract_pcuic_params -> global_env -> (global_env, String.t) result **)

let typed_transfoms params _UU03a3_ =
  if params.optimize_prop_discr
  then let _UU03a3_0 =
         timed (String ((Ascii (false, true, false, false, true, false, true,
           false)), (String ((Ascii (true, false, true, false, false, true,
           true, false)), (String ((Ascii (true, false, true, true, false,
           true, true, false)), (String ((Ascii (true, true, true, true,
           false, true, true, false)), (String ((Ascii (false, true, true,
           false, true, true, true, false)), (String ((Ascii (true, false,
           false, false, false, true, true, false)), (String ((Ascii (false,
           false, true, true, false, true, true, false)), (String ((Ascii
           (false, false, false, false, false, true, false, false)), (String
           ((Ascii (true, true, true, true, false, true, true, false)),
           (String ((Ascii (false, true, true, false, false, true, true,
           false)), (String ((Ascii (false, false, false, false, false, true,
           false, false)), (String ((Ascii (false, false, false, false, true,
           true, true, false)), (String ((Ascii (false, true, false, false,
           true, true, true, false)), (String ((Ascii (true, true, true,
           true, false, true, true, false)), (String ((Ascii (false, false,
           false, false, true, true, true, false)), (String ((Ascii (false,
           false, false, false, false, true, false, false)), (String ((Ascii
           (false, false, true, false, false, true, true, false)), (String
           ((Ascii (true, false, false, true, false, true, true, false)),
           (String ((Ascii (true, true, false, false, true, true, true,
           false)), (String ((Ascii (true, true, false, false, false, true,
           true, false)), (String ((Ascii (false, true, false, false, true,
           true, true, false)), (String ((Ascii (true, false, false, true,
           false, true, true, false)), (String ((Ascii (true, false, true,
           true, false, true, true, false)), (String ((Ascii (true, false,
           false, true, false, true, true, false)), (String ((Ascii (false,
           true, true, true, false, true, true, false)), (String ((Ascii
           (true, false, false, false, false, true, true, false)), (String
           ((Ascii (false, false, true, false, true, true, true, false)),
           (String ((Ascii (true, false, false, true, false, true, true,
           false)), (String ((Ascii (true, true, true, true, false, true,
           true, false)), (String ((Ascii (false, true, true, true, false,
           true, true, false)),
           EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
           (fun _ -> remove_match_on_box_env _UU03a3_)
       in
       compose_transforms params.extract_transforms _UU03a3_0
  else Ok _UU03a3_
