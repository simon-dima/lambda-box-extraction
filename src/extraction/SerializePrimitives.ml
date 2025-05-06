open Ascii
open CeresDeserialize
open CeresS
open Datatypes
open EPrimitive
open Primitive
open String0

(** val prim_int_of_string : string -> Uint63.t **)

let prim_int_of_string = (fun s -> s |> Camlcoq.camlstring_of_coqstring |> Int64.of_string |> Uint63.of_int64)

(** val prim_float_of_string : string -> Float64.t **)

let prim_float_of_string = (fun s -> s |> Camlcoq.camlstring_of_coqstring |> Int64.of_string |> Int64.float_of_bits |> Float64.of_float)

(** val coq_Deserialize_prim_tag : prim_tag coq_Deserialize **)

let coq_Deserialize_prim_tag l e =
  Deser.match_con (String ((Ascii (false, false, false, false, true, true,
    true, false)), (String ((Ascii (false, true, false, false, true, true,
    true, false)), (String ((Ascii (true, false, false, true, false, true,
    true, false)), (String ((Ascii (true, false, true, true, false, true,
    true, false)), (String ((Ascii (true, true, true, true, true, false,
    true, false)), (String ((Ascii (false, false, true, false, true, true,
    true, false)), (String ((Ascii (true, false, false, false, false, true,
    true, false)), (String ((Ascii (true, true, true, false, false, true,
    true, false)), EmptyString)))))))))))))))) (((String ((Ascii (false,
    false, false, false, true, true, true, false)), (String ((Ascii (false,
    true, false, false, true, true, true, false)), (String ((Ascii (true,
    false, false, true, false, true, true, false)), (String ((Ascii (true,
    false, true, true, false, true, true, false)), (String ((Ascii (true,
    false, false, true, false, false, true, false)), (String ((Ascii (false,
    true, true, true, false, true, true, false)), (String ((Ascii (false,
    false, true, false, true, true, true, false)), EmptyString)))))))))))))),
    Coq_primInt) :: (((String ((Ascii (false, false, false, false, true,
    true, true, false)), (String ((Ascii (false, true, false, false, true,
    true, true, false)), (String ((Ascii (true, false, false, true, false,
    true, true, false)), (String ((Ascii (true, false, true, true, false,
    true, true, false)), (String ((Ascii (false, true, true, false, false,
    false, true, false)), (String ((Ascii (false, false, true, true, false,
    true, true, false)), (String ((Ascii (true, true, true, true, false,
    true, true, false)), (String ((Ascii (true, false, false, false, false,
    true, true, false)), (String ((Ascii (false, false, true, false, true,
    true, true, false)), EmptyString)))))))))))))))))),
    Coq_primFloat) :: (((String ((Ascii (false, false, false, false, true,
    true, true, false)), (String ((Ascii (false, true, false, false, true,
    true, true, false)), (String ((Ascii (true, false, false, true, false,
    true, true, false)), (String ((Ascii (true, false, true, true, false,
    true, true, false)), (String ((Ascii (true, false, false, false, false,
    false, true, false)), (String ((Ascii (false, true, false, false, true,
    true, true, false)), (String ((Ascii (false, true, false, false, true,
    true, true, false)), (String ((Ascii (true, false, false, false, false,
    true, true, false)), (String ((Ascii (true, false, false, true, true,
    true, true, false)), EmptyString)))))))))))))))))),
    Coq_primArray) :: []))) [] l e

(** val coq_Deserialize_prim_int : Uint63.t coq_Deserialize **)

let coq_Deserialize_prim_int l = function
| Atom_ a ->
  (match a with
   | Str i -> Coq_inr (prim_int_of_string i)
   | _ ->
     Coq_inl (DeserError (l, (MsgStr (String ((Ascii (true, false, true,
       false, false, true, true, false)), (String ((Ascii (false, true,
       false, false, true, true, true, false)), (String ((Ascii (false, true,
       false, false, true, true, true, false)), (String ((Ascii (true, true,
       true, true, false, true, true, false)), (String ((Ascii (false, true,
       false, false, true, true, true, false)), EmptyString))))))))))))))
| List _ ->
  Coq_inl (DeserError (l, (MsgStr (String ((Ascii (true, false, true, false,
    false, true, true, false)), (String ((Ascii (false, true, false, false,
    true, true, true, false)), (String ((Ascii (false, true, false, false,
    true, true, true, false)), (String ((Ascii (true, true, true, true,
    false, true, true, false)), (String ((Ascii (false, true, false, false,
    true, true, true, false)), EmptyString)))))))))))))

(** val coq_Deserialize_prim_float : Float64.t coq_Deserialize **)

let coq_Deserialize_prim_float l = function
| Atom_ a ->
  (match a with
   | Str s -> Coq_inr (prim_float_of_string s)
   | _ ->
     Coq_inl (DeserError (l, (MsgStr (String ((Ascii (true, false, true,
       false, false, true, true, false)), (String ((Ascii (false, true,
       false, false, true, true, true, false)), (String ((Ascii (false, true,
       false, false, true, true, true, false)), (String ((Ascii (true, true,
       true, true, false, true, true, false)), (String ((Ascii (false, true,
       false, false, true, true, true, false)), EmptyString))))))))))))))
| List _ ->
  Coq_inl (DeserError (l, (MsgStr (String ((Ascii (true, false, true, false,
    false, true, true, false)), (String ((Ascii (false, true, false, false,
    true, true, true, false)), (String ((Ascii (false, true, false, false,
    true, true, true, false)), (String ((Ascii (true, true, true, true,
    false, true, true, false)), (String ((Ascii (false, true, false, false,
    true, true, true, false)), EmptyString)))))))))))))

(** val coq_Deserialize_array_model :
    'a1 coq_Deserialize -> 'a1 array_model coq_Deserialize **)

let coq_Deserialize_array_model h l e =
  Deser.match_con (String ((Ascii (true, false, false, false, false, true,
    true, false)), (String ((Ascii (false, true, false, false, true, true,
    true, false)), (String ((Ascii (false, true, false, false, true, true,
    true, false)), (String ((Ascii (true, false, false, false, false, true,
    true, false)), (String ((Ascii (true, false, false, true, true, true,
    true, false)), (String ((Ascii (true, true, true, true, true, false,
    true, false)), (String ((Ascii (true, false, true, true, false, true,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), (String ((Ascii (false, false, true, false, false, true,
    true, false)), (String ((Ascii (true, false, true, false, false, true,
    true, false)), (String ((Ascii (false, false, true, true, false, true,
    true, false)), EmptyString)))))))))))))))))))))) [] (((String ((Ascii
    (true, false, false, false, false, true, true, false)), (String ((Ascii
    (false, true, false, false, true, true, true, false)), (String ((Ascii
    (false, true, false, false, true, true, true, false)), (String ((Ascii
    (true, false, false, false, false, true, true, false)), (String ((Ascii
    (true, false, false, true, true, true, true, false)), (String ((Ascii
    (true, true, true, true, true, false, true, false)), (String ((Ascii
    (true, false, true, true, false, true, true, false)), (String ((Ascii
    (true, true, true, true, false, true, true, false)), (String ((Ascii
    (false, false, true, false, false, true, true, false)), (String ((Ascii
    (true, false, true, false, false, true, true, false)), (String ((Ascii
    (false, false, true, true, false, true, true, false)),
    EmptyString)))))))))))))))))))))),
    (Deser.con2_ (fun x x0 -> { array_default = x; array_value = x0 }) h
      (coq_Deserialize_list h))) :: []) l e

(** val coq_Deserialize_prim_val :
    'a1 coq_Deserialize -> 'a1 prim_val coq_Deserialize **)

let coq_Deserialize_prim_val h l = function
| Atom_ _ ->
  Coq_inl (DeserError (l, (MsgStr (String ((Ascii (true, false, true, false,
    false, true, true, false)), (String ((Ascii (false, true, false, false,
    true, true, true, false)), (String ((Ascii (false, true, false, false,
    true, true, true, false)), (String ((Ascii (true, true, true, true,
    false, true, true, false)), (String ((Ascii (false, true, false, false,
    true, true, true, false)), EmptyString)))))))))))))
| List xs ->
  (match xs with
   | [] ->
     Coq_inl (DeserError (l, (MsgStr (String ((Ascii (true, false, true,
       false, false, true, true, false)), (String ((Ascii (false, true,
       false, false, true, true, true, false)), (String ((Ascii (false, true,
       false, false, true, true, true, false)), (String ((Ascii (true, true,
       true, true, false, true, true, false)), (String ((Ascii (false, true,
       false, false, true, true, true, false)), EmptyString)))))))))))))
   | e1 :: l0 ->
     (match l0 with
      | [] ->
        Coq_inl (DeserError (l, (MsgStr (String ((Ascii (true, false, true,
          false, false, true, true, false)), (String ((Ascii (false, true,
          false, false, true, true, true, false)), (String ((Ascii (false,
          true, false, false, true, true, true, false)), (String ((Ascii
          (true, true, true, true, false, true, true, false)), (String
          ((Ascii (false, true, false, false, true, true, true, false)),
          EmptyString)))))))))))))
      | e2 :: l1 ->
        (match l1 with
         | [] ->
           let t = _from_sexp coq_Deserialize_prim_tag l e1 in
           (match t with
            | Coq_inl e0 -> Coq_inl e0
            | Coq_inr p ->
              (match p with
               | Coq_primInt ->
                 let v = _from_sexp coq_Deserialize_prim_int l e2 in
                 (match v with
                  | Coq_inl e0 -> Coq_inl e0
                  | Coq_inr v0 -> Coq_inr (prim_int v0))
               | Coq_primFloat ->
                 let v = _from_sexp coq_Deserialize_prim_float l e2 in
                 (match v with
                  | Coq_inl e0 -> Coq_inl e0
                  | Coq_inr v0 -> Coq_inr (prim_float v0))
               | Coq_primArray ->
                 let v = _from_sexp (coq_Deserialize_array_model h) l e2 in
                 (match v with
                  | Coq_inl e0 -> Coq_inl e0
                  | Coq_inr v0 -> Coq_inr (prim_array v0))))
         | _ :: _ ->
           Coq_inl (DeserError (l, (MsgStr (String ((Ascii (true, false,
             true, false, false, true, true, false)), (String ((Ascii (false,
             true, false, false, true, true, true, false)), (String ((Ascii
             (false, true, false, false, true, true, true, false)), (String
             ((Ascii (true, true, true, true, false, true, true, false)),
             (String ((Ascii (false, true, false, false, true, true, true,
             false)), EmptyString))))))))))))))))
