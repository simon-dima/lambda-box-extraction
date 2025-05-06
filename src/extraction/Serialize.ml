open Ascii
open BinInt
open BinNums
open BinPos
open Byte
open Byte0
open ByteCompareSpec
open CeresS
open CeresSerialize
open CeresString
open Datatypes
open DecimalString
open FloatOps
open HexadecimalString
open Kernames
open List0
open MCString
open Malfunction
open Nat0
open PeanoNat
open PrimInt63
open Sint63
open SpecFloat
open String0
open Bytestring

(** val _escape_ident : String.t -> String.t -> String.t **)

let rec _escape_ident _end = function
| String.EmptyString -> _end
| String.String (c, s') ->
  if (||) ((||) (byte_reflect_eq c Coq_x27) (byte_reflect_eq c Coq_x20))
       (byte_reflect_eq c Coq_x2e)
  then String.String (Coq_x5f, (_escape_ident _end s'))
  else (match s' with
        | String.EmptyString -> String.String (c, (_escape_ident _end s'))
        | String.String (c2, s'') ->
          if IdentOT.reflect_eq_string (String.String (c, (String.String (c2,
               String.EmptyString)))) (String.String (Coq_xce, (String.String
               (Coq_x93, String.EmptyString))))
          then String.append (String.String (Coq_x47, (String.String
                 (Coq_x61, (String.String (Coq_x6d, (String.String (Coq_x6d,
                 (String.String (Coq_x61, String.EmptyString))))))))))
                 (_escape_ident _end s'')
          else if IdentOT.reflect_eq_string (String.String (c, (String.String
                    (c2, String.EmptyString)))) (String.String (Coq_xcf,
                    (String.String (Coq_x86, String.EmptyString))))
               then String.append (String.String (Coq_x50, (String.String
                      (Coq_x68, (String.String (Coq_x69,
                      String.EmptyString)))))) (_escape_ident _end s'')
               else if IdentOT.reflect_eq_string (String.String (c,
                         (String.String (c2, String.EmptyString))))
                         (String.String (Coq_xce, (String.String (Coq_x94,
                         String.EmptyString))))
                    then String.append (String.String (Coq_x44,
                           (String.String (Coq_x65, (String.String (Coq_x6c,
                           (String.String (Coq_x74, (String.String (Coq_x61,
                           String.EmptyString))))))))))
                           (_escape_ident _end s'')
                    else if IdentOT.reflect_eq_string (String.String (c,
                              (String.String (c2, String.EmptyString))))
                              (String.String (Coq_xcf, (String.String
                              (Coq_x80, String.EmptyString))))
                         then String.append (String.String (Coq_x70,
                                (String.String (Coq_x69,
                                String.EmptyString))))
                                (_escape_ident _end s'')
                         else if IdentOT.reflect_eq_string (String.String (c,
                                   (String.String (c2, String.EmptyString))))
                                   (String.String (Coq_xcf, (String.String
                                   (Coq_x81, String.EmptyString))))
                              then String.append (String.String (Coq_x72,
                                     (String.String (Coq_x68, (String.String
                                     (Coq_x6f, String.EmptyString))))))
                                     (_escape_ident _end s'')
                              else if IdentOT.reflect_eq_string
                                        (String.String (c, (String.String
                                        (c2, String.EmptyString))))
                                        (String.String (Coq_xce,
                                        (String.String (Coq_xa3,
                                        String.EmptyString))))
                                   then String.append (String.String
                                          (Coq_x53, (String.String (Coq_x69,
                                          (String.String (Coq_x67,
                                          (String.String (Coq_x6d,
                                          (String.String (Coq_x61,
                                          String.EmptyString))))))))))
                                          (_escape_ident _end s'')
                                   else String.String (c,
                                          (_escape_ident _end s')))

(** val coq_Serialize_Ident : Ident.t coq_Serialize **)

let coq_Serialize_Ident a =
  Atom_ (Raw
    (append (String ((Ascii (false, false, true, false, false, true, false,
      false)), EmptyString))
      (String.to_string (_escape_ident String.EmptyString a))))

(** val sint_to_Z : Uint63.t -> coq_Z **)

let sint_to_Z =
  to_Z

(** val coq_Serialize_int : Uint63.t coq_Serialize **)

let coq_Serialize_int i =
  to_sexp (coq_Serialize_Integral coq_Integral_Z) (sint_to_Z i)

(** val string_of_specfloat : spec_float -> string **)

let string_of_specfloat = function
| S754_zero sign ->
  if sign
  then String ((Ascii (true, false, true, true, false, true, false, false)),
         (String ((Ascii (false, false, false, false, true, true, false,
         false)), (String ((Ascii (false, true, true, true, false, true,
         false, false)), (String ((Ascii (false, false, false, false, true,
         true, false, false)), EmptyString)))))))
  else String ((Ascii (false, false, false, false, true, true, false,
         false)), (String ((Ascii (false, true, true, true, false, true,
         false, false)), (String ((Ascii (false, false, false, false, true,
         true, false, false)), EmptyString)))))
| S754_infinity sign ->
  if sign
  then String ((Ascii (false, true, true, true, false, true, true, false)),
         (String ((Ascii (true, false, true, false, false, true, true,
         false)), (String ((Ascii (true, true, true, false, false, true,
         true, false)), (String ((Ascii (true, true, true, true, true, false,
         true, false)), (String ((Ascii (true, false, false, true, false,
         true, true, false)), (String ((Ascii (false, true, true, true,
         false, true, true, false)), (String ((Ascii (false, true, true,
         false, false, true, true, false)), (String ((Ascii (true, false,
         false, true, false, true, true, false)), (String ((Ascii (false,
         true, true, true, false, true, true, false)), (String ((Ascii (true,
         false, false, true, false, true, true, false)), (String ((Ascii
         (false, false, true, false, true, true, true, false)), (String
         ((Ascii (true, false, false, true, true, true, true, false)),
         EmptyString)))))))))))))))))))))))
  else String ((Ascii (true, false, false, true, false, true, true, false)),
         (String ((Ascii (false, true, true, true, false, true, true,
         false)), (String ((Ascii (false, true, true, false, false, true,
         true, false)), (String ((Ascii (true, false, false, true, false,
         true, true, false)), (String ((Ascii (false, true, true, true,
         false, true, true, false)), (String ((Ascii (true, false, false,
         true, false, true, true, false)), (String ((Ascii (false, false,
         true, false, true, true, true, false)), (String ((Ascii (true,
         false, false, true, true, true, true, false)),
         EmptyString)))))))))))))))
| S754_nan ->
  String ((Ascii (false, true, true, true, false, true, true, false)),
    (String ((Ascii (true, false, false, false, false, true, true, false)),
    (String ((Ascii (false, true, true, true, false, true, true, false)),
    EmptyString)))))
| S754_finite (sign, p, z) ->
  let abs =
    append (String ((Ascii (false, false, false, false, true, true, false,
      false)), (String ((Ascii (false, false, false, true, true, true, true,
      false)), EmptyString))))
      (append (NilZero.string_of_uint (Pos.to_hex_uint p))
        (append (String ((Ascii (false, false, false, false, true, true,
          true, false)), EmptyString))
          (DecimalString.NilZero.string_of_int (BinInt.Z.to_int z))))
  in
  if sign
  then append (String ((Ascii (true, false, true, true, false, true, false,
         false)), EmptyString)) abs
  else abs

(** val coq_Serialize_numconst : numconst coq_Serialize **)

let coq_Serialize_numconst = function
| Coq_numconst_Int i ->
  to_sexp (coq_Serialize_Integral coq_Integral_Z) (sint_to_Z i)
| Coq_numconst_Bigint x ->
  Atom_ (Raw
    (append (CeresString.string_of_Z x) (String ((Ascii (false, true, true,
      true, false, true, false, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (false, true, false,
      false, false, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (true, true, true,
      false, false, true, true, false)), EmptyString))))))))))))
| Coq_numconst_Float64 x ->
  Atom_ (Raw
    (append (string_of_specfloat (coq_Prim2SF x)) (String ((Ascii (false,
      true, true, true, false, true, false, false)), (String ((Ascii (false,
      true, true, false, false, true, true, false)), (String ((Ascii (false,
      true, true, false, true, true, false, false)), (String ((Ascii (false,
      false, true, false, true, true, false, false)), EmptyString))))))))))

(** val coq_Cons : atom sexp_ -> atom sexp_ -> atom sexp_ **)

let coq_Cons x l = match l with
| Atom_ _ -> l
| List l0 -> List (x :: l0)

(** val coq_App : atom sexp_ -> atom sexp_ -> atom sexp_ **)

let coq_App l1 l2 =
  match l1 with
  | Atom_ _ -> l2
  | List l3 -> (match l2 with
                | Atom_ _ -> l2
                | List l4 -> List (app l3 l4))

(** val rawapp : atom sexp_ -> string -> atom sexp_ **)

let rawapp s a =
  match s with
  | Atom_ a0 -> (match a0 with
                 | Raw s0 -> Atom_ (Raw (append s0 a))
                 | _ -> s)
  | List _ -> s

(** val coq_Serialize_case : case coq_Serialize **)

let coq_Serialize_case = function
| Tag tag ->
  List ((Atom_ (Raw (String ((Ascii (false, false, true, false, true, true,
    true, false)), (String ((Ascii (true, false, false, false, false, true,
    true, false)), (String ((Ascii (true, true, true, false, false, true,
    true, false)), EmptyString)))))))) :: ((Atom_ (Num
    (sint_to_Z tag))) :: []))
| Deftag ->
  List ((Atom_ (Raw (String ((Ascii (false, false, true, false, true, true,
    true, false)), (String ((Ascii (true, false, false, false, false, true,
    true, false)), (String ((Ascii (true, true, true, false, false, true,
    true, false)), EmptyString)))))))) :: ((Atom_ (Raw (String ((Ascii (true,
    true, true, true, true, false, true, false)), EmptyString)))) :: []))
| Intrange p ->
  let (i1, i2) = p in
  if leb i1 i2
  then List
         ((to_sexp coq_Serialize_int i1) :: ((to_sexp coq_Serialize_int i2) :: []))
  else Atom_ (Raw (String ((Ascii (true, true, true, true, true, false, true,
         false)), EmptyString)))

(** val coq_Serialize_unary_num_op : unary_num_op coq_Serialize **)

let coq_Serialize_unary_num_op = function
| Neg ->
  Atom_ (Raw (String ((Ascii (false, true, true, true, false, true, true,
    false)), (String ((Ascii (true, false, true, false, false, true, true,
    false)), (String ((Ascii (true, true, true, false, false, true, true,
    false)), EmptyString)))))))
| Not ->
  Atom_ (Raw (String ((Ascii (false, true, true, true, false, true, true,
    false)), (String ((Ascii (true, true, true, true, false, true, true,
    false)), (String ((Ascii (false, false, true, false, true, true, true,
    false)), EmptyString)))))))

(** val numtype_to_string : numtype -> string **)

let numtype_to_string = function
| Coq_embed_inttype x ->
  (match x with
   | Int -> EmptyString
   | Int32 ->
     String ((Ascii (false, true, true, true, false, true, false, false)),
       (String ((Ascii (true, false, false, true, false, true, true, false)),
       (String ((Ascii (true, true, false, false, true, true, false, false)),
       (String ((Ascii (false, true, false, false, true, true, false,
       false)), EmptyString)))))))
   | Int64 ->
     String ((Ascii (false, true, true, true, false, true, false, false)),
       (String ((Ascii (true, false, false, true, false, true, true, false)),
       (String ((Ascii (false, true, true, false, true, true, false, false)),
       (String ((Ascii (false, false, true, false, true, true, false,
       false)), EmptyString)))))))
   | Bigint ->
     String ((Ascii (false, true, true, true, false, true, false, false)),
       (String ((Ascii (true, false, false, true, false, true, true, false)),
       (String ((Ascii (false, true, false, false, false, true, true,
       false)), (String ((Ascii (true, false, false, true, false, true, true,
       false)), (String ((Ascii (true, true, true, false, false, true, true,
       false)), EmptyString))))))))))
| Float64 -> EmptyString

(** val vector_type_to_string : vector_type -> string **)

let vector_type_to_string = function
| Array -> EmptyString
| Bytevec ->
  String ((Ascii (false, true, true, true, false, true, false, false)),
    (String ((Ascii (false, true, false, false, false, true, true, false)),
    (String ((Ascii (true, false, false, true, true, true, true, false)),
    (String ((Ascii (false, false, true, false, true, true, true, false)),
    (String ((Ascii (true, false, true, false, false, true, true, false)),
    EmptyString)))))))))

(** val coq_Serialize_binary_arith_op : binary_arith_op coq_Serialize **)

let coq_Serialize_binary_arith_op a =
  Atom_
    (match a with
     | Add ->
       Raw (String ((Ascii (true, true, false, true, false, true, false,
         false)), EmptyString))
     | Sub ->
       Raw (String ((Ascii (true, false, true, true, false, true, false,
         false)), EmptyString))
     | Mul ->
       Raw (String ((Ascii (false, true, false, true, false, true, false,
         false)), EmptyString))
     | Div ->
       Raw (String ((Ascii (true, true, true, true, false, true, false,
         false)), EmptyString))
     | Mod ->
       Raw (String ((Ascii (true, false, true, false, false, true, false,
         false)), EmptyString)))

(** val coq_Serialize_binary_bitwise_op : binary_bitwise_op coq_Serialize **)

let coq_Serialize_binary_bitwise_op a =
  Atom_
    (match a with
     | And ->
       Raw (String ((Ascii (false, true, true, false, false, true, false,
         false)), EmptyString))
     | Or ->
       Raw (String ((Ascii (false, false, true, true, true, true, true,
         false)), EmptyString))
     | Xor ->
       Raw (String ((Ascii (false, true, true, true, true, false, true,
         false)), EmptyString))
     | Lsl ->
       Raw (String ((Ascii (false, false, true, true, true, true, false,
         false)), (String ((Ascii (false, false, true, true, true, true,
         false, false)), EmptyString))))
     | Lsr ->
       Raw (String ((Ascii (false, true, true, true, true, true, false,
         false)), (String ((Ascii (false, true, true, true, true, true,
         false, false)), EmptyString))))
     | Asr ->
       Raw (String ((Ascii (true, false, false, false, false, true, true,
         false)), (String ((Ascii (false, true, true, true, true, true,
         false, false)), (String ((Ascii (false, true, true, true, true,
         true, false, false)), EmptyString)))))))

(** val coq_Serialize_binary_comparison : binary_comparison coq_Serialize **)

let coq_Serialize_binary_comparison a =
  Atom_
    (match a with
     | Lt ->
       Raw (String ((Ascii (false, false, true, true, true, true, false,
         false)), EmptyString))
     | Gt ->
       Raw (String ((Ascii (false, true, true, true, true, true, false,
         false)), EmptyString))
     | Lte ->
       Raw (String ((Ascii (false, false, true, true, true, true, false,
         false)), (String ((Ascii (true, false, true, true, true, true,
         false, false)), EmptyString))))
     | Gte ->
       Raw (String ((Ascii (false, true, true, true, true, true, false,
         false)), (String ((Ascii (true, false, true, true, true, true,
         false, false)), EmptyString))))
     | Eq ->
       Raw (String ((Ascii (true, false, true, true, true, true, false,
         false)), (String ((Ascii (true, false, true, true, true, true,
         false, false)), EmptyString)))))

(** val coq_Serialize_binary_num_op : binary_num_op coq_Serialize **)

let coq_Serialize_binary_num_op = function
| Coq_embed_binary_arith_op x -> to_sexp coq_Serialize_binary_arith_op x
| Coq_embed_binary_bitwise_op x -> to_sexp coq_Serialize_binary_bitwise_op x
| Coq_embed_binary_comparison x -> to_sexp coq_Serialize_binary_comparison x

(** val to_sexp_t : t -> atom sexp_ **)

let rec to_sexp_t = function
| Mvar x -> to_sexp coq_Serialize_Ident x
| Mlambda p ->
  let (ids, x) = p in
  List ((Atom_ (Raw (String ((Ascii (false, false, true, true, false, true,
  true, false)), (String ((Ascii (true, false, false, false, false, true,
  true, false)), (String ((Ascii (true, false, true, true, false, true, true,
  false)), (String ((Ascii (false, true, false, false, false, true, true,
  false)), (String ((Ascii (false, false, true, false, false, true, true,
  false)), (String ((Ascii (true, false, false, false, false, true, true,
  false)),
  EmptyString)))))))))))))) :: ((to_sexp
                                  (coq_Serialize_list coq_Serialize_Ident)
                                  ids) :: ((to_sexp_t x) :: [])))
| Mapply p ->
  let (x, args) = p in
  List ((Atom_ (Raw (String ((Ascii (true, false, false, false, false, true,
  true, false)), (String ((Ascii (false, false, false, false, true, true,
  true, false)), (String ((Ascii (false, false, false, false, true, true,
  true, false)), (String ((Ascii (false, false, true, true, false, true,
  true, false)), (String ((Ascii (true, false, false, true, true, true, true,
  false)), EmptyString)))))))))))) :: ((to_sexp_t x) :: (map to_sexp_t args)))
| Mlet p ->
  let (binds, x) = p in
  List ((Atom_ (Raw (String ((Ascii (false, false, true, true, false, true,
  true, false)), (String ((Ascii (true, false, true, false, false, true,
  true, false)), (String ((Ascii (false, false, true, false, true, true,
  true, false)),
  EmptyString)))))))) :: (app (map to_sexp_binding binds)
                           ((to_sexp_t x) :: [])))
| Mnum x -> to_sexp coq_Serialize_numconst x
| Mstring x -> Atom_ (Str (String.to_string x))
| Mglobal x ->
  to_sexp coq_Serialize_Ident
    (String.append (String.String (Coq_x64, (String.String (Coq_x65,
      (String.String (Coq_x66, (String.String (Coq_x5f,
      String.EmptyString)))))))) x)
| Mswitch p ->
  let (x, sels) = p in
  coq_Cons (Atom_ (Raw (String ((Ascii (true, true, false, false, true, true,
    true, false)), (String ((Ascii (true, true, true, false, true, true,
    true, false)), (String ((Ascii (true, false, false, true, false, true,
    true, false)), (String ((Ascii (false, false, true, false, true, true,
    true, false)), (String ((Ascii (true, true, false, false, false, true,
    true, false)), (String ((Ascii (false, false, false, true, false, true,
    true, false)), EmptyString))))))))))))))
    (coq_Cons (to_sexp_t x)
      (coq_Serialize_list (fun pat ->
        let (sel, t0) = pat in
        coq_App (to_sexp (coq_Serialize_list coq_Serialize_case) sel) (List
          ((to_sexp_t t0) :: []))) sels))
| Mnumop1 p ->
  let (p0, x) = p in
  let (op, num) = p0 in
  List
  ((rawapp (to_sexp coq_Serialize_unary_num_op op) (numtype_to_string num)) :: (
  (to_sexp_t x) :: []))
| Mnumop2 p ->
  let (p0, x2) = p in
  let (p1, x1) = p0 in
  let (op, num) = p1 in
  List
  ((rawapp (to_sexp coq_Serialize_binary_num_op op) (numtype_to_string num)) :: (
  (to_sexp_t x1) :: ((to_sexp_t x2) :: [])))
| Mconvert p ->
  let (p0, x) = p in
  let (from, to0) = p0 in
  List
  ((rawapp
     (rawapp (Atom_ (Raw (String ((Ascii (true, true, false, false, false,
       true, true, false)), (String ((Ascii (true, true, true, true, false,
       true, true, false)), (String ((Ascii (false, true, true, true, false,
       true, true, false)), (String ((Ascii (false, true, true, false, true,
       true, true, false)), (String ((Ascii (true, false, true, false, false,
       true, true, false)), (String ((Ascii (false, true, false, false, true,
       true, true, false)), (String ((Ascii (false, false, true, false, true,
       true, true, false)), EmptyString))))))))))))))))
       (numtype_to_string from)) (numtype_to_string to0)) :: ((to_sexp_t x) :: []))
| Mvecnew p ->
  let (p0, x2) = p in
  let (ty, x1) = p0 in
  List ((Atom_ (Raw
  (append (String ((Ascii (true, false, true, true, false, true, true,
    false)), (String ((Ascii (true, false, false, false, false, true, true,
    false)), (String ((Ascii (true, true, false, true, false, true, true,
    false)), (String ((Ascii (true, false, true, false, false, true, true,
    false)), (String ((Ascii (false, true, true, false, true, true, true,
    false)), (String ((Ascii (true, false, true, false, false, true, true,
    false)), (String ((Ascii (true, true, false, false, false, true, true,
    false)), EmptyString)))))))))))))) (vector_type_to_string ty)))) :: (
  (to_sexp_t x1) :: ((to_sexp_t x2) :: [])))
| Mvecget p ->
  let (p0, x2) = p in
  let (ty, x1) = p0 in
  List ((Atom_ (Raw
  (append (String ((Ascii (false, false, true, true, false, true, true,
    false)), (String ((Ascii (true, true, true, true, false, true, true,
    false)), (String ((Ascii (true, false, false, false, false, true, true,
    false)), (String ((Ascii (false, false, true, false, false, true, true,
    false)), EmptyString)))))))) (vector_type_to_string ty)))) :: ((to_sexp_t
                                                                    x1) :: (
  (to_sexp_t x2) :: [])))
| Mvecset p ->
  let (p0, x3) = p in
  let (p1, x2) = p0 in
  let (ty, x1) = p1 in
  List ((Atom_ (Raw
  (append (String ((Ascii (true, true, false, false, true, true, true,
    false)), (String ((Ascii (false, false, true, false, true, true, true,
    false)), (String ((Ascii (true, true, true, true, false, true, true,
    false)), (String ((Ascii (false, true, false, false, true, true, true,
    false)), (String ((Ascii (true, false, true, false, false, true, true,
    false)), EmptyString)))))))))) (vector_type_to_string ty)))) :: (
  (to_sexp_t x1) :: ((to_sexp_t x2) :: ((to_sexp_t x3) :: []))))
| Mveclen p ->
  let (ty, x) = p in
  List ((Atom_ (Raw
  (append (String ((Ascii (false, false, true, true, false, true, true,
    false)), (String ((Ascii (true, true, true, true, false, true, true,
    false)), (String ((Ascii (true, false, false, false, false, true, true,
    false)), (String ((Ascii (false, false, true, false, false, true, true,
    false)), EmptyString)))))))) (vector_type_to_string ty)))) :: ((to_sexp_t
                                                                    x) :: []))
| Mlazy x ->
  List ((Atom_ (Raw (String ((Ascii (false, false, true, true, false, true,
    true, false)), (String ((Ascii (true, false, false, false, false, true,
    true, false)), (String ((Ascii (false, true, false, true, true, true,
    true, false)), (String ((Ascii (true, false, false, true, true, true,
    true, false)), EmptyString)))))))))) :: ((to_sexp_t x) :: []))
| Mforce x ->
  List ((Atom_ (Raw (String ((Ascii (false, true, true, false, false, true,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), (String ((Ascii (false, true, false, false, true, true,
    true, false)), (String ((Ascii (true, true, false, false, false, true,
    true, false)), (String ((Ascii (true, false, true, false, false, true,
    true, false)), EmptyString)))))))))))) :: ((to_sexp_t x) :: []))
| Mblock p ->
  let (tag, xs) = p in
  List ((Atom_ (Raw (String ((Ascii (false, true, false, false, false, true,
  true, false)), (String ((Ascii (false, false, true, true, false, true,
  true, false)), (String ((Ascii (true, true, true, true, false, true, true,
  false)), (String ((Ascii (true, true, false, false, false, true, true,
  false)), (String ((Ascii (true, true, false, true, false, true, true,
  false)), EmptyString)))))))))))) :: ((List ((Atom_ (Raw (String ((Ascii
  (false, false, true, false, true, true, true, false)), (String ((Ascii
  (true, false, false, false, false, true, true, false)), (String ((Ascii
  (true, true, true, false, false, true, true, false)),
  EmptyString)))))))) :: ((Atom_ (Num
  (sint_to_Z tag))) :: []))) :: (map to_sexp_t xs)))
| Mfield p ->
  let (i, x) = p in
  List ((Atom_ (Raw (String ((Ascii (false, true, true, false, false, true,
  true, false)), (String ((Ascii (true, false, false, true, false, true,
  true, false)), (String ((Ascii (true, false, true, false, false, true,
  true, false)), (String ((Ascii (false, false, true, true, false, true,
  true, false)), (String ((Ascii (false, false, true, false, false, true,
  true, false)),
  EmptyString)))))))))))) :: ((to_sexp coq_Serialize_int i) :: ((to_sexp_t x) :: [])))

(** val to_sexp_binding : binding -> atom sexp_ **)

and to_sexp_binding = function
| Unnamed x ->
  List ((Atom_ (Raw (String ((Ascii (true, true, true, true, true, false,
    true, false)), EmptyString)))) :: ((to_sexp_t x) :: []))
| Named p ->
  let (id, x) = p in
  List ((to_sexp coq_Serialize_Ident id) :: ((to_sexp_t x) :: []))
| Recursive x ->
  coq_Cons (Atom_ (Raw (String ((Ascii (false, true, false, false, true,
    true, true, false)), (String ((Ascii (true, false, true, false, false,
    true, true, false)), (String ((Ascii (true, true, false, false, false,
    true, true, false)), EmptyString))))))))
    (coq_Serialize_list (coq_Serialize_product coq_Serialize_Ident to_sexp_t)
      x)

(** val coq_Serialize_t : t coq_Serialize **)

let coq_Serialize_t =
  to_sexp_t

(** val uncapitalize_char : byte -> byte **)

let uncapitalize_char c =
  let n = to_nat c in
  if (&&)
       (Nat.leb (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
         (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
         (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
         O))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) n)
       (Nat.leb n (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
         (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
         (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
         (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
         (S
         O)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  then (match of_nat
                (Nat0.add n (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                  (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                  O))))))))))))))))))))))))))))))))) with
        | Some c0 -> c0
        | None -> c)
  else c

(** val uncapitalize : String.t -> String.t **)

let uncapitalize = function
| String.EmptyString -> String.EmptyString
| String.String (c, s0) -> String.String ((uncapitalize_char c), s0)

(** val encode_name : String.t -> String.t **)

let encode_name s =
  _escape_ident String.EmptyString s

(** val bytestring_atom : String.t -> string **)

let bytestring_atom s =
  String ((Ascii (false, false, true, false, false, true, false, false)),
    (String.to_string s))

(** val find_prim : Ident.t -> primitives -> string prim_def option **)

let rec find_prim id = function
| [] -> None
| p :: prims0 ->
  let (kn, primdef) = p in
  if IdentOT.reflect_eq_string id kn
  then (match primdef with
        | Global (modname, label) ->
          Some (Global ((bytestring_atom modname), (bytestring_atom label)))
        | Primitive (symbol, arity) -> Some (Primitive (symbol, arity))
        | Erased -> Some Erased)
  else find_prim id prims0

(** val add_suffix : String.t -> nat -> String.t **)

let add_suffix x n =
  String.append x (string_of_nat n)

(** val binders : String.t -> nat -> String.t list -> String.t list **)

let rec binders x n acc =
  match n with
  | O -> acc
  | S n0 -> binders x n0 ((add_suffix x n0) :: acc)

(** val mk_eta_exp : nat -> atom -> atom sexp_ **)

let mk_eta_exp n s =
  let binders0 = binders (String.String (Coq_x78, String.EmptyString)) n [] in
  List ((Atom_ (Raw (String ((Ascii (false, false, true, true, false, true,
  true, false)), (String ((Ascii (true, false, false, false, false, true,
  true, false)), (String ((Ascii (true, false, true, true, false, true, true,
  false)), (String ((Ascii (false, true, false, false, false, true, true,
  false)), (String ((Ascii (false, false, true, false, false, true, true,
  false)), (String ((Ascii (true, false, false, false, false, true, true,
  false)),
  EmptyString)))))))))))))) :: ((to_sexp
                                  (coq_Serialize_list coq_Serialize_Ident)
                                  binders0) :: ((List ((Atom_
  s) :: (map (to_sexp coq_Serialize_Ident) binders0))) :: [])))

(** val global_serializer :
    primitives -> (Ident.t * t option) coq_Serialize **)

let global_serializer prims = function
| (i, b) ->
  (match b with
   | Some x ->
     to_sexp (coq_Serialize_product coq_Serialize_Ident coq_Serialize_t)
       ((String.append (String.String (Coq_x64, (String.String (Coq_x65,
          (String.String (Coq_x66, (String.String (Coq_x5f,
          String.EmptyString)))))))) i), x)
   | None ->
     (match find_prim i prims with
      | Some p ->
        (match p with
         | Global (modname, label) ->
           let na =
             String.to_string
               (uncapitalize
                 (String.append (String.String (Coq_x64, (String.String
                   (Coq_x65, (String.String (Coq_x66, (String.String
                   (Coq_x5f, String.EmptyString)))))))) (encode_name i)))
           in
           List ((Atom_ (Raw (String ((Ascii (false, false, true, false,
           false, true, false, false)), na)))) :: ((List ((Atom_ (Raw (String
           ((Ascii (true, true, true, false, false, true, true, false)),
           (String ((Ascii (false, false, true, true, false, true, true,
           false)), (String ((Ascii (true, true, true, true, false, true,
           true, false)), (String ((Ascii (false, true, false, false, false,
           true, true, false)), (String ((Ascii (true, false, false, false,
           false, true, true, false)), (String ((Ascii (false, false, true,
           true, false, true, true, false)),
           EmptyString)))))))))))))) :: ((Atom_ (Raw modname)) :: ((Atom_
           (Raw label)) :: [])))) :: []))
         | Primitive (symbol, arity) ->
           let na =
             String.to_string
               (uncapitalize
                 (String.append (String.String (Coq_x64, (String.String
                   (Coq_x65, (String.String (Coq_x66, (String.String
                   (Coq_x5f, String.EmptyString)))))))) (encode_name i)))
           in
           List ((Atom_ (Raw (String ((Ascii (false, false, true, false,
           false, true, false, false)),
           na)))) :: ((mk_eta_exp arity (Raw (String.to_string symbol))) :: []))
         | Erased ->
           let na =
             String.to_string
               (uncapitalize
                 (String.append (String.String (Coq_x64, (String.String
                   (Coq_x65, (String.String (Coq_x66, (String.String
                   (Coq_x5f, String.EmptyString)))))))) (encode_name i)))
           in
           List ((Atom_ (Raw (String ((Ascii (false, false, true, false,
           false, true, false, false)), na)))) :: ((List ((Atom_ (Raw (String
           ((Ascii (true, true, true, false, false, true, true, false)),
           (String ((Ascii (false, false, true, true, false, true, true,
           false)), (String ((Ascii (true, true, true, true, false, true,
           true, false)), (String ((Ascii (false, true, false, false, false,
           true, true, false)), (String ((Ascii (true, false, false, false,
           false, true, true, false)), (String ((Ascii (false, false, true,
           true, false, true, true, false)),
           EmptyString)))))))))))))) :: ((Atom_ (Raw (String ((Ascii (false,
           false, true, false, false, true, false, false)), (String ((Ascii
           (true, false, false, false, false, false, true, false)), (String
           ((Ascii (false, false, false, true, true, true, true, false)),
           (String ((Ascii (true, false, false, true, false, true, true,
           false)), (String ((Ascii (true, true, true, true, false, true,
           true, false)), (String ((Ascii (true, false, true, true, false,
           true, true, false)), (String ((Ascii (true, true, false, false,
           true, true, true, false)), EmptyString)))))))))))))))) :: ((Atom_
           (Raw (String ((Ascii (false, false, true, false, false, true,
           false, false)), na)))) :: [])))) :: [])))
      | None ->
        let na =
          String.to_string
            (uncapitalize
              (String.append (String.String (Coq_x64, (String.String
                (Coq_x65, (String.String (Coq_x66, (String.String (Coq_x5f,
                String.EmptyString)))))))) (encode_name i)))
        in
        List ((Atom_ (Raw (String ((Ascii (false, false, true, false, false,
        true, false, false)), na)))) :: ((List ((Atom_ (Raw (String ((Ascii
        (true, true, true, false, false, true, true, false)), (String ((Ascii
        (false, false, true, true, false, true, true, false)), (String
        ((Ascii (true, true, true, true, false, true, true, false)), (String
        ((Ascii (false, true, false, false, false, true, true, false)),
        (String ((Ascii (true, false, false, false, false, true, true,
        false)), (String ((Ascii (false, false, true, true, false, true,
        true, false)), EmptyString)))))))))))))) :: ((Atom_ (Raw (String
        ((Ascii (false, false, true, false, false, true, false, false)),
        (String ((Ascii (true, false, false, false, false, false, true,
        false)), (String ((Ascii (false, false, false, true, true, true,
        true, false)), (String ((Ascii (true, false, false, true, false,
        true, true, false)), (String ((Ascii (true, true, true, true, false,
        true, true, false)), (String ((Ascii (true, false, true, true, false,
        true, true, false)), (String ((Ascii (true, true, false, false, true,
        true, true, false)), EmptyString)))))))))))))))) :: ((Atom_ (Raw
        (String ((Ascii (false, false, true, false, false, true, false,
        false)), na)))) :: [])))) :: []))))

(** val filter_erased_prims :
    primitives -> (Ident.t * t option) list -> (Ident.t * t option) list **)

let rec filter_erased_prims prims = function
| [] -> []
| x :: xs ->
  let (id, o) = x in
  (match o with
   | Some _ -> x :: (filter_erased_prims prims xs)
   | None ->
     (match find_prim id prims with
      | Some p ->
        (match p with
         | Erased -> filter_erased_prims prims xs
         | _ -> x :: (filter_erased_prims prims xs))
      | None -> x :: (filter_erased_prims prims xs)))

(** val thename : byte list -> String.t -> String.t **)

let rec thename a = function
| String.EmptyString -> String.of_string (string_of_list_byte (rev a))
| String.String (b, s0) ->
  if byte_reflect_eq b Coq_x2e then thename [] s0 else thename (b :: a) s0

type program_type =
| Standalone
| Shared_lib of String.t * String.t

(** val shared_lib_register :
    String.t -> String.t -> (String.t * String.t) -> atom sexp_ **)

let shared_lib_register modname label = function
| (name, export) ->
  let code = List ((Atom_ (Raw (String ((Ascii (true, false, false, false,
    false, true, true, false)), (String ((Ascii (false, false, false, false,
    true, true, true, false)), (String ((Ascii (false, false, false, false,
    true, true, true, false)), (String ((Ascii (false, false, true, true,
    false, true, true, false)), (String ((Ascii (true, false, false, true,
    true, true, true, false)), EmptyString)))))))))))) :: ((List ((Atom_ (Raw
    (String ((Ascii (true, true, true, false, false, true, true, false)),
    (String ((Ascii (false, false, true, true, false, true, true, false)),
    (String ((Ascii (true, true, true, true, false, true, true, false)),
    (String ((Ascii (false, true, false, false, false, true, true, false)),
    (String ((Ascii (true, false, false, false, false, true, true, false)),
    (String ((Ascii (false, false, true, true, false, true, true, false)),
    EmptyString)))))))))))))) :: ((Atom_ (Raw
    (append (String ((Ascii (false, false, true, false, false, true, false,
      false)), EmptyString)) (String.to_string modname)))) :: ((Atom_ (Raw
    (append (String ((Ascii (false, false, true, false, false, true, false,
      false)), EmptyString)) (String.to_string label)))) :: [])))) :: ((Atom_
    (Str (String.to_string name))) :: ((Atom_ (Raw
    (append (String ((Ascii (false, false, true, false, false, true, false,
      false)), EmptyString)) (String.to_string export)))) :: []))))
  in
  coq_Cons (Atom_ (Raw (String ((Ascii (true, true, true, true, true, false,
    true, false)), EmptyString)))) (List (code :: []))

(** val coq_Serialize_module :
    primitives -> program_type -> String.t list -> program coq_Serialize **)

let coq_Serialize_module prims pt names = function
| (m, x) ->
  let main = String.String (Coq_x6d, (String.String (Coq_x61, (String.String
    (Coq_x69, (String.String (Coq_x6e, String.EmptyString)))))))
  in
  let names0 =
    app names ((String.String (Coq_x6d, (String.String (Coq_x61,
      (String.String (Coq_x69, (String.String (Coq_x6e,
      String.EmptyString)))))))) :: [])
  in
  let shortnames = map (fun name -> uncapitalize (thename [] name)) names0 in
  let longnames =
    map (fun name ->
      to_sexp coq_Serialize_Ident
        (String.append (String.String (Coq_x64, (String.String (Coq_x65,
          (String.String (Coq_x66, (String.String (Coq_x5f,
          String.EmptyString)))))))) name)) names0
  in
  let allnames = combine shortnames longnames in
  let exports =
    map (fun shortname -> Atom_ (Raw
      (append (String ((Ascii (false, false, true, false, false, true, false,
        false)), EmptyString)) (String.to_string shortname)))) shortnames
  in
  let m0 = filter_erased_prims prims m in
  let linkopt =
    match pt with
    | Standalone -> []
    | Shared_lib (modname, label) ->
      map (shared_lib_register modname label) (combine names0 shortnames)
  in
  (match coq_Cons (Atom_ (Raw (String ((Ascii (true, false, true, true,
           false, true, true, false)), (String ((Ascii (true, true, true,
           true, false, true, true, false)), (String ((Ascii (false, false,
           true, false, false, true, true, false)), (String ((Ascii (true,
           false, true, false, true, true, true, false)), (String ((Ascii
           (false, false, true, true, false, true, true, false)), (String
           ((Ascii (true, false, true, false, false, true, true, false)),
           EmptyString))))))))))))))
           (coq_Serialize_list (global_serializer prims)
             (rev ((main, (Some x)) :: m0))) with
   | Atom_ a -> Atom_ a
   | List l ->
     List
       (app l
         (app
           (map (fun pat0 ->
             let (shortname, longname) = pat0 in
             coq_Cons (Atom_ (Raw
               (append (String ((Ascii (false, false, true, false, false,
                 true, false, false)), EmptyString))
                 (String.to_string shortname)))) (List (longname :: [])))
             allnames)
           (app linkopt
             ((coq_Cons (Atom_ (Raw (String ((Ascii (true, false, true,
                false, false, true, true, false)), (String ((Ascii (false,
                false, false, true, true, true, true, false)), (String
                ((Ascii (false, false, false, false, true, true, true,
                false)), (String ((Ascii (true, true, true, true, false,
                true, true, false)), (String ((Ascii (false, true, false,
                false, true, true, true, false)), (String ((Ascii (false,
                false, true, false, true, true, true, false)),
                EmptyString)))))))))))))) (List exports)) :: [])))))
