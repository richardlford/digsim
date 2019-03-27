open Archi
open BinNums
open BinPos
open Coqlib
open Datatypes
open Fappli_IEEE
open Fappli_IEEE_bits
open Fappli_IEEE_extra
open Fcore_Zaux
open Integers

type float = binary64

(** val cmp_of_comparison : comparison -> Datatypes.comparison option -> bool **)

let cmp_of_comparison c x =
  match c with
  | Ceq -> (match x with
            | Some c0 -> (match c0 with
                          | Eq -> true
                          | _ -> false)
            | None -> false)
  | Cne -> (match x with
            | Some c0 -> (match c0 with
                          | Eq -> false
                          | _ -> true)
            | None -> true)
  | Clt -> (match x with
            | Some c0 -> (match c0 with
                          | Lt -> true
                          | _ -> false)
            | None -> false)
  | Cle -> (match x with
            | Some c0 -> (match c0 with
                          | Gt -> false
                          | _ -> true)
            | None -> false)
  | Cgt -> (match x with
            | Some c0 -> (match c0 with
                          | Gt -> true
                          | _ -> false)
            | None -> false)
  | Cge -> (match x with
            | Some c0 -> (match c0 with
                          | Lt -> false
                          | _ -> true)
            | None -> false)

module Float =
 struct
  (** val transform_quiet_pl : nan_pl -> nan_pl **)

  let transform_quiet_pl pl =
    Pos.coq_lor pl
      (iter_nat (fun x -> Coq_xO x) (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        O))))))))))))))))))))))))))))))))))))))))))))))))))) Coq_xH)

  (** val neg_pl : bool -> nan_pl -> bool * nan_pl **)

  let neg_pl s pl =
    ((negb s), pl)

  (** val abs_pl : bool -> nan_pl -> bool * nan_pl **)

  let abs_pl _ pl =
    (false, pl)

  (** val binop_pl : binary64 -> binary64 -> bool * nan_pl **)

  let binop_pl x y =
    match x with
    | B754_nan (s1, pl1) ->
      (match y with
       | B754_nan (s2, pl2) ->
         if choose_binop_pl_64 s1 pl1 s2 pl2
         then (s2, (transform_quiet_pl pl2))
         else (s1, (transform_quiet_pl pl1))
       | _ -> (s1, (transform_quiet_pl pl1)))
    | _ -> (match y with
            | B754_nan (s2, pl2) -> (s2, (transform_quiet_pl pl2))
            | _ -> default_pl_64)

  (** val zero : float **)

  let zero =
    B754_zero false

  (** val neg : float -> float **)

  let neg =
    coq_Bopp (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))) (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))))) neg_pl

  (** val abs : float -> float **)

  let abs =
    coq_Babs (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))) (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))))) abs_pl

  (** val add : float -> float -> float **)

  let add =
    coq_Bplus (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))) (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))))) binop_pl Coq_mode_NE

  (** val sub : float -> float -> float **)

  let sub =
    coq_Bminus (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))) (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))))) binop_pl Coq_mode_NE

  (** val mul : float -> float -> float **)

  let mul =
    coq_Bmult (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))) (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))))) binop_pl Coq_mode_NE

  (** val div : float -> float -> float **)

  let div =
    coq_Bdiv (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))) (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))))) binop_pl Coq_mode_NE

  (** val compare : float -> float -> Datatypes.comparison option **)

  let compare f1 f2 =
    coq_Bcompare (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))) (Zpos (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))))) f1 f2

  (** val cmp : comparison -> float -> float -> bool **)

  let cmp c f1 f2 =
    cmp_of_comparison c (compare f1 f2)

  (** val to_long : float -> Int64.int option **)

  let to_long f =
    option_map Int64.repr
      (coq_ZofB_range (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))) (Zpos (Coq_xO (Coq_xO (Coq_xO
        (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))))) f Int64.min_signed
        Int64.max_signed)

  (** val from_parsed : positive -> positive -> coq_Z -> float **)

  let from_parsed base intPart expPart =
    coq_Bparse (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))) (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))))) base intPart expPart

  (** val to_bits : float -> Int64.int **)

  let to_bits f =
    Int64.repr (bits_of_b64 f)

  (** val of_bits : Int64.int -> float **)

  let of_bits b =
    b64_of_bits (Int64.unsigned b)
 end
