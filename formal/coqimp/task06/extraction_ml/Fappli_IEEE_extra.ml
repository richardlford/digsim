open BinInt
open BinNums
open BinPos
open Datatypes
open Fappli_IEEE
open Fcore_Zaux

(** val coq_BofZ : coq_Z -> coq_Z -> coq_Z -> binary_float **)

let coq_BofZ prec emax n =
  binary_normalize prec emax Coq_mode_NE n Z0 false

(** val coq_ZofB : coq_Z -> coq_Z -> binary_float -> coq_Z option **)

let coq_ZofB _ _ = function
| B754_zero _ -> Some Z0
| B754_finite (s, m, e0) ->
  (match e0 with
   | Z0 -> Some (cond_Zopp s (Zpos m))
   | Zpos e ->
     Some (Z.mul (cond_Zopp s (Zpos m)) (Z.pow_pos (radix_val radix2) e))
   | Zneg e ->
     Some (cond_Zopp s (Z.div (Zpos m) (Z.pow_pos (radix_val radix2) e))))
| _ -> None

(** val coq_ZofB_range :
    coq_Z -> coq_Z -> binary_float -> coq_Z -> coq_Z -> coq_Z option **)

let coq_ZofB_range prec emax f zmin zmax =
  match coq_ZofB prec emax f with
  | Some z -> if (&&) (Z.leb zmin z) (Z.leb z zmax) then Some z else None
  | None -> None

(** val pos_pow : positive -> positive -> positive **)

let rec pos_pow x = function
| Coq_xI y0 -> Pos.mul x (Pos.square (pos_pow x y0))
| Coq_xO y0 -> Pos.square (pos_pow x y0)
| Coq_xH -> x

(** val coq_Bparse :
    coq_Z -> coq_Z -> positive -> positive -> coq_Z -> binary_float **)

let coq_Bparse prec emax base m e =
  let emin = Z.sub (Z.sub (Zpos (Coq_xI Coq_xH)) emax) prec in
  (match e with
   | Z0 -> coq_BofZ prec emax (Zpos m)
   | Zpos p ->
     if Z.ltb (Z.mul e (Z.log2 (Zpos base))) emax
     then coq_BofZ prec emax (Z.mul (Zpos m) (Zpos (pos_pow base p)))
     else B754_infinity false
   | Zneg p ->
     if Z.ltb (Z.add (Z.mul e (Z.log2 (Zpos base))) (Z.log2_up (Zpos m))) emin
     then B754_zero false
     else coq_FF2B prec emax
            (let (p0, lz) =
               coq_Fdiv_core_binary prec (Zpos m) Z0 (Zpos (pos_pow base p))
                 Z0
             in
             let (mz, ez) = p0 in
             (match mz with
              | Zpos mz0 ->
                binary_round_aux prec emax Coq_mode_NE (xorb false false) mz0
                  ez lz
              | _ -> F754_nan (false, Coq_xH))))
