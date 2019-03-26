module Fappli_IEEE_extra where

import qualified Prelude
import qualified BinInt
import qualified BinPos
import qualified Datatypes
import qualified Fappli_IEEE
import qualified Fcore_Zaux

coq_BofZ :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer -> Fappli_IEEE.Coq_binary_float
coq_BofZ prec emax n =
  Fappli_IEEE.binary_normalize prec emax Fappli_IEEE.Coq_mode_NE n 0 Prelude.False

coq_ZofB :: Prelude.Integer -> Prelude.Integer -> Fappli_IEEE.Coq_binary_float -> Prelude.Maybe
            Prelude.Integer
coq_ZofB _ _ f =
  case f of {
   Fappli_IEEE.B754_zero _ -> Prelude.Just 0;
   Fappli_IEEE.B754_finite s m e0 ->
    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
      (\_ -> Prelude.Just (Fcore_Zaux.cond_Zopp s ((\x -> x) m)))
      (\e -> Prelude.Just
      ((Prelude.*) (Fcore_Zaux.cond_Zopp s ((\x -> x) m))
        (BinInt._Z__pow_pos (Fcore_Zaux.radix_val Fcore_Zaux.radix2) e)))
      (\e -> Prelude.Just
      (Fcore_Zaux.cond_Zopp s
        (BinInt._Z__div ((\x -> x) m)
          (BinInt._Z__pow_pos (Fcore_Zaux.radix_val Fcore_Zaux.radix2) e))))
      e0;
   _ -> Prelude.Nothing}

coq_ZofB_range :: Prelude.Integer -> Prelude.Integer -> Fappli_IEEE.Coq_binary_float ->
                  Prelude.Integer -> Prelude.Integer -> Prelude.Maybe Prelude.Integer
coq_ZofB_range prec emax f zmin zmax =
  case coq_ZofB prec emax f of {
   Prelude.Just z ->
    case (Prelude.&&) (BinInt._Z__leb zmin z) (BinInt._Z__leb z zmax) of {
     Prelude.True -> Prelude.Just z;
     Prelude.False -> Prelude.Nothing};
   Prelude.Nothing -> Prelude.Nothing}

pos_pow :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
pos_pow x y =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\y0 -> BinPos._Pos__mul x (BinPos._Pos__square (pos_pow x y0)))
    (\y0 -> BinPos._Pos__square (pos_pow x y0))
    (\_ -> x)
    y

coq_Bparse :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer -> Prelude.Integer ->
              Prelude.Integer -> Fappli_IEEE.Coq_binary_float
coq_Bparse prec emax base m e =
  let {emin = (Prelude.-) ((Prelude.-) ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1)) emax) prec}
  in
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> coq_BofZ prec emax ((\x -> x) m))
    (\p ->
    case BinInt._Z__ltb ((Prelude.*) e (BinInt._Z__log2 ((\x -> x) base))) emax of {
     Prelude.True -> coq_BofZ prec emax ((Prelude.*) ((\x -> x) m) ((\x -> x) (pos_pow base p)));
     Prelude.False -> Fappli_IEEE.B754_infinity Prelude.False})
    (\p ->
    case BinInt._Z__ltb
           ((Prelude.+) ((Prelude.*) e (BinInt._Z__log2 ((\x -> x) base)))
             (BinInt._Z__log2_up ((\x -> x) m))) emin of {
     Prelude.True -> Fappli_IEEE.B754_zero Prelude.False;
     Prelude.False ->
      Fappli_IEEE.coq_FF2B prec emax
        (case Fappli_IEEE.coq_Fdiv_core_binary prec ((\x -> x) m) 0 ((\x -> x) (pos_pow base p)) 0 of {
          (,) p0 lz ->
           case p0 of {
            (,) mz ez ->
             (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
               (\_ -> Fappli_IEEE.F754_nan Prelude.False 1)
               (\mz0 ->
               Fappli_IEEE.binary_round_aux prec emax Fappli_IEEE.Coq_mode_NE
                 (Datatypes.xorb Prelude.False Prelude.False) mz0 ez lz)
               (\_ -> Fappli_IEEE.F754_nan Prelude.False 1)
               mz}})})
    e

