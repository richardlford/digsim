module Fappli_IEEE_extra where

import qualified Prelude
import qualified BinInt
import qualified BinPos
import qualified Datatypes
import qualified Fappli_IEEE
import qualified Fcore_Zaux
import qualified Logic
import qualified ZArith_dec

__ :: any
__ = Prelude.error "Logical or arity value used"

is_finite_pos0 :: Prelude.Integer -> Prelude.Integer -> Fappli_IEEE.Coq_binary_float
                  -> Prelude.Bool
is_finite_pos0 _ _ f =
  case f of {
   Fappli_IEEE.B754_zero s -> Prelude.not s;
   Fappli_IEEE.B754_finite _ _ _ -> Prelude.True;
   _ -> Prelude.False}

coq_Beq_dec :: Prelude.Integer -> Prelude.Integer -> Fappli_IEEE.Coq_binary_float ->
               Fappli_IEEE.Coq_binary_float -> Prelude.Bool
coq_Beq_dec _ _ f1 f2 =
  case f1 of {
   Fappli_IEEE.B754_zero b ->
    case f2 of {
     Fappli_IEEE.B754_zero b0 ->
      case b of {
       Prelude.True ->
        case b0 of {
         Prelude.True -> Prelude.True;
         Prelude.False -> Prelude.False};
       Prelude.False ->
        case b0 of {
         Prelude.True -> Prelude.False;
         Prelude.False -> Prelude.True}};
     _ -> Prelude.False};
   Fappli_IEEE.B754_infinity b ->
    case f2 of {
     Fappli_IEEE.B754_infinity b0 ->
      case b of {
       Prelude.True ->
        case b0 of {
         Prelude.True -> Prelude.True;
         Prelude.False -> Prelude.False};
       Prelude.False ->
        case b0 of {
         Prelude.True -> Prelude.False;
         Prelude.False -> Prelude.True}};
     _ -> Prelude.False};
   Fappli_IEEE.B754_nan b n ->
    case f2 of {
     Fappli_IEEE.B754_nan b0 n0 ->
      case b of {
       Prelude.True ->
        case b0 of {
         Prelude.True ->
          let {s = BinPos._Pos__eq_dec n n0} in
          case s of {
           Prelude.True -> Logic.eq_rec_r n0 (\_ -> Prelude.True) n __;
           Prelude.False -> Prelude.False};
         Prelude.False -> Prelude.False};
       Prelude.False ->
        case b0 of {
         Prelude.True -> Prelude.False;
         Prelude.False ->
          let {s = BinPos._Pos__eq_dec n n0} in
          case s of {
           Prelude.True -> Logic.eq_rec_r n0 (\_ -> Prelude.True) n __;
           Prelude.False -> Prelude.False}}};
     _ -> Prelude.False};
   Fappli_IEEE.B754_finite b m e ->
    case f2 of {
     Fappli_IEEE.B754_finite b0 m0 e1 ->
      case b of {
       Prelude.True ->
        case b0 of {
         Prelude.True ->
          let {s = BinPos._Pos__eq_dec m m0} in
          case s of {
           Prelude.True ->
            let {s0 = BinInt._Z__eq_dec e e1} in
            case s0 of {
             Prelude.True ->
              Logic.eq_rec_r m0 (\_ -> Logic.eq_rec_r e1 (\_ -> Prelude.True) e __)
                m __;
             Prelude.False -> Prelude.False};
           Prelude.False -> Prelude.False};
         Prelude.False -> Prelude.False};
       Prelude.False ->
        case b0 of {
         Prelude.True -> Prelude.False;
         Prelude.False ->
          let {s = BinPos._Pos__eq_dec m m0} in
          case s of {
           Prelude.True ->
            let {s0 = BinInt._Z__eq_dec e e1} in
            case s0 of {
             Prelude.True ->
              Logic.eq_rec_r m0 (\_ -> Logic.eq_rec_r e1 (\_ -> Prelude.True) e __)
                m __;
             Prelude.False -> Prelude.False};
           Prelude.False -> Prelude.False}}};
     _ -> Prelude.False}}

coq_BofZ :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer ->
            Fappli_IEEE.Coq_binary_float
coq_BofZ prec emax n =
  Fappli_IEEE.binary_normalize prec emax Fappli_IEEE.Coq_mode_NE n 0 Prelude.False

int_round_odd :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
int_round_odd x p =
  (Prelude.*)
    (case (Prelude.||)
            (BinInt._Z__eqb
              (BinInt._Z__modulo x
                (BinInt._Z__pow ((\x -> x) ((\x -> 2 Prelude.* x) 1)) p)) 0)
            (BinInt._Z__odd
              (BinInt._Z__div x
                (BinInt._Z__pow ((\x -> x) ((\x -> 2 Prelude.* x) 1)) p))) of {
      Prelude.True ->
       BinInt._Z__div x (BinInt._Z__pow ((\x -> x) ((\x -> 2 Prelude.* x) 1)) p);
      Prelude.False ->
       (Prelude.+)
         (BinInt._Z__div x (BinInt._Z__pow ((\x -> x) ((\x -> 2 Prelude.* x) 1)) p))
         ((\x -> x) 1)}) (BinInt._Z__pow ((\x -> x) ((\x -> 2 Prelude.* x) 1)) p)

coq_ZofB :: Prelude.Integer -> Prelude.Integer -> Fappli_IEEE.Coq_binary_float ->
            Prelude.Maybe Prelude.Integer
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

coq_ZofB_range :: Prelude.Integer -> Prelude.Integer -> Fappli_IEEE.Coq_binary_float
                  -> Prelude.Integer -> Prelude.Integer -> Prelude.Maybe
                  Prelude.Integer
coq_ZofB_range prec emax f zmin zmax =
  case coq_ZofB prec emax f of {
   Prelude.Just z ->
    case (Prelude.&&) (BinInt._Z__leb zmin z) (BinInt._Z__leb z zmax) of {
     Prelude.True -> Prelude.Just z;
     Prelude.False -> Prelude.Nothing};
   Prelude.Nothing -> Prelude.Nothing}

coq_Bexact_inverse_mantissa :: Prelude.Integer -> Prelude.Integer
coq_Bexact_inverse_mantissa prec =
  BinInt._Z__iter ((Prelude.-) prec ((\x -> x) 1)) (\x -> (\x -> 2 Prelude.* x) x) 1

coq_Bexact_inverse :: Prelude.Integer -> Prelude.Integer ->
                      Fappli_IEEE.Coq_binary_float -> Prelude.Maybe
                      Fappli_IEEE.Coq_binary_float
coq_Bexact_inverse prec emax f =
  let {
   emin = (Prelude.-)
            ((Prelude.-) ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1)) emax)
            prec}
  in
  case f of {
   Fappli_IEEE.B754_finite s m e ->
    case BinPos._Pos__eq_dec m (coq_Bexact_inverse_mantissa prec) of {
     Prelude.True ->
      let {
       e' = (Prelude.-) (BinInt._Z__opp e)
              ((Prelude.*) ((Prelude.-) prec ((\x -> x) 1)) ((\x -> x)
                ((\x -> 2 Prelude.* x) 1)))}
      in
      case ZArith_dec.coq_Z_le_dec emin e' of {
       Prelude.True ->
        case ZArith_dec.coq_Z_le_dec e' emax of {
         Prelude.True -> Prelude.Just (Fappli_IEEE.B754_finite s m e');
         Prelude.False -> Prelude.Nothing};
       Prelude.False -> Prelude.Nothing};
     Prelude.False -> Prelude.Nothing};
   _ -> Prelude.Nothing}

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

coq_Bparse :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer ->
              Prelude.Integer -> Prelude.Integer -> Fappli_IEEE.Coq_binary_float
coq_Bparse prec emax base m e =
  let {
   emin = (Prelude.-)
            ((Prelude.-) ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1)) emax)
            prec}
  in
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> coq_BofZ prec emax ((\x -> x) m))
    (\p ->
    case BinInt._Z__ltb ((Prelude.*) e (BinInt._Z__log2 ((\x -> x) base))) emax of {
     Prelude.True ->
      coq_BofZ prec emax ((Prelude.*) ((\x -> x) m) ((\x -> x) (pos_pow base p)));
     Prelude.False -> Fappli_IEEE.B754_infinity Prelude.False})
    (\p ->
    case BinInt._Z__ltb
           ((Prelude.+) ((Prelude.*) e (BinInt._Z__log2 ((\x -> x) base)))
             (BinInt._Z__log2_up ((\x -> x) m))) emin of {
     Prelude.True -> Fappli_IEEE.B754_zero Prelude.False;
     Prelude.False ->
      Fappli_IEEE.coq_FF2B prec emax
        (case Fappli_IEEE.coq_Fdiv_core_binary prec ((\x -> x) m) 0 ((\x -> x)
                (pos_pow base p)) 0 of {
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

coq_Bconv :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer ->
             Prelude.Integer -> (Prelude.Bool -> Fappli_IEEE.Coq_nan_pl -> (,)
             Prelude.Bool Fappli_IEEE.Coq_nan_pl) -> Fappli_IEEE.Coq_mode ->
             Fappli_IEEE.Coq_binary_float -> Fappli_IEEE.Coq_binary_float
coq_Bconv _ _ prec2 emax2 conv_nan md f =
  case f of {
   Fappli_IEEE.B754_nan s pl ->
    case conv_nan s pl of {
     (,) s0 pl0 -> Fappli_IEEE.B754_nan s0 pl0};
   Fappli_IEEE.B754_finite s m e ->
    Fappli_IEEE.binary_normalize prec2 emax2 md
      (Fcore_Zaux.cond_Zopp s ((\x -> x) m)) e s;
   x -> x}

