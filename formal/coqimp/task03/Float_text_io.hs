{-# OPTIONS_GHC -cpp -XMagicHash #-}
{- For Hugs, use the option -F"cpp -P -traditional" -}

module Float_text_io where

import qualified Prelude
import qualified BinInt
import qualified Decimal
import qualified Fappli_IEEE
import qualified Fappli_IEEE_bits
import qualified Fappli_IEEE_extra
import qualified Floats
import qualified Integers
import qualified Nat
import qualified PeanoNat
import qualified Specif
import qualified String0
import qualified Monad
import qualified Data.Char
import qualified Data.Bits

#ifdef __GLASGOW_HASKELL__
import qualified GHC.Base
#else
-- HUGS
import qualified IOExts
#endif

#ifdef __GLASGOW_HASKELL__
unsafeCoerce :: a -> b
unsafeCoerce = GHC.Base.unsafeCoerce#
#else
-- HUGS
unsafeCoerce :: a -> b
unsafeCoerce = IOExts.unsafeCoerce
#endif

nl :: Prelude.String
nl =
  (:) '\n' ([])

_FloatIO__coq_Z_to_float :: Prelude.Integer -> Floats.Coq_float
_FloatIO__coq_Z_to_float z =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> Floats._Float__zero)
    (\x ->
    Floats._Float__from_parsed ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) 1))) x 0)
    (\x ->
    Floats._Float__neg
      (Floats._Float__from_parsed ((\x -> 2 Prelude.* x)
        ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) 1))) x 0))
    z

_FloatIO__fraction_to_float :: Prelude.Integer -> Prelude.Integer ->
                               Floats.Coq_float
_FloatIO__fraction_to_float m d =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> Floats._Float__zero)
    (\x ->
    Floats._Float__from_parsed ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) 1))) x
      (BinInt._Z__opp d))
    (\x ->
    Floats._Float__neg
      (Floats._Float__from_parsed ((\x -> 2 Prelude.* x)
        ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) 1))) x
        (BinInt._Z__opp d)))
    m

_FloatIO__floating_one :: Floats.Coq_float
_FloatIO__floating_one =
  _FloatIO__coq_Z_to_float ((\x -> x) 1)

_FloatIO__nat_power_of_float :: Floats.Coq_float -> Prelude.Integer ->
                                Floats.Coq_float
_FloatIO__nat_power_of_float x n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> _FloatIO__floating_one)
    (\p -> Floats._Float__mul x (_FloatIO__nat_power_of_float x p))
    n

_FloatIO__Details__precision_of_float :: Prelude.Integer
_FloatIO__Details__precision_of_float =
  (\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))

_FloatIO__Details__log10of2scaled3 :: Prelude.Integer
_FloatIO__Details__log10of2scaled3 =
  (\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) 1))))))))

data FloatIO__Details__FloatParts =
   FloatIO__Details__Build_FloatParts Prelude.Bool Prelude.String Prelude.String 
 Prelude.String

_FloatIO__Details__mantissaSign :: FloatIO__Details__FloatParts -> Prelude.Bool
_FloatIO__Details__mantissaSign f =
  case f of {
   FloatIO__Details__Build_FloatParts mantissaSign0 _ _ _ -> mantissaSign0}

_FloatIO__Details__intPart :: FloatIO__Details__FloatParts -> Prelude.String
_FloatIO__Details__intPart f =
  case f of {
   FloatIO__Details__Build_FloatParts _ intPart0 _ _ -> intPart0}

_FloatIO__Details__fracPart :: FloatIO__Details__FloatParts -> Prelude.String
_FloatIO__Details__fracPart f =
  case f of {
   FloatIO__Details__Build_FloatParts _ _ fracPart0 _ -> fracPart0}

_FloatIO__Details__expPart :: FloatIO__Details__FloatParts -> Prelude.String
_FloatIO__Details__expPart f =
  case f of {
   FloatIO__Details__Build_FloatParts _ _ _ expPart0 -> expPart0}

_FloatIO__Details__strToUintHelper :: Prelude.String -> Prelude.Maybe
                                      Decimal.Coq_uint
_FloatIO__Details__strToUintHelper s =
  case s of {
   ([]) -> Prelude.Just Decimal.Nil;
   (:) x x0 ->
    Monad.bind (unsafeCoerce Monad.optionM) (_FloatIO__Details__strToUintHelper x0)
      (\lower ->
      (\f a -> f (Data.Bits.testBit (Data.Char.ord a) 0)
              (Data.Bits.testBit (Data.Char.ord a) 1)
              (Data.Bits.testBit (Data.Char.ord a) 2)
              (Data.Bits.testBit (Data.Char.ord a) 3)
              (Data.Bits.testBit (Data.Char.ord a) 4)
              (Data.Bits.testBit (Data.Char.ord a) 5)
              (Data.Bits.testBit (Data.Char.ord a) 6)
              (Data.Bits.testBit (Data.Char.ord a) 7))
        (\b b0 b1 b2 b3 b4 b5 b6 ->
        case b of {
         Prelude.True ->
          case b0 of {
           Prelude.True ->
            case b1 of {
             Prelude.True ->
              case b2 of {
               Prelude.True -> Prelude.Nothing;
               Prelude.False ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just (Decimal.D7 lower)}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing}};
             Prelude.False ->
              case b2 of {
               Prelude.True -> Prelude.Nothing;
               Prelude.False ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just (Decimal.D3 lower)}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing}}};
           Prelude.False ->
            case b1 of {
             Prelude.True ->
              case b2 of {
               Prelude.True -> Prelude.Nothing;
               Prelude.False ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just (Decimal.D5 lower)}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing}};
             Prelude.False ->
              case b2 of {
               Prelude.True ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just (Decimal.D9 lower)}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing};
               Prelude.False ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just (Decimal.D1 lower)}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing}}}};
         Prelude.False ->
          case b0 of {
           Prelude.True ->
            case b1 of {
             Prelude.True ->
              case b2 of {
               Prelude.True -> Prelude.Nothing;
               Prelude.False ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just (Decimal.D6 lower)}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing}};
             Prelude.False ->
              case b2 of {
               Prelude.True -> Prelude.Nothing;
               Prelude.False ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just (Decimal.D2 lower)}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing}}};
           Prelude.False ->
            case b1 of {
             Prelude.True ->
              case b2 of {
               Prelude.True -> Prelude.Nothing;
               Prelude.False ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just (Decimal.D4 lower)}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing}};
             Prelude.False ->
              case b2 of {
               Prelude.True ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just (Decimal.D8 lower)}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing};
               Prelude.False ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just (Decimal.D0 lower)}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing}}}}})
        x)}

_FloatIO__Details__strToUint :: Prelude.String -> Prelude.Maybe Decimal.Coq_uint
_FloatIO__Details__strToUint s =
  Monad.bind (unsafeCoerce Monad.optionM) (_FloatIO__Details__strToUintHelper s)
    (\x -> Prelude.Just (Decimal.unorm x))

_FloatIO__Details__strToIntHelper :: Prelude.String -> Prelude.Maybe Decimal.Coq_int
_FloatIO__Details__strToIntHelper s =
  case s of {
   ([]) -> Prelude.Just (Decimal.Pos (Decimal.D0 Decimal.Nil));
   (:) x x0 ->
    (\f a -> f (Data.Bits.testBit (Data.Char.ord a) 0)
              (Data.Bits.testBit (Data.Char.ord a) 1)
              (Data.Bits.testBit (Data.Char.ord a) 2)
              (Data.Bits.testBit (Data.Char.ord a) 3)
              (Data.Bits.testBit (Data.Char.ord a) 4)
              (Data.Bits.testBit (Data.Char.ord a) 5)
              (Data.Bits.testBit (Data.Char.ord a) 6)
              (Data.Bits.testBit (Data.Char.ord a) 7))
      (\b b0 b1 b2 b3 b4 b5 b6 ->
      case b of {
       Prelude.True ->
        case b0 of {
         Prelude.True ->
          case b1 of {
           Prelude.True ->
            let {sign = Prelude.False} in
            Monad.bind (unsafeCoerce Monad.optionM)
              (unsafeCoerce _FloatIO__Details__strToUint s) (\uintVal ->
              Prelude.Just
              (case sign of {
                Prelude.True -> Decimal.Neg uintVal;
                Prelude.False -> Decimal.Pos uintVal}));
           Prelude.False ->
            case b2 of {
             Prelude.True ->
              case b3 of {
               Prelude.True ->
                let {sign = Prelude.False} in
                Monad.bind (unsafeCoerce Monad.optionM)
                  (unsafeCoerce _FloatIO__Details__strToUint s) (\uintVal ->
                  Prelude.Just
                  (case sign of {
                    Prelude.True -> Decimal.Neg uintVal;
                    Prelude.False -> Decimal.Pos uintVal}));
               Prelude.False ->
                case b4 of {
                 Prelude.True ->
                  case b5 of {
                   Prelude.True ->
                    let {sign = Prelude.False} in
                    Monad.bind (unsafeCoerce Monad.optionM)
                      (unsafeCoerce _FloatIO__Details__strToUint s) (\uintVal ->
                      Prelude.Just
                      (case sign of {
                        Prelude.True -> Decimal.Neg uintVal;
                        Prelude.False -> Decimal.Pos uintVal}));
                   Prelude.False ->
                    case b6 of {
                     Prelude.True ->
                      let {sign = Prelude.False} in
                      Monad.bind (unsafeCoerce Monad.optionM)
                        (unsafeCoerce _FloatIO__Details__strToUint s) (\uintVal ->
                        Prelude.Just
                        (case sign of {
                          Prelude.True -> Decimal.Neg uintVal;
                          Prelude.False -> Decimal.Pos uintVal}));
                     Prelude.False ->
                      let {sign = Prelude.False} in
                      Monad.bind (unsafeCoerce Monad.optionM)
                        (unsafeCoerce _FloatIO__Details__strToUint x0) (\uintVal ->
                        Prelude.Just
                        (case sign of {
                          Prelude.True -> Decimal.Neg uintVal;
                          Prelude.False -> Decimal.Pos uintVal}))}};
                 Prelude.False ->
                  let {sign = Prelude.False} in
                  Monad.bind (unsafeCoerce Monad.optionM)
                    (unsafeCoerce _FloatIO__Details__strToUint s) (\uintVal ->
                    Prelude.Just
                    (case sign of {
                      Prelude.True -> Decimal.Neg uintVal;
                      Prelude.False -> Decimal.Pos uintVal}))}};
             Prelude.False ->
              let {sign = Prelude.False} in
              Monad.bind (unsafeCoerce Monad.optionM)
                (unsafeCoerce _FloatIO__Details__strToUint s) (\uintVal ->
                Prelude.Just
                (case sign of {
                  Prelude.True -> Decimal.Neg uintVal;
                  Prelude.False -> Decimal.Pos uintVal}))}};
         Prelude.False ->
          case b1 of {
           Prelude.True ->
            case b2 of {
             Prelude.True ->
              case b3 of {
               Prelude.True ->
                let {sign = Prelude.False} in
                Monad.bind (unsafeCoerce Monad.optionM)
                  (unsafeCoerce _FloatIO__Details__strToUint s) (\uintVal ->
                  Prelude.Just
                  (case sign of {
                    Prelude.True -> Decimal.Neg uintVal;
                    Prelude.False -> Decimal.Pos uintVal}));
               Prelude.False ->
                case b4 of {
                 Prelude.True ->
                  case b5 of {
                   Prelude.True ->
                    let {sign = Prelude.False} in
                    Monad.bind (unsafeCoerce Monad.optionM)
                      (unsafeCoerce _FloatIO__Details__strToUint s) (\uintVal ->
                      Prelude.Just
                      (case sign of {
                        Prelude.True -> Decimal.Neg uintVal;
                        Prelude.False -> Decimal.Pos uintVal}));
                   Prelude.False ->
                    case b6 of {
                     Prelude.True ->
                      let {sign = Prelude.False} in
                      Monad.bind (unsafeCoerce Monad.optionM)
                        (unsafeCoerce _FloatIO__Details__strToUint s) (\uintVal ->
                        Prelude.Just
                        (case sign of {
                          Prelude.True -> Decimal.Neg uintVal;
                          Prelude.False -> Decimal.Pos uintVal}));
                     Prelude.False ->
                      let {sign = Prelude.True} in
                      Monad.bind (unsafeCoerce Monad.optionM)
                        (unsafeCoerce _FloatIO__Details__strToUint x0) (\uintVal ->
                        Prelude.Just
                        (case sign of {
                          Prelude.True -> Decimal.Neg uintVal;
                          Prelude.False -> Decimal.Pos uintVal}))}};
                 Prelude.False ->
                  let {sign = Prelude.False} in
                  Monad.bind (unsafeCoerce Monad.optionM)
                    (unsafeCoerce _FloatIO__Details__strToUint s) (\uintVal ->
                    Prelude.Just
                    (case sign of {
                      Prelude.True -> Decimal.Neg uintVal;
                      Prelude.False -> Decimal.Pos uintVal}))}};
             Prelude.False ->
              let {sign = Prelude.False} in
              Monad.bind (unsafeCoerce Monad.optionM)
                (unsafeCoerce _FloatIO__Details__strToUint s) (\uintVal ->
                Prelude.Just
                (case sign of {
                  Prelude.True -> Decimal.Neg uintVal;
                  Prelude.False -> Decimal.Pos uintVal}))};
           Prelude.False ->
            let {sign = Prelude.False} in
            Monad.bind (unsafeCoerce Monad.optionM)
              (unsafeCoerce _FloatIO__Details__strToUint s) (\uintVal ->
              Prelude.Just
              (case sign of {
                Prelude.True -> Decimal.Neg uintVal;
                Prelude.False -> Decimal.Pos uintVal}))}};
       Prelude.False ->
        let {sign = Prelude.False} in
        Monad.bind (unsafeCoerce Monad.optionM)
          (unsafeCoerce _FloatIO__Details__strToUint s) (\uintVal -> Prelude.Just
          (case sign of {
            Prelude.True -> Decimal.Neg uintVal;
            Prelude.False -> Decimal.Pos uintVal}))})
      x}

_FloatIO__Details__strToInt :: Prelude.String -> Prelude.Maybe Decimal.Coq_int
_FloatIO__Details__strToInt s =
  Monad.bind (unsafeCoerce Monad.optionM) (_FloatIO__Details__strToIntHelper s)
    (\x -> Prelude.Just (Decimal.norm x))

_FloatIO__Details__strToZ :: Prelude.String -> Prelude.Maybe Prelude.Integer
_FloatIO__Details__strToZ s =
  Monad.bind (unsafeCoerce Monad.optionM)
    (unsafeCoerce _FloatIO__Details__strToInt s) (\x -> Prelude.Just
    (BinInt._Z__of_int x))

_FloatIO__Details__splitAtExponent :: Prelude.String -> (,) Prelude.String
                                      Prelude.String
_FloatIO__Details__splitAtExponent s =
  let {
   maybeEpos = case String0.index 0 ((:) 'E' ([])) s of {
                Prelude.Just epos -> Prelude.Just epos;
                Prelude.Nothing -> String0.index 0 ((:) 'e' ([])) s}}
  in
  case maybeEpos of {
   Prelude.Just epos -> (,) (String0.substring 0 epos s)
    (String0.substring (Prelude.succ epos)
      (Nat.sub (Nat.sub (String0.length s) epos) (Prelude.succ 0)) s);
   Prelude.Nothing -> (,) s ([])}

_FloatIO__Details__splitAtPoint :: Prelude.String -> (,) Prelude.String
                                   Prelude.String
_FloatIO__Details__splitAtPoint s =
  case String0.index 0 ((:) '.' ([])) s of {
   Prelude.Just dpos -> (,) (String0.substring 0 dpos s)
    (String0.substring (Prelude.succ dpos)
      (Nat.sub (Nat.sub (String0.length s) dpos) (Prelude.succ 0)) s);
   Prelude.Nothing -> (,) s ([])}

_FloatIO__Details__decomposeFloatString :: Prelude.String -> (,)
                                           ((,) Prelude.String Prelude.String)
                                           Prelude.String
_FloatIO__Details__decomposeFloatString s =
  case _FloatIO__Details__splitAtExponent s of {
   (,) frac exp -> (,) (_FloatIO__Details__splitAtPoint frac) exp}

_FloatIO__Details__strToFloatHelper :: Prelude.String -> Prelude.Maybe
                                       ((,) Prelude.Integer Prelude.Integer)
_FloatIO__Details__strToFloatHelper s =
  case _FloatIO__Details__decomposeFloatString s of {
   (,) p exp ->
    case p of {
     (,) intPart0 fracPart0 ->
      let {mantstring = String0.append intPart0 fracPart0} in
      Monad.bind (unsafeCoerce Monad.optionM)
        (unsafeCoerce _FloatIO__Details__strToZ mantstring) (\zmant ->
        Monad.bind (unsafeCoerce Monad.optionM)
          (unsafeCoerce _FloatIO__Details__strToZ exp) (\zexp ->
          let {
           adjustedZexp = (Prelude.-) zexp
                            (BinInt._Z__of_nat (String0.length fracPart0))}
          in
          Monad.ret (unsafeCoerce Monad.optionM) ((,) zmant adjustedZexp)))}}

_FloatIO__Details__my_nanpl :: Fappli_IEEE.Coq_nan_pl
_FloatIO__Details__my_nanpl =
  Prelude.snd Fappli_IEEE_bits.default_nan_pl64

_FloatIO__Details__my_nan :: Floats.Coq_float
_FloatIO__Details__my_nan =
  Fappli_IEEE.B754_nan Prelude.False _FloatIO__Details__my_nanpl

_FloatIO__Details__strToFloat :: Prelude.String -> Floats.Coq_float
_FloatIO__Details__strToFloat s =
  case _FloatIO__Details__strToFloatHelper s of {
   Prelude.Just p ->
    case p of {
     (,) zmant adjustedZexp ->
      (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
        (\_ -> Floats._Float__zero)
        (\x ->
        Floats._Float__from_parsed ((\x -> 2 Prelude.* x)
          ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) 1))) x
          adjustedZexp)
        (\x ->
        Floats._Float__neg
          (Floats._Float__from_parsed ((\x -> 2 Prelude.* x)
            ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) 1))) x
            adjustedZexp))
        zmant};
   Prelude.Nothing -> _FloatIO__Details__my_nan}

_FloatIO__Details__scale_exp :: Prelude.Integer -> Prelude.Integer ->
                                Prelude.Integer
_FloatIO__Details__scale_exp d e =
  (Prelude.-) ((Prelude.-) d ((\x -> x) ((\x -> 2 Prelude.* x) 1)))
    (BinInt._Z__div
      ((Prelude.*)
        ((Prelude.-) ((Prelude.+) e _FloatIO__Details__precision_of_float)
          ((\x -> x) 1)) _FloatIO__Details__log10of2scaled3) ((\x -> x)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x Prelude.+ 1)
      ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x Prelude.+ 1)
      1)))))))))))

_FloatIO__Details__digits :: Prelude.Integer -> Prelude.String
_FloatIO__Details__digits n =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> (:) '0' ([]))
    (\p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\p0 ->
      (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
        (\p1 ->
        (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
          (\p2 ->
          (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
            (\p3 ->
            (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
              (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
              ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
              'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
              ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
              ([])))))))))))))))))))))))))))
              (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
              ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
              'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
              ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
              ([])))))))))))))))))))))))))))
              (\_ -> (:) 'v' ([]))
              p3)
            (\p3 ->
            (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
              (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
              ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
              'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
              ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
              ([])))))))))))))))))))))))))))
              (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
              ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
              'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
              ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
              ([])))))))))))))))))))))))))))
              (\_ -> (:) 'n' ([]))
              p3)
            (\_ -> (:) 'f' ([]))
            p2)
          (\p2 ->
          (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
            (\p3 ->
            (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
              (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
              ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
              'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
              ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
              ([])))))))))))))))))))))))))))
              (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
              ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
              'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
              ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
              ([])))))))))))))))))))))))))))
              (\_ -> (:) 'r' ([]))
              p3)
            (\p3 ->
            (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
              (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
              ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
              'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
              ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
              ([])))))))))))))))))))))))))))
              (\p4 ->
              (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
                (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
                ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
                'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
                ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
                ([])))))))))))))))))))))))))))
                (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
                ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
                'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
                ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
                ([])))))))))))))))))))))))))))
                (\_ -> (:) 'z' ([]))
                p4)
              (\_ -> (:) 'j' ([]))
              p3)
            (\_ -> (:) 'b' ([]))
            p2)
          (\_ -> (:) '7' ([]))
          p1)
        (\p1 ->
        (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
          (\p2 ->
          (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
            (\p3 ->
            (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
              (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
              ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
              'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
              ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
              ([])))))))))))))))))))))))))))
              (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
              ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
              'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
              ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
              ([])))))))))))))))))))))))))))
              (\_ -> (:) 't' ([]))
              p3)
            (\p3 ->
            (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
              (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
              ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
              'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
              ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
              ([])))))))))))))))))))))))))))
              (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
              ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
              'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
              ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
              ([])))))))))))))))))))))))))))
              (\_ -> (:) 'l' ([]))
              p3)
            (\_ -> (:) 'd' ([]))
            p2)
          (\p2 ->
          (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
            (\p3 ->
            (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
              (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
              ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
              'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
              ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
              ([])))))))))))))))))))))))))))
              (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
              ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
              'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
              ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
              ([])))))))))))))))))))))))))))
              (\_ -> (:) 'p' ([]))
              p3)
            (\p3 ->
            (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
              (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
              ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
              'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
              ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
              ([])))))))))))))))))))))))))))
              (\p4 ->
              (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
                (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
                ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
                'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
                ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
                ([])))))))))))))))))))))))))))
                (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
                ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
                'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
                ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
                ([])))))))))))))))))))))))))))
                (\_ -> (:) 'x' ([]))
                p4)
              (\_ -> (:) 'h' ([]))
              p3)
            (\_ -> (:) '9' ([]))
            p2)
          (\_ -> (:) '5' ([]))
          p1)
        (\_ -> (:) '3' ([]))
        p0)
      (\p0 ->
      (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
        (\p1 ->
        (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
          (\p2 ->
          (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
            (\p3 ->
            (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
              (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
              ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
              'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
              ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
              ([])))))))))))))))))))))))))))
              (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
              ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
              'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
              ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
              ([])))))))))))))))))))))))))))
              (\_ -> (:) 'u' ([]))
              p3)
            (\p3 ->
            (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
              (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
              ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
              'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
              ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
              ([])))))))))))))))))))))))))))
              (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
              ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
              'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
              ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
              ([])))))))))))))))))))))))))))
              (\_ -> (:) 'm' ([]))
              p3)
            (\_ -> (:) 'e' ([]))
            p2)
          (\p2 ->
          (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
            (\p3 ->
            (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
              (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
              ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
              'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
              ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
              ([])))))))))))))))))))))))))))
              (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
              ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
              'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
              ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
              ([])))))))))))))))))))))))))))
              (\_ -> (:) 'q' ([]))
              p3)
            (\p3 ->
            (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
              (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
              ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
              'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
              ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
              ([])))))))))))))))))))))))))))
              (\p4 ->
              (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
                (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
                ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
                'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
                ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
                ([])))))))))))))))))))))))))))
                (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
                ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
                'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
                ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
                ([])))))))))))))))))))))))))))
                (\_ -> (:) 'y' ([]))
                p4)
              (\_ -> (:) 'i' ([]))
              p3)
            (\_ -> (:) 'a' ([]))
            p2)
          (\_ -> (:) '6' ([]))
          p1)
        (\p1 ->
        (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
          (\p2 ->
          (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
            (\p3 ->
            (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
              (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
              ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
              'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
              ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
              ([])))))))))))))))))))))))))))
              (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
              ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
              'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
              ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
              ([])))))))))))))))))))))))))))
              (\_ -> (:) 's' ([]))
              p3)
            (\p3 ->
            (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
              (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
              ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
              'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
              ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
              ([])))))))))))))))))))))))))))
              (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
              ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
              'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
              ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
              ([])))))))))))))))))))))))))))
              (\_ -> (:) 'k' ([]))
              p3)
            (\_ -> (:) 'c' ([]))
            p2)
          (\p2 ->
          (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
            (\p3 ->
            (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
              (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
              ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
              'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
              ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
              ([])))))))))))))))))))))))))))
              (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
              ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
              'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
              ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
              ([])))))))))))))))))))))))))))
              (\_ -> (:) 'o' ([]))
              p3)
            (\p3 ->
            (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
              (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
              ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
              'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
              ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
              ([])))))))))))))))))))))))))))
              (\p4 ->
              (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
                (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
                ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
                'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
                ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
                ([])))))))))))))))))))))))))))
                (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r'
                ((:) '.' ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:)
                'a' ((:) 'd' ((:) 'i' ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' '
                ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
                ([])))))))))))))))))))))))))))
                (\_ -> (:) 'w' ([]))
                p4)
              (\_ -> (:) 'g' ([]))
              p3)
            (\_ -> (:) '8' ([]))
            p2)
          (\_ -> (:) '4' ([]))
          p1)
        (\_ -> (:) '2' ([]))
        p0)
      (\_ -> (:) '1' ([]))
      p)
    (\_ -> (:) '!' ((:) ' ' ((:) 'E' ((:) 'r' ((:) 'r' ((:) 'o' ((:) 'r' ((:) '.'
    ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'x' ((:) ' ' ((:) 'r' ((:) 'a' ((:) 'd' ((:) 'i'
    ((:) 'x' ((:) ' ' ((:) 'i' ((:) 's' ((:) ' ' ((:) '3' ((:) '5' ((:) ' ' ((:) '!'
    ([])))))))))))))))))))))))))))
    n

_FloatIO__Details__repeat_string :: Prelude.Integer -> Prelude.String ->
                                    Prelude.String
_FloatIO__Details__repeat_string n c =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> ([]))
    (\x -> String0.append c (_FloatIO__Details__repeat_string x c))
    n

_FloatIO__Details__coq_Z_to_string_base10_aux :: Prelude.Integer -> Prelude.Integer
                                                 -> Prelude.Integer ->
                                                 Prelude.String
_FloatIO__Details__coq_Z_to_string_base10_aux min_digits fuel num =
  case BinInt._Z__ltb num 0 of {
   Prelude.True -> (:) 'R' ((:) 'e' ((:) 'q' ((:) 'u' ((:) 'i' ((:) 'r' ((:) 'e'
    ((:) ' ' ((:) 'n' ((:) 'o' ((:) 'n' ((:) '-' ((:) 'n' ((:) 'e' ((:) 'g' ((:) 'a'
    ((:) 't' ((:) 'i' ((:) 'v' ((:) 'e' ((:) ' ' ((:) 'n' ((:) 'u' ((:) 'm' ((:) '.'
    ([])))))))))))))))))))))))));
   Prelude.False ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> (:) 'o' ((:) 'u' ((:) 't' ((:) ' ' ((:) 'o' ((:) 'f' ((:) ' ' ((:) 'f'
      ((:) 'u' ((:) 'e' ((:) 'l' ([]))))))))))))
      (\x ->
      (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
        (\_ -> _FloatIO__Details__repeat_string min_digits ((:) '0' ([])))
        (\_ ->
        String0.append
          (_FloatIO__Details__coq_Z_to_string_base10_aux (Nat.pred min_digits) x
            (BinInt._Z__div num ((\x -> x) ((\x -> 2 Prelude.* x)
              ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) 1))))))
          (_FloatIO__Details__digits
            (BinInt._Z__modulo num ((\x -> x) ((\x -> 2 Prelude.* x)
              ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) 1)))))))
        (\_ ->
        String0.append
          (_FloatIO__Details__coq_Z_to_string_base10_aux (Nat.pred min_digits) x
            (BinInt._Z__div num ((\x -> x) ((\x -> 2 Prelude.* x)
              ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) 1))))))
          (_FloatIO__Details__digits
            (BinInt._Z__modulo num ((\x -> x) ((\x -> 2 Prelude.* x)
              ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) 1)))))))
        num)
      fuel}

_FloatIO__Details__coq_Z_to_string_base10 :: Prelude.Integer -> Prelude.Integer ->
                                             Prelude.String
_FloatIO__Details__coq_Z_to_string_base10 min_digits num =
  case BinInt._Z__ltb num 0 of {
   Prelude.True ->
    String0.append ((:) '-' ([]))
      (_FloatIO__Details__coq_Z_to_string_base10_aux min_digits (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
        (BinInt._Z__opp num));
   Prelude.False ->
    case BinInt._Z__eqb num 0 of {
     Prelude.True -> _FloatIO__Details__repeat_string min_digits ((:) '0' ([]));
     Prelude.False ->
      _FloatIO__Details__coq_Z_to_string_base10_aux min_digits (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
        num}}

_FloatIO__Details__coq_Z_to_signed_string_base10 :: Prelude.Integer ->
                                                    Prelude.Integer ->
                                                    Prelude.String
_FloatIO__Details__coq_Z_to_signed_string_base10 min_digits num =
  case BinInt._Z__geb num 0 of {
   Prelude.True ->
    String0.append ((:) '+' ([]))
      (_FloatIO__Details__coq_Z_to_string_base10 min_digits num);
   Prelude.False -> _FloatIO__Details__coq_Z_to_string_base10 min_digits num}

_FloatIO__Details__coq_Z_to_string_base16_aux :: Prelude.Integer -> Prelude.Integer
                                                 -> Prelude.Integer ->
                                                 Prelude.String
_FloatIO__Details__coq_Z_to_string_base16_aux min_digits fuel num =
  case BinInt._Z__ltb num 0 of {
   Prelude.True -> (:) 'R' ((:) 'e' ((:) 'q' ((:) 'u' ((:) 'i' ((:) 'r' ((:) 'e'
    ((:) ' ' ((:) 'n' ((:) 'o' ((:) 'n' ((:) '-' ((:) 'n' ((:) 'e' ((:) 'g' ((:) 'a'
    ((:) 't' ((:) 'i' ((:) 'v' ((:) 'e' ((:) ' ' ((:) 'n' ((:) 'u' ((:) 'm' ((:) '.'
    ([])))))))))))))))))))))))));
   Prelude.False ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> (:) 'o' ((:) 'u' ((:) 't' ((:) ' ' ((:) 'o' ((:) 'f' ((:) ' ' ((:) 'f'
      ((:) 'u' ((:) 'e' ((:) 'l' ([]))))))))))))
      (\x ->
      (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
        (\_ -> _FloatIO__Details__repeat_string min_digits ((:) '0' ([])))
        (\_ ->
        String0.append
          (_FloatIO__Details__coq_Z_to_string_base16_aux (Nat.pred min_digits) x
            (BinInt._Z__div num ((\x -> x) ((\x -> 2 Prelude.* x)
              ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
              1)))))))
          (_FloatIO__Details__digits
            (BinInt._Z__modulo num ((\x -> x) ((\x -> 2 Prelude.* x)
              ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
              1))))))))
        (\_ ->
        String0.append
          (_FloatIO__Details__coq_Z_to_string_base16_aux (Nat.pred min_digits) x
            (BinInt._Z__div num ((\x -> x) ((\x -> 2 Prelude.* x)
              ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
              1)))))))
          (_FloatIO__Details__digits
            (BinInt._Z__modulo num ((\x -> x) ((\x -> 2 Prelude.* x)
              ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
              1))))))))
        num)
      fuel}

_FloatIO__Details__coq_Z_to_string_base16 :: Prelude.Integer -> Prelude.Integer ->
                                             Prelude.String
_FloatIO__Details__coq_Z_to_string_base16 min_digits num =
  case BinInt._Z__ltb num 0 of {
   Prelude.True ->
    String0.append ((:) '-' ([]))
      (_FloatIO__Details__coq_Z_to_string_base16_aux min_digits (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
        (BinInt._Z__opp num));
   Prelude.False ->
    case BinInt._Z__eqb num 0 of {
     Prelude.True -> _FloatIO__Details__repeat_string min_digits ((:) '0' ([]));
     Prelude.False ->
      _FloatIO__Details__coq_Z_to_string_base16_aux min_digits (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
        num}}

_FloatIO__Details__scaled_float_to_Z :: Floats.Coq_float -> Prelude.Integer ->
                                        Prelude.Integer
_FloatIO__Details__scaled_float_to_Z x fdigs =
  case Floats._Float__to_long
         (Floats._Float__mul x
           (Floats._Float__from_parsed ((\x -> 2 Prelude.* x)
             ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) 1))) 1 fdigs)) of {
   Prelude.Just ii -> Integers._Int64__signed ii;
   Prelude.Nothing -> 0}

_FloatIO__Details__insert_decimal :: Prelude.String -> Prelude.Integer ->
                                     Prelude.String
_FloatIO__Details__insert_decimal s fdigs =
  let {len = String0.length s} in
  let {front = Nat.sub len fdigs} in
  String0.append (String0.substring 0 front s)
    (String0.append ((:) '.' ([])) (String0.substring front fdigs s))

_FloatIO__Details__float_to_string_unsigned :: Floats.Coq_float -> Prelude.Integer
                                               -> Prelude.String
_FloatIO__Details__float_to_string_unsigned x fdigs =
  case x of {
   Fappli_IEEE.B754_finite b _ e ->
    case b of {
     Prelude.True -> ([]);
     Prelude.False ->
      let {digs_after_dec = Nat.pred fdigs} in
      let {scale = _FloatIO__Details__scale_exp (BinInt._Z__of_nat fdigs) e} in
      let {scaled = _FloatIO__Details__scaled_float_to_Z x scale} in
      let {b10 = _FloatIO__Details__coq_Z_to_string_base10 (Prelude.succ 0) scaled}
      in
      let {lb10 = String0.length b10} in
      let {
       scale' = case PeanoNat._Nat__ltb lb10 fdigs of {
                 Prelude.True ->
                  (Prelude.+) scale (BinInt._Z__of_nat (Nat.sub fdigs lb10));
                 Prelude.False -> scale}}
      in
      let {
       b10' = case PeanoNat._Nat__ltb lb10 fdigs of {
               Prelude.True ->
                let {scaled' = _FloatIO__Details__scaled_float_to_Z x scale'} in
                _FloatIO__Details__coq_Z_to_string_base10 fdigs scaled';
               Prelude.False -> b10}}
      in
      let {d10 = _FloatIO__Details__insert_decimal b10' digs_after_dec} in
      let {
       true_exp = (Prelude.-) ((Prelude.-) (BinInt._Z__of_nat fdigs) scale')
                    ((\x -> x) 1)}
      in
      let {
       exp_string = _FloatIO__Details__coq_Z_to_signed_string_base10 (Prelude.succ
                      (Prelude.succ 0)) true_exp}
      in
      String0.append d10 (String0.append ((:) 'e' ([])) exp_string)};
   _ -> ([])}

_FloatIO__Details__zero_to_string_unpadded :: Prelude.Integer -> Prelude.String
_FloatIO__Details__zero_to_string_unpadded fdigs =
  let {digs_after_dec = Nat.pred fdigs} in
  let {frac = _FloatIO__Details__repeat_string digs_after_dec ((:) '0' ([]))} in
  String0.append ((:) '0' ((:) '.' ([])))
    (String0.append frac ((:) 'e' ((:) '+' ((:) '0' ((:) '0' ([]))))))

_FloatIO__Details__float_to_string_unpadded :: Floats.Coq_float -> Prelude.Integer
                                               -> Prelude.String
_FloatIO__Details__float_to_string_unpadded x fdigs =
  case x of {
   Fappli_IEEE.B754_zero b ->
    case b of {
     Prelude.True ->
      String0.append ((:) '-' ([]))
        (_FloatIO__Details__zero_to_string_unpadded fdigs);
     Prelude.False ->
      String0.append ([]) (_FloatIO__Details__zero_to_string_unpadded fdigs)};
   Fappli_IEEE.B754_infinity b ->
    case b of {
     Prelude.True -> (:) '-' ((:) 'i' ((:) 'n' ((:) 'f' ([]))));
     Prelude.False -> (:) 'i' ((:) 'n' ((:) 'f' ([])))};
   Fappli_IEEE.B754_nan b pl ->
    case b of {
     Prelude.True ->
      String0.append ((:) '-' ((:) 'n' ((:) 'a' ((:) 'n' ([])))))
        (_FloatIO__Details__coq_Z_to_string_base16 (Prelude.succ (Prelude.succ
          (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
          (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
          (Prelude.succ (Prelude.succ 0)))))))))))))) ((\x -> x)
          (Specif.proj1_sig pl)));
     Prelude.False ->
      String0.append ((:) 'n' ((:) 'a' ((:) 'n' ([]))))
        (_FloatIO__Details__coq_Z_to_string_base16 (Prelude.succ (Prelude.succ
          (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
          (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
          (Prelude.succ (Prelude.succ 0)))))))))))))) ((\x -> x)
          (Specif.proj1_sig pl)))};
   Fappli_IEEE.B754_finite b _ _ ->
    case b of {
     Prelude.True ->
      String0.append ((:) '-' ([]))
        (_FloatIO__Details__float_to_string_unsigned (Floats._Float__abs x) fdigs);
     Prelude.False -> _FloatIO__Details__float_to_string_unsigned x fdigs}}

_FloatIO__Details__pad_to_width :: Prelude.Integer -> Prelude.String ->
                                   Prelude.String
_FloatIO__Details__pad_to_width width s =
  let {ls = String0.length s} in
  let {pads = _FloatIO__Details__repeat_string (Nat.sub width ls) ((:) ' ' ([]))} in
  String0.append pads s

_FloatIO__Details__float_to_string :: Prelude.Integer -> Prelude.Integer ->
                                      Floats.Coq_float -> Prelude.String
_FloatIO__Details__float_to_string width fdigs x =
  _FloatIO__Details__pad_to_width width
    (_FloatIO__Details__float_to_string_unpadded x fdigs)

_FloatIO__Details__float_list_to_string :: Prelude.Integer -> Prelude.Integer ->
                                           Prelude.Bool -> (([]) Floats.Coq_float)
                                           -> Prelude.String
_FloatIO__Details__float_list_to_string width fdigs add_new_line al =
  case al of {
   ([]) -> case add_new_line of {
            Prelude.True -> nl;
            Prelude.False -> ([])};
   (:) hd tl ->
    String0.append (_FloatIO__Details__float_to_string width fdigs hd)
      (_FloatIO__Details__float_list_to_string width fdigs add_new_line tl)}

_FloatIO__coq_Z_to_string_base10 :: Prelude.Integer -> Prelude.Integer ->
                                    Prelude.String
_FloatIO__coq_Z_to_string_base10 =
  _FloatIO__Details__coq_Z_to_string_base10

_FloatIO__pad_to_width :: Prelude.Integer -> Prelude.String -> Prelude.String
_FloatIO__pad_to_width =
  _FloatIO__Details__pad_to_width

_FloatIO__float_to_string :: Prelude.Integer -> Prelude.Integer -> Floats.Coq_float
                             -> Prelude.String
_FloatIO__float_to_string =
  _FloatIO__Details__float_to_string

_FloatIO__float_list_to_string :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
                                  -> (([]) Floats.Coq_float) -> Prelude.String
_FloatIO__float_list_to_string =
  _FloatIO__Details__float_list_to_string

_FloatIO__strToFloat :: Prelude.String -> Floats.Coq_float
_FloatIO__strToFloat =
  _FloatIO__Details__strToFloat

_FloatIO__coq_ZofFloat :: Floats.Coq_float -> Prelude.Integer
_FloatIO__coq_ZofFloat f =
  case Fappli_IEEE_extra.coq_ZofB ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
         ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
         ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1))))))
         ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
         ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
         ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
         ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1))))))))))) f of {
   Prelude.Just z -> z;
   Prelude.Nothing -> 0}

_FloatIO__round :: Floats.Coq_float -> Floats.Coq_float
_FloatIO__round f =
  let {
   z = _FloatIO__coq_ZofFloat
         (Floats._Float__add f
           (_FloatIO__Details__strToFloat ((:) '0' ((:) '.' ((:) '5' ([]))))))}
  in
  _FloatIO__coq_Z_to_float z

_FloatIO__sqrt :: Floats.Coq_float -> Floats.Coq_float
_FloatIO__sqrt arg =
  Fappli_IEEE_bits.b64_sqrt Fappli_IEEE.Coq_mode_NE arg

