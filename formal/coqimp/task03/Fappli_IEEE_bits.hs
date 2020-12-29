module Fappli_IEEE_bits where

import qualified Prelude
import qualified BinInt
import qualified Fappli_IEEE
import qualified Fcore_Zaux
import qualified Zbool

join_bits :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool -> Prelude.Integer
             -> Prelude.Integer -> Prelude.Integer
join_bits mw ew s m e =
  (Prelude.+)
    (BinInt._Z__shiftl
      ((Prelude.+)
        (case s of {
          Prelude.True -> BinInt._Z__pow ((\x -> x) ((\x -> 2 Prelude.* x) 1)) ew;
          Prelude.False -> 0}) e) mw) m

split_bits :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer -> (,)
              ((,) Prelude.Bool Prelude.Integer) Prelude.Integer
split_bits mw ew x =
  let {mm = BinInt._Z__pow ((\x -> x) ((\x -> 2 Prelude.* x) 1)) mw} in
  let {em = BinInt._Z__pow ((\x -> x) ((\x -> 2 Prelude.* x) 1)) ew} in
  (,) ((,) (BinInt._Z__leb ((Prelude.*) mm em) x) (BinInt._Z__modulo x mm))
  (BinInt._Z__modulo (BinInt._Z__div x mm) em)

bits_of_binary_float :: Prelude.Integer -> Prelude.Integer ->
                        Fappli_IEEE.Coq_binary_float -> Prelude.Integer
bits_of_binary_float mw ew =
  let {
   emax = BinInt._Z__pow ((\x -> x) ((\x -> 2 Prelude.* x) 1))
            ((Prelude.-) ew ((\x -> x) 1))}
  in
  let {prec = (Prelude.+) mw ((\x -> x) 1)} in
  let {
   emin = (Prelude.-)
            ((Prelude.-) ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1)) emax)
            prec}
  in
  (\x ->
  case x of {
   Fappli_IEEE.B754_zero sx -> join_bits mw ew sx 0 0;
   Fappli_IEEE.B754_infinity sx ->
    join_bits mw ew sx 0
      ((Prelude.-) (BinInt._Z__pow ((\x -> x) ((\x -> 2 Prelude.* x) 1)) ew)
        ((\x -> x) 1));
   Fappli_IEEE.B754_nan sx n ->
    join_bits mw ew sx ((\x -> x) n)
      ((Prelude.-) (BinInt._Z__pow ((\x -> x) ((\x -> 2 Prelude.* x) 1)) ew)
        ((\x -> x) 1));
   Fappli_IEEE.B754_finite sx mx ex ->
    let {
     m = (Prelude.-) ((\x -> x) mx)
           (BinInt._Z__pow ((\x -> x) ((\x -> 2 Prelude.* x) 1)) mw)}
    in
    case BinInt._Z__leb 0 m of {
     Prelude.True ->
      join_bits mw ew sx m ((Prelude.+) ((Prelude.-) ex emin) ((\x -> x) 1));
     Prelude.False -> join_bits mw ew sx ((\x -> x) mx) 0}})

split_bits_of_binary_float :: Prelude.Integer -> Prelude.Integer ->
                              Fappli_IEEE.Coq_binary_float -> (,)
                              ((,) Prelude.Bool Prelude.Integer) Prelude.Integer
split_bits_of_binary_float mw ew =
  let {
   emax = BinInt._Z__pow ((\x -> x) ((\x -> 2 Prelude.* x) 1))
            ((Prelude.-) ew ((\x -> x) 1))}
  in
  let {prec = (Prelude.+) mw ((\x -> x) 1)} in
  let {
   emin = (Prelude.-)
            ((Prelude.-) ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1)) emax)
            prec}
  in
  (\x ->
  case x of {
   Fappli_IEEE.B754_zero sx -> (,) ((,) sx 0) 0;
   Fappli_IEEE.B754_infinity sx -> (,) ((,) sx 0)
    ((Prelude.-) (BinInt._Z__pow ((\x -> x) ((\x -> 2 Prelude.* x) 1)) ew)
      ((\x -> x) 1));
   Fappli_IEEE.B754_nan sx n -> (,) ((,) sx ((\x -> x) n))
    ((Prelude.-) (BinInt._Z__pow ((\x -> x) ((\x -> 2 Prelude.* x) 1)) ew)
      ((\x -> x) 1));
   Fappli_IEEE.B754_finite sx mx ex ->
    let {
     m = (Prelude.-) ((\x -> x) mx)
           (BinInt._Z__pow ((\x -> x) ((\x -> 2 Prelude.* x) 1)) mw)}
    in
    case BinInt._Z__leb 0 m of {
     Prelude.True -> (,) ((,) sx m)
      ((Prelude.+) ((Prelude.-) ex emin) ((\x -> x) 1));
     Prelude.False -> (,) ((,) sx ((\x -> x) mx)) 0}})

binary_float_of_bits_aux :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer ->
                            Fappli_IEEE.Coq_full_float
binary_float_of_bits_aux mw ew =
  let {
   emax = BinInt._Z__pow ((\x -> x) ((\x -> 2 Prelude.* x) 1))
            ((Prelude.-) ew ((\x -> x) 1))}
  in
  let {prec = (Prelude.+) mw ((\x -> x) 1)} in
  let {
   emin = (Prelude.-)
            ((Prelude.-) ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1)) emax)
            prec}
  in
  (\x ->
  case split_bits mw ew x of {
   (,) p ex ->
    case p of {
     (,) sx mx ->
      case Zbool.coq_Zeq_bool ex 0 of {
       Prelude.True ->
        (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
          (\_ -> Fappli_IEEE.F754_zero sx)
          (\px -> Fappli_IEEE.F754_finite sx px emin)
          (\_ -> Fappli_IEEE.F754_nan Prelude.False 1)
          mx;
       Prelude.False ->
        case Zbool.coq_Zeq_bool ex
               ((Prelude.-)
                 (BinInt._Z__pow ((\x -> x) ((\x -> 2 Prelude.* x) 1)) ew)
                 ((\x -> x) 1)) of {
         Prelude.True ->
          (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
            (\_ -> Fappli_IEEE.F754_infinity sx)
            (\plx -> Fappli_IEEE.F754_nan sx plx)
            (\_ -> Fappli_IEEE.F754_nan Prelude.False 1)
            mx;
         Prelude.False ->
          (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
            (\_ -> Fappli_IEEE.F754_nan Prelude.False 1)
            (\px -> Fappli_IEEE.F754_finite sx px
            ((Prelude.-) ((Prelude.+) ex emin) ((\x -> x) 1)))
            (\_ -> Fappli_IEEE.F754_nan Prelude.False
            1)
            ((Prelude.+) mx
              (BinInt._Z__pow ((\x -> x) ((\x -> 2 Prelude.* x) 1)) mw))}}}})

binary_float_of_bits :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer ->
                        Fappli_IEEE.Coq_binary_float
binary_float_of_bits mw ew x =
  let {
   emax = BinInt._Z__pow ((\x -> x) ((\x -> 2 Prelude.* x) 1))
            ((Prelude.-) ew ((\x -> x) 1))}
  in
  let {prec = (Prelude.+) mw ((\x -> x) 1)} in
  Fappli_IEEE.coq_FF2B prec emax (binary_float_of_bits_aux mw ew x)

type Coq_binary32 = Fappli_IEEE.Coq_binary_float

default_nan_pl32 :: (,) Prelude.Bool Fappli_IEEE.Coq_nan_pl
default_nan_pl32 =
  (,) Prelude.False
    (Fcore_Zaux.iter_nat (\x -> (\x -> 2 Prelude.* x) x) (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      0)))))))))))))))))))))) 1)

unop_nan_pl32 :: Coq_binary32 -> (,) Prelude.Bool Fappli_IEEE.Coq_nan_pl
unop_nan_pl32 f =
  case f of {
   Fappli_IEEE.B754_nan s pl -> (,) s pl;
   _ -> default_nan_pl32}

binop_nan_pl32 :: Coq_binary32 -> Coq_binary32 -> (,) Prelude.Bool
                  Fappli_IEEE.Coq_nan_pl
binop_nan_pl32 f1 f2 =
  case f1 of {
   Fappli_IEEE.B754_nan s1 pl1 -> (,) s1 pl1;
   _ ->
    case f2 of {
     Fappli_IEEE.B754_nan s2 pl2 -> (,) s2 pl2;
     _ -> default_nan_pl32}}

b32_opp :: Fappli_IEEE.Coq_binary_float -> Fappli_IEEE.Coq_binary_float
b32_opp =
  Fappli_IEEE.coq_Bopp ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1))))) ((\x -> x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) 1)))))))) (\x x0 -> (,) x x0)

b32_plus :: Fappli_IEEE.Coq_mode -> Fappli_IEEE.Coq_binary_float ->
            Fappli_IEEE.Coq_binary_float -> Fappli_IEEE.Coq_binary_float
b32_plus =
  Fappli_IEEE.coq_Bplus ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1))))) ((\x -> x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) 1)))))))) binop_nan_pl32

b32_minus :: Fappli_IEEE.Coq_mode -> Fappli_IEEE.Coq_binary_float ->
             Fappli_IEEE.Coq_binary_float -> Fappli_IEEE.Coq_binary_float
b32_minus =
  Fappli_IEEE.coq_Bminus ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1))))) ((\x -> x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) 1)))))))) binop_nan_pl32

b32_mult :: Fappli_IEEE.Coq_mode -> Fappli_IEEE.Coq_binary_float ->
            Fappli_IEEE.Coq_binary_float -> Fappli_IEEE.Coq_binary_float
b32_mult =
  Fappli_IEEE.coq_Bmult ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1))))) ((\x -> x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) 1)))))))) binop_nan_pl32

b32_div :: Fappli_IEEE.Coq_mode -> Fappli_IEEE.Coq_binary_float ->
           Fappli_IEEE.Coq_binary_float -> Fappli_IEEE.Coq_binary_float
b32_div =
  Fappli_IEEE.coq_Bdiv ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1))))) ((\x -> x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) 1)))))))) binop_nan_pl32

b32_sqrt :: Fappli_IEEE.Coq_mode -> Fappli_IEEE.Coq_binary_float ->
            Fappli_IEEE.Coq_binary_float
b32_sqrt =
  Fappli_IEEE.coq_Bsqrt ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1))))) ((\x -> x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) 1)))))))) unop_nan_pl32

b32_of_bits :: Prelude.Integer -> Coq_binary32
b32_of_bits =
  binary_float_of_bits ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) 1))))) ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1))))

bits_of_b32 :: Coq_binary32 -> Prelude.Integer
bits_of_b32 =
  bits_of_binary_float ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) 1))))) ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1))))

type Coq_binary64 = Fappli_IEEE.Coq_binary_float

default_nan_pl64 :: (,) Prelude.Bool Fappli_IEEE.Coq_nan_pl
default_nan_pl64 =
  (,) Prelude.False
    (Fcore_Zaux.iter_nat (\x -> (\x -> 2 Prelude.* x) x) (Prelude.succ (Prelude.succ
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
      0))))))))))))))))))))))))))))))))))))))))))))))))))) 1)

unop_nan_pl64 :: Coq_binary64 -> (,) Prelude.Bool Fappli_IEEE.Coq_nan_pl
unop_nan_pl64 f =
  case f of {
   Fappli_IEEE.B754_nan s pl -> (,) s pl;
   _ -> default_nan_pl64}

binop_nan_pl64 :: Coq_binary64 -> Coq_binary64 -> (,) Prelude.Bool
                  Fappli_IEEE.Coq_nan_pl
binop_nan_pl64 pl1 pl2 =
  case pl1 of {
   Fappli_IEEE.B754_nan s1 pl3 -> (,) s1 pl3;
   _ ->
    case pl2 of {
     Fappli_IEEE.B754_nan s2 pl3 -> (,) s2 pl3;
     _ -> default_nan_pl64}}

b64_opp :: Fappli_IEEE.Coq_binary_float -> Fappli_IEEE.Coq_binary_float
b64_opp =
  Fappli_IEEE.coq_Bopp ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))) ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    1))))))))))) (\x x0 -> (,) x x0)

b64_plus :: Fappli_IEEE.Coq_mode -> Fappli_IEEE.Coq_binary_float ->
            Fappli_IEEE.Coq_binary_float -> Fappli_IEEE.Coq_binary_float
b64_plus =
  Fappli_IEEE.coq_Bplus ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))) ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    1))))))))))) binop_nan_pl64

b64_minus :: Fappli_IEEE.Coq_mode -> Fappli_IEEE.Coq_binary_float ->
             Fappli_IEEE.Coq_binary_float -> Fappli_IEEE.Coq_binary_float
b64_minus =
  Fappli_IEEE.coq_Bminus ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))) ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    1))))))))))) binop_nan_pl64

b64_mult :: Fappli_IEEE.Coq_mode -> Fappli_IEEE.Coq_binary_float ->
            Fappli_IEEE.Coq_binary_float -> Fappli_IEEE.Coq_binary_float
b64_mult =
  Fappli_IEEE.coq_Bmult ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))) ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    1))))))))))) binop_nan_pl64

b64_div :: Fappli_IEEE.Coq_mode -> Fappli_IEEE.Coq_binary_float ->
           Fappli_IEEE.Coq_binary_float -> Fappli_IEEE.Coq_binary_float
b64_div =
  Fappli_IEEE.coq_Bdiv ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))) ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    1))))))))))) binop_nan_pl64

b64_sqrt :: Fappli_IEEE.Coq_mode -> Fappli_IEEE.Coq_binary_float ->
            Fappli_IEEE.Coq_binary_float
b64_sqrt =
  Fappli_IEEE.coq_Bsqrt ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))) ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    1))))))))))) unop_nan_pl64

b64_of_bits :: Prelude.Integer -> Coq_binary64
b64_of_bits =
  binary_float_of_bits ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))) ((\x -> x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) 1))))

bits_of_b64 :: Coq_binary64 -> Prelude.Integer
bits_of_b64 =
  bits_of_binary_float ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))) ((\x -> x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) 1))))

