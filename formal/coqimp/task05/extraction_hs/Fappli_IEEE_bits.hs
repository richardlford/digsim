module Fappli_IEEE_bits where

import qualified Prelude
import qualified Fappli_IEEE
import qualified Fcore_Zaux

type Coq_binary64 = Fappli_IEEE.Coq_binary_float

default_nan_pl64 :: (,) Prelude.Bool Fappli_IEEE.Coq_nan_pl
default_nan_pl64 =
  (,) Prelude.False
    (Fcore_Zaux.iter_nat (\x -> (\x -> 2 Prelude.* x) x) (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      0))))))))))))))))))))))))))))))))))))))))))))))))))) 1)

unop_nan_pl64 :: Coq_binary64 -> (,) Prelude.Bool Fappli_IEEE.Coq_nan_pl
unop_nan_pl64 f =
  case f of {
   Fappli_IEEE.B754_nan s pl -> (,) s pl;
   _ -> default_nan_pl64}

b64_sqrt :: Fappli_IEEE.Coq_mode -> Fappli_IEEE.Coq_binary_float -> Fappli_IEEE.Coq_binary_float
b64_sqrt =
  Fappli_IEEE.coq_Bsqrt ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    1)))))) ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1))))))))))) unop_nan_pl64

