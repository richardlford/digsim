module Archi where

import qualified Prelude
import qualified Datatypes
import qualified Fappli_IEEE

ptr64 :: Prelude.Bool
ptr64 =
  Prelude.True

big_endian :: Prelude.Bool
big_endian =
  Prelude.False

align_int64 :: Prelude.Integer
align_int64 =
  (\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) 1)))

align_float64 :: Prelude.Integer
align_float64 =
  (\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) 1)))

splitlong :: Prelude.Bool
splitlong =
  Prelude.not ptr64

default_pl_64 :: (,) Prelude.Bool Fappli_IEEE.Coq_nan_pl
default_pl_64 =
  (,) Prelude.True
    (Datatypes.nat_rect 1 (\_ x -> (\x -> 2 Prelude.* x) x) (Prelude.succ
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
      0))))))))))))))))))))))))))))))))))))))))))))))))))))

choose_binop_pl_64 :: Prelude.Bool -> Fappli_IEEE.Coq_nan_pl -> Prelude.Bool
                      -> Fappli_IEEE.Coq_nan_pl -> Prelude.Bool
choose_binop_pl_64 _ _ _ _ =
  Prelude.False

default_pl_32 :: (,) Prelude.Bool Fappli_IEEE.Coq_nan_pl
default_pl_32 =
  (,) Prelude.True
    (Datatypes.nat_rect 1 (\_ x -> (\x -> 2 Prelude.* x) x) (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ 0)))))))))))))))))))))))

choose_binop_pl_32 :: Prelude.Bool -> Fappli_IEEE.Coq_nan_pl -> Prelude.Bool
                      -> Fappli_IEEE.Coq_nan_pl -> Prelude.Bool
choose_binop_pl_32 _ _ _ _ =
  Prelude.False

float_of_single_preserves_sNaN :: Prelude.Bool
float_of_single_preserves_sNaN =
  Prelude.False

