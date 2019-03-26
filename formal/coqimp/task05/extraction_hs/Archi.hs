module Archi where

import qualified Prelude
import qualified Datatypes
import qualified Fappli_IEEE

default_pl_64 :: (,) Prelude.Bool Fappli_IEEE.Coq_nan_pl
default_pl_64 =
  (,) Prelude.True
    (Datatypes.nat_rect 1 (\_ x -> (\x -> 2 Prelude.* x) x) (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      0))))))))))))))))))))))))))))))))))))))))))))))))))))

choose_binop_pl_64 :: Prelude.Bool -> Fappli_IEEE.Coq_nan_pl -> Prelude.Bool ->
                      Fappli_IEEE.Coq_nan_pl -> Prelude.Bool
choose_binop_pl_64 _ _ _ _ =
  Prelude.False

