module Fcore_generic_fmt where

import qualified Prelude
import qualified BinInt
import qualified Datatypes
import qualified Fcore_Raux
import qualified Fcore_Zaux
import qualified Fcore_defs
import qualified Rdefinitions

canonic_exp :: Fcore_Zaux.Coq_radix -> (Prelude.Integer -> Prelude.Integer) ->
               Rdefinitions.R -> Prelude.Integer
canonic_exp beta fexp x =
  fexp (Fcore_Raux.ln_beta_val beta x (Fcore_Raux.ln_beta beta x))

scaled_mantissa :: Fcore_Zaux.Coq_radix -> (Prelude.Integer -> Prelude.Integer) ->
                   Rdefinitions.R -> Rdefinitions.R
scaled_mantissa beta fexp x =
  Rdefinitions.coq_Rmult x
    (Fcore_Raux.bpow beta (BinInt._Z__opp (canonic_exp beta fexp x)))

round :: Fcore_Zaux.Coq_radix -> (Prelude.Integer -> Prelude.Integer) ->
         (Rdefinitions.R -> Prelude.Integer) -> Rdefinitions.R -> Rdefinitions.R
round beta fexp rnd x =
  \x -> throw false beta (Fcore_defs.Float (rnd (scaled_mantissa beta fexp x))
    (canonic_exp beta fexp x))

coq_Zrnd_opp :: (Rdefinitions.R -> Prelude.Integer) -> Rdefinitions.R ->
                Prelude.Integer
coq_Zrnd_opp rnd x =
  BinInt._Z__opp (rnd (Rdefinitions.coq_Ropp x))

coq_Znearest :: (Prelude.Integer -> Prelude.Bool) -> Rdefinitions.R ->
                Prelude.Integer
coq_Znearest choice x =
  case Fcore_Raux.coq_Rcompare
         (Rdefinitions.coq_Rminus x (Fcore_Raux.coq_Z2R (Fcore_Raux.coq_Zfloor x)))
         (Rdefinitions.coq_Rinv
           (Rdefinitions.coq_IZR ((\x -> x) ((\x -> 2 Prelude.* x) 1)))) of {
   Datatypes.Eq ->
    case choice (Fcore_Raux.coq_Zfloor x) of {
     Prelude.True -> Fcore_Raux.coq_Zceil x;
     Prelude.False -> Fcore_Raux.coq_Zfloor x};
   Datatypes.Lt -> Fcore_Raux.coq_Zfloor x;
   Datatypes.Gt -> Fcore_Raux.coq_Zceil x}

