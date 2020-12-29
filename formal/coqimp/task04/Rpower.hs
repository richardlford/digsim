module Rpower where

import qualified Prelude
import qualified RIneq
import qualified R_sqrt
import qualified Rdefinitions
import qualified Rpow_def
import qualified Rsqrt_def
import qualified Rtrigo_def

ln_exists1 :: Rdefinitions.R -> Rdefinitions.R
ln_exists1 y =
  let {f = \x -> Rdefinitions.coq_Rminus (Rtrigo_def.exp x) y} in
  Rsqrt_def.coq_IVT_cor f (Rdefinitions.coq_IZR 0) y

ln_exists :: Rdefinitions.R -> Rdefinitions.R
ln_exists y =
  let {s = RIneq.coq_Rle_dec (Rdefinitions.coq_IZR ((\x -> x) 1)) y} in
  case s of {
   Prelude.True -> ln_exists1 y;
   Prelude.False ->
    let {s0 = ln_exists1 (Rdefinitions.coq_Rinv y)} in
    Rdefinitions.coq_Ropp s0}

coq_Rln :: RIneq.Coq_posreal -> Rdefinitions.R
coq_Rln y =
  ln_exists (RIneq.pos y)

ln :: Rdefinitions.R -> Rdefinitions.R
ln x =
  case RIneq.coq_Rlt_dec (Rdefinitions.coq_IZR 0) x of {
   Prelude.True -> coq_Rln x;
   Prelude.False -> Rdefinitions.coq_IZR 0}

coq_Rpower :: Rdefinitions.R -> Rdefinitions.R -> Rdefinitions.R
coq_Rpower x y =
  Rtrigo_def.exp (Rdefinitions.coq_Rmult y (ln x))

arcsinh :: Rdefinitions.R -> Rdefinitions.R
arcsinh x =
  ln
    (Rdefinitions.coq_Rplus x
      (R_sqrt.sqrt
        (Rdefinitions.coq_Rplus
          (Rpow_def.pow x (Prelude.succ (Prelude.succ 0)))
          (Rdefinitions.coq_IZR ((\x -> x) 1)))))

