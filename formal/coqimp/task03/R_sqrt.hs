module R_sqrt where

import qualified Prelude
import qualified RIneq
import qualified Rbasic_fun
import qualified Rdefinitions
import qualified Rsqrt_def

sqrt :: Rdefinitions.R -> Rdefinitions.R
sqrt x =
  case Rbasic_fun.coq_Rcase_abs x of {
   Prelude.True -> Rdefinitions.coq_IZR 0;
   Prelude.False -> Rsqrt_def.coq_Rsqrt x}

coq_Delta :: RIneq.Coq_nonzeroreal -> Rdefinitions.R -> Rdefinitions.R ->
             Rdefinitions.R
coq_Delta a b c =
  Rdefinitions.coq_Rminus (RIneq.coq_Rsqr b)
    (Rdefinitions.coq_Rmult
      (Rdefinitions.coq_Rmult
        (Rdefinitions.coq_IZR ((\x -> x) ((\x -> 2 Prelude.* x)
          ((\x -> 2 Prelude.* x) 1)))) (RIneq.nonzero a)) c)

sol_x1 :: RIneq.Coq_nonzeroreal -> Rdefinitions.R -> Rdefinitions.R ->
          Rdefinitions.R
sol_x1 a b c =
  Rdefinitions.coq_Rdiv
    (Rdefinitions.coq_Rplus (Rdefinitions.coq_Ropp b) (sqrt (coq_Delta a b c)))
    (Rdefinitions.coq_Rmult
      (Rdefinitions.coq_IZR ((\x -> x) ((\x -> 2 Prelude.* x) 1)))
      (RIneq.nonzero a))

sol_x2 :: RIneq.Coq_nonzeroreal -> Rdefinitions.R -> Rdefinitions.R ->
          Rdefinitions.R
sol_x2 a b c =
  Rdefinitions.coq_Rdiv
    (Rdefinitions.coq_Rminus (Rdefinitions.coq_Ropp b) (sqrt (coq_Delta a b c)))
    (Rdefinitions.coq_Rmult
      (Rdefinitions.coq_IZR ((\x -> x) ((\x -> 2 Prelude.* x) 1)))
      (RIneq.nonzero a))

