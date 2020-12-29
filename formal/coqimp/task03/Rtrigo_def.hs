module Rtrigo_def where

import qualified Prelude
import qualified Alembert
import qualified Factorial
import qualified RIneq
import qualified Raxioms
import qualified Rdefinitions
import qualified Rpow_def
import qualified Specif

exist_exp :: Rdefinitions.R -> Rdefinitions.R
exist_exp x =
  Alembert.coq_Alembert_C3 (\n ->
    Rdefinitions.coq_Rinv (Raxioms.coq_INR (Factorial.fact n))) x

exp :: Rdefinitions.R -> Rdefinitions.R
exp x =
  Specif.proj1_sig (exist_exp x)

exist_exp0 :: Rdefinitions.R
exist_exp0 =
  Rdefinitions.coq_IZR ((\x -> x) 1)

cosh :: Rdefinitions.R -> Rdefinitions.R
cosh x =
  Rdefinitions.coq_Rdiv
    (Rdefinitions.coq_Rplus (exp x) (exp (Rdefinitions.coq_Ropp x)))
    (Rdefinitions.coq_IZR ((\x -> x) ((\x -> 2 Prelude.* x) 1)))

sinh :: Rdefinitions.R -> Rdefinitions.R
sinh x =
  Rdefinitions.coq_Rdiv
    (Rdefinitions.coq_Rminus (exp x) (exp (Rdefinitions.coq_Ropp x)))
    (Rdefinitions.coq_IZR ((\x -> x) ((\x -> 2 Prelude.* x) 1)))

tanh :: Rdefinitions.R -> Rdefinitions.R
tanh x =
  Rdefinitions.coq_Rdiv (sinh x) (cosh x)

cos_n :: Prelude.Integer -> Rdefinitions.R
cos_n n =
  Rdefinitions.coq_Rdiv (Rpow_def.pow (Rdefinitions.coq_IZR (Prelude.negate 1)) n)
    (Raxioms.coq_INR
      (Factorial.fact ((Prelude.*) (Prelude.succ (Prelude.succ 0)) n)))

exist_cos :: Rdefinitions.R -> Rdefinitions.R
exist_cos x =
  Alembert.coq_Alembert_C3 cos_n x

cos :: Rdefinitions.R -> Rdefinitions.R
cos x =
  exist_cos (RIneq.coq_Rsqr x)

sin_n :: Prelude.Integer -> Rdefinitions.R
sin_n n =
  Rdefinitions.coq_Rdiv (Rpow_def.pow (Rdefinitions.coq_IZR (Prelude.negate 1)) n)
    (Raxioms.coq_INR
      (Factorial.fact
        ((Prelude.+) ((Prelude.*) (Prelude.succ (Prelude.succ 0)) n) (Prelude.succ
          0))))

exist_sin :: Rdefinitions.R -> Rdefinitions.R
exist_sin x =
  Alembert.coq_Alembert_C3 sin_n x

sin :: Rdefinitions.R -> Rdefinitions.R
sin x =
  Rdefinitions.coq_Rmult x (exist_sin (RIneq.coq_Rsqr x))

exist_cos0 :: Rdefinitions.R
exist_cos0 =
  Rdefinitions.coq_IZR ((\x -> x) 1)

