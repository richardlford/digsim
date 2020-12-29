module PartSum where

import qualified Prelude
import qualified Rcomplete
import qualified Rdefinitions
import qualified Rfunctions

cv_cauchy_2 :: (Prelude.Integer -> Rdefinitions.R) -> Rdefinitions.R
cv_cauchy_2 an =
  Rcomplete.coq_R_complete (\n -> Rfunctions.sum_f_R0 an n)

coq_SP :: (Prelude.Integer -> Rdefinitions.R -> Rdefinitions.R) ->
          Prelude.Integer -> Rdefinitions.R -> Rdefinitions.R
coq_SP fn n x =
  Rfunctions.sum_f_R0 (\k -> fn k x) n

