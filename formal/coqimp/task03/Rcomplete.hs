module Rcomplete where

import qualified Prelude
import qualified Rdefinitions
import qualified SeqProp

coq_R_complete :: (Prelude.Integer -> Rdefinitions.R) -> Rdefinitions.R
coq_R_complete =
  SeqProp.maj_cv

