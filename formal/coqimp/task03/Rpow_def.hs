module Rpow_def where

import qualified Prelude
import qualified Rdefinitions

pow :: Rdefinitions.R -> Prelude.Integer -> Rdefinitions.R
pow r n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> Rdefinitions.coq_IZR ((\x -> x) 1))
    (\n0 -> Rdefinitions.coq_Rmult r (pow r n0))
    n

