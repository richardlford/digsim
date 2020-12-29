module Raxioms where

import qualified Prelude
import qualified Rdefinitions

total_order_T :: Rdefinitions.R -> Rdefinitions.R -> Prelude.Maybe
                 Prelude.Bool
total_order_T =
  Prelude.error "AXIOM TO BE REALIZED"

coq_INR :: Prelude.Integer -> Rdefinitions.R
coq_INR n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> Rdefinitions.coq_IZR 0)
    (\n0 ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> Rdefinitions.coq_IZR ((\x -> x) 1))
      (\_ ->
      Rdefinitions.coq_Rplus (coq_INR n0)
        (Rdefinitions.coq_IZR ((\x -> x) 1)))
      n0)
    n

completeness :: Rdefinitions.R
completeness =
  Prelude.error "AXIOM TO BE REALIZED"

