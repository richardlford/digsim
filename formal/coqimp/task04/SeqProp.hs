module SeqProp where

import qualified Prelude
import qualified Raxioms
import qualified Rdefinitions
import qualified Specif

opp_seq :: (Prelude.Integer -> Rdefinitions.R) -> Prelude.Integer ->
           Rdefinitions.R
opp_seq un n =
  Rdefinitions.coq_Ropp (un n)

growing_cv :: (Prelude.Integer -> Rdefinitions.R) -> Rdefinitions.R
growing_cv _ =
  Specif.proj1_sig Raxioms.completeness

decreasing_cv :: (Prelude.Integer -> Rdefinitions.R) -> Rdefinitions.R
decreasing_cv un =
  let {h1 = growing_cv (opp_seq un)} in Rdefinitions.coq_Ropp h1

ub_to_lub :: (Prelude.Integer -> Rdefinitions.R) -> Rdefinitions.R
ub_to_lub _ =
  Raxioms.completeness

lb_to_glb :: (Prelude.Integer -> Rdefinitions.R) -> Rdefinitions.R
lb_to_glb _ =
  Raxioms.completeness

lub :: (Prelude.Integer -> Rdefinitions.R) -> Rdefinitions.R
lub =
  ub_to_lub

glb :: (Prelude.Integer -> Rdefinitions.R) -> Rdefinitions.R
glb un =
  Rdefinitions.coq_Ropp (lb_to_glb un)

sequence_ub :: (Prelude.Integer -> Rdefinitions.R) -> Prelude.Integer ->
               Rdefinitions.R
sequence_ub un i =
  lub (\k -> un ((Prelude.+) i k))

sequence_lb :: (Prelude.Integer -> Rdefinitions.R) -> Prelude.Integer ->
               Rdefinitions.R
sequence_lb un i =
  glb (\k -> un ((Prelude.+) i k))

maj_cv :: (Prelude.Integer -> Rdefinitions.R) -> Rdefinitions.R
maj_cv un =
  decreasing_cv (sequence_ub un)

min_cv :: (Prelude.Integer -> Rdefinitions.R) -> Rdefinitions.R
min_cv un =
  growing_cv (sequence_lb un)

