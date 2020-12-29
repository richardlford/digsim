module Rdefinitions where

import qualified Prelude

type R = () -- AXIOM TO BE REALIZED
  

coq_R0 :: R
coq_R0 =
  Prelude.error "AXIOM TO BE REALIZED"

coq_R1 :: R
coq_R1 =
  Prelude.error "AXIOM TO BE REALIZED"

coq_Rplus :: R -> R -> R
coq_Rplus =
  Prelude.error "AXIOM TO BE REALIZED"

coq_Rmult :: R -> R -> R
coq_Rmult =
  Prelude.error "AXIOM TO BE REALIZED"

coq_Ropp :: R -> R
coq_Ropp =
  Prelude.error "AXIOM TO BE REALIZED"

coq_Rinv :: R -> R
coq_Rinv =
  Prelude.error "AXIOM TO BE REALIZED"

up :: R -> Prelude.Integer
up =
  Prelude.error "AXIOM TO BE REALIZED"

coq_Rminus :: R -> R -> R
coq_Rminus r1 r2 =
  coq_Rplus r1 (coq_Ropp r2)

coq_Rdiv :: R -> R -> R
coq_Rdiv r1 r2 =
  coq_Rmult r1 (coq_Rinv r2)

coq_IPR_2 :: Prelude.Integer -> R
coq_IPR_2 p =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p0 ->
    coq_Rmult (coq_Rplus coq_R1 coq_R1) (coq_Rplus coq_R1 (coq_IPR_2 p0)))
    (\p0 -> coq_Rmult (coq_Rplus coq_R1 coq_R1) (coq_IPR_2 p0))
    (\_ -> coq_Rplus coq_R1 coq_R1)
    p

coq_IPR :: Prelude.Integer -> R
coq_IPR p =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\p0 -> coq_Rplus coq_R1 (coq_IPR_2 p0))
    (\p0 -> coq_IPR_2 p0)
    (\_ -> coq_R1)
    p

coq_IZR :: Prelude.Integer -> R
coq_IZR z =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> coq_R0)
    (\n -> coq_IPR n)
    (\n -> coq_Ropp (coq_IPR n))
    z

