module Fcore_defs where

import qualified Prelude
import qualified Fcore_Zaux

data Coq_float =
   Float Prelude.Integer Prelude.Integer

coq_Fnum :: Fcore_Zaux.Coq_radix -> Coq_float -> Prelude.Integer
coq_Fnum _ f =
  case f of {
   Float fnum _ -> fnum}

coq_Fexp :: Fcore_Zaux.Coq_radix -> Coq_float -> Prelude.Integer
coq_Fexp _ f =
  case f of {
   Float _ fexp -> fexp}

