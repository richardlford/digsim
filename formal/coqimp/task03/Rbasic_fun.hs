module Rbasic_fun where

import qualified Prelude
import qualified RIneq
import qualified Rdefinitions
import qualified Specif

__ :: any
__ = Prelude.error "Logical or arity value used"

coq_Rmin :: Rdefinitions.R -> Rdefinitions.R -> Rdefinitions.R
coq_Rmin x y =
  case RIneq.coq_Rle_dec x y of {
   Prelude.True -> x;
   Prelude.False -> y}

coq_Rmin_case :: Rdefinitions.R -> Rdefinitions.R -> a1 -> a1 -> a1
coq_Rmin_case r1 r2 h1 h2 =
  case RIneq.coq_Rle_dec r1 r2 of {
   Prelude.True -> h1;
   Prelude.False -> h2}

coq_Rmin_case_strong :: Rdefinitions.R -> Rdefinitions.R -> (() -> a1) -> (() -> a1)
                        -> a1
coq_Rmin_case_strong r1 r2 h1 h2 =
  let {s = RIneq.coq_Rle_dec r1 r2} in
  case s of {
   Prelude.True -> h1 __;
   Prelude.False -> h2 __}

coq_Rmax :: Rdefinitions.R -> Rdefinitions.R -> Rdefinitions.R
coq_Rmax x y =
  case RIneq.coq_Rle_dec x y of {
   Prelude.True -> y;
   Prelude.False -> x}

coq_Rmax_case :: Rdefinitions.R -> Rdefinitions.R -> a1 -> a1 -> a1
coq_Rmax_case r1 r2 h1 h2 =
  case RIneq.coq_Rle_dec r1 r2 of {
   Prelude.True -> h2;
   Prelude.False -> h1}

coq_Rmax_case_strong :: Rdefinitions.R -> Rdefinitions.R -> (() -> a1) -> (() -> a1)
                        -> a1
coq_Rmax_case_strong r1 r2 h1 h2 =
  case RIneq.coq_Rle_dec r1 r2 of {
   Prelude.True -> h2 __;
   Prelude.False -> h1 __}

coq_Rcase_abs :: Rdefinitions.R -> Prelude.Bool
coq_Rcase_abs r =
  let {x = RIneq.coq_Rle_dec (Rdefinitions.coq_IZR 0) r} in
  Specif.sumbool_rec (\_ -> Prelude.False) (\_ -> Prelude.True) x

coq_Rabs :: Rdefinitions.R -> Rdefinitions.R
coq_Rabs r =
  case coq_Rcase_abs r of {
   Prelude.True -> Rdefinitions.coq_Ropp r;
   Prelude.False -> r}

