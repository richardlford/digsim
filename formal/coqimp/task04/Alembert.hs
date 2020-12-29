module Alembert where

import qualified Prelude
import qualified PartSum
import qualified Raxioms
import qualified Rbasic_fun
import qualified Rdefinitions
import qualified Rpow_def
import qualified Specif

coq_Alembert_C1 :: (Prelude.Integer -> Rdefinitions.R) -> Rdefinitions.R
coq_Alembert_C1 _ =
  Raxioms.completeness

coq_Alembert_C2 :: (Prelude.Integer -> Rdefinitions.R) -> Rdefinitions.R
coq_Alembert_C2 an =
  let {
   vn = \i ->
    Rdefinitions.coq_Rdiv
      (Rdefinitions.coq_Rplus
        (Rdefinitions.coq_Rmult
          (Rdefinitions.coq_IZR ((\x -> x) ((\x -> 2 Prelude.* x) 1)))
          (Rbasic_fun.coq_Rabs (an i))) (an i))
      (Rdefinitions.coq_IZR ((\x -> x) ((\x -> 2 Prelude.* x) 1)))}
  in
  let {
   wn = \i ->
    Rdefinitions.coq_Rdiv
      (Rdefinitions.coq_Rminus
        (Rdefinitions.coq_Rmult
          (Rdefinitions.coq_IZR ((\x -> x) ((\x -> 2 Prelude.* x) 1)))
          (Rbasic_fun.coq_Rabs (an i))) (an i))
      (Rdefinitions.coq_IZR ((\x -> x) ((\x -> 2 Prelude.* x) 1)))}
  in
  let {h5 = coq_Alembert_C1 vn} in
  let {h6 = coq_Alembert_C1 wn} in Rdefinitions.coq_Rminus h5 h6

coq_AlembertC3_step1 :: (Prelude.Integer -> Rdefinitions.R) -> Rdefinitions.R
                        -> Rdefinitions.R
coq_AlembertC3_step1 an x =
  let {bn = \i -> Rdefinitions.coq_Rmult (an i) (Rpow_def.pow x i)} in
  coq_Alembert_C2 bn

coq_AlembertC3_step2 :: (Prelude.Integer -> Rdefinitions.R) -> Rdefinitions.R
                        -> Rdefinitions.R
coq_AlembertC3_step2 an _ =
  an 0

coq_Alembert_C3 :: (Prelude.Integer -> Rdefinitions.R) -> Rdefinitions.R ->
                   Rdefinitions.R
coq_Alembert_C3 an x =
  let {s = Raxioms.total_order_T x (Rdefinitions.coq_IZR 0)} in
  case s of {
   Prelude.Just s0 ->
    case s0 of {
     Prelude.True -> coq_AlembertC3_step1 an x;
     Prelude.False -> coq_AlembertC3_step2 an x};
   Prelude.Nothing -> coq_AlembertC3_step1 an x}

coq_Alembert_C4 :: (Prelude.Integer -> Rdefinitions.R) -> Rdefinitions.R ->
                   Rdefinitions.R
coq_Alembert_C4 _ _ =
  Raxioms.completeness

coq_Alembert_C5 :: (Prelude.Integer -> Rdefinitions.R) -> Rdefinitions.R ->
                   Rdefinitions.R
coq_Alembert_C5 an _ =
  let {x = PartSum.cv_cauchy_2 an} in Specif.sig_rec (\x0 _ -> x0) x

coq_Alembert_C6 :: (Prelude.Integer -> Rdefinitions.R) -> Rdefinitions.R ->
                   Rdefinitions.R -> Rdefinitions.R
coq_Alembert_C6 an x k =
  let {
   h3 = let {s = Raxioms.total_order_T x (Rdefinitions.coq_IZR 0)} in
        case s of {
         Prelude.Just s0 ->
          case s0 of {
           Prelude.True ->
            coq_Alembert_C5 (\i ->
              Rdefinitions.coq_Rmult (an i) (Rpow_def.pow x i))
              (Rdefinitions.coq_Rmult k (Rbasic_fun.coq_Rabs x));
           Prelude.False -> an 0};
         Prelude.Nothing ->
          coq_Alembert_C5 (\i ->
            Rdefinitions.coq_Rmult (an i) (Rpow_def.pow x i))
            (Rdefinitions.coq_Rmult k (Rbasic_fun.coq_Rabs x))}}
  in
  Specif.sig_rec (\x0 _ -> x0) h3

