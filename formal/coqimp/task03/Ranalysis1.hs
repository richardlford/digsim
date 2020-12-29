module Ranalysis1 where

import qualified Prelude
import qualified Nat
import qualified Raxioms
import qualified Rdefinitions
import qualified Rpow_def
import qualified Specif

plus_fct :: (Rdefinitions.R -> Rdefinitions.R) -> (Rdefinitions.R -> Rdefinitions.R)
            -> Rdefinitions.R -> Rdefinitions.R
plus_fct f1 f2 x =
  Rdefinitions.coq_Rplus (f1 x) (f2 x)

opp_fct :: (Rdefinitions.R -> Rdefinitions.R) -> Rdefinitions.R -> Rdefinitions.R
opp_fct f x =
  Rdefinitions.coq_Ropp (f x)

mult_fct :: (Rdefinitions.R -> Rdefinitions.R) -> (Rdefinitions.R -> Rdefinitions.R)
            -> Rdefinitions.R -> Rdefinitions.R
mult_fct f1 f2 x =
  Rdefinitions.coq_Rmult (f1 x) (f2 x)

mult_real_fct :: Rdefinitions.R -> (Rdefinitions.R -> Rdefinitions.R) ->
                 Rdefinitions.R -> Rdefinitions.R
mult_real_fct a f x =
  Rdefinitions.coq_Rmult a (f x)

minus_fct :: (Rdefinitions.R -> Rdefinitions.R) -> (Rdefinitions.R ->
             Rdefinitions.R) -> Rdefinitions.R -> Rdefinitions.R
minus_fct f1 f2 x =
  Rdefinitions.coq_Rminus (f1 x) (f2 x)

div_fct :: (Rdefinitions.R -> Rdefinitions.R) -> (Rdefinitions.R -> Rdefinitions.R)
           -> Rdefinitions.R -> Rdefinitions.R
div_fct f1 f2 x =
  Rdefinitions.coq_Rdiv (f1 x) (f2 x)

div_real_fct :: Rdefinitions.R -> (Rdefinitions.R -> Rdefinitions.R) ->
                Rdefinitions.R -> Rdefinitions.R
div_real_fct a f x =
  Rdefinitions.coq_Rdiv a (f x)

comp :: (Rdefinitions.R -> Rdefinitions.R) -> (Rdefinitions.R -> Rdefinitions.R) ->
        Rdefinitions.R -> Rdefinitions.R
comp f1 f2 x =
  f1 (f2 x)

inv_fct :: (Rdefinitions.R -> Rdefinitions.R) -> Rdefinitions.R -> Rdefinitions.R
inv_fct f x =
  Rdefinitions.coq_Rinv (f x)

fct_cte :: Rdefinitions.R -> Rdefinitions.R -> Rdefinitions.R
fct_cte a _ =
  a

id :: Rdefinitions.R -> Rdefinitions.R
id x =
  x

type Coq_derivable_pt = Rdefinitions.R

type Coq_derivable = Rdefinitions.R -> Coq_derivable_pt

derive_pt :: (Rdefinitions.R -> Rdefinitions.R) -> Rdefinitions.R ->
             Coq_derivable_pt -> Rdefinitions.R
derive_pt _ _ =
  Specif.proj1_sig

derive :: (Rdefinitions.R -> Rdefinitions.R) -> Coq_derivable -> Rdefinitions.R ->
          Rdefinitions.R
derive f pr x =
  derive_pt f x (pr x)

data Differential =
   Coq_mkDifferential (Rdefinitions.R -> Rdefinitions.R) Coq_derivable

d1 :: Differential -> Rdefinitions.R -> Rdefinitions.R
d1 d =
  case d of {
   Coq_mkDifferential d3 _ -> d3}

cond_diff :: Differential -> Coq_derivable
cond_diff d =
  case d of {
   Coq_mkDifferential _ cond_diff0 -> cond_diff0}

data Differential_D2 =
   Coq_mkDifferential_D2 (Rdefinitions.R -> Rdefinitions.R) Coq_derivable Coq_derivable

d2 :: Differential_D2 -> Rdefinitions.R -> Rdefinitions.R
d2 d =
  case d of {
   Coq_mkDifferential_D2 d3 _ _ -> d3}

cond_D1 :: Differential_D2 -> Coq_derivable
cond_D1 d =
  case d of {
   Coq_mkDifferential_D2 _ cond_D3 _ -> cond_D3}

cond_D2 :: Differential_D2 -> Coq_derivable
cond_D2 d =
  case d of {
   Coq_mkDifferential_D2 _ _ cond_D3 -> cond_D3}

derivable_pt_plus :: (Rdefinitions.R -> Rdefinitions.R) -> (Rdefinitions.R ->
                     Rdefinitions.R) -> Rdefinitions.R -> Coq_derivable_pt ->
                     Coq_derivable_pt -> Coq_derivable_pt
derivable_pt_plus _ _ _ x x0 =
  Specif.sig_rec (\x1 _ ->
    Specif.sig_rec (\x2 _ -> Rdefinitions.coq_Rplus x1 x2) x0) x

derivable_pt_opp :: (Rdefinitions.R -> Rdefinitions.R) -> Rdefinitions.R ->
                    Coq_derivable_pt -> Coq_derivable_pt
derivable_pt_opp _ _ x =
  Specif.sig_rec (\x0 _ -> Rdefinitions.coq_Ropp x0) x

derivable_pt_minus :: (Rdefinitions.R -> Rdefinitions.R) -> (Rdefinitions.R ->
                      Rdefinitions.R) -> Rdefinitions.R -> Coq_derivable_pt ->
                      Coq_derivable_pt -> Coq_derivable_pt
derivable_pt_minus _ _ _ x x0 =
  Specif.sig_rec (\x1 _ ->
    Specif.sig_rec (\x2 _ -> Rdefinitions.coq_Rminus x1 x2) x0) x

derivable_pt_mult :: (Rdefinitions.R -> Rdefinitions.R) -> (Rdefinitions.R ->
                     Rdefinitions.R) -> Rdefinitions.R -> Coq_derivable_pt ->
                     Coq_derivable_pt -> Coq_derivable_pt
derivable_pt_mult f1 f2 x x0 x1 =
  Specif.sig_rec (\x2 _ ->
    Specif.sig_rec (\x3 _ ->
      Rdefinitions.coq_Rplus (Rdefinitions.coq_Rmult x2 (f2 x))
        (Rdefinitions.coq_Rmult (f1 x) x3)) x1) x0

derivable_pt_const :: Rdefinitions.R -> Rdefinitions.R -> Coq_derivable_pt
derivable_pt_const _ _ =
  Rdefinitions.coq_IZR 0

derivable_pt_scal :: (Rdefinitions.R -> Rdefinitions.R) -> Rdefinitions.R ->
                     Rdefinitions.R -> Coq_derivable_pt -> Coq_derivable_pt
derivable_pt_scal _ a _ x =
  Specif.sig_rec (\x0 _ -> Rdefinitions.coq_Rmult a x0) x

derivable_pt_id :: Rdefinitions.R -> Coq_derivable_pt
derivable_pt_id _ =
  Rdefinitions.coq_IZR ((\x -> x) 1)

derivable_pt_Rsqr :: Rdefinitions.R -> Coq_derivable_pt
derivable_pt_Rsqr x =
  Rdefinitions.coq_Rmult
    (Rdefinitions.coq_IZR ((\x -> x) ((\x -> 2 Prelude.* x) 1))) x

derivable_pt_comp :: (Rdefinitions.R -> Rdefinitions.R) -> (Rdefinitions.R ->
                     Rdefinitions.R) -> Rdefinitions.R -> Coq_derivable_pt ->
                     Coq_derivable_pt -> Coq_derivable_pt
derivable_pt_comp _ _ _ x x0 =
  Specif.sig_rec (\x1 _ ->
    Specif.sig_rec (\x2 _ -> Rdefinitions.coq_Rmult x2 x1) x0) x

derivable_plus :: (Rdefinitions.R -> Rdefinitions.R) -> (Rdefinitions.R ->
                  Rdefinitions.R) -> Coq_derivable -> Coq_derivable -> Coq_derivable
derivable_plus f1 f2 x x0 x1 =
  derivable_pt_plus f1 f2 x1 (x x1) (x0 x1)

derivable_opp :: (Rdefinitions.R -> Rdefinitions.R) -> Coq_derivable ->
                 Coq_derivable
derivable_opp f x x0 =
  derivable_pt_opp f x0 (x x0)

derivable_minus :: (Rdefinitions.R -> Rdefinitions.R) -> (Rdefinitions.R ->
                   Rdefinitions.R) -> Coq_derivable -> Coq_derivable ->
                   Coq_derivable
derivable_minus f1 f2 x x0 x1 =
  derivable_pt_minus f1 f2 x1 (x x1) (x0 x1)

derivable_mult :: (Rdefinitions.R -> Rdefinitions.R) -> (Rdefinitions.R ->
                  Rdefinitions.R) -> Coq_derivable -> Coq_derivable -> Coq_derivable
derivable_mult f1 f2 x x0 x1 =
  derivable_pt_mult f1 f2 x1 (x x1) (x0 x1)

derivable_const :: Rdefinitions.R -> Coq_derivable
derivable_const =
  derivable_pt_const

derivable_scal :: (Rdefinitions.R -> Rdefinitions.R) -> Rdefinitions.R ->
                  Coq_derivable -> Coq_derivable
derivable_scal f a x x0 =
  derivable_pt_scal f a x0 (x x0)

derivable_id :: Coq_derivable
derivable_id =
  derivable_pt_id

derivable_Rsqr :: Coq_derivable
derivable_Rsqr =
  derivable_pt_Rsqr

derivable_comp :: (Rdefinitions.R -> Rdefinitions.R) -> (Rdefinitions.R ->
                  Rdefinitions.R) -> Coq_derivable -> Coq_derivable -> Coq_derivable
derivable_comp f1 f2 x x0 x1 =
  derivable_pt_comp f1 f2 x1 (x x1) (x0 (f1 x1))

pow_fct :: Prelude.Integer -> Rdefinitions.R -> Rdefinitions.R
pow_fct n y =
  Rpow_def.pow y n

derivable_pt_pow :: Prelude.Integer -> Rdefinitions.R -> Coq_derivable_pt
derivable_pt_pow n x =
  Rdefinitions.coq_Rmult (Raxioms.coq_INR n) (Rpow_def.pow x (Nat.pred n))

derivable_pow :: Prelude.Integer -> Coq_derivable
derivable_pow =
  derivable_pt_pow

