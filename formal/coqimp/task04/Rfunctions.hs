{-# OPTIONS_GHC -cpp -XMagicHash #-}
{- For Hugs, use the option -F"cpp -P -traditional" -}

module Rfunctions where

import qualified Prelude
import qualified BinInt
import qualified BinPos
import qualified CMorphisms
import qualified CRelationClasses
import qualified Logic
import qualified Nat
import qualified Rbasic_fun
import qualified Rdefinitions
import qualified Rpow_def

#ifdef __GLASGOW_HASKELL__
import qualified GHC.Base
#else
-- HUGS
import qualified IOExts
#endif

#ifdef __GLASGOW_HASKELL__
unsafeCoerce :: a -> b
unsafeCoerce = GHC.Base.unsafeCoerce#
#else
-- HUGS
unsafeCoerce :: a -> b
unsafeCoerce = IOExts.unsafeCoerce
#endif

__ :: any
__ = Prelude.error "Logical or arity value used"

data Z_spec =
   ZintNull
 | ZintPos Prelude.Integer
 | ZintNeg Prelude.Integer

coq_Z_spec_rect :: Prelude.Integer -> (() -> a1) -> (Prelude.Integer -> () ->
                   a1) -> (Prelude.Integer -> () -> a1) -> Prelude.Integer ->
                   Z_spec -> a1
coq_Z_spec_rect _ f f0 f1 _ z =
  case z of {
   ZintNull -> f __;
   ZintPos x -> f0 x __;
   ZintNeg x -> f1 x __}

coq_Z_spec_rec :: Prelude.Integer -> (() -> a1) -> (Prelude.Integer -> () ->
                  a1) -> (Prelude.Integer -> () -> a1) -> Prelude.Integer ->
                  Z_spec -> a1
coq_Z_spec_rec =
  coq_Z_spec_rect

intP :: Prelude.Integer -> Z_spec
intP x =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> ZintNull)
    (\p ->
    CMorphisms.coq_Reflexive_partial_app_morphism __
      (CMorphisms.reflexive_proper
        (unsafeCoerce (\x0 x1 x2 _ ->
          CMorphisms.reflexive_eq_dom_reflexive (\x3 x4 x5 _ ->
            CMorphisms.reflexive_eq_dom_reflexive
              (CRelationClasses.flip_Reflexive (\_ ->
                CRelationClasses.arrow_Reflexive)) x3 x4 x5) x0 x1 x2)) __)
      ((\x -> x) p) __ ((\x -> x) p)
      (BinInt._Z__of_nat (BinPos._Pos__to_nat p)) __ (ZintPos
      (BinPos._Pos__to_nat p)))
    (\p ->
    Logic.eq_rec (BinInt._Z__opp ((\x -> x) p))
      (CMorphisms.coq_Reflexive_partial_app_morphism __
        (CMorphisms.reflexive_proper
          (unsafeCoerce (\x0 x1 x2 _ ->
            CMorphisms.reflexive_eq_dom_reflexive (\x3 x4 x5 _ ->
              CMorphisms.reflexive_eq_dom_reflexive
                (CRelationClasses.flip_Reflexive (\_ ->
                  CRelationClasses.arrow_Reflexive)) x3 x4 x5) x0 x1 x2)) __)
        (BinInt._Z__opp ((\x -> x) p)) __ (BinInt._Z__opp ((\x -> x) p))
        (BinInt._Z__opp (BinInt._Z__of_nat (BinPos._Pos__to_nat p))) __
        (ZintNeg (BinPos._Pos__to_nat p))) (Prelude.negate p))
    x

powerRZ :: Rdefinitions.R -> Prelude.Integer -> Rdefinitions.R
powerRZ x n =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> Rdefinitions.coq_IZR ((\x -> x) 1))
    (\p -> Rpow_def.pow x (BinPos._Pos__to_nat p))
    (\p -> Rdefinitions.coq_Rinv (Rpow_def.pow x (BinPos._Pos__to_nat p)))
    n

decimal_exp :: Rdefinitions.R -> Prelude.Integer -> Rdefinitions.R
decimal_exp r z =
  Rdefinitions.coq_Rmult r
    (powerRZ
      (Rdefinitions.coq_IZR ((\x -> x) ((\x -> 2 Prelude.* x)
        ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) 1))))) z)

sum_nat_f_O :: (Prelude.Integer -> Prelude.Integer) -> Prelude.Integer ->
               Prelude.Integer
sum_nat_f_O f n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> f 0)
    (\n' -> (Prelude.+) (sum_nat_f_O f n') (f (Prelude.succ n')))
    n

sum_nat_f :: Prelude.Integer -> Prelude.Integer -> (Prelude.Integer ->
             Prelude.Integer) -> Prelude.Integer
sum_nat_f s n f =
  sum_nat_f_O (\x -> f ((Prelude.+) x s)) (Nat.sub n s)

sum_nat_O :: Prelude.Integer -> Prelude.Integer
sum_nat_O n =
  sum_nat_f_O (\x -> x) n

sum_nat :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
sum_nat s n =
  sum_nat_f s n (\x -> x)

sum_f_R0 :: (Prelude.Integer -> Rdefinitions.R) -> Prelude.Integer ->
            Rdefinitions.R
sum_f_R0 f n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> f 0)
    (\i -> Rdefinitions.coq_Rplus (sum_f_R0 f i) (f (Prelude.succ i)))
    n

sum_f :: Prelude.Integer -> Prelude.Integer -> (Prelude.Integer ->
         Rdefinitions.R) -> Rdefinitions.R
sum_f s n f =
  sum_f_R0 (\x -> f ((Prelude.+) x s)) (Nat.sub n s)

coq_R_dist :: Rdefinitions.R -> Rdefinitions.R -> Rdefinitions.R
coq_R_dist x y =
  Rbasic_fun.coq_Rabs (Rdefinitions.coq_Rminus x y)

