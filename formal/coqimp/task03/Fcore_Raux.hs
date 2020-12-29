module Fcore_Raux where

import qualified Prelude
import qualified BinInt
import qualified Datatypes
import qualified Fcore_Zaux
import qualified RIneq
import qualified Raxioms
import qualified Rbasic_fun
import qualified Rdefinitions
import qualified Rpower

coq_P2R :: Prelude.Integer -> Rdefinitions.R
coq_P2R p =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\t ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\_ ->
      Rdefinitions.coq_Rplus (Rdefinitions.coq_IZR ((\x -> x) 1))
        (Rdefinitions.coq_Rmult
          (Rdefinitions.coq_IZR ((\x -> x) ((\x -> 2 Prelude.* x) 1))) (coq_P2R t)))
      (\_ ->
      Rdefinitions.coq_Rplus (Rdefinitions.coq_IZR ((\x -> x) 1))
        (Rdefinitions.coq_Rmult
          (Rdefinitions.coq_IZR ((\x -> x) ((\x -> 2 Prelude.* x) 1))) (coq_P2R t)))
      (\_ ->
      Rdefinitions.coq_IZR ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))
      t)
    (\t ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\_ ->
      Rdefinitions.coq_Rmult
        (Rdefinitions.coq_IZR ((\x -> x) ((\x -> 2 Prelude.* x) 1))) (coq_P2R t))
      (\_ ->
      Rdefinitions.coq_Rmult
        (Rdefinitions.coq_IZR ((\x -> x) ((\x -> 2 Prelude.* x) 1))) (coq_P2R t))
      (\_ -> Rdefinitions.coq_IZR ((\x -> x) ((\x -> 2 Prelude.* x) 1)))
      t)
    (\_ -> Rdefinitions.coq_IZR ((\x -> x) 1))
    p

coq_Z2R :: Prelude.Integer -> Rdefinitions.R
coq_Z2R n =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> Rdefinitions.coq_IZR 0)
    (\p -> coq_P2R p)
    (\p -> Rdefinitions.coq_Ropp (coq_P2R p))
    n

coq_Rcompare :: Rdefinitions.R -> Rdefinitions.R -> Datatypes.Coq_comparison
coq_Rcompare x y =
  case Raxioms.total_order_T x y of {
   Prelude.Just s ->
    case s of {
     Prelude.True -> Datatypes.Lt;
     Prelude.False -> Datatypes.Eq};
   Prelude.Nothing -> Datatypes.Gt}

coq_Rle_bool :: Rdefinitions.R -> Rdefinitions.R -> Prelude.Bool
coq_Rle_bool x y =
  case coq_Rcompare x y of {
   Datatypes.Gt -> Prelude.False;
   _ -> Prelude.True}

coq_Rlt_bool :: Rdefinitions.R -> Rdefinitions.R -> Prelude.Bool
coq_Rlt_bool x y =
  case coq_Rcompare x y of {
   Datatypes.Lt -> Prelude.True;
   _ -> Prelude.False}

coq_Req_bool :: Rdefinitions.R -> Rdefinitions.R -> Prelude.Bool
coq_Req_bool x y =
  case coq_Rcompare x y of {
   Datatypes.Eq -> Prelude.True;
   _ -> Prelude.False}

coq_Zfloor :: Rdefinitions.R -> Prelude.Integer
coq_Zfloor x =
  (Prelude.-) (Rdefinitions.up x) ((\x -> x) 1)

coq_Zceil :: Rdefinitions.R -> Prelude.Integer
coq_Zceil x =
  BinInt._Z__opp (coq_Zfloor (Rdefinitions.coq_Ropp x))

coq_Ztrunc :: Rdefinitions.R -> Prelude.Integer
coq_Ztrunc x =
  case coq_Rlt_bool x (Rdefinitions.coq_IZR 0) of {
   Prelude.True -> coq_Zceil x;
   Prelude.False -> coq_Zfloor x}

coq_Zaway :: Rdefinitions.R -> Prelude.Integer
coq_Zaway x =
  case coq_Rlt_bool x (Rdefinitions.coq_IZR 0) of {
   Prelude.True -> coq_Zfloor x;
   Prelude.False -> coq_Zceil x}

bpow :: Fcore_Zaux.Coq_radix -> Prelude.Integer -> Rdefinitions.R
bpow r e =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> Rdefinitions.coq_IZR ((\x -> x) 1))
    (\p -> coq_Z2R (BinInt._Z__pow_pos (Fcore_Zaux.radix_val r) p))
    (\p ->
    Rdefinitions.coq_Rinv (coq_Z2R (BinInt._Z__pow_pos (Fcore_Zaux.radix_val r) p)))
    e

type Coq_ln_beta_prop =
  Prelude.Integer
  -- singleton inductive, whose constructor was Build_ln_beta_prop
  
ln_beta_val :: Fcore_Zaux.Coq_radix -> Rdefinitions.R -> Coq_ln_beta_prop ->
               Prelude.Integer
ln_beta_val _ _ l =
  l

ln_beta :: Fcore_Zaux.Coq_radix -> Rdefinitions.R -> Coq_ln_beta_prop
ln_beta r x =
  let {fact = Rpower.ln (coq_Z2R (Fcore_Zaux.radix_val r))} in
  (Prelude.+)
    (coq_Zfloor (Rdefinitions.coq_Rdiv (Rpower.ln (Rbasic_fun.coq_Rabs x)) fact))
    ((\x -> x) 1)

cond_Ropp :: Prelude.Bool -> Rdefinitions.R -> Rdefinitions.R
cond_Ropp b m =
  case b of {
   Prelude.True -> Rdefinitions.coq_Ropp m;
   Prelude.False -> m}

coq_LPO_min :: Prelude.Maybe Prelude.Integer
coq_LPO_min =
  let {s = RIneq.coq_Rle_lt_dec Raxioms.completeness (Rdefinitions.coq_IZR 0)} in
  case s of {
   Prelude.True -> Prelude.Nothing;
   Prelude.False -> Prelude.Just
    (BinInt._Z__abs_nat
      ((Prelude.-) (Rdefinitions.up (Rdefinitions.coq_Rinv Raxioms.completeness))
        ((\x -> x) ((\x -> 2 Prelude.* x) 1))))}

coq_LPO :: Prelude.Maybe Prelude.Integer
coq_LPO =
  coq_LPO_min

coq_LPO_Z :: Prelude.Maybe Prelude.Integer
coq_LPO_Z =
  case coq_LPO of {
   Prelude.Just j -> Prelude.Just (BinInt._Z__of_nat j);
   Prelude.Nothing ->
    case coq_LPO of {
     Prelude.Just k -> Prelude.Just (BinInt._Z__opp (BinInt._Z__of_nat k));
     Prelude.Nothing -> Prelude.Nothing}}

