module RIneq where

import qualified Prelude
import qualified Logic
import qualified Raxioms
import qualified Rdefinitions
import qualified Specif

coq_Rlt_dec :: Rdefinitions.R -> Rdefinitions.R -> Prelude.Bool
coq_Rlt_dec r1 r2 =
  let {h = Raxioms.total_order_T r1 r2} in
  Specif.sumor_rec (\a ->
    Specif.sumbool_rec (\_ -> Prelude.True) (\_ -> Prelude.False) a) (\_ ->
    Prelude.False) h

coq_Rle_dec :: Rdefinitions.R -> Rdefinitions.R -> Prelude.Bool
coq_Rle_dec r1 r2 =
  let {h = Raxioms.total_order_T r1 r2} in
  Specif.sumor_rec (\a ->
    Specif.sumbool_rec (\_ -> Prelude.True) (\_ -> Prelude.True) a) (\_ ->
    Prelude.False) h

coq_Rgt_dec :: Rdefinitions.R -> Rdefinitions.R -> Prelude.Bool
coq_Rgt_dec r1 r2 =
  coq_Rlt_dec r2 r1

coq_Rge_dec :: Rdefinitions.R -> Rdefinitions.R -> Prelude.Bool
coq_Rge_dec r1 r2 =
  coq_Rle_dec r2 r1

coq_Rlt_le_dec :: Rdefinitions.R -> Rdefinitions.R -> Prelude.Bool
coq_Rlt_le_dec r1 r2 =
  let {h = Raxioms.total_order_T r1 r2} in
  Specif.sumor_rec (\a ->
    Specif.sumbool_rec (\_ -> Prelude.True) (\_ -> Prelude.False) a) (\_ ->
    Prelude.False) h

coq_Rgt_ge_dec :: Rdefinitions.R -> Rdefinitions.R -> Prelude.Bool
coq_Rgt_ge_dec r1 r2 =
  coq_Rlt_le_dec r2 r1

coq_Rle_lt_dec :: Rdefinitions.R -> Rdefinitions.R -> Prelude.Bool
coq_Rle_lt_dec r1 r2 =
  let {h = Raxioms.total_order_T r1 r2} in
  Specif.sumor_rec (\a ->
    Specif.sumbool_rec (\_ -> Prelude.True) (\_ -> Prelude.True) a) (\_ ->
    Prelude.False) h

coq_Rge_gt_dec :: Rdefinitions.R -> Rdefinitions.R -> Prelude.Bool
coq_Rge_gt_dec r1 r2 =
  coq_Rle_lt_dec r2 r1

coq_Rle_lt_or_eq_dec :: Rdefinitions.R -> Rdefinitions.R -> Prelude.Bool
coq_Rle_lt_or_eq_dec r1 r2 =
  let {h0 = Raxioms.total_order_T r1 r2} in
  Specif.sumor_rec (\a ->
    Specif.sumbool_rec (\_ -> Prelude.True) (\_ -> Prelude.False) a) (\_ ->
    Prelude.False) h0

inser_trans_R :: Rdefinitions.R -> Rdefinitions.R -> Rdefinitions.R ->
                 Rdefinitions.R -> Prelude.Bool
inser_trans_R _ m _ q =
  let {h0 = coq_Rlt_le_dec m q} in
  Logic.and_rec (\_ _ ->
    Specif.sumbool_rec (\_ -> Prelude.True) (\_ -> Prelude.False) h0)

coq_Rsqr :: Rdefinitions.R -> Rdefinitions.R
coq_Rsqr r =
  Rdefinitions.coq_Rmult r r

coq_Req_EM_T :: Rdefinitions.R -> Rdefinitions.R -> Prelude.Bool
coq_Req_EM_T r1 r2 =
  let {s = Raxioms.total_order_T r1 r2} in
  case s of {
   Prelude.Just s0 ->
    case s0 of {
     Prelude.True -> Prelude.False;
     Prelude.False -> Prelude.True};
   Prelude.Nothing -> Prelude.False}

type Coq_nonnegreal =
  Rdefinitions.R
  -- singleton inductive, whose constructor was mknonnegreal
  
nonneg :: Coq_nonnegreal -> Rdefinitions.R
nonneg n =
  n

type Coq_posreal =
  Rdefinitions.R
  -- singleton inductive, whose constructor was mkposreal
  
pos :: Coq_posreal -> Rdefinitions.R
pos p =
  p

type Coq_nonposreal =
  Rdefinitions.R
  -- singleton inductive, whose constructor was mknonposreal
  
nonpos :: Coq_nonposreal -> Rdefinitions.R
nonpos n =
  n

type Coq_negreal =
  Rdefinitions.R
  -- singleton inductive, whose constructor was mknegreal
  
neg :: Coq_negreal -> Rdefinitions.R
neg n =
  n

type Coq_nonzeroreal =
  Rdefinitions.R
  -- singleton inductive, whose constructor was mknonzeroreal
  
nonzero :: Coq_nonzeroreal -> Rdefinitions.R
nonzero n =
  n

