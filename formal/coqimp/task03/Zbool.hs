module Zbool where

import qualified Prelude
import qualified BinInt
import qualified Datatypes
import qualified Sumbool
import qualified ZArith_dec
import qualified Zeven

coq_Z_lt_ge_bool :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
coq_Z_lt_ge_bool x y =
  Sumbool.bool_of_sumbool (ZArith_dec.coq_Z_lt_ge_dec x y)

coq_Z_ge_lt_bool :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
coq_Z_ge_lt_bool x y =
  Sumbool.bool_of_sumbool ((Prelude.>=) x y)

coq_Z_le_gt_bool :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
coq_Z_le_gt_bool x y =
  Sumbool.bool_of_sumbool (ZArith_dec.coq_Z_le_gt_dec x y)

coq_Z_gt_le_bool :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
coq_Z_gt_le_bool x y =
  Sumbool.bool_of_sumbool ((Prelude.>) x y)

coq_Z_eq_bool :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
coq_Z_eq_bool x y =
  Sumbool.bool_of_sumbool (BinInt._Z__eq_dec x y)

coq_Z_noteq_bool :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
coq_Z_noteq_bool x y =
  Sumbool.bool_of_sumbool (ZArith_dec.coq_Z_noteq_dec x y)

coq_Zeven_odd_bool :: Prelude.Integer -> Prelude.Bool
coq_Zeven_odd_bool x =
  Sumbool.bool_of_sumbool (Zeven.coq_Zeven_odd_dec x)

coq_Zeq_bool :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
coq_Zeq_bool x y =
  case BinInt._Z__compare x y of {
   Datatypes.Eq -> Prelude.True;
   _ -> Prelude.False}

coq_Zneq_bool :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
coq_Zneq_bool x y =
  case BinInt._Z__compare x y of {
   Datatypes.Eq -> Prelude.False;
   _ -> Prelude.True}

coq_Zle_bool_total :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
coq_Zle_bool_total x y =
  case BinInt._Z__leb x y of {
   Prelude.True -> Prelude.True;
   Prelude.False -> Prelude.False}

