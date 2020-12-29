module ZArith_dec where

import qualified Prelude
import qualified BinInt
import qualified Datatypes
import qualified Logic
import qualified Specif
import qualified Sumbool

__ :: any
__ = Prelude.error "Logical or arity value used"

coq_Dcompare_inf :: Datatypes.Coq_comparison -> Prelude.Maybe Prelude.Bool
coq_Dcompare_inf r =
  Datatypes.comparison_rec (Prelude.Just Prelude.True) (Prelude.Just Prelude.False)
    Prelude.Nothing r

coq_Zcompare_rect :: Prelude.Integer -> Prelude.Integer -> (() -> a1) -> (() -> a1)
                     -> (() -> a1) -> a1
coq_Zcompare_rect n m h1 h2 h3 =
  let {c = BinInt._Z__compare n m} in
  case c of {
   Datatypes.Eq -> h1 __;
   Datatypes.Lt -> h2 __;
   Datatypes.Gt -> h3 __}

coq_Zcompare_rec :: Prelude.Integer -> Prelude.Integer -> (() -> a1) -> (() -> a1)
                    -> (() -> a1) -> a1
coq_Zcompare_rec =
  coq_Zcompare_rect

coq_Z_lt_dec :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
coq_Z_lt_dec x y =
  case BinInt._Z__compare x y of {
   Datatypes.Lt -> Prelude.True;
   _ -> Prelude.False}

coq_Z_le_dec :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
coq_Z_le_dec x y =
  case BinInt._Z__compare x y of {
   Datatypes.Gt -> Prelude.False;
   _ -> Prelude.True}

coq_Z_gt_dec :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
coq_Z_gt_dec x y =
  case BinInt._Z__compare x y of {
   Datatypes.Gt -> Prelude.True;
   _ -> Prelude.False}

coq_Z_ge_dec :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
coq_Z_ge_dec x y =
  case BinInt._Z__compare x y of {
   Datatypes.Lt -> Prelude.False;
   _ -> Prelude.True}

coq_Z_lt_ge_dec :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
coq_Z_lt_ge_dec =
  coq_Z_lt_dec

coq_Z_lt_le_dec :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
coq_Z_lt_le_dec x y =
  Specif.sumbool_rec (\_ -> Prelude.True) (\_ -> Prelude.False)
    (coq_Z_lt_ge_dec x y)

coq_Z_le_gt_dec :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
coq_Z_le_gt_dec x y =
  Specif.sumbool_rec (\_ -> Prelude.True) (\_ -> Prelude.False) (coq_Z_le_dec x y)

coq_Z_le_lt_eq_dec :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
coq_Z_le_lt_eq_dec x y =
  coq_Zcompare_rec x y (\_ -> Prelude.False) (\_ -> Prelude.True) (\_ ->
    Logic.coq_False_rec)

coq_Zlt_cotrans :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer ->
                   Prelude.Bool
coq_Zlt_cotrans x _ z =
  coq_Z_lt_ge_dec x z

coq_Zlt_cotrans_pos :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
coq_Zlt_cotrans_pos x y =
  coq_Zlt_cotrans 0 ((Prelude.+) x y) x

coq_Zlt_cotrans_neg :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
coq_Zlt_cotrans_neg x y =
  case coq_Zlt_cotrans ((Prelude.+) x y) 0 x of {
   Prelude.True -> Prelude.False;
   Prelude.False -> Prelude.True}

not_Zeq_inf :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
not_Zeq_inf x y =
  case coq_Z_lt_ge_dec x y of {
   Prelude.True -> Prelude.True;
   Prelude.False ->
    case coq_Z_le_lt_eq_dec y x of {
     Prelude.True -> Prelude.False;
     Prelude.False -> Logic.coq_False_rec}}

coq_Z_dec :: Prelude.Integer -> Prelude.Integer -> Prelude.Maybe Prelude.Bool
coq_Z_dec x y =
  case coq_Z_lt_ge_dec x y of {
   Prelude.True -> Prelude.Just Prelude.True;
   Prelude.False ->
    case coq_Z_le_lt_eq_dec y x of {
     Prelude.True -> Prelude.Just Prelude.False;
     Prelude.False -> Prelude.Nothing}}

coq_Z_dec' :: Prelude.Integer -> Prelude.Integer -> Prelude.Maybe Prelude.Bool
coq_Z_dec' x y =
  case BinInt._Z__eq_dec x y of {
   Prelude.True -> Prelude.Nothing;
   Prelude.False -> Prelude.Just (not_Zeq_inf x y)}

coq_Z_zerop :: Prelude.Integer -> Prelude.Bool
coq_Z_zerop x =
  BinInt._Z__eq_dec x 0

coq_Z_notzerop :: Prelude.Integer -> Prelude.Bool
coq_Z_notzerop x =
  Sumbool.sumbool_not (coq_Z_zerop x)

coq_Z_noteq_dec :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
coq_Z_noteq_dec x y =
  Sumbool.sumbool_not (BinInt._Z__eq_dec x y)

