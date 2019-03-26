module Floats where

import qualified Prelude
import qualified Archi
import qualified BinPos
import qualified Coqlib
import qualified Datatypes
import qualified Fappli_IEEE
import qualified Fappli_IEEE_bits
import qualified Fappli_IEEE_extra
import qualified Fcore_Zaux
import qualified Integers
import qualified Specif

type Coq_float = Fappli_IEEE_bits.Coq_binary64

cmp_of_comparison :: Integers.Coq_comparison -> (Prelude.Maybe Datatypes.Coq_comparison) ->
                     Prelude.Bool
cmp_of_comparison c x =
  case c of {
   Integers.Ceq ->
    case x of {
     Prelude.Just c0 -> case c0 of {
                         Datatypes.Eq -> Prelude.True;
                         _ -> Prelude.False};
     Prelude.Nothing -> Prelude.False};
   Integers.Cne ->
    case x of {
     Prelude.Just c0 -> case c0 of {
                         Datatypes.Eq -> Prelude.False;
                         _ -> Prelude.True};
     Prelude.Nothing -> Prelude.True};
   Integers.Clt ->
    case x of {
     Prelude.Just c0 -> case c0 of {
                         Datatypes.Lt -> Prelude.True;
                         _ -> Prelude.False};
     Prelude.Nothing -> Prelude.False};
   Integers.Cle ->
    case x of {
     Prelude.Just c0 -> case c0 of {
                         Datatypes.Gt -> Prelude.False;
                         _ -> Prelude.True};
     Prelude.Nothing -> Prelude.False};
   Integers.Cgt ->
    case x of {
     Prelude.Just c0 -> case c0 of {
                         Datatypes.Gt -> Prelude.True;
                         _ -> Prelude.False};
     Prelude.Nothing -> Prelude.False};
   Integers.Cge ->
    case x of {
     Prelude.Just c0 -> case c0 of {
                         Datatypes.Lt -> Prelude.False;
                         _ -> Prelude.True};
     Prelude.Nothing -> Prelude.False}}

_Float__transform_quiet_pl :: Fappli_IEEE.Coq_nan_pl -> Fappli_IEEE.Coq_nan_pl
_Float__transform_quiet_pl pl =
  BinPos._Pos__lor (Specif.proj1_sig pl)
    (Fcore_Zaux.iter_nat (\x -> (\x -> 2 Prelude.* x) x) (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      0))))))))))))))))))))))))))))))))))))))))))))))))))) 1)

_Float__neg_pl :: Prelude.Bool -> Fappli_IEEE.Coq_nan_pl -> (,) Prelude.Bool Fappli_IEEE.Coq_nan_pl
_Float__neg_pl s pl =
  (,) (Prelude.not s) pl

_Float__abs_pl :: Prelude.Bool -> Fappli_IEEE.Coq_nan_pl -> (,) Prelude.Bool Fappli_IEEE.Coq_nan_pl
_Float__abs_pl _ pl =
  (,) Prelude.False pl

_Float__binop_pl :: Fappli_IEEE_bits.Coq_binary64 -> Fappli_IEEE_bits.Coq_binary64 -> (,)
                    Prelude.Bool Fappli_IEEE.Coq_nan_pl
_Float__binop_pl x y =
  case x of {
   Fappli_IEEE.B754_nan s1 pl1 ->
    case y of {
     Fappli_IEEE.B754_nan s2 pl2 ->
      case Archi.choose_binop_pl_64 s1 pl1 s2 pl2 of {
       Prelude.True -> (,) s2 (_Float__transform_quiet_pl pl2);
       Prelude.False -> (,) s1 (_Float__transform_quiet_pl pl1)};
     _ -> (,) s1 (_Float__transform_quiet_pl pl1)};
   _ ->
    case y of {
     Fappli_IEEE.B754_nan s2 pl2 -> (,) s2 (_Float__transform_quiet_pl pl2);
     _ -> Archi.default_pl_64}}

_Float__zero :: Coq_float
_Float__zero =
  Fappli_IEEE.B754_zero Prelude.False

_Float__neg :: Coq_float -> Coq_float
_Float__neg =
  Fappli_IEEE.coq_Bopp ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    1)))))) ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1))))))))))) _Float__neg_pl

_Float__abs :: Coq_float -> Coq_float
_Float__abs =
  Fappli_IEEE.coq_Babs ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    1)))))) ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1))))))))))) _Float__abs_pl

_Float__add :: Coq_float -> Coq_float -> Coq_float
_Float__add =
  Fappli_IEEE.coq_Bplus ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    1)))))) ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1)))))))))))
    _Float__binop_pl Fappli_IEEE.Coq_mode_NE

_Float__sub :: Coq_float -> Coq_float -> Coq_float
_Float__sub =
  Fappli_IEEE.coq_Bminus ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    1)))))) ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1)))))))))))
    _Float__binop_pl Fappli_IEEE.Coq_mode_NE

_Float__mul :: Coq_float -> Coq_float -> Coq_float
_Float__mul =
  Fappli_IEEE.coq_Bmult ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    1)))))) ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1)))))))))))
    _Float__binop_pl Fappli_IEEE.Coq_mode_NE

_Float__div :: Coq_float -> Coq_float -> Coq_float
_Float__div =
  Fappli_IEEE.coq_Bdiv ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    1)))))) ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1)))))))))))
    _Float__binop_pl Fappli_IEEE.Coq_mode_NE

_Float__compare :: Coq_float -> Coq_float -> Prelude.Maybe Datatypes.Coq_comparison
_Float__compare f1 f2 =
  Fappli_IEEE.coq_Bcompare ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    1)))))) ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1))))))))))) f1 f2

_Float__cmp :: Integers.Coq_comparison -> Coq_float -> Coq_float -> Prelude.Bool
_Float__cmp c f1 f2 =
  cmp_of_comparison c (_Float__compare f1 f2)

_Float__to_long :: Coq_float -> Prelude.Maybe Integers.Int64__Coq_int
_Float__to_long f =
  Coqlib.option_map Integers._Int64__repr
    (Fappli_IEEE_extra.coq_ZofB_range ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))) ((\x -> x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x) 1))))))))))) f Integers._Int64__min_signed Integers._Int64__max_signed)

_Float__from_parsed :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer -> Coq_float
_Float__from_parsed base intPart expPart =
  Fappli_IEEE_extra.coq_Bparse ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    1)))))) ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1))))))))))) base intPart
    expPart

