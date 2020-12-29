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

type Coq_float32 = Fappli_IEEE_bits.Coq_binary32

cmp_of_comparison :: Integers.Coq_comparison -> (Prelude.Maybe
                     Datatypes.Coq_comparison) -> Prelude.Bool
cmp_of_comparison c x =
  case c of {
   Integers.Ceq ->
    case x of {
     Prelude.Just c0 ->
      case c0 of {
       Datatypes.Eq -> Prelude.True;
       _ -> Prelude.False};
     Prelude.Nothing -> Prelude.False};
   Integers.Cne ->
    case x of {
     Prelude.Just c0 ->
      case c0 of {
       Datatypes.Eq -> Prelude.False;
       _ -> Prelude.True};
     Prelude.Nothing -> Prelude.True};
   Integers.Clt ->
    case x of {
     Prelude.Just c0 ->
      case c0 of {
       Datatypes.Lt -> Prelude.True;
       _ -> Prelude.False};
     Prelude.Nothing -> Prelude.False};
   Integers.Cle ->
    case x of {
     Prelude.Just c0 ->
      case c0 of {
       Datatypes.Gt -> Prelude.False;
       _ -> Prelude.True};
     Prelude.Nothing -> Prelude.False};
   Integers.Cgt ->
    case x of {
     Prelude.Just c0 ->
      case c0 of {
       Datatypes.Gt -> Prelude.True;
       _ -> Prelude.False};
     Prelude.Nothing -> Prelude.False};
   Integers.Cge ->
    case x of {
     Prelude.Just c0 ->
      case c0 of {
       Datatypes.Lt -> Prelude.False;
       _ -> Prelude.True};
     Prelude.Nothing -> Prelude.False}}

ordered_of_comparison :: (Prelude.Maybe Datatypes.Coq_comparison) -> Prelude.Bool
ordered_of_comparison x =
  case x of {
   Prelude.Just _ -> Prelude.True;
   Prelude.Nothing -> Prelude.False}

_Float__transform_quiet_pl :: Fappli_IEEE.Coq_nan_pl -> Fappli_IEEE.Coq_nan_pl
_Float__transform_quiet_pl pl =
  BinPos._Pos__lor (Specif.proj1_sig pl)
    (Fcore_Zaux.iter_nat (\x -> (\x -> 2 Prelude.* x) x) (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      0))))))))))))))))))))))))))))))))))))))))))))))))))) 1)

_Float__expand_pl :: Fappli_IEEE.Coq_nan_pl -> Fappli_IEEE.Coq_nan_pl
_Float__expand_pl pl =
  BinPos._Pos__shiftl_nat (Specif.proj1_sig pl) (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ 0)))))))))))))))))))))))))))))

_Float__of_single_pl :: Prelude.Bool -> Fappli_IEEE.Coq_nan_pl -> (,) Prelude.Bool
                        Fappli_IEEE.Coq_nan_pl
_Float__of_single_pl s pl =
  (,) s
    (case Archi.float_of_single_preserves_sNaN of {
      Prelude.True -> _Float__expand_pl pl;
      Prelude.False -> _Float__transform_quiet_pl (_Float__expand_pl pl)})

_Float__reduce_pl :: Fappli_IEEE.Coq_nan_pl -> Fappli_IEEE.Coq_nan_pl
_Float__reduce_pl pl =
  BinPos._Pos__shiftr_nat (Specif.proj1_sig pl) (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ 0)))))))))))))))))))))))))))))

_Float__to_single_pl :: Prelude.Bool -> Fappli_IEEE.Coq_nan_pl -> (,) Prelude.Bool
                        Fappli_IEEE.Coq_nan_pl
_Float__to_single_pl s pl =
  (,) s (_Float__reduce_pl (_Float__transform_quiet_pl pl))

_Float__neg_pl :: Prelude.Bool -> Fappli_IEEE.Coq_nan_pl -> (,) Prelude.Bool
                  Fappli_IEEE.Coq_nan_pl
_Float__neg_pl s pl =
  (,) (Prelude.not s) pl

_Float__abs_pl :: Prelude.Bool -> Fappli_IEEE.Coq_nan_pl -> (,) Prelude.Bool
                  Fappli_IEEE.Coq_nan_pl
_Float__abs_pl _ pl =
  (,) Prelude.False pl

_Float__binop_pl :: Fappli_IEEE_bits.Coq_binary64 -> Fappli_IEEE_bits.Coq_binary64
                    -> (,) Prelude.Bool Fappli_IEEE.Coq_nan_pl
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

_Float__eq_dec :: Coq_float -> Coq_float -> Prelude.Bool
_Float__eq_dec =
  Fappli_IEEE_extra.coq_Beq_dec ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))) ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    1)))))))))))

_Float__neg :: Coq_float -> Coq_float
_Float__neg =
  Fappli_IEEE.coq_Bopp ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))) ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    1))))))))))) _Float__neg_pl

_Float__abs :: Coq_float -> Coq_float
_Float__abs =
  Fappli_IEEE.coq_Babs ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))) ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    1))))))))))) _Float__abs_pl

_Float__add :: Coq_float -> Coq_float -> Coq_float
_Float__add =
  Fappli_IEEE.coq_Bplus ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))) ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    1))))))))))) _Float__binop_pl Fappli_IEEE.Coq_mode_NE

_Float__sub :: Coq_float -> Coq_float -> Coq_float
_Float__sub =
  Fappli_IEEE.coq_Bminus ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))) ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    1))))))))))) _Float__binop_pl Fappli_IEEE.Coq_mode_NE

_Float__mul :: Coq_float -> Coq_float -> Coq_float
_Float__mul =
  Fappli_IEEE.coq_Bmult ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))) ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    1))))))))))) _Float__binop_pl Fappli_IEEE.Coq_mode_NE

_Float__div :: Coq_float -> Coq_float -> Coq_float
_Float__div =
  Fappli_IEEE.coq_Bdiv ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))) ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    1))))))))))) _Float__binop_pl Fappli_IEEE.Coq_mode_NE

_Float__compare :: Coq_float -> Coq_float -> Prelude.Maybe Datatypes.Coq_comparison
_Float__compare f1 f2 =
  Fappli_IEEE.coq_Bcompare ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))) ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    1))))))))))) f1 f2

_Float__cmp :: Integers.Coq_comparison -> Coq_float -> Coq_float -> Prelude.Bool
_Float__cmp c f1 f2 =
  cmp_of_comparison c (_Float__compare f1 f2)

_Float__ordered :: Coq_float -> Coq_float -> Prelude.Bool
_Float__ordered f1 f2 =
  ordered_of_comparison (_Float__compare f1 f2)

_Float__of_single :: Coq_float32 -> Coq_float
_Float__of_single =
  Fappli_IEEE_extra.coq_Bconv ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    1))))) ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1)))))))) ((\x -> x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))) ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    1))))))))))) _Float__of_single_pl Fappli_IEEE.Coq_mode_NE

_Float__to_single :: Coq_float -> Coq_float32
_Float__to_single =
  Fappli_IEEE_extra.coq_Bconv ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))) ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    1))))))))))) ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1))))) ((\x -> x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) 1)))))))) _Float__to_single_pl Fappli_IEEE.Coq_mode_NE

_Float__to_int :: Coq_float -> Prelude.Maybe Integers.Int__Coq_int
_Float__to_int f =
  Coqlib.option_map Integers._Int__repr
    (Fappli_IEEE_extra.coq_ZofB_range ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))) ((\x -> x)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x) 1))))))))))) f Integers._Int__min_signed
      Integers._Int__max_signed)

_Float__to_intu :: Coq_float -> Prelude.Maybe Integers.Int__Coq_int
_Float__to_intu f =
  Coqlib.option_map Integers._Int__repr
    (Fappli_IEEE_extra.coq_ZofB_range ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))) ((\x -> x)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x) 1))))))))))) f 0 Integers._Int__max_unsigned)

_Float__to_long :: Coq_float -> Prelude.Maybe Integers.Int64__Coq_int
_Float__to_long f =
  Coqlib.option_map Integers._Int64__repr
    (Fappli_IEEE_extra.coq_ZofB_range ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))) ((\x -> x)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x) 1))))))))))) f Integers._Int64__min_signed
      Integers._Int64__max_signed)

_Float__to_longu :: Coq_float -> Prelude.Maybe Integers.Int64__Coq_int
_Float__to_longu f =
  Coqlib.option_map Integers._Int64__repr
    (Fappli_IEEE_extra.coq_ZofB_range ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))) ((\x -> x)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x) 1))))))))))) f 0 Integers._Int64__max_unsigned)

_Float__of_int :: Integers.Int__Coq_int -> Coq_float
_Float__of_int n =
  Fappli_IEEE_extra.coq_BofZ ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))) ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    1))))))))))) (Integers._Int__signed n)

_Float__of_intu :: Integers.Int__Coq_int -> Coq_float
_Float__of_intu n =
  Fappli_IEEE_extra.coq_BofZ ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))) ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    1))))))))))) (Integers._Int__unsigned n)

_Float__of_long :: Integers.Int64__Coq_int -> Coq_float
_Float__of_long n =
  Fappli_IEEE_extra.coq_BofZ ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))) ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    1))))))))))) (Integers._Int64__signed n)

_Float__of_longu :: Integers.Int64__Coq_int -> Coq_float
_Float__of_longu n =
  Fappli_IEEE_extra.coq_BofZ ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))) ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    1))))))))))) (Integers._Int64__unsigned n)

_Float__from_parsed :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer ->
                       Coq_float
_Float__from_parsed base intPart expPart =
  Fappli_IEEE_extra.coq_Bparse ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))) ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    1))))))))))) base intPart expPart

_Float__to_bits :: Coq_float -> Integers.Int64__Coq_int
_Float__to_bits f =
  Integers._Int64__repr (Fappli_IEEE_bits.bits_of_b64 f)

_Float__of_bits :: Integers.Int64__Coq_int -> Coq_float
_Float__of_bits b =
  Fappli_IEEE_bits.b64_of_bits (Integers._Int64__unsigned b)

_Float__from_words :: Integers.Int__Coq_int -> Integers.Int__Coq_int -> Coq_float
_Float__from_words hi lo =
  _Float__of_bits (Integers._Int64__ofwords hi lo)

_Float__exact_inverse :: Coq_float -> Prelude.Maybe Coq_float
_Float__exact_inverse =
  Fappli_IEEE_extra.coq_Bexact_inverse ((\x -> x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) 1)))))) ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    1)))))))))))

_Float__ox8000_0000 :: Integers.Int__Coq_int
_Float__ox8000_0000 =
  Integers._Int__repr Integers._Int__half_modulus

_Float__ox4330_0000 :: Integers.Int__Coq_int
_Float__ox4330_0000 =
  Integers._Int__repr ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1)))))))))))))))))))))))))))))))

_Float__ox4530_0000 :: Integers.Int__Coq_int
_Float__ox4530_0000 =
  Integers._Int__repr ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1)))))))))))))))))))))))))))))))

_Float32__transform_quiet_pl :: Fappli_IEEE.Coq_nan_pl -> Fappli_IEEE.Coq_nan_pl
_Float32__transform_quiet_pl pl =
  BinPos._Pos__lor (Specif.proj1_sig pl)
    (Fcore_Zaux.iter_nat (\x -> (\x -> 2 Prelude.* x) x) (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      0)))))))))))))))))))))) 1)

_Float32__neg_pl :: Prelude.Bool -> Fappli_IEEE.Coq_nan_pl -> (,) Prelude.Bool
                    Fappli_IEEE.Coq_nan_pl
_Float32__neg_pl s pl =
  (,) (Prelude.not s) pl

_Float32__abs_pl :: Prelude.Bool -> Fappli_IEEE.Coq_nan_pl -> (,) Prelude.Bool
                    Fappli_IEEE.Coq_nan_pl
_Float32__abs_pl _ pl =
  (,) Prelude.False pl

_Float32__binop_pl :: Fappli_IEEE_bits.Coq_binary32 -> Fappli_IEEE_bits.Coq_binary32
                      -> (,) Prelude.Bool Fappli_IEEE.Coq_nan_pl
_Float32__binop_pl x y =
  case x of {
   Fappli_IEEE.B754_nan s1 pl1 ->
    case y of {
     Fappli_IEEE.B754_nan s2 pl2 ->
      case Archi.choose_binop_pl_32 s1 pl1 s2 pl2 of {
       Prelude.True -> (,) s2 (_Float32__transform_quiet_pl pl2);
       Prelude.False -> (,) s1 (_Float32__transform_quiet_pl pl1)};
     _ -> (,) s1 (_Float32__transform_quiet_pl pl1)};
   _ ->
    case y of {
     Fappli_IEEE.B754_nan s2 pl2 -> (,) s2 (_Float32__transform_quiet_pl pl2);
     _ -> Archi.default_pl_32}}

_Float32__zero :: Coq_float32
_Float32__zero =
  Fappli_IEEE.B754_zero Prelude.False

_Float32__eq_dec :: Coq_float32 -> Coq_float32 -> Prelude.Bool
_Float32__eq_dec =
  Fappli_IEEE_extra.coq_Beq_dec ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    1))))) ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1))))))))

_Float32__neg :: Coq_float32 -> Coq_float32
_Float32__neg =
  Fappli_IEEE.coq_Bopp ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1))))) ((\x -> x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) 1)))))))) _Float32__neg_pl

_Float32__abs :: Coq_float32 -> Coq_float32
_Float32__abs =
  Fappli_IEEE.coq_Babs ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1))))) ((\x -> x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) 1)))))))) _Float32__abs_pl

_Float32__add :: Coq_float32 -> Coq_float32 -> Coq_float32
_Float32__add =
  Fappli_IEEE.coq_Bplus ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1))))) ((\x -> x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) 1)))))))) _Float32__binop_pl Fappli_IEEE.Coq_mode_NE

_Float32__sub :: Coq_float32 -> Coq_float32 -> Coq_float32
_Float32__sub =
  Fappli_IEEE.coq_Bminus ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1))))) ((\x -> x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) 1)))))))) _Float32__binop_pl Fappli_IEEE.Coq_mode_NE

_Float32__mul :: Coq_float32 -> Coq_float32 -> Coq_float32
_Float32__mul =
  Fappli_IEEE.coq_Bmult ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1))))) ((\x -> x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) 1)))))))) _Float32__binop_pl Fappli_IEEE.Coq_mode_NE

_Float32__div :: Coq_float32 -> Coq_float32 -> Coq_float32
_Float32__div =
  Fappli_IEEE.coq_Bdiv ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1))))) ((\x -> x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) 1)))))))) _Float32__binop_pl Fappli_IEEE.Coq_mode_NE

_Float32__compare :: Coq_float32 -> Coq_float32 -> Prelude.Maybe
                     Datatypes.Coq_comparison
_Float32__compare f1 f2 =
  Fappli_IEEE.coq_Bcompare ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1) 1))))) ((\x -> x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) 1)))))))) f1 f2

_Float32__cmp :: Integers.Coq_comparison -> Coq_float32 -> Coq_float32 ->
                 Prelude.Bool
_Float32__cmp c f1 f2 =
  cmp_of_comparison c (_Float32__compare f1 f2)

_Float32__ordered :: Coq_float32 -> Coq_float32 -> Prelude.Bool
_Float32__ordered f1 f2 =
  ordered_of_comparison (_Float32__compare f1 f2)

_Float32__of_double :: Coq_float -> Coq_float32
_Float32__of_double =
  _Float__to_single

_Float32__to_double :: Coq_float32 -> Coq_float
_Float32__to_double =
  _Float__of_single

_Float32__to_int :: Coq_float32 -> Prelude.Maybe Integers.Int__Coq_int
_Float32__to_int f =
  Coqlib.option_map Integers._Int__repr
    (Fappli_IEEE_extra.coq_ZofB_range ((\x -> x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x Prelude.+ 1) 1))))) ((\x -> x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1))))))))
      f Integers._Int__min_signed Integers._Int__max_signed)

_Float32__to_intu :: Coq_float32 -> Prelude.Maybe Integers.Int__Coq_int
_Float32__to_intu f =
  Coqlib.option_map Integers._Int__repr
    (Fappli_IEEE_extra.coq_ZofB_range ((\x -> x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x Prelude.+ 1) 1))))) ((\x -> x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1))))))))
      f 0 Integers._Int__max_unsigned)

_Float32__to_long :: Coq_float32 -> Prelude.Maybe Integers.Int64__Coq_int
_Float32__to_long f =
  Coqlib.option_map Integers._Int64__repr
    (Fappli_IEEE_extra.coq_ZofB_range ((\x -> x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x Prelude.+ 1) 1))))) ((\x -> x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1))))))))
      f Integers._Int64__min_signed Integers._Int64__max_signed)

_Float32__to_longu :: Coq_float32 -> Prelude.Maybe Integers.Int64__Coq_int
_Float32__to_longu f =
  Coqlib.option_map Integers._Int64__repr
    (Fappli_IEEE_extra.coq_ZofB_range ((\x -> x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x Prelude.+ 1) 1))))) ((\x -> x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
      ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1))))))))
      f 0 Integers._Int64__max_unsigned)

_Float32__of_int :: Integers.Int__Coq_int -> Coq_float32
_Float32__of_int n =
  Fappli_IEEE_extra.coq_BofZ ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    1))))) ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1))))))))
    (Integers._Int__signed n)

_Float32__of_intu :: Integers.Int__Coq_int -> Coq_float32
_Float32__of_intu n =
  Fappli_IEEE_extra.coq_BofZ ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    1))))) ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1))))))))
    (Integers._Int__unsigned n)

_Float32__of_long :: Integers.Int64__Coq_int -> Coq_float32
_Float32__of_long n =
  Fappli_IEEE_extra.coq_BofZ ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    1))))) ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1))))))))
    (Integers._Int64__signed n)

_Float32__of_longu :: Integers.Int64__Coq_int -> Coq_float32
_Float32__of_longu n =
  Fappli_IEEE_extra.coq_BofZ ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    1))))) ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1))))))))
    (Integers._Int64__unsigned n)

_Float32__from_parsed :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer ->
                         Coq_float32
_Float32__from_parsed base intPart expPart =
  Fappli_IEEE_extra.coq_Bparse ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    1))))) ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1)))))))) base intPart expPart

_Float32__to_bits :: Coq_float32 -> Integers.Int__Coq_int
_Float32__to_bits f =
  Integers._Int__repr (Fappli_IEEE_bits.bits_of_b32 f)

_Float32__of_bits :: Integers.Int__Coq_int -> Coq_float32
_Float32__of_bits b =
  Fappli_IEEE_bits.b32_of_bits (Integers._Int__unsigned b)

_Float32__exact_inverse :: Coq_float32 -> Prelude.Maybe Coq_float32
_Float32__exact_inverse =
  Fappli_IEEE_extra.coq_Bexact_inverse ((\x -> x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x Prelude.+ 1)
    1))))) ((\x -> x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x)
    ((\x -> 2 Prelude.* x) ((\x -> 2 Prelude.* x) 1))))))))

