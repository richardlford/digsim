module Integers where

import qualified Prelude
import qualified Archi
import qualified BinInt
import qualified BinPos
import qualified Coqlib
import qualified List0
import qualified Zpower

data Coq_comparison =
   Ceq
 | Cne
 | Clt
 | Cle
 | Cgt
 | Cge

comparison_rect :: a1 -> a1 -> a1 -> a1 -> a1 -> a1 -> Coq_comparison -> a1
comparison_rect f f0 f1 f2 f3 f4 c =
  case c of {
   Ceq -> f;
   Cne -> f0;
   Clt -> f1;
   Cle -> f2;
   Cgt -> f3;
   Cge -> f4}

comparison_rec :: a1 -> a1 -> a1 -> a1 -> a1 -> a1 -> Coq_comparison -> a1
comparison_rec =
  comparison_rect

negate_comparison :: Coq_comparison -> Coq_comparison
negate_comparison c =
  case c of {
   Ceq -> Cne;
   Cne -> Ceq;
   Clt -> Cge;
   Cle -> Cgt;
   Cgt -> Cle;
   Cge -> Clt}

swap_comparison :: Coq_comparison -> Coq_comparison
swap_comparison c =
  case c of {
   Clt -> Cgt;
   Cle -> Cge;
   Cgt -> Clt;
   Cge -> Cle;
   x -> x}

_Wordsize_32__wordsize :: Prelude.Integer
_Wordsize_32__wordsize =
  Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ 0)))))))))))))))))))))))))))))))

_Int__wordsize :: Prelude.Integer
_Int__wordsize =
  _Wordsize_32__wordsize

_Int__zwordsize :: Prelude.Integer
_Int__zwordsize =
  BinInt._Z__of_nat _Int__wordsize

_Int__modulus :: Prelude.Integer
_Int__modulus =
  Zpower.two_power_nat _Int__wordsize

_Int__half_modulus :: Prelude.Integer
_Int__half_modulus =
  BinInt._Z__div _Int__modulus ((\x -> x) ((\x -> 2 Prelude.* x) 1))

_Int__max_unsigned :: Prelude.Integer
_Int__max_unsigned =
  (Prelude.-) _Int__modulus ((\x -> x) 1)

_Int__max_signed :: Prelude.Integer
_Int__max_signed =
  (Prelude.-) _Int__half_modulus ((\x -> x) 1)

_Int__min_signed :: Prelude.Integer
_Int__min_signed =
  BinInt._Z__opp _Int__half_modulus

type Int__Coq_int =
  Prelude.Integer
  -- singleton inductive, whose constructor was mkint
  
_Int__intval :: Int__Coq_int -> Prelude.Integer
_Int__intval i =
  i

_Int__coq_P_mod_two_p :: Prelude.Integer -> Prelude.Integer ->
                         Prelude.Integer
_Int__coq_P_mod_two_p p n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> 0)
    (\m ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> BinInt._Z__succ_double (_Int__coq_P_mod_two_p q m))
      (\q -> BinInt._Z__double (_Int__coq_P_mod_two_p q m))
      (\_ -> (\x -> x) 1)
      p)
    n

_Int__coq_Z_mod_modulus :: Prelude.Integer -> Prelude.Integer
_Int__coq_Z_mod_modulus x =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> 0)
    (\p -> _Int__coq_P_mod_two_p p _Int__wordsize)
    (\p ->
    let {r = _Int__coq_P_mod_two_p p _Int__wordsize} in
    case Coqlib.zeq r 0 of {
     Prelude.True -> 0;
     Prelude.False -> (Prelude.-) _Int__modulus r})
    x

_Int__unsigned :: Int__Coq_int -> Prelude.Integer
_Int__unsigned =
  _Int__intval

_Int__signed :: Int__Coq_int -> Prelude.Integer
_Int__signed n =
  let {x = _Int__unsigned n} in
  case Coqlib.zlt x _Int__half_modulus of {
   Prelude.True -> x;
   Prelude.False -> (Prelude.-) x _Int__modulus}

_Int__repr :: Prelude.Integer -> Int__Coq_int
_Int__repr =
  _Int__coq_Z_mod_modulus

_Int__zero :: Int__Coq_int
_Int__zero =
  _Int__repr 0

_Int__one :: Int__Coq_int
_Int__one =
  _Int__repr ((\x -> x) 1)

_Int__mone :: Int__Coq_int
_Int__mone =
  _Int__repr (Prelude.negate 1)

_Int__iwordsize :: Int__Coq_int
_Int__iwordsize =
  _Int__repr _Int__zwordsize

_Int__eq_dec :: Int__Coq_int -> Int__Coq_int -> Prelude.Bool
_Int__eq_dec =
  Coqlib.zeq

_Int__eq :: Int__Coq_int -> Int__Coq_int -> Prelude.Bool
_Int__eq x y =
  case Coqlib.zeq (_Int__unsigned x) (_Int__unsigned y) of {
   Prelude.True -> Prelude.True;
   Prelude.False -> Prelude.False}

_Int__lt :: Int__Coq_int -> Int__Coq_int -> Prelude.Bool
_Int__lt x y =
  case Coqlib.zlt (_Int__signed x) (_Int__signed y) of {
   Prelude.True -> Prelude.True;
   Prelude.False -> Prelude.False}

_Int__ltu :: Int__Coq_int -> Int__Coq_int -> Prelude.Bool
_Int__ltu x y =
  case Coqlib.zlt (_Int__unsigned x) (_Int__unsigned y) of {
   Prelude.True -> Prelude.True;
   Prelude.False -> Prelude.False}

_Int__neg :: Int__Coq_int -> Int__Coq_int
_Int__neg x =
  _Int__repr (BinInt._Z__opp (_Int__unsigned x))

_Int__add :: Int__Coq_int -> Int__Coq_int -> Int__Coq_int
_Int__add x y =
  _Int__repr ((Prelude.+) (_Int__unsigned x) (_Int__unsigned y))

_Int__sub :: Int__Coq_int -> Int__Coq_int -> Int__Coq_int
_Int__sub x y =
  _Int__repr ((Prelude.-) (_Int__unsigned x) (_Int__unsigned y))

_Int__mul :: Int__Coq_int -> Int__Coq_int -> Int__Coq_int
_Int__mul x y =
  _Int__repr ((Prelude.*) (_Int__unsigned x) (_Int__unsigned y))

_Int__divs :: Int__Coq_int -> Int__Coq_int -> Int__Coq_int
_Int__divs x y =
  _Int__repr (BinInt._Z__quot (_Int__signed x) (_Int__signed y))

_Int__mods :: Int__Coq_int -> Int__Coq_int -> Int__Coq_int
_Int__mods x y =
  _Int__repr (BinInt._Z__rem (_Int__signed x) (_Int__signed y))

_Int__divu :: Int__Coq_int -> Int__Coq_int -> Int__Coq_int
_Int__divu x y =
  _Int__repr (BinInt._Z__div (_Int__unsigned x) (_Int__unsigned y))

_Int__modu :: Int__Coq_int -> Int__Coq_int -> Int__Coq_int
_Int__modu x y =
  _Int__repr (BinInt._Z__modulo (_Int__unsigned x) (_Int__unsigned y))

_Int__and :: Int__Coq_int -> Int__Coq_int -> Int__Coq_int
_Int__and x y =
  _Int__repr (BinInt._Z__land (_Int__unsigned x) (_Int__unsigned y))

_Int__or :: Int__Coq_int -> Int__Coq_int -> Int__Coq_int
_Int__or x y =
  _Int__repr (BinInt._Z__lor (_Int__unsigned x) (_Int__unsigned y))

_Int__xor :: Int__Coq_int -> Int__Coq_int -> Int__Coq_int
_Int__xor x y =
  _Int__repr (BinInt._Z__lxor (_Int__unsigned x) (_Int__unsigned y))

_Int__not :: Int__Coq_int -> Int__Coq_int
_Int__not x =
  _Int__xor x _Int__mone

_Int__shl :: Int__Coq_int -> Int__Coq_int -> Int__Coq_int
_Int__shl x y =
  _Int__repr (BinInt._Z__shiftl (_Int__unsigned x) (_Int__unsigned y))

_Int__shru :: Int__Coq_int -> Int__Coq_int -> Int__Coq_int
_Int__shru x y =
  _Int__repr (BinInt._Z__shiftr (_Int__unsigned x) (_Int__unsigned y))

_Int__shr :: Int__Coq_int -> Int__Coq_int -> Int__Coq_int
_Int__shr x y =
  _Int__repr (BinInt._Z__shiftr (_Int__signed x) (_Int__unsigned y))

_Int__rol :: Int__Coq_int -> Int__Coq_int -> Int__Coq_int
_Int__rol x y =
  let {n = BinInt._Z__modulo (_Int__unsigned y) _Int__zwordsize} in
  _Int__repr
    (BinInt._Z__lor (BinInt._Z__shiftl (_Int__unsigned x) n)
      (BinInt._Z__shiftr (_Int__unsigned x) ((Prelude.-) _Int__zwordsize n)))

_Int__ror :: Int__Coq_int -> Int__Coq_int -> Int__Coq_int
_Int__ror x y =
  let {n = BinInt._Z__modulo (_Int__unsigned y) _Int__zwordsize} in
  _Int__repr
    (BinInt._Z__lor (BinInt._Z__shiftr (_Int__unsigned x) n)
      (BinInt._Z__shiftl (_Int__unsigned x) ((Prelude.-) _Int__zwordsize n)))

_Int__rolm :: Int__Coq_int -> Int__Coq_int -> Int__Coq_int -> Int__Coq_int
_Int__rolm x a m =
  _Int__and (_Int__rol x a) m

_Int__shrx :: Int__Coq_int -> Int__Coq_int -> Int__Coq_int
_Int__shrx x y =
  _Int__divs x (_Int__shl _Int__one y)

_Int__mulhu :: Int__Coq_int -> Int__Coq_int -> Int__Coq_int
_Int__mulhu x y =
  _Int__repr
    (BinInt._Z__div ((Prelude.*) (_Int__unsigned x) (_Int__unsigned y))
      _Int__modulus)

_Int__mulhs :: Int__Coq_int -> Int__Coq_int -> Int__Coq_int
_Int__mulhs x y =
  _Int__repr
    (BinInt._Z__div ((Prelude.*) (_Int__signed x) (_Int__signed y))
      _Int__modulus)

_Int__negative :: Int__Coq_int -> Int__Coq_int
_Int__negative x =
  case _Int__lt x _Int__zero of {
   Prelude.True -> _Int__one;
   Prelude.False -> _Int__zero}

_Int__add_carry :: Int__Coq_int -> Int__Coq_int -> Int__Coq_int ->
                   Int__Coq_int
_Int__add_carry x y cin =
  case Coqlib.zlt
         ((Prelude.+) ((Prelude.+) (_Int__unsigned x) (_Int__unsigned y))
           (_Int__unsigned cin)) _Int__modulus of {
   Prelude.True -> _Int__zero;
   Prelude.False -> _Int__one}

_Int__add_overflow :: Int__Coq_int -> Int__Coq_int -> Int__Coq_int ->
                      Int__Coq_int
_Int__add_overflow x y cin =
  let {
   s = (Prelude.+) ((Prelude.+) (_Int__signed x) (_Int__signed y))
         (_Int__signed cin)}
  in
  case (Prelude.&&) ((\x -> x) (Coqlib.zle _Int__min_signed s))
         ((\x -> x) (Coqlib.zle s _Int__max_signed)) of {
   Prelude.True -> _Int__zero;
   Prelude.False -> _Int__one}

_Int__sub_borrow :: Int__Coq_int -> Int__Coq_int -> Int__Coq_int ->
                    Int__Coq_int
_Int__sub_borrow x y bin =
  case Coqlib.zlt
         ((Prelude.-) ((Prelude.-) (_Int__unsigned x) (_Int__unsigned y))
           (_Int__unsigned bin)) 0 of {
   Prelude.True -> _Int__one;
   Prelude.False -> _Int__zero}

_Int__sub_overflow :: Int__Coq_int -> Int__Coq_int -> Int__Coq_int ->
                      Int__Coq_int
_Int__sub_overflow x y bin =
  let {
   s = (Prelude.-) ((Prelude.-) (_Int__signed x) (_Int__signed y))
         (_Int__signed bin)}
  in
  case (Prelude.&&) ((\x -> x) (Coqlib.zle _Int__min_signed s))
         ((\x -> x) (Coqlib.zle s _Int__max_signed)) of {
   Prelude.True -> _Int__zero;
   Prelude.False -> _Int__one}

_Int__shr_carry :: Int__Coq_int -> Int__Coq_int -> Int__Coq_int
_Int__shr_carry x y =
  case (Prelude.&&) (_Int__lt x _Int__zero)
         (Prelude.not
           (_Int__eq
             (_Int__and x (_Int__sub (_Int__shl _Int__one y) _Int__one))
             _Int__zero)) of {
   Prelude.True -> _Int__one;
   Prelude.False -> _Int__zero}

_Int__coq_Zshiftin :: Prelude.Bool -> Prelude.Integer -> Prelude.Integer
_Int__coq_Zshiftin b x =
  case b of {
   Prelude.True -> BinInt._Z__succ_double x;
   Prelude.False -> BinInt._Z__double x}

_Int__coq_Zzero_ext :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Int__coq_Zzero_ext n x =
  BinInt._Z__iter n (\rec x0 ->
    _Int__coq_Zshiftin (BinInt._Z__odd x0) (rec (BinInt._Z__div2 x0))) (\_ ->
    0) x

_Int__coq_Zsign_ext :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Int__coq_Zsign_ext n x =
  BinInt._Z__iter (BinInt._Z__pred n) (\rec x0 ->
    _Int__coq_Zshiftin (BinInt._Z__odd x0) (rec (BinInt._Z__div2 x0)))
    (\x0 ->
    case BinInt._Z__odd x0 of {
     Prelude.True -> Prelude.negate 1;
     Prelude.False -> 0}) x

_Int__zero_ext :: Prelude.Integer -> Int__Coq_int -> Int__Coq_int
_Int__zero_ext n x =
  _Int__repr (_Int__coq_Zzero_ext n (_Int__unsigned x))

_Int__sign_ext :: Prelude.Integer -> Int__Coq_int -> Int__Coq_int
_Int__sign_ext n x =
  _Int__repr (_Int__coq_Zsign_ext n (_Int__unsigned x))

_Int__coq_Z_one_bits :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
                        -> ([]) Prelude.Integer
_Int__coq_Z_one_bits n x i =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> ([]))
    (\m ->
    case BinInt._Z__odd x of {
     Prelude.True -> (:) i
      (_Int__coq_Z_one_bits m (BinInt._Z__div2 x)
        ((Prelude.+) i ((\x -> x) 1)));
     Prelude.False ->
      _Int__coq_Z_one_bits m (BinInt._Z__div2 x)
        ((Prelude.+) i ((\x -> x) 1))})
    n

_Int__one_bits :: Int__Coq_int -> ([]) Int__Coq_int
_Int__one_bits x =
  List0.map _Int__repr
    (_Int__coq_Z_one_bits _Int__wordsize (_Int__unsigned x) 0)

_Int__is_power2 :: Int__Coq_int -> Prelude.Maybe Int__Coq_int
_Int__is_power2 x =
  case _Int__coq_Z_one_bits _Int__wordsize (_Int__unsigned x) 0 of {
   ([]) -> Prelude.Nothing;
   (:) i l ->
    case l of {
     ([]) -> Prelude.Just (_Int__repr i);
     (:) _ _ -> Prelude.Nothing}}

_Int__cmp :: Coq_comparison -> Int__Coq_int -> Int__Coq_int -> Prelude.Bool
_Int__cmp c x y =
  case c of {
   Ceq -> _Int__eq x y;
   Cne -> Prelude.not (_Int__eq x y);
   Clt -> _Int__lt x y;
   Cle -> Prelude.not (_Int__lt y x);
   Cgt -> _Int__lt y x;
   Cge -> Prelude.not (_Int__lt x y)}

_Int__cmpu :: Coq_comparison -> Int__Coq_int -> Int__Coq_int -> Prelude.Bool
_Int__cmpu c x y =
  case c of {
   Ceq -> _Int__eq x y;
   Cne -> Prelude.not (_Int__eq x y);
   Clt -> _Int__ltu x y;
   Cle -> Prelude.not (_Int__ltu y x);
   Cgt -> _Int__ltu y x;
   Cge -> Prelude.not (_Int__ltu x y)}

_Int__notbool :: Int__Coq_int -> Int__Coq_int
_Int__notbool x =
  case _Int__eq x _Int__zero of {
   Prelude.True -> _Int__one;
   Prelude.False -> _Int__zero}

_Int__divmodu2 :: Int__Coq_int -> Int__Coq_int -> Int__Coq_int ->
                  Prelude.Maybe ((,) Int__Coq_int Int__Coq_int)
_Int__divmodu2 nhi nlo d =
  case _Int__eq_dec d _Int__zero of {
   Prelude.True -> Prelude.Nothing;
   Prelude.False ->
    case BinInt._Z__div_eucl
           ((Prelude.+) ((Prelude.*) (_Int__unsigned nhi) _Int__modulus)
             (_Int__unsigned nlo)) (_Int__unsigned d) of {
     (,) q r ->
      case Coqlib.zle q _Int__max_unsigned of {
       Prelude.True -> Prelude.Just ((,) (_Int__repr q) (_Int__repr r));
       Prelude.False -> Prelude.Nothing}}}

_Int__divmods2 :: Int__Coq_int -> Int__Coq_int -> Int__Coq_int ->
                  Prelude.Maybe ((,) Int__Coq_int Int__Coq_int)
_Int__divmods2 nhi nlo d =
  case _Int__eq_dec d _Int__zero of {
   Prelude.True -> Prelude.Nothing;
   Prelude.False ->
    case BinInt._Z__quotrem
           ((Prelude.+) ((Prelude.*) (_Int__signed nhi) _Int__modulus)
             (_Int__unsigned nlo)) (_Int__signed d) of {
     (,) q r ->
      case (Prelude.&&) ((\x -> x) (Coqlib.zle _Int__min_signed q))
             ((\x -> x) (Coqlib.zle q _Int__max_signed)) of {
       Prelude.True -> Prelude.Just ((,) (_Int__repr q) (_Int__repr r));
       Prelude.False -> Prelude.Nothing}}}

_Int__testbit :: Int__Coq_int -> Prelude.Integer -> Prelude.Bool
_Int__testbit x i =
  BinInt._Z__testbit (_Int__unsigned x) i

_Int__powerserie :: (([]) Prelude.Integer) -> Prelude.Integer
_Int__powerserie l =
  case l of {
   ([]) -> 0;
   (:) x xs -> (Prelude.+) (Zpower.two_p x) (_Int__powerserie xs)}

_Int__int_of_one_bits :: (([]) Int__Coq_int) -> Int__Coq_int
_Int__int_of_one_bits l =
  case l of {
   ([]) -> _Int__zero;
   (:) a b -> _Int__add (_Int__shl _Int__one a) (_Int__int_of_one_bits b)}

_Int__no_overlap :: Int__Coq_int -> Prelude.Integer -> Int__Coq_int ->
                    Prelude.Integer -> Prelude.Bool
_Int__no_overlap ofs1 sz1 ofs2 sz2 =
  let {x1 = _Int__unsigned ofs1} in
  let {x2 = _Int__unsigned ofs2} in
  (Prelude.&&)
    ((Prelude.&&) ((\x -> x) (Coqlib.zlt ((Prelude.+) x1 sz1) _Int__modulus))
      ((\x -> x) (Coqlib.zlt ((Prelude.+) x2 sz2) _Int__modulus)))
    ((Prelude.||) ((\x -> x) (Coqlib.zle ((Prelude.+) x1 sz1) x2))
      ((\x -> x) (Coqlib.zle ((Prelude.+) x2 sz2) x1)))

_Int__coq_Zsize :: Prelude.Integer -> Prelude.Integer
_Int__coq_Zsize x =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> 0)
    (\p -> (\x -> x) (BinPos._Pos__size p))
    (\_ -> 0)
    x

_Int__size :: Int__Coq_int -> Prelude.Integer
_Int__size x =
  _Int__coq_Zsize (_Int__unsigned x)

_Wordsize_8__wordsize :: Prelude.Integer
_Wordsize_8__wordsize =
  Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ 0)))))))

_Byte__wordsize :: Prelude.Integer
_Byte__wordsize =
  _Wordsize_8__wordsize

_Byte__zwordsize :: Prelude.Integer
_Byte__zwordsize =
  BinInt._Z__of_nat _Byte__wordsize

_Byte__modulus :: Prelude.Integer
_Byte__modulus =
  Zpower.two_power_nat _Byte__wordsize

_Byte__half_modulus :: Prelude.Integer
_Byte__half_modulus =
  BinInt._Z__div _Byte__modulus ((\x -> x) ((\x -> 2 Prelude.* x) 1))

_Byte__max_unsigned :: Prelude.Integer
_Byte__max_unsigned =
  (Prelude.-) _Byte__modulus ((\x -> x) 1)

_Byte__max_signed :: Prelude.Integer
_Byte__max_signed =
  (Prelude.-) _Byte__half_modulus ((\x -> x) 1)

_Byte__min_signed :: Prelude.Integer
_Byte__min_signed =
  BinInt._Z__opp _Byte__half_modulus

type Byte__Coq_int =
  Prelude.Integer
  -- singleton inductive, whose constructor was mkint
  
_Byte__intval :: Byte__Coq_int -> Prelude.Integer
_Byte__intval i =
  i

_Byte__coq_P_mod_two_p :: Prelude.Integer -> Prelude.Integer ->
                          Prelude.Integer
_Byte__coq_P_mod_two_p p n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> 0)
    (\m ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> BinInt._Z__succ_double (_Byte__coq_P_mod_two_p q m))
      (\q -> BinInt._Z__double (_Byte__coq_P_mod_two_p q m))
      (\_ -> (\x -> x) 1)
      p)
    n

_Byte__coq_Z_mod_modulus :: Prelude.Integer -> Prelude.Integer
_Byte__coq_Z_mod_modulus x =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> 0)
    (\p -> _Byte__coq_P_mod_two_p p _Byte__wordsize)
    (\p ->
    let {r = _Byte__coq_P_mod_two_p p _Byte__wordsize} in
    case Coqlib.zeq r 0 of {
     Prelude.True -> 0;
     Prelude.False -> (Prelude.-) _Byte__modulus r})
    x

_Byte__unsigned :: Byte__Coq_int -> Prelude.Integer
_Byte__unsigned =
  _Byte__intval

_Byte__signed :: Byte__Coq_int -> Prelude.Integer
_Byte__signed n =
  let {x = _Byte__unsigned n} in
  case Coqlib.zlt x _Byte__half_modulus of {
   Prelude.True -> x;
   Prelude.False -> (Prelude.-) x _Byte__modulus}

_Byte__repr :: Prelude.Integer -> Byte__Coq_int
_Byte__repr =
  _Byte__coq_Z_mod_modulus

_Byte__zero :: Byte__Coq_int
_Byte__zero =
  _Byte__repr 0

_Byte__one :: Byte__Coq_int
_Byte__one =
  _Byte__repr ((\x -> x) 1)

_Byte__mone :: Byte__Coq_int
_Byte__mone =
  _Byte__repr (Prelude.negate 1)

_Byte__iwordsize :: Byte__Coq_int
_Byte__iwordsize =
  _Byte__repr _Byte__zwordsize

_Byte__eq_dec :: Byte__Coq_int -> Byte__Coq_int -> Prelude.Bool
_Byte__eq_dec =
  Coqlib.zeq

_Byte__eq :: Byte__Coq_int -> Byte__Coq_int -> Prelude.Bool
_Byte__eq x y =
  case Coqlib.zeq (_Byte__unsigned x) (_Byte__unsigned y) of {
   Prelude.True -> Prelude.True;
   Prelude.False -> Prelude.False}

_Byte__lt :: Byte__Coq_int -> Byte__Coq_int -> Prelude.Bool
_Byte__lt x y =
  case Coqlib.zlt (_Byte__signed x) (_Byte__signed y) of {
   Prelude.True -> Prelude.True;
   Prelude.False -> Prelude.False}

_Byte__ltu :: Byte__Coq_int -> Byte__Coq_int -> Prelude.Bool
_Byte__ltu x y =
  case Coqlib.zlt (_Byte__unsigned x) (_Byte__unsigned y) of {
   Prelude.True -> Prelude.True;
   Prelude.False -> Prelude.False}

_Byte__neg :: Byte__Coq_int -> Byte__Coq_int
_Byte__neg x =
  _Byte__repr (BinInt._Z__opp (_Byte__unsigned x))

_Byte__add :: Byte__Coq_int -> Byte__Coq_int -> Byte__Coq_int
_Byte__add x y =
  _Byte__repr ((Prelude.+) (_Byte__unsigned x) (_Byte__unsigned y))

_Byte__sub :: Byte__Coq_int -> Byte__Coq_int -> Byte__Coq_int
_Byte__sub x y =
  _Byte__repr ((Prelude.-) (_Byte__unsigned x) (_Byte__unsigned y))

_Byte__mul :: Byte__Coq_int -> Byte__Coq_int -> Byte__Coq_int
_Byte__mul x y =
  _Byte__repr ((Prelude.*) (_Byte__unsigned x) (_Byte__unsigned y))

_Byte__divs :: Byte__Coq_int -> Byte__Coq_int -> Byte__Coq_int
_Byte__divs x y =
  _Byte__repr (BinInt._Z__quot (_Byte__signed x) (_Byte__signed y))

_Byte__mods :: Byte__Coq_int -> Byte__Coq_int -> Byte__Coq_int
_Byte__mods x y =
  _Byte__repr (BinInt._Z__rem (_Byte__signed x) (_Byte__signed y))

_Byte__divu :: Byte__Coq_int -> Byte__Coq_int -> Byte__Coq_int
_Byte__divu x y =
  _Byte__repr (BinInt._Z__div (_Byte__unsigned x) (_Byte__unsigned y))

_Byte__modu :: Byte__Coq_int -> Byte__Coq_int -> Byte__Coq_int
_Byte__modu x y =
  _Byte__repr (BinInt._Z__modulo (_Byte__unsigned x) (_Byte__unsigned y))

_Byte__and :: Byte__Coq_int -> Byte__Coq_int -> Byte__Coq_int
_Byte__and x y =
  _Byte__repr (BinInt._Z__land (_Byte__unsigned x) (_Byte__unsigned y))

_Byte__or :: Byte__Coq_int -> Byte__Coq_int -> Byte__Coq_int
_Byte__or x y =
  _Byte__repr (BinInt._Z__lor (_Byte__unsigned x) (_Byte__unsigned y))

_Byte__xor :: Byte__Coq_int -> Byte__Coq_int -> Byte__Coq_int
_Byte__xor x y =
  _Byte__repr (BinInt._Z__lxor (_Byte__unsigned x) (_Byte__unsigned y))

_Byte__not :: Byte__Coq_int -> Byte__Coq_int
_Byte__not x =
  _Byte__xor x _Byte__mone

_Byte__shl :: Byte__Coq_int -> Byte__Coq_int -> Byte__Coq_int
_Byte__shl x y =
  _Byte__repr (BinInt._Z__shiftl (_Byte__unsigned x) (_Byte__unsigned y))

_Byte__shru :: Byte__Coq_int -> Byte__Coq_int -> Byte__Coq_int
_Byte__shru x y =
  _Byte__repr (BinInt._Z__shiftr (_Byte__unsigned x) (_Byte__unsigned y))

_Byte__shr :: Byte__Coq_int -> Byte__Coq_int -> Byte__Coq_int
_Byte__shr x y =
  _Byte__repr (BinInt._Z__shiftr (_Byte__signed x) (_Byte__unsigned y))

_Byte__rol :: Byte__Coq_int -> Byte__Coq_int -> Byte__Coq_int
_Byte__rol x y =
  let {n = BinInt._Z__modulo (_Byte__unsigned y) _Byte__zwordsize} in
  _Byte__repr
    (BinInt._Z__lor (BinInt._Z__shiftl (_Byte__unsigned x) n)
      (BinInt._Z__shiftr (_Byte__unsigned x)
        ((Prelude.-) _Byte__zwordsize n)))

_Byte__ror :: Byte__Coq_int -> Byte__Coq_int -> Byte__Coq_int
_Byte__ror x y =
  let {n = BinInt._Z__modulo (_Byte__unsigned y) _Byte__zwordsize} in
  _Byte__repr
    (BinInt._Z__lor (BinInt._Z__shiftr (_Byte__unsigned x) n)
      (BinInt._Z__shiftl (_Byte__unsigned x)
        ((Prelude.-) _Byte__zwordsize n)))

_Byte__rolm :: Byte__Coq_int -> Byte__Coq_int -> Byte__Coq_int ->
               Byte__Coq_int
_Byte__rolm x a m =
  _Byte__and (_Byte__rol x a) m

_Byte__shrx :: Byte__Coq_int -> Byte__Coq_int -> Byte__Coq_int
_Byte__shrx x y =
  _Byte__divs x (_Byte__shl _Byte__one y)

_Byte__mulhu :: Byte__Coq_int -> Byte__Coq_int -> Byte__Coq_int
_Byte__mulhu x y =
  _Byte__repr
    (BinInt._Z__div ((Prelude.*) (_Byte__unsigned x) (_Byte__unsigned y))
      _Byte__modulus)

_Byte__mulhs :: Byte__Coq_int -> Byte__Coq_int -> Byte__Coq_int
_Byte__mulhs x y =
  _Byte__repr
    (BinInt._Z__div ((Prelude.*) (_Byte__signed x) (_Byte__signed y))
      _Byte__modulus)

_Byte__negative :: Byte__Coq_int -> Byte__Coq_int
_Byte__negative x =
  case _Byte__lt x _Byte__zero of {
   Prelude.True -> _Byte__one;
   Prelude.False -> _Byte__zero}

_Byte__add_carry :: Byte__Coq_int -> Byte__Coq_int -> Byte__Coq_int ->
                    Byte__Coq_int
_Byte__add_carry x y cin =
  case Coqlib.zlt
         ((Prelude.+) ((Prelude.+) (_Byte__unsigned x) (_Byte__unsigned y))
           (_Byte__unsigned cin)) _Byte__modulus of {
   Prelude.True -> _Byte__zero;
   Prelude.False -> _Byte__one}

_Byte__add_overflow :: Byte__Coq_int -> Byte__Coq_int -> Byte__Coq_int ->
                       Byte__Coq_int
_Byte__add_overflow x y cin =
  let {
   s = (Prelude.+) ((Prelude.+) (_Byte__signed x) (_Byte__signed y))
         (_Byte__signed cin)}
  in
  case (Prelude.&&) ((\x -> x) (Coqlib.zle _Byte__min_signed s))
         ((\x -> x) (Coqlib.zle s _Byte__max_signed)) of {
   Prelude.True -> _Byte__zero;
   Prelude.False -> _Byte__one}

_Byte__sub_borrow :: Byte__Coq_int -> Byte__Coq_int -> Byte__Coq_int ->
                     Byte__Coq_int
_Byte__sub_borrow x y bin =
  case Coqlib.zlt
         ((Prelude.-) ((Prelude.-) (_Byte__unsigned x) (_Byte__unsigned y))
           (_Byte__unsigned bin)) 0 of {
   Prelude.True -> _Byte__one;
   Prelude.False -> _Byte__zero}

_Byte__sub_overflow :: Byte__Coq_int -> Byte__Coq_int -> Byte__Coq_int ->
                       Byte__Coq_int
_Byte__sub_overflow x y bin =
  let {
   s = (Prelude.-) ((Prelude.-) (_Byte__signed x) (_Byte__signed y))
         (_Byte__signed bin)}
  in
  case (Prelude.&&) ((\x -> x) (Coqlib.zle _Byte__min_signed s))
         ((\x -> x) (Coqlib.zle s _Byte__max_signed)) of {
   Prelude.True -> _Byte__zero;
   Prelude.False -> _Byte__one}

_Byte__shr_carry :: Byte__Coq_int -> Byte__Coq_int -> Byte__Coq_int
_Byte__shr_carry x y =
  case (Prelude.&&) (_Byte__lt x _Byte__zero)
         (Prelude.not
           (_Byte__eq
             (_Byte__and x (_Byte__sub (_Byte__shl _Byte__one y) _Byte__one))
             _Byte__zero)) of {
   Prelude.True -> _Byte__one;
   Prelude.False -> _Byte__zero}

_Byte__coq_Zshiftin :: Prelude.Bool -> Prelude.Integer -> Prelude.Integer
_Byte__coq_Zshiftin b x =
  case b of {
   Prelude.True -> BinInt._Z__succ_double x;
   Prelude.False -> BinInt._Z__double x}

_Byte__coq_Zzero_ext :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Byte__coq_Zzero_ext n x =
  BinInt._Z__iter n (\rec x0 ->
    _Byte__coq_Zshiftin (BinInt._Z__odd x0) (rec (BinInt._Z__div2 x0)))
    (\_ -> 0) x

_Byte__coq_Zsign_ext :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_Byte__coq_Zsign_ext n x =
  BinInt._Z__iter (BinInt._Z__pred n) (\rec x0 ->
    _Byte__coq_Zshiftin (BinInt._Z__odd x0) (rec (BinInt._Z__div2 x0)))
    (\x0 ->
    case BinInt._Z__odd x0 of {
     Prelude.True -> Prelude.negate 1;
     Prelude.False -> 0}) x

_Byte__zero_ext :: Prelude.Integer -> Byte__Coq_int -> Byte__Coq_int
_Byte__zero_ext n x =
  _Byte__repr (_Byte__coq_Zzero_ext n (_Byte__unsigned x))

_Byte__sign_ext :: Prelude.Integer -> Byte__Coq_int -> Byte__Coq_int
_Byte__sign_ext n x =
  _Byte__repr (_Byte__coq_Zsign_ext n (_Byte__unsigned x))

_Byte__coq_Z_one_bits :: Prelude.Integer -> Prelude.Integer ->
                         Prelude.Integer -> ([]) Prelude.Integer
_Byte__coq_Z_one_bits n x i =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> ([]))
    (\m ->
    case BinInt._Z__odd x of {
     Prelude.True -> (:) i
      (_Byte__coq_Z_one_bits m (BinInt._Z__div2 x)
        ((Prelude.+) i ((\x -> x) 1)));
     Prelude.False ->
      _Byte__coq_Z_one_bits m (BinInt._Z__div2 x)
        ((Prelude.+) i ((\x -> x) 1))})
    n

_Byte__one_bits :: Byte__Coq_int -> ([]) Byte__Coq_int
_Byte__one_bits x =
  List0.map _Byte__repr
    (_Byte__coq_Z_one_bits _Byte__wordsize (_Byte__unsigned x) 0)

_Byte__is_power2 :: Byte__Coq_int -> Prelude.Maybe Byte__Coq_int
_Byte__is_power2 x =
  case _Byte__coq_Z_one_bits _Byte__wordsize (_Byte__unsigned x) 0 of {
   ([]) -> Prelude.Nothing;
   (:) i l ->
    case l of {
     ([]) -> Prelude.Just (_Byte__repr i);
     (:) _ _ -> Prelude.Nothing}}

_Byte__cmp :: Coq_comparison -> Byte__Coq_int -> Byte__Coq_int ->
              Prelude.Bool
_Byte__cmp c x y =
  case c of {
   Ceq -> _Byte__eq x y;
   Cne -> Prelude.not (_Byte__eq x y);
   Clt -> _Byte__lt x y;
   Cle -> Prelude.not (_Byte__lt y x);
   Cgt -> _Byte__lt y x;
   Cge -> Prelude.not (_Byte__lt x y)}

_Byte__cmpu :: Coq_comparison -> Byte__Coq_int -> Byte__Coq_int ->
               Prelude.Bool
_Byte__cmpu c x y =
  case c of {
   Ceq -> _Byte__eq x y;
   Cne -> Prelude.not (_Byte__eq x y);
   Clt -> _Byte__ltu x y;
   Cle -> Prelude.not (_Byte__ltu y x);
   Cgt -> _Byte__ltu y x;
   Cge -> Prelude.not (_Byte__ltu x y)}

_Byte__notbool :: Byte__Coq_int -> Byte__Coq_int
_Byte__notbool x =
  case _Byte__eq x _Byte__zero of {
   Prelude.True -> _Byte__one;
   Prelude.False -> _Byte__zero}

_Byte__divmodu2 :: Byte__Coq_int -> Byte__Coq_int -> Byte__Coq_int ->
                   Prelude.Maybe ((,) Byte__Coq_int Byte__Coq_int)
_Byte__divmodu2 nhi nlo d =
  case _Byte__eq_dec d _Byte__zero of {
   Prelude.True -> Prelude.Nothing;
   Prelude.False ->
    case BinInt._Z__div_eucl
           ((Prelude.+) ((Prelude.*) (_Byte__unsigned nhi) _Byte__modulus)
             (_Byte__unsigned nlo)) (_Byte__unsigned d) of {
     (,) q r ->
      case Coqlib.zle q _Byte__max_unsigned of {
       Prelude.True -> Prelude.Just ((,) (_Byte__repr q) (_Byte__repr r));
       Prelude.False -> Prelude.Nothing}}}

_Byte__divmods2 :: Byte__Coq_int -> Byte__Coq_int -> Byte__Coq_int ->
                   Prelude.Maybe ((,) Byte__Coq_int Byte__Coq_int)
_Byte__divmods2 nhi nlo d =
  case _Byte__eq_dec d _Byte__zero of {
   Prelude.True -> Prelude.Nothing;
   Prelude.False ->
    case BinInt._Z__quotrem
           ((Prelude.+) ((Prelude.*) (_Byte__signed nhi) _Byte__modulus)
             (_Byte__unsigned nlo)) (_Byte__signed d) of {
     (,) q r ->
      case (Prelude.&&) ((\x -> x) (Coqlib.zle _Byte__min_signed q))
             ((\x -> x) (Coqlib.zle q _Byte__max_signed)) of {
       Prelude.True -> Prelude.Just ((,) (_Byte__repr q) (_Byte__repr r));
       Prelude.False -> Prelude.Nothing}}}

_Byte__testbit :: Byte__Coq_int -> Prelude.Integer -> Prelude.Bool
_Byte__testbit x i =
  BinInt._Z__testbit (_Byte__unsigned x) i

_Byte__powerserie :: (([]) Prelude.Integer) -> Prelude.Integer
_Byte__powerserie l =
  case l of {
   ([]) -> 0;
   (:) x xs -> (Prelude.+) (Zpower.two_p x) (_Byte__powerserie xs)}

_Byte__int_of_one_bits :: (([]) Byte__Coq_int) -> Byte__Coq_int
_Byte__int_of_one_bits l =
  case l of {
   ([]) -> _Byte__zero;
   (:) a b -> _Byte__add (_Byte__shl _Byte__one a) (_Byte__int_of_one_bits b)}

_Byte__no_overlap :: Byte__Coq_int -> Prelude.Integer -> Byte__Coq_int ->
                     Prelude.Integer -> Prelude.Bool
_Byte__no_overlap ofs1 sz1 ofs2 sz2 =
  let {x1 = _Byte__unsigned ofs1} in
  let {x2 = _Byte__unsigned ofs2} in
  (Prelude.&&)
    ((Prelude.&&)
      ((\x -> x) (Coqlib.zlt ((Prelude.+) x1 sz1) _Byte__modulus))
      ((\x -> x) (Coqlib.zlt ((Prelude.+) x2 sz2) _Byte__modulus)))
    ((Prelude.||) ((\x -> x) (Coqlib.zle ((Prelude.+) x1 sz1) x2))
      ((\x -> x) (Coqlib.zle ((Prelude.+) x2 sz2) x1)))

_Byte__coq_Zsize :: Prelude.Integer -> Prelude.Integer
_Byte__coq_Zsize x =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> 0)
    (\p -> (\x -> x) (BinPos._Pos__size p))
    (\_ -> 0)
    x

_Byte__size :: Byte__Coq_int -> Prelude.Integer
_Byte__size x =
  _Byte__coq_Zsize (_Byte__unsigned x)

_Wordsize_64__wordsize :: Prelude.Integer
_Wordsize_64__wordsize =
  Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
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
    0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

_Int64__wordsize :: Prelude.Integer
_Int64__wordsize =
  _Wordsize_64__wordsize

_Int64__zwordsize :: Prelude.Integer
_Int64__zwordsize =
  BinInt._Z__of_nat _Int64__wordsize

_Int64__modulus :: Prelude.Integer
_Int64__modulus =
  Zpower.two_power_nat _Int64__wordsize

_Int64__half_modulus :: Prelude.Integer
_Int64__half_modulus =
  BinInt._Z__div _Int64__modulus ((\x -> x) ((\x -> 2 Prelude.* x) 1))

_Int64__max_unsigned :: Prelude.Integer
_Int64__max_unsigned =
  (Prelude.-) _Int64__modulus ((\x -> x) 1)

_Int64__max_signed :: Prelude.Integer
_Int64__max_signed =
  (Prelude.-) _Int64__half_modulus ((\x -> x) 1)

_Int64__min_signed :: Prelude.Integer
_Int64__min_signed =
  BinInt._Z__opp _Int64__half_modulus

type Int64__Coq_int =
  Prelude.Integer
  -- singleton inductive, whose constructor was mkint
  
_Int64__intval :: Int64__Coq_int -> Prelude.Integer
_Int64__intval i =
  i

_Int64__coq_P_mod_two_p :: Prelude.Integer -> Prelude.Integer ->
                           Prelude.Integer
_Int64__coq_P_mod_two_p p n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> 0)
    (\m ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> BinInt._Z__succ_double (_Int64__coq_P_mod_two_p q m))
      (\q -> BinInt._Z__double (_Int64__coq_P_mod_two_p q m))
      (\_ -> (\x -> x) 1)
      p)
    n

_Int64__coq_Z_mod_modulus :: Prelude.Integer -> Prelude.Integer
_Int64__coq_Z_mod_modulus x =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> 0)
    (\p -> _Int64__coq_P_mod_two_p p _Int64__wordsize)
    (\p ->
    let {r = _Int64__coq_P_mod_two_p p _Int64__wordsize} in
    case Coqlib.zeq r 0 of {
     Prelude.True -> 0;
     Prelude.False -> (Prelude.-) _Int64__modulus r})
    x

_Int64__unsigned :: Int64__Coq_int -> Prelude.Integer
_Int64__unsigned =
  _Int64__intval

_Int64__signed :: Int64__Coq_int -> Prelude.Integer
_Int64__signed n =
  let {x = _Int64__unsigned n} in
  case Coqlib.zlt x _Int64__half_modulus of {
   Prelude.True -> x;
   Prelude.False -> (Prelude.-) x _Int64__modulus}

_Int64__repr :: Prelude.Integer -> Int64__Coq_int
_Int64__repr =
  _Int64__coq_Z_mod_modulus

_Int64__zero :: Int64__Coq_int
_Int64__zero =
  _Int64__repr 0

_Int64__one :: Int64__Coq_int
_Int64__one =
  _Int64__repr ((\x -> x) 1)

_Int64__mone :: Int64__Coq_int
_Int64__mone =
  _Int64__repr (Prelude.negate 1)

_Int64__iwordsize :: Int64__Coq_int
_Int64__iwordsize =
  _Int64__repr _Int64__zwordsize

_Int64__eq_dec :: Int64__Coq_int -> Int64__Coq_int -> Prelude.Bool
_Int64__eq_dec =
  Coqlib.zeq

_Int64__eq :: Int64__Coq_int -> Int64__Coq_int -> Prelude.Bool
_Int64__eq x y =
  case Coqlib.zeq (_Int64__unsigned x) (_Int64__unsigned y) of {
   Prelude.True -> Prelude.True;
   Prelude.False -> Prelude.False}

_Int64__lt :: Int64__Coq_int -> Int64__Coq_int -> Prelude.Bool
_Int64__lt x y =
  case Coqlib.zlt (_Int64__signed x) (_Int64__signed y) of {
   Prelude.True -> Prelude.True;
   Prelude.False -> Prelude.False}

_Int64__ltu :: Int64__Coq_int -> Int64__Coq_int -> Prelude.Bool
_Int64__ltu x y =
  case Coqlib.zlt (_Int64__unsigned x) (_Int64__unsigned y) of {
   Prelude.True -> Prelude.True;
   Prelude.False -> Prelude.False}

_Int64__neg :: Int64__Coq_int -> Int64__Coq_int
_Int64__neg x =
  _Int64__repr (BinInt._Z__opp (_Int64__unsigned x))

_Int64__add :: Int64__Coq_int -> Int64__Coq_int -> Int64__Coq_int
_Int64__add x y =
  _Int64__repr ((Prelude.+) (_Int64__unsigned x) (_Int64__unsigned y))

_Int64__sub :: Int64__Coq_int -> Int64__Coq_int -> Int64__Coq_int
_Int64__sub x y =
  _Int64__repr ((Prelude.-) (_Int64__unsigned x) (_Int64__unsigned y))

_Int64__mul :: Int64__Coq_int -> Int64__Coq_int -> Int64__Coq_int
_Int64__mul x y =
  _Int64__repr ((Prelude.*) (_Int64__unsigned x) (_Int64__unsigned y))

_Int64__divs :: Int64__Coq_int -> Int64__Coq_int -> Int64__Coq_int
_Int64__divs x y =
  _Int64__repr (BinInt._Z__quot (_Int64__signed x) (_Int64__signed y))

_Int64__mods :: Int64__Coq_int -> Int64__Coq_int -> Int64__Coq_int
_Int64__mods x y =
  _Int64__repr (BinInt._Z__rem (_Int64__signed x) (_Int64__signed y))

_Int64__divu :: Int64__Coq_int -> Int64__Coq_int -> Int64__Coq_int
_Int64__divu x y =
  _Int64__repr (BinInt._Z__div (_Int64__unsigned x) (_Int64__unsigned y))

_Int64__modu :: Int64__Coq_int -> Int64__Coq_int -> Int64__Coq_int
_Int64__modu x y =
  _Int64__repr (BinInt._Z__modulo (_Int64__unsigned x) (_Int64__unsigned y))

_Int64__and :: Int64__Coq_int -> Int64__Coq_int -> Int64__Coq_int
_Int64__and x y =
  _Int64__repr (BinInt._Z__land (_Int64__unsigned x) (_Int64__unsigned y))

_Int64__or :: Int64__Coq_int -> Int64__Coq_int -> Int64__Coq_int
_Int64__or x y =
  _Int64__repr (BinInt._Z__lor (_Int64__unsigned x) (_Int64__unsigned y))

_Int64__xor :: Int64__Coq_int -> Int64__Coq_int -> Int64__Coq_int
_Int64__xor x y =
  _Int64__repr (BinInt._Z__lxor (_Int64__unsigned x) (_Int64__unsigned y))

_Int64__not :: Int64__Coq_int -> Int64__Coq_int
_Int64__not x =
  _Int64__xor x _Int64__mone

_Int64__shl :: Int64__Coq_int -> Int64__Coq_int -> Int64__Coq_int
_Int64__shl x y =
  _Int64__repr (BinInt._Z__shiftl (_Int64__unsigned x) (_Int64__unsigned y))

_Int64__shru :: Int64__Coq_int -> Int64__Coq_int -> Int64__Coq_int
_Int64__shru x y =
  _Int64__repr (BinInt._Z__shiftr (_Int64__unsigned x) (_Int64__unsigned y))

_Int64__shr :: Int64__Coq_int -> Int64__Coq_int -> Int64__Coq_int
_Int64__shr x y =
  _Int64__repr (BinInt._Z__shiftr (_Int64__signed x) (_Int64__unsigned y))

_Int64__rol :: Int64__Coq_int -> Int64__Coq_int -> Int64__Coq_int
_Int64__rol x y =
  let {n = BinInt._Z__modulo (_Int64__unsigned y) _Int64__zwordsize} in
  _Int64__repr
    (BinInt._Z__lor (BinInt._Z__shiftl (_Int64__unsigned x) n)
      (BinInt._Z__shiftr (_Int64__unsigned x)
        ((Prelude.-) _Int64__zwordsize n)))

_Int64__ror :: Int64__Coq_int -> Int64__Coq_int -> Int64__Coq_int
_Int64__ror x y =
  let {n = BinInt._Z__modulo (_Int64__unsigned y) _Int64__zwordsize} in
  _Int64__repr
    (BinInt._Z__lor (BinInt._Z__shiftr (_Int64__unsigned x) n)
      (BinInt._Z__shiftl (_Int64__unsigned x)
        ((Prelude.-) _Int64__zwordsize n)))

_Int64__rolm :: Int64__Coq_int -> Int64__Coq_int -> Int64__Coq_int ->
                Int64__Coq_int
_Int64__rolm x a m =
  _Int64__and (_Int64__rol x a) m

_Int64__shrx :: Int64__Coq_int -> Int64__Coq_int -> Int64__Coq_int
_Int64__shrx x y =
  _Int64__divs x (_Int64__shl _Int64__one y)

_Int64__mulhu :: Int64__Coq_int -> Int64__Coq_int -> Int64__Coq_int
_Int64__mulhu x y =
  _Int64__repr
    (BinInt._Z__div ((Prelude.*) (_Int64__unsigned x) (_Int64__unsigned y))
      _Int64__modulus)

_Int64__mulhs :: Int64__Coq_int -> Int64__Coq_int -> Int64__Coq_int
_Int64__mulhs x y =
  _Int64__repr
    (BinInt._Z__div ((Prelude.*) (_Int64__signed x) (_Int64__signed y))
      _Int64__modulus)

_Int64__negative :: Int64__Coq_int -> Int64__Coq_int
_Int64__negative x =
  case _Int64__lt x _Int64__zero of {
   Prelude.True -> _Int64__one;
   Prelude.False -> _Int64__zero}

_Int64__add_carry :: Int64__Coq_int -> Int64__Coq_int -> Int64__Coq_int ->
                     Int64__Coq_int
_Int64__add_carry x y cin =
  case Coqlib.zlt
         ((Prelude.+) ((Prelude.+) (_Int64__unsigned x) (_Int64__unsigned y))
           (_Int64__unsigned cin)) _Int64__modulus of {
   Prelude.True -> _Int64__zero;
   Prelude.False -> _Int64__one}

_Int64__add_overflow :: Int64__Coq_int -> Int64__Coq_int -> Int64__Coq_int ->
                        Int64__Coq_int
_Int64__add_overflow x y cin =
  let {
   s = (Prelude.+) ((Prelude.+) (_Int64__signed x) (_Int64__signed y))
         (_Int64__signed cin)}
  in
  case (Prelude.&&) ((\x -> x) (Coqlib.zle _Int64__min_signed s))
         ((\x -> x) (Coqlib.zle s _Int64__max_signed)) of {
   Prelude.True -> _Int64__zero;
   Prelude.False -> _Int64__one}

_Int64__sub_borrow :: Int64__Coq_int -> Int64__Coq_int -> Int64__Coq_int ->
                      Int64__Coq_int
_Int64__sub_borrow x y bin =
  case Coqlib.zlt
         ((Prelude.-) ((Prelude.-) (_Int64__unsigned x) (_Int64__unsigned y))
           (_Int64__unsigned bin)) 0 of {
   Prelude.True -> _Int64__one;
   Prelude.False -> _Int64__zero}

_Int64__sub_overflow :: Int64__Coq_int -> Int64__Coq_int -> Int64__Coq_int ->
                        Int64__Coq_int
_Int64__sub_overflow x y bin =
  let {
   s = (Prelude.-) ((Prelude.-) (_Int64__signed x) (_Int64__signed y))
         (_Int64__signed bin)}
  in
  case (Prelude.&&) ((\x -> x) (Coqlib.zle _Int64__min_signed s))
         ((\x -> x) (Coqlib.zle s _Int64__max_signed)) of {
   Prelude.True -> _Int64__zero;
   Prelude.False -> _Int64__one}

_Int64__shr_carry :: Int64__Coq_int -> Int64__Coq_int -> Int64__Coq_int
_Int64__shr_carry x y =
  case (Prelude.&&) (_Int64__lt x _Int64__zero)
         (Prelude.not
           (_Int64__eq
             (_Int64__and x
               (_Int64__sub (_Int64__shl _Int64__one y) _Int64__one))
             _Int64__zero)) of {
   Prelude.True -> _Int64__one;
   Prelude.False -> _Int64__zero}

_Int64__coq_Zshiftin :: Prelude.Bool -> Prelude.Integer -> Prelude.Integer
_Int64__coq_Zshiftin b x =
  case b of {
   Prelude.True -> BinInt._Z__succ_double x;
   Prelude.False -> BinInt._Z__double x}

_Int64__coq_Zzero_ext :: Prelude.Integer -> Prelude.Integer ->
                         Prelude.Integer
_Int64__coq_Zzero_ext n x =
  BinInt._Z__iter n (\rec x0 ->
    _Int64__coq_Zshiftin (BinInt._Z__odd x0) (rec (BinInt._Z__div2 x0)))
    (\_ -> 0) x

_Int64__coq_Zsign_ext :: Prelude.Integer -> Prelude.Integer ->
                         Prelude.Integer
_Int64__coq_Zsign_ext n x =
  BinInt._Z__iter (BinInt._Z__pred n) (\rec x0 ->
    _Int64__coq_Zshiftin (BinInt._Z__odd x0) (rec (BinInt._Z__div2 x0)))
    (\x0 ->
    case BinInt._Z__odd x0 of {
     Prelude.True -> Prelude.negate 1;
     Prelude.False -> 0}) x

_Int64__zero_ext :: Prelude.Integer -> Int64__Coq_int -> Int64__Coq_int
_Int64__zero_ext n x =
  _Int64__repr (_Int64__coq_Zzero_ext n (_Int64__unsigned x))

_Int64__sign_ext :: Prelude.Integer -> Int64__Coq_int -> Int64__Coq_int
_Int64__sign_ext n x =
  _Int64__repr (_Int64__coq_Zsign_ext n (_Int64__unsigned x))

_Int64__coq_Z_one_bits :: Prelude.Integer -> Prelude.Integer ->
                          Prelude.Integer -> ([]) Prelude.Integer
_Int64__coq_Z_one_bits n x i =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> ([]))
    (\m ->
    case BinInt._Z__odd x of {
     Prelude.True -> (:) i
      (_Int64__coq_Z_one_bits m (BinInt._Z__div2 x)
        ((Prelude.+) i ((\x -> x) 1)));
     Prelude.False ->
      _Int64__coq_Z_one_bits m (BinInt._Z__div2 x)
        ((Prelude.+) i ((\x -> x) 1))})
    n

_Int64__one_bits :: Int64__Coq_int -> ([]) Int64__Coq_int
_Int64__one_bits x =
  List0.map _Int64__repr
    (_Int64__coq_Z_one_bits _Int64__wordsize (_Int64__unsigned x) 0)

_Int64__is_power2 :: Int64__Coq_int -> Prelude.Maybe Int64__Coq_int
_Int64__is_power2 x =
  case _Int64__coq_Z_one_bits _Int64__wordsize (_Int64__unsigned x) 0 of {
   ([]) -> Prelude.Nothing;
   (:) i l ->
    case l of {
     ([]) -> Prelude.Just (_Int64__repr i);
     (:) _ _ -> Prelude.Nothing}}

_Int64__cmp :: Coq_comparison -> Int64__Coq_int -> Int64__Coq_int ->
               Prelude.Bool
_Int64__cmp c x y =
  case c of {
   Ceq -> _Int64__eq x y;
   Cne -> Prelude.not (_Int64__eq x y);
   Clt -> _Int64__lt x y;
   Cle -> Prelude.not (_Int64__lt y x);
   Cgt -> _Int64__lt y x;
   Cge -> Prelude.not (_Int64__lt x y)}

_Int64__cmpu :: Coq_comparison -> Int64__Coq_int -> Int64__Coq_int ->
                Prelude.Bool
_Int64__cmpu c x y =
  case c of {
   Ceq -> _Int64__eq x y;
   Cne -> Prelude.not (_Int64__eq x y);
   Clt -> _Int64__ltu x y;
   Cle -> Prelude.not (_Int64__ltu y x);
   Cgt -> _Int64__ltu y x;
   Cge -> Prelude.not (_Int64__ltu x y)}

_Int64__notbool :: Int64__Coq_int -> Int64__Coq_int
_Int64__notbool x =
  case _Int64__eq x _Int64__zero of {
   Prelude.True -> _Int64__one;
   Prelude.False -> _Int64__zero}

_Int64__divmodu2 :: Int64__Coq_int -> Int64__Coq_int -> Int64__Coq_int ->
                    Prelude.Maybe ((,) Int64__Coq_int Int64__Coq_int)
_Int64__divmodu2 nhi nlo d =
  case _Int64__eq_dec d _Int64__zero of {
   Prelude.True -> Prelude.Nothing;
   Prelude.False ->
    case BinInt._Z__div_eucl
           ((Prelude.+) ((Prelude.*) (_Int64__unsigned nhi) _Int64__modulus)
             (_Int64__unsigned nlo)) (_Int64__unsigned d) of {
     (,) q r ->
      case Coqlib.zle q _Int64__max_unsigned of {
       Prelude.True -> Prelude.Just ((,) (_Int64__repr q) (_Int64__repr r));
       Prelude.False -> Prelude.Nothing}}}

_Int64__divmods2 :: Int64__Coq_int -> Int64__Coq_int -> Int64__Coq_int ->
                    Prelude.Maybe ((,) Int64__Coq_int Int64__Coq_int)
_Int64__divmods2 nhi nlo d =
  case _Int64__eq_dec d _Int64__zero of {
   Prelude.True -> Prelude.Nothing;
   Prelude.False ->
    case BinInt._Z__quotrem
           ((Prelude.+) ((Prelude.*) (_Int64__signed nhi) _Int64__modulus)
             (_Int64__unsigned nlo)) (_Int64__signed d) of {
     (,) q r ->
      case (Prelude.&&) ((\x -> x) (Coqlib.zle _Int64__min_signed q))
             ((\x -> x) (Coqlib.zle q _Int64__max_signed)) of {
       Prelude.True -> Prelude.Just ((,) (_Int64__repr q) (_Int64__repr r));
       Prelude.False -> Prelude.Nothing}}}

_Int64__testbit :: Int64__Coq_int -> Prelude.Integer -> Prelude.Bool
_Int64__testbit x i =
  BinInt._Z__testbit (_Int64__unsigned x) i

_Int64__powerserie :: (([]) Prelude.Integer) -> Prelude.Integer
_Int64__powerserie l =
  case l of {
   ([]) -> 0;
   (:) x xs -> (Prelude.+) (Zpower.two_p x) (_Int64__powerserie xs)}

_Int64__int_of_one_bits :: (([]) Int64__Coq_int) -> Int64__Coq_int
_Int64__int_of_one_bits l =
  case l of {
   ([]) -> _Int64__zero;
   (:) a b ->
    _Int64__add (_Int64__shl _Int64__one a) (_Int64__int_of_one_bits b)}

_Int64__no_overlap :: Int64__Coq_int -> Prelude.Integer -> Int64__Coq_int ->
                      Prelude.Integer -> Prelude.Bool
_Int64__no_overlap ofs1 sz1 ofs2 sz2 =
  let {x1 = _Int64__unsigned ofs1} in
  let {x2 = _Int64__unsigned ofs2} in
  (Prelude.&&)
    ((Prelude.&&)
      ((\x -> x) (Coqlib.zlt ((Prelude.+) x1 sz1) _Int64__modulus))
      ((\x -> x) (Coqlib.zlt ((Prelude.+) x2 sz2) _Int64__modulus)))
    ((Prelude.||) ((\x -> x) (Coqlib.zle ((Prelude.+) x1 sz1) x2))
      ((\x -> x) (Coqlib.zle ((Prelude.+) x2 sz2) x1)))

_Int64__coq_Zsize :: Prelude.Integer -> Prelude.Integer
_Int64__coq_Zsize x =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> 0)
    (\p -> (\x -> x) (BinPos._Pos__size p))
    (\_ -> 0)
    x

_Int64__size :: Int64__Coq_int -> Prelude.Integer
_Int64__size x =
  _Int64__coq_Zsize (_Int64__unsigned x)

_Int64__iwordsize' :: Int__Coq_int
_Int64__iwordsize' =
  _Int__repr _Int64__zwordsize

_Int64__shl' :: Int64__Coq_int -> Int__Coq_int -> Int64__Coq_int
_Int64__shl' x y =
  _Int64__repr (BinInt._Z__shiftl (_Int64__unsigned x) (_Int__unsigned y))

_Int64__shru' :: Int64__Coq_int -> Int__Coq_int -> Int64__Coq_int
_Int64__shru' x y =
  _Int64__repr (BinInt._Z__shiftr (_Int64__unsigned x) (_Int__unsigned y))

_Int64__shr' :: Int64__Coq_int -> Int__Coq_int -> Int64__Coq_int
_Int64__shr' x y =
  _Int64__repr (BinInt._Z__shiftr (_Int64__signed x) (_Int__unsigned y))

_Int64__rol' :: Int64__Coq_int -> Int__Coq_int -> Int64__Coq_int
_Int64__rol' x y =
  _Int64__rol x (_Int64__repr (_Int__unsigned y))

_Int64__shrx' :: Int64__Coq_int -> Int__Coq_int -> Int64__Coq_int
_Int64__shrx' x y =
  _Int64__divs x (_Int64__shl' _Int64__one y)

_Int64__shr_carry' :: Int64__Coq_int -> Int__Coq_int -> Int64__Coq_int
_Int64__shr_carry' x y =
  case (Prelude.&&) (_Int64__lt x _Int64__zero)
         (Prelude.not
           (_Int64__eq
             (_Int64__and x
               (_Int64__sub (_Int64__shl' _Int64__one y) _Int64__one))
             _Int64__zero)) of {
   Prelude.True -> _Int64__one;
   Prelude.False -> _Int64__zero}

_Int64__one_bits' :: Int64__Coq_int -> ([]) Int__Coq_int
_Int64__one_bits' x =
  List0.map _Int__repr
    (_Int64__coq_Z_one_bits _Int64__wordsize (_Int64__unsigned x) 0)

_Int64__is_power2' :: Int64__Coq_int -> Prelude.Maybe Int__Coq_int
_Int64__is_power2' x =
  case _Int64__coq_Z_one_bits _Int64__wordsize (_Int64__unsigned x) 0 of {
   ([]) -> Prelude.Nothing;
   (:) i l ->
    case l of {
     ([]) -> Prelude.Just (_Int__repr i);
     (:) _ _ -> Prelude.Nothing}}

_Int64__int_of_one_bits' :: (([]) Int__Coq_int) -> Int64__Coq_int
_Int64__int_of_one_bits' l =
  case l of {
   ([]) -> _Int64__zero;
   (:) a b ->
    _Int64__add (_Int64__shl' _Int64__one a) (_Int64__int_of_one_bits' b)}

_Int64__loword :: Int64__Coq_int -> Int__Coq_int
_Int64__loword n =
  _Int__repr (_Int64__unsigned n)

_Int64__hiword :: Int64__Coq_int -> Int__Coq_int
_Int64__hiword n =
  _Int__repr
    (_Int64__unsigned (_Int64__shru n (_Int64__repr _Int__zwordsize)))

_Int64__ofwords :: Int__Coq_int -> Int__Coq_int -> Int64__Coq_int
_Int64__ofwords hi lo =
  _Int64__or
    (_Int64__shl (_Int64__repr (_Int__unsigned hi))
      (_Int64__repr _Int__zwordsize)) (_Int64__repr (_Int__unsigned lo))

_Int64__mul' :: Int__Coq_int -> Int__Coq_int -> Int64__Coq_int
_Int64__mul' x y =
  _Int64__repr ((Prelude.*) (_Int__unsigned x) (_Int__unsigned y))

_Wordsize_Ptrofs__wordsize :: Prelude.Integer
_Wordsize_Ptrofs__wordsize =
  case Archi.ptr64 of {
   Prelude.True -> Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
   Prelude.False -> Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ
    0)))))))))))))))))))))))))))))))}

_Ptrofs__wordsize :: Prelude.Integer
_Ptrofs__wordsize =
  _Wordsize_Ptrofs__wordsize

_Ptrofs__zwordsize :: Prelude.Integer
_Ptrofs__zwordsize =
  BinInt._Z__of_nat _Ptrofs__wordsize

_Ptrofs__modulus :: Prelude.Integer
_Ptrofs__modulus =
  Zpower.two_power_nat _Ptrofs__wordsize

_Ptrofs__half_modulus :: Prelude.Integer
_Ptrofs__half_modulus =
  BinInt._Z__div _Ptrofs__modulus ((\x -> x) ((\x -> 2 Prelude.* x) 1))

_Ptrofs__max_unsigned :: Prelude.Integer
_Ptrofs__max_unsigned =
  (Prelude.-) _Ptrofs__modulus ((\x -> x) 1)

_Ptrofs__max_signed :: Prelude.Integer
_Ptrofs__max_signed =
  (Prelude.-) _Ptrofs__half_modulus ((\x -> x) 1)

_Ptrofs__min_signed :: Prelude.Integer
_Ptrofs__min_signed =
  BinInt._Z__opp _Ptrofs__half_modulus

type Ptrofs__Coq_int =
  Prelude.Integer
  -- singleton inductive, whose constructor was mkint
  
_Ptrofs__intval :: Ptrofs__Coq_int -> Prelude.Integer
_Ptrofs__intval i =
  i

_Ptrofs__coq_P_mod_two_p :: Prelude.Integer -> Prelude.Integer ->
                            Prelude.Integer
_Ptrofs__coq_P_mod_two_p p n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> 0)
    (\m ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\q -> BinInt._Z__succ_double (_Ptrofs__coq_P_mod_two_p q m))
      (\q -> BinInt._Z__double (_Ptrofs__coq_P_mod_two_p q m))
      (\_ -> (\x -> x) 1)
      p)
    n

_Ptrofs__coq_Z_mod_modulus :: Prelude.Integer -> Prelude.Integer
_Ptrofs__coq_Z_mod_modulus x =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> 0)
    (\p -> _Ptrofs__coq_P_mod_two_p p _Ptrofs__wordsize)
    (\p ->
    let {r = _Ptrofs__coq_P_mod_two_p p _Ptrofs__wordsize} in
    case Coqlib.zeq r 0 of {
     Prelude.True -> 0;
     Prelude.False -> (Prelude.-) _Ptrofs__modulus r})
    x

_Ptrofs__unsigned :: Ptrofs__Coq_int -> Prelude.Integer
_Ptrofs__unsigned =
  _Ptrofs__intval

_Ptrofs__signed :: Ptrofs__Coq_int -> Prelude.Integer
_Ptrofs__signed n =
  let {x = _Ptrofs__unsigned n} in
  case Coqlib.zlt x _Ptrofs__half_modulus of {
   Prelude.True -> x;
   Prelude.False -> (Prelude.-) x _Ptrofs__modulus}

_Ptrofs__repr :: Prelude.Integer -> Ptrofs__Coq_int
_Ptrofs__repr =
  _Ptrofs__coq_Z_mod_modulus

_Ptrofs__zero :: Ptrofs__Coq_int
_Ptrofs__zero =
  _Ptrofs__repr 0

_Ptrofs__one :: Ptrofs__Coq_int
_Ptrofs__one =
  _Ptrofs__repr ((\x -> x) 1)

_Ptrofs__mone :: Ptrofs__Coq_int
_Ptrofs__mone =
  _Ptrofs__repr (Prelude.negate 1)

_Ptrofs__iwordsize :: Ptrofs__Coq_int
_Ptrofs__iwordsize =
  _Ptrofs__repr _Ptrofs__zwordsize

_Ptrofs__eq_dec :: Ptrofs__Coq_int -> Ptrofs__Coq_int -> Prelude.Bool
_Ptrofs__eq_dec =
  Coqlib.zeq

_Ptrofs__eq :: Ptrofs__Coq_int -> Ptrofs__Coq_int -> Prelude.Bool
_Ptrofs__eq x y =
  case Coqlib.zeq (_Ptrofs__unsigned x) (_Ptrofs__unsigned y) of {
   Prelude.True -> Prelude.True;
   Prelude.False -> Prelude.False}

_Ptrofs__lt :: Ptrofs__Coq_int -> Ptrofs__Coq_int -> Prelude.Bool
_Ptrofs__lt x y =
  case Coqlib.zlt (_Ptrofs__signed x) (_Ptrofs__signed y) of {
   Prelude.True -> Prelude.True;
   Prelude.False -> Prelude.False}

_Ptrofs__ltu :: Ptrofs__Coq_int -> Ptrofs__Coq_int -> Prelude.Bool
_Ptrofs__ltu x y =
  case Coqlib.zlt (_Ptrofs__unsigned x) (_Ptrofs__unsigned y) of {
   Prelude.True -> Prelude.True;
   Prelude.False -> Prelude.False}

_Ptrofs__neg :: Ptrofs__Coq_int -> Ptrofs__Coq_int
_Ptrofs__neg x =
  _Ptrofs__repr (BinInt._Z__opp (_Ptrofs__unsigned x))

_Ptrofs__add :: Ptrofs__Coq_int -> Ptrofs__Coq_int -> Ptrofs__Coq_int
_Ptrofs__add x y =
  _Ptrofs__repr ((Prelude.+) (_Ptrofs__unsigned x) (_Ptrofs__unsigned y))

_Ptrofs__sub :: Ptrofs__Coq_int -> Ptrofs__Coq_int -> Ptrofs__Coq_int
_Ptrofs__sub x y =
  _Ptrofs__repr ((Prelude.-) (_Ptrofs__unsigned x) (_Ptrofs__unsigned y))

_Ptrofs__mul :: Ptrofs__Coq_int -> Ptrofs__Coq_int -> Ptrofs__Coq_int
_Ptrofs__mul x y =
  _Ptrofs__repr ((Prelude.*) (_Ptrofs__unsigned x) (_Ptrofs__unsigned y))

_Ptrofs__divs :: Ptrofs__Coq_int -> Ptrofs__Coq_int -> Ptrofs__Coq_int
_Ptrofs__divs x y =
  _Ptrofs__repr (BinInt._Z__quot (_Ptrofs__signed x) (_Ptrofs__signed y))

_Ptrofs__mods :: Ptrofs__Coq_int -> Ptrofs__Coq_int -> Ptrofs__Coq_int
_Ptrofs__mods x y =
  _Ptrofs__repr (BinInt._Z__rem (_Ptrofs__signed x) (_Ptrofs__signed y))

_Ptrofs__divu :: Ptrofs__Coq_int -> Ptrofs__Coq_int -> Ptrofs__Coq_int
_Ptrofs__divu x y =
  _Ptrofs__repr (BinInt._Z__div (_Ptrofs__unsigned x) (_Ptrofs__unsigned y))

_Ptrofs__modu :: Ptrofs__Coq_int -> Ptrofs__Coq_int -> Ptrofs__Coq_int
_Ptrofs__modu x y =
  _Ptrofs__repr
    (BinInt._Z__modulo (_Ptrofs__unsigned x) (_Ptrofs__unsigned y))

_Ptrofs__and :: Ptrofs__Coq_int -> Ptrofs__Coq_int -> Ptrofs__Coq_int
_Ptrofs__and x y =
  _Ptrofs__repr (BinInt._Z__land (_Ptrofs__unsigned x) (_Ptrofs__unsigned y))

_Ptrofs__or :: Ptrofs__Coq_int -> Ptrofs__Coq_int -> Ptrofs__Coq_int
_Ptrofs__or x y =
  _Ptrofs__repr (BinInt._Z__lor (_Ptrofs__unsigned x) (_Ptrofs__unsigned y))

_Ptrofs__xor :: Ptrofs__Coq_int -> Ptrofs__Coq_int -> Ptrofs__Coq_int
_Ptrofs__xor x y =
  _Ptrofs__repr (BinInt._Z__lxor (_Ptrofs__unsigned x) (_Ptrofs__unsigned y))

_Ptrofs__not :: Ptrofs__Coq_int -> Ptrofs__Coq_int
_Ptrofs__not x =
  _Ptrofs__xor x _Ptrofs__mone

_Ptrofs__shl :: Ptrofs__Coq_int -> Ptrofs__Coq_int -> Ptrofs__Coq_int
_Ptrofs__shl x y =
  _Ptrofs__repr
    (BinInt._Z__shiftl (_Ptrofs__unsigned x) (_Ptrofs__unsigned y))

_Ptrofs__shru :: Ptrofs__Coq_int -> Ptrofs__Coq_int -> Ptrofs__Coq_int
_Ptrofs__shru x y =
  _Ptrofs__repr
    (BinInt._Z__shiftr (_Ptrofs__unsigned x) (_Ptrofs__unsigned y))

_Ptrofs__shr :: Ptrofs__Coq_int -> Ptrofs__Coq_int -> Ptrofs__Coq_int
_Ptrofs__shr x y =
  _Ptrofs__repr (BinInt._Z__shiftr (_Ptrofs__signed x) (_Ptrofs__unsigned y))

_Ptrofs__rol :: Ptrofs__Coq_int -> Ptrofs__Coq_int -> Ptrofs__Coq_int
_Ptrofs__rol x y =
  let {n = BinInt._Z__modulo (_Ptrofs__unsigned y) _Ptrofs__zwordsize} in
  _Ptrofs__repr
    (BinInt._Z__lor (BinInt._Z__shiftl (_Ptrofs__unsigned x) n)
      (BinInt._Z__shiftr (_Ptrofs__unsigned x)
        ((Prelude.-) _Ptrofs__zwordsize n)))

_Ptrofs__ror :: Ptrofs__Coq_int -> Ptrofs__Coq_int -> Ptrofs__Coq_int
_Ptrofs__ror x y =
  let {n = BinInt._Z__modulo (_Ptrofs__unsigned y) _Ptrofs__zwordsize} in
  _Ptrofs__repr
    (BinInt._Z__lor (BinInt._Z__shiftr (_Ptrofs__unsigned x) n)
      (BinInt._Z__shiftl (_Ptrofs__unsigned x)
        ((Prelude.-) _Ptrofs__zwordsize n)))

_Ptrofs__rolm :: Ptrofs__Coq_int -> Ptrofs__Coq_int -> Ptrofs__Coq_int ->
                 Ptrofs__Coq_int
_Ptrofs__rolm x a m =
  _Ptrofs__and (_Ptrofs__rol x a) m

_Ptrofs__shrx :: Ptrofs__Coq_int -> Ptrofs__Coq_int -> Ptrofs__Coq_int
_Ptrofs__shrx x y =
  _Ptrofs__divs x (_Ptrofs__shl _Ptrofs__one y)

_Ptrofs__mulhu :: Ptrofs__Coq_int -> Ptrofs__Coq_int -> Ptrofs__Coq_int
_Ptrofs__mulhu x y =
  _Ptrofs__repr
    (BinInt._Z__div ((Prelude.*) (_Ptrofs__unsigned x) (_Ptrofs__unsigned y))
      _Ptrofs__modulus)

_Ptrofs__mulhs :: Ptrofs__Coq_int -> Ptrofs__Coq_int -> Ptrofs__Coq_int
_Ptrofs__mulhs x y =
  _Ptrofs__repr
    (BinInt._Z__div ((Prelude.*) (_Ptrofs__signed x) (_Ptrofs__signed y))
      _Ptrofs__modulus)

_Ptrofs__negative :: Ptrofs__Coq_int -> Ptrofs__Coq_int
_Ptrofs__negative x =
  case _Ptrofs__lt x _Ptrofs__zero of {
   Prelude.True -> _Ptrofs__one;
   Prelude.False -> _Ptrofs__zero}

_Ptrofs__add_carry :: Ptrofs__Coq_int -> Ptrofs__Coq_int -> Ptrofs__Coq_int
                      -> Ptrofs__Coq_int
_Ptrofs__add_carry x y cin =
  case Coqlib.zlt
         ((Prelude.+)
           ((Prelude.+) (_Ptrofs__unsigned x) (_Ptrofs__unsigned y))
           (_Ptrofs__unsigned cin)) _Ptrofs__modulus of {
   Prelude.True -> _Ptrofs__zero;
   Prelude.False -> _Ptrofs__one}

_Ptrofs__add_overflow :: Ptrofs__Coq_int -> Ptrofs__Coq_int ->
                         Ptrofs__Coq_int -> Ptrofs__Coq_int
_Ptrofs__add_overflow x y cin =
  let {
   s = (Prelude.+) ((Prelude.+) (_Ptrofs__signed x) (_Ptrofs__signed y))
         (_Ptrofs__signed cin)}
  in
  case (Prelude.&&) ((\x -> x) (Coqlib.zle _Ptrofs__min_signed s))
         ((\x -> x) (Coqlib.zle s _Ptrofs__max_signed)) of {
   Prelude.True -> _Ptrofs__zero;
   Prelude.False -> _Ptrofs__one}

_Ptrofs__sub_borrow :: Ptrofs__Coq_int -> Ptrofs__Coq_int -> Ptrofs__Coq_int
                       -> Ptrofs__Coq_int
_Ptrofs__sub_borrow x y bin =
  case Coqlib.zlt
         ((Prelude.-)
           ((Prelude.-) (_Ptrofs__unsigned x) (_Ptrofs__unsigned y))
           (_Ptrofs__unsigned bin)) 0 of {
   Prelude.True -> _Ptrofs__one;
   Prelude.False -> _Ptrofs__zero}

_Ptrofs__sub_overflow :: Ptrofs__Coq_int -> Ptrofs__Coq_int ->
                         Ptrofs__Coq_int -> Ptrofs__Coq_int
_Ptrofs__sub_overflow x y bin =
  let {
   s = (Prelude.-) ((Prelude.-) (_Ptrofs__signed x) (_Ptrofs__signed y))
         (_Ptrofs__signed bin)}
  in
  case (Prelude.&&) ((\x -> x) (Coqlib.zle _Ptrofs__min_signed s))
         ((\x -> x) (Coqlib.zle s _Ptrofs__max_signed)) of {
   Prelude.True -> _Ptrofs__zero;
   Prelude.False -> _Ptrofs__one}

_Ptrofs__shr_carry :: Ptrofs__Coq_int -> Ptrofs__Coq_int -> Ptrofs__Coq_int
_Ptrofs__shr_carry x y =
  case (Prelude.&&) (_Ptrofs__lt x _Ptrofs__zero)
         (Prelude.not
           (_Ptrofs__eq
             (_Ptrofs__and x
               (_Ptrofs__sub (_Ptrofs__shl _Ptrofs__one y) _Ptrofs__one))
             _Ptrofs__zero)) of {
   Prelude.True -> _Ptrofs__one;
   Prelude.False -> _Ptrofs__zero}

_Ptrofs__coq_Zshiftin :: Prelude.Bool -> Prelude.Integer -> Prelude.Integer
_Ptrofs__coq_Zshiftin b x =
  case b of {
   Prelude.True -> BinInt._Z__succ_double x;
   Prelude.False -> BinInt._Z__double x}

_Ptrofs__coq_Zzero_ext :: Prelude.Integer -> Prelude.Integer ->
                          Prelude.Integer
_Ptrofs__coq_Zzero_ext n x =
  BinInt._Z__iter n (\rec x0 ->
    _Ptrofs__coq_Zshiftin (BinInt._Z__odd x0) (rec (BinInt._Z__div2 x0)))
    (\_ -> 0) x

_Ptrofs__coq_Zsign_ext :: Prelude.Integer -> Prelude.Integer ->
                          Prelude.Integer
_Ptrofs__coq_Zsign_ext n x =
  BinInt._Z__iter (BinInt._Z__pred n) (\rec x0 ->
    _Ptrofs__coq_Zshiftin (BinInt._Z__odd x0) (rec (BinInt._Z__div2 x0)))
    (\x0 ->
    case BinInt._Z__odd x0 of {
     Prelude.True -> Prelude.negate 1;
     Prelude.False -> 0}) x

_Ptrofs__zero_ext :: Prelude.Integer -> Ptrofs__Coq_int -> Ptrofs__Coq_int
_Ptrofs__zero_ext n x =
  _Ptrofs__repr (_Ptrofs__coq_Zzero_ext n (_Ptrofs__unsigned x))

_Ptrofs__sign_ext :: Prelude.Integer -> Ptrofs__Coq_int -> Ptrofs__Coq_int
_Ptrofs__sign_ext n x =
  _Ptrofs__repr (_Ptrofs__coq_Zsign_ext n (_Ptrofs__unsigned x))

_Ptrofs__coq_Z_one_bits :: Prelude.Integer -> Prelude.Integer ->
                           Prelude.Integer -> ([]) Prelude.Integer
_Ptrofs__coq_Z_one_bits n x i =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> ([]))
    (\m ->
    case BinInt._Z__odd x of {
     Prelude.True -> (:) i
      (_Ptrofs__coq_Z_one_bits m (BinInt._Z__div2 x)
        ((Prelude.+) i ((\x -> x) 1)));
     Prelude.False ->
      _Ptrofs__coq_Z_one_bits m (BinInt._Z__div2 x)
        ((Prelude.+) i ((\x -> x) 1))})
    n

_Ptrofs__one_bits :: Ptrofs__Coq_int -> ([]) Ptrofs__Coq_int
_Ptrofs__one_bits x =
  List0.map _Ptrofs__repr
    (_Ptrofs__coq_Z_one_bits _Ptrofs__wordsize (_Ptrofs__unsigned x) 0)

_Ptrofs__is_power2 :: Ptrofs__Coq_int -> Prelude.Maybe Ptrofs__Coq_int
_Ptrofs__is_power2 x =
  case _Ptrofs__coq_Z_one_bits _Ptrofs__wordsize (_Ptrofs__unsigned x) 0 of {
   ([]) -> Prelude.Nothing;
   (:) i l ->
    case l of {
     ([]) -> Prelude.Just (_Ptrofs__repr i);
     (:) _ _ -> Prelude.Nothing}}

_Ptrofs__cmp :: Coq_comparison -> Ptrofs__Coq_int -> Ptrofs__Coq_int ->
                Prelude.Bool
_Ptrofs__cmp c x y =
  case c of {
   Ceq -> _Ptrofs__eq x y;
   Cne -> Prelude.not (_Ptrofs__eq x y);
   Clt -> _Ptrofs__lt x y;
   Cle -> Prelude.not (_Ptrofs__lt y x);
   Cgt -> _Ptrofs__lt y x;
   Cge -> Prelude.not (_Ptrofs__lt x y)}

_Ptrofs__cmpu :: Coq_comparison -> Ptrofs__Coq_int -> Ptrofs__Coq_int ->
                 Prelude.Bool
_Ptrofs__cmpu c x y =
  case c of {
   Ceq -> _Ptrofs__eq x y;
   Cne -> Prelude.not (_Ptrofs__eq x y);
   Clt -> _Ptrofs__ltu x y;
   Cle -> Prelude.not (_Ptrofs__ltu y x);
   Cgt -> _Ptrofs__ltu y x;
   Cge -> Prelude.not (_Ptrofs__ltu x y)}

_Ptrofs__notbool :: Ptrofs__Coq_int -> Ptrofs__Coq_int
_Ptrofs__notbool x =
  case _Ptrofs__eq x _Ptrofs__zero of {
   Prelude.True -> _Ptrofs__one;
   Prelude.False -> _Ptrofs__zero}

_Ptrofs__divmodu2 :: Ptrofs__Coq_int -> Ptrofs__Coq_int -> Ptrofs__Coq_int ->
                     Prelude.Maybe ((,) Ptrofs__Coq_int Ptrofs__Coq_int)
_Ptrofs__divmodu2 nhi nlo d =
  case _Ptrofs__eq_dec d _Ptrofs__zero of {
   Prelude.True -> Prelude.Nothing;
   Prelude.False ->
    case BinInt._Z__div_eucl
           ((Prelude.+)
             ((Prelude.*) (_Ptrofs__unsigned nhi) _Ptrofs__modulus)
             (_Ptrofs__unsigned nlo)) (_Ptrofs__unsigned d) of {
     (,) q r ->
      case Coqlib.zle q _Ptrofs__max_unsigned of {
       Prelude.True -> Prelude.Just ((,) (_Ptrofs__repr q) (_Ptrofs__repr r));
       Prelude.False -> Prelude.Nothing}}}

_Ptrofs__divmods2 :: Ptrofs__Coq_int -> Ptrofs__Coq_int -> Ptrofs__Coq_int ->
                     Prelude.Maybe ((,) Ptrofs__Coq_int Ptrofs__Coq_int)
_Ptrofs__divmods2 nhi nlo d =
  case _Ptrofs__eq_dec d _Ptrofs__zero of {
   Prelude.True -> Prelude.Nothing;
   Prelude.False ->
    case BinInt._Z__quotrem
           ((Prelude.+) ((Prelude.*) (_Ptrofs__signed nhi) _Ptrofs__modulus)
             (_Ptrofs__unsigned nlo)) (_Ptrofs__signed d) of {
     (,) q r ->
      case (Prelude.&&) ((\x -> x) (Coqlib.zle _Ptrofs__min_signed q))
             ((\x -> x) (Coqlib.zle q _Ptrofs__max_signed)) of {
       Prelude.True -> Prelude.Just ((,) (_Ptrofs__repr q) (_Ptrofs__repr r));
       Prelude.False -> Prelude.Nothing}}}

_Ptrofs__testbit :: Ptrofs__Coq_int -> Prelude.Integer -> Prelude.Bool
_Ptrofs__testbit x i =
  BinInt._Z__testbit (_Ptrofs__unsigned x) i

_Ptrofs__powerserie :: (([]) Prelude.Integer) -> Prelude.Integer
_Ptrofs__powerserie l =
  case l of {
   ([]) -> 0;
   (:) x xs -> (Prelude.+) (Zpower.two_p x) (_Ptrofs__powerserie xs)}

_Ptrofs__int_of_one_bits :: (([]) Ptrofs__Coq_int) -> Ptrofs__Coq_int
_Ptrofs__int_of_one_bits l =
  case l of {
   ([]) -> _Ptrofs__zero;
   (:) a b ->
    _Ptrofs__add (_Ptrofs__shl _Ptrofs__one a) (_Ptrofs__int_of_one_bits b)}

_Ptrofs__no_overlap :: Ptrofs__Coq_int -> Prelude.Integer -> Ptrofs__Coq_int
                       -> Prelude.Integer -> Prelude.Bool
_Ptrofs__no_overlap ofs1 sz1 ofs2 sz2 =
  let {x1 = _Ptrofs__unsigned ofs1} in
  let {x2 = _Ptrofs__unsigned ofs2} in
  (Prelude.&&)
    ((Prelude.&&)
      ((\x -> x) (Coqlib.zlt ((Prelude.+) x1 sz1) _Ptrofs__modulus))
      ((\x -> x) (Coqlib.zlt ((Prelude.+) x2 sz2) _Ptrofs__modulus)))
    ((Prelude.||) ((\x -> x) (Coqlib.zle ((Prelude.+) x1 sz1) x2))
      ((\x -> x) (Coqlib.zle ((Prelude.+) x2 sz2) x1)))

_Ptrofs__coq_Zsize :: Prelude.Integer -> Prelude.Integer
_Ptrofs__coq_Zsize x =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> 0)
    (\p -> (\x -> x) (BinPos._Pos__size p))
    (\_ -> 0)
    x

_Ptrofs__size :: Ptrofs__Coq_int -> Prelude.Integer
_Ptrofs__size x =
  _Ptrofs__coq_Zsize (_Ptrofs__unsigned x)

_Ptrofs__to_int :: Ptrofs__Coq_int -> Int__Coq_int
_Ptrofs__to_int x =
  _Int__repr (_Ptrofs__unsigned x)

_Ptrofs__to_int64 :: Ptrofs__Coq_int -> Int64__Coq_int
_Ptrofs__to_int64 x =
  _Int64__repr (_Ptrofs__unsigned x)

_Ptrofs__of_int :: Int__Coq_int -> Ptrofs__Coq_int
_Ptrofs__of_int x =
  _Ptrofs__repr (_Int__unsigned x)

_Ptrofs__of_intu :: Int__Coq_int -> Ptrofs__Coq_int
_Ptrofs__of_intu =
  _Ptrofs__of_int

_Ptrofs__of_ints :: Int__Coq_int -> Ptrofs__Coq_int
_Ptrofs__of_ints x =
  _Ptrofs__repr (_Int__signed x)

_Ptrofs__of_int64 :: Int64__Coq_int -> Ptrofs__Coq_int
_Ptrofs__of_int64 x =
  _Ptrofs__repr (_Int64__unsigned x)

_Ptrofs__of_int64u :: Int64__Coq_int -> Ptrofs__Coq_int
_Ptrofs__of_int64u =
  _Ptrofs__of_int64

_Ptrofs__of_int64s :: Int64__Coq_int -> Ptrofs__Coq_int
_Ptrofs__of_int64s x =
  _Ptrofs__repr (_Int64__signed x)

