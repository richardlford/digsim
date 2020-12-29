module BinNat where

import qualified Prelude
import qualified BinNums
import qualified BinPos
import qualified Bool
import qualified Datatypes
import qualified Decimal
import qualified Logic
import qualified Specif

__ :: any
__ = Prelude.error "Logical or arity value used"

type N__Coq_t = BinNums.N

_N__zero :: BinNums.N
_N__zero =
  BinNums.N0

_N__one :: BinNums.N
_N__one =
  BinNums.Npos 1

_N__two :: BinNums.N
_N__two =
  BinNums.Npos ((\x -> 2 Prelude.* x) 1)

_N__succ_double :: BinNums.N -> BinNums.N
_N__succ_double x =
  case x of {
   BinNums.N0 -> BinNums.Npos 1;
   BinNums.Npos p -> BinNums.Npos ((\x -> 2 Prelude.* x Prelude.+ 1) p)}

_N__double :: BinNums.N -> BinNums.N
_N__double n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p -> BinNums.Npos ((\x -> 2 Prelude.* x) p)}

_N__succ :: BinNums.N -> BinNums.N
_N__succ n =
  case n of {
   BinNums.N0 -> BinNums.Npos 1;
   BinNums.Npos p -> BinNums.Npos (BinPos._Pos__succ p)}

_N__pred :: BinNums.N -> BinNums.N
_N__pred n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p -> BinPos._Pos__pred_N p}

_N__succ_pos :: BinNums.N -> Prelude.Integer
_N__succ_pos n =
  case n of {
   BinNums.N0 -> 1;
   BinNums.Npos p -> BinPos._Pos__succ p}

_N__add :: BinNums.N -> BinNums.N -> BinNums.N
_N__add n m =
  case n of {
   BinNums.N0 -> m;
   BinNums.Npos p ->
    case m of {
     BinNums.N0 -> n;
     BinNums.Npos q -> BinNums.Npos (BinPos._Pos__add p q)}}

_N__sub :: BinNums.N -> BinNums.N -> BinNums.N
_N__sub n m =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos n' ->
    case m of {
     BinNums.N0 -> n;
     BinNums.Npos m' ->
      case BinPos._Pos__sub_mask n' m' of {
       BinPos.Pos__IsPos p -> BinNums.Npos p;
       _ -> BinNums.N0}}}

_N__mul :: BinNums.N -> BinNums.N -> BinNums.N
_N__mul n m =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p ->
    case m of {
     BinNums.N0 -> BinNums.N0;
     BinNums.Npos q -> BinNums.Npos (BinPos._Pos__mul p q)}}

_N__compare :: BinNums.N -> BinNums.N -> Datatypes.Coq_comparison
_N__compare n m =
  case n of {
   BinNums.N0 ->
    case m of {
     BinNums.N0 -> Datatypes.Eq;
     BinNums.Npos _ -> Datatypes.Lt};
   BinNums.Npos n' ->
    case m of {
     BinNums.N0 -> Datatypes.Gt;
     BinNums.Npos m' -> BinPos._Pos__compare n' m'}}

_N__eqb :: BinNums.N -> BinNums.N -> Prelude.Bool
_N__eqb n m =
  case n of {
   BinNums.N0 ->
    case m of {
     BinNums.N0 -> Prelude.True;
     BinNums.Npos _ -> Prelude.False};
   BinNums.Npos p ->
    case m of {
     BinNums.N0 -> Prelude.False;
     BinNums.Npos q -> BinPos._Pos__eqb p q}}

_N__leb :: BinNums.N -> BinNums.N -> Prelude.Bool
_N__leb x y =
  case _N__compare x y of {
   Datatypes.Gt -> Prelude.False;
   _ -> Prelude.True}

_N__ltb :: BinNums.N -> BinNums.N -> Prelude.Bool
_N__ltb x y =
  case _N__compare x y of {
   Datatypes.Lt -> Prelude.True;
   _ -> Prelude.False}

_N__min :: BinNums.N -> BinNums.N -> BinNums.N
_N__min n n' =
  case _N__compare n n' of {
   Datatypes.Gt -> n';
   _ -> n}

_N__max :: BinNums.N -> BinNums.N -> BinNums.N
_N__max n n' =
  case _N__compare n n' of {
   Datatypes.Gt -> n;
   _ -> n'}

_N__div2 :: BinNums.N -> BinNums.N
_N__div2 n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p0 ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\p -> BinNums.Npos p)
      (\p -> BinNums.Npos p)
      (\_ -> BinNums.N0)
      p0}

_N__even :: BinNums.N -> Prelude.Bool
_N__even n =
  case n of {
   BinNums.N0 -> Prelude.True;
   BinNums.Npos p ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\_ -> Prelude.False)
      (\_ -> Prelude.True)
      (\_ -> Prelude.False)
      p}

_N__odd :: BinNums.N -> Prelude.Bool
_N__odd n =
  Prelude.not (_N__even n)

_N__pow :: BinNums.N -> BinNums.N -> BinNums.N
_N__pow n p =
  case p of {
   BinNums.N0 -> BinNums.Npos 1;
   BinNums.Npos p0 ->
    case n of {
     BinNums.N0 -> BinNums.N0;
     BinNums.Npos q -> BinNums.Npos (BinPos._Pos__pow q p0)}}

_N__square :: BinNums.N -> BinNums.N
_N__square n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p -> BinNums.Npos (BinPos._Pos__square p)}

_N__log2 :: BinNums.N -> BinNums.N
_N__log2 n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p0 ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\p -> BinNums.Npos (BinPos._Pos__size p))
      (\p -> BinNums.Npos (BinPos._Pos__size p))
      (\_ -> BinNums.N0)
      p0}

_N__size :: BinNums.N -> BinNums.N
_N__size n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p -> BinNums.Npos (BinPos._Pos__size p)}

_N__size_nat :: BinNums.N -> Prelude.Integer
_N__size_nat n =
  case n of {
   BinNums.N0 -> 0;
   BinNums.Npos p -> BinPos._Pos__size_nat p}

_N__pos_div_eucl :: Prelude.Integer -> BinNums.N -> (,) BinNums.N BinNums.N
_N__pos_div_eucl a b =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\a' ->
    case _N__pos_div_eucl a' b of {
     (,) q r ->
      let {r' = _N__succ_double r} in
      case _N__leb b r' of {
       Prelude.True -> (,) (_N__succ_double q) (_N__sub r' b);
       Prelude.False -> (,) (_N__double q) r'}})
    (\a' ->
    case _N__pos_div_eucl a' b of {
     (,) q r ->
      let {r' = _N__double r} in
      case _N__leb b r' of {
       Prelude.True -> (,) (_N__succ_double q) (_N__sub r' b);
       Prelude.False -> (,) (_N__double q) r'}})
    (\_ ->
    case b of {
     BinNums.N0 -> (,) BinNums.N0 (BinNums.Npos 1);
     BinNums.Npos p ->
      (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
        (\_ -> (,) BinNums.N0 (BinNums.Npos 1))
        (\_ -> (,) BinNums.N0 (BinNums.Npos 1))
        (\_ -> (,) (BinNums.Npos 1) BinNums.N0)
        p})
    a

_N__div_eucl :: BinNums.N -> BinNums.N -> (,) BinNums.N BinNums.N
_N__div_eucl a b =
  case a of {
   BinNums.N0 -> (,) BinNums.N0 BinNums.N0;
   BinNums.Npos na ->
    case b of {
     BinNums.N0 -> (,) BinNums.N0 a;
     BinNums.Npos _ -> _N__pos_div_eucl na b}}

_N__div :: BinNums.N -> BinNums.N -> BinNums.N
_N__div a b =
  Prelude.fst (_N__div_eucl a b)

_N__modulo :: BinNums.N -> BinNums.N -> BinNums.N
_N__modulo a b =
  Prelude.snd (_N__div_eucl a b)

_N__gcd :: BinNums.N -> BinNums.N -> BinNums.N
_N__gcd a b =
  case a of {
   BinNums.N0 -> b;
   BinNums.Npos p ->
    case b of {
     BinNums.N0 -> a;
     BinNums.Npos q -> BinNums.Npos (BinPos._Pos__gcd p q)}}

_N__ggcd :: BinNums.N -> BinNums.N -> (,) BinNums.N ((,) BinNums.N BinNums.N)
_N__ggcd a b =
  case a of {
   BinNums.N0 -> (,) b ((,) BinNums.N0 (BinNums.Npos 1));
   BinNums.Npos p ->
    case b of {
     BinNums.N0 -> (,) a ((,) (BinNums.Npos 1) BinNums.N0);
     BinNums.Npos q ->
      case BinPos._Pos__ggcd p q of {
       (,) g p0 ->
        case p0 of {
         (,) aa bb -> (,) (BinNums.Npos g) ((,) (BinNums.Npos aa) (BinNums.Npos bb))}}}}

_N__sqrtrem :: BinNums.N -> (,) BinNums.N BinNums.N
_N__sqrtrem n =
  case n of {
   BinNums.N0 -> (,) BinNums.N0 BinNums.N0;
   BinNums.Npos p ->
    case BinPos._Pos__sqrtrem p of {
     (,) s m ->
      case m of {
       BinPos.Pos__IsPos r -> (,) (BinNums.Npos s) (BinNums.Npos r);
       _ -> (,) (BinNums.Npos s) BinNums.N0}}}

_N__sqrt :: BinNums.N -> BinNums.N
_N__sqrt n =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p -> BinNums.Npos (BinPos._Pos__sqrt p)}

_N__lor :: BinNums.N -> BinNums.N -> BinNums.N
_N__lor n m =
  case n of {
   BinNums.N0 -> m;
   BinNums.Npos p ->
    case m of {
     BinNums.N0 -> n;
     BinNums.Npos q -> BinNums.Npos (BinPos._Pos__lor p q)}}

_N__land :: BinNums.N -> BinNums.N -> BinNums.N
_N__land n m =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p ->
    case m of {
     BinNums.N0 -> BinNums.N0;
     BinNums.Npos q -> BinPos._Pos__land p q}}

_N__ldiff :: BinNums.N -> BinNums.N -> BinNums.N
_N__ldiff n m =
  case n of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos p ->
    case m of {
     BinNums.N0 -> n;
     BinNums.Npos q -> BinPos._Pos__ldiff p q}}

_N__lxor :: BinNums.N -> BinNums.N -> BinNums.N
_N__lxor n m =
  case n of {
   BinNums.N0 -> m;
   BinNums.Npos p ->
    case m of {
     BinNums.N0 -> n;
     BinNums.Npos q -> BinPos._Pos__lxor p q}}

_N__shiftl_nat :: BinNums.N -> Prelude.Integer -> BinNums.N
_N__shiftl_nat a =
  Datatypes.nat_rect a (\_ -> _N__double)

_N__shiftr_nat :: BinNums.N -> Prelude.Integer -> BinNums.N
_N__shiftr_nat a =
  Datatypes.nat_rect a (\_ -> _N__div2)

_N__shiftl :: BinNums.N -> BinNums.N -> BinNums.N
_N__shiftl a n =
  case a of {
   BinNums.N0 -> BinNums.N0;
   BinNums.Npos a0 -> BinNums.Npos (BinPos._Pos__shiftl a0 n)}

_N__shiftr :: BinNums.N -> BinNums.N -> BinNums.N
_N__shiftr a n =
  case n of {
   BinNums.N0 -> a;
   BinNums.Npos p -> BinPos._Pos__iter _N__div2 a p}

_N__testbit_nat :: BinNums.N -> Prelude.Integer -> Prelude.Bool
_N__testbit_nat a =
  case a of {
   BinNums.N0 -> (\_ -> Prelude.False);
   BinNums.Npos p -> BinPos._Pos__testbit_nat p}

_N__testbit :: BinNums.N -> BinNums.N -> Prelude.Bool
_N__testbit a n =
  case a of {
   BinNums.N0 -> Prelude.False;
   BinNums.Npos p -> BinPos._Pos__testbit p n}

_N__to_nat :: BinNums.N -> Prelude.Integer
_N__to_nat a =
  case a of {
   BinNums.N0 -> 0;
   BinNums.Npos p -> BinPos._Pos__to_nat p}

_N__of_nat :: Prelude.Integer -> BinNums.N
_N__of_nat n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> BinNums.N0)
    (\n' -> BinNums.Npos (BinPos._Pos__of_succ_nat n'))
    n

_N__iter :: BinNums.N -> (a1 -> a1) -> a1 -> a1
_N__iter n f x =
  case n of {
   BinNums.N0 -> x;
   BinNums.Npos p -> BinPos._Pos__iter f x p}

_N__of_uint :: Decimal.Coq_uint -> BinNums.N
_N__of_uint =
  BinPos._Pos__of_uint

_N__of_int :: Decimal.Coq_int -> Prelude.Maybe BinNums.N
_N__of_int d =
  case Decimal.norm d of {
   Decimal.Pos d0 -> Prelude.Just (BinPos._Pos__of_uint d0);
   Decimal.Neg _ -> Prelude.Nothing}

_N__to_uint :: BinNums.N -> Decimal.Coq_uint
_N__to_uint n =
  case n of {
   BinNums.N0 -> Decimal.D0 Decimal.Nil;
   BinNums.Npos p -> BinPos._Pos__to_uint p}

_N__to_int :: BinNums.N -> Decimal.Coq_int
_N__to_int n =
  Decimal.Pos (_N__to_uint n)

_N__eq_dec :: BinNums.N -> BinNums.N -> Prelude.Bool
_N__eq_dec n m =
  BinNums.coq_N_rec (\x ->
    case x of {
     BinNums.N0 -> Prelude.True;
     BinNums.Npos _ -> Prelude.False}) (\p x ->
    case x of {
     BinNums.N0 -> Prelude.False;
     BinNums.Npos p0 ->
      Specif.sumbool_rec (\_ -> Prelude.True) (\_ -> Prelude.False)
        (BinPos._Pos__eq_dec p p0)}) n m

_N__discr :: BinNums.N -> Prelude.Maybe Prelude.Integer
_N__discr n =
  case n of {
   BinNums.N0 -> Prelude.Nothing;
   BinNums.Npos p -> Prelude.Just p}

_N__binary_rect :: a1 -> (BinNums.N -> a1 -> a1) -> (BinNums.N -> a1 -> a1) ->
                   BinNums.N -> a1
_N__binary_rect f0 f2 fS2 n =
  let {f2' = \p -> f2 (BinNums.Npos p)} in
  let {fS2' = \p -> fS2 (BinNums.Npos p)} in
  case n of {
   BinNums.N0 -> f0;
   BinNums.Npos p -> BinNums.positive_rect fS2' f2' (fS2 BinNums.N0 f0) p}

_N__binary_rec :: a1 -> (BinNums.N -> a1 -> a1) -> (BinNums.N -> a1 -> a1) ->
                  BinNums.N -> a1
_N__binary_rec =
  _N__binary_rect

_N__peano_rect :: a1 -> (BinNums.N -> a1 -> a1) -> BinNums.N -> a1
_N__peano_rect f0 f n =
  let {f' = \p -> f (BinNums.Npos p)} in
  case n of {
   BinNums.N0 -> f0;
   BinNums.Npos p -> BinPos._Pos__peano_rect (f BinNums.N0 f0) f' p}

_N__peano_rec :: a1 -> (BinNums.N -> a1 -> a1) -> BinNums.N -> a1
_N__peano_rec =
  _N__peano_rect

_N__recursion :: a1 -> (BinNums.N -> a1 -> a1) -> BinNums.N -> a1
_N__recursion =
  _N__peano_rect

_N__leb_spec0 :: BinNums.N -> BinNums.N -> Bool.Coq_reflect
_N__leb_spec0 x y =
  Bool.iff_reflect (_N__leb x y)

_N__ltb_spec0 :: BinNums.N -> BinNums.N -> Bool.Coq_reflect
_N__ltb_spec0 x y =
  Bool.iff_reflect (_N__ltb x y)

_N__Private_Dec__max_case_strong :: BinNums.N -> BinNums.N -> (BinNums.N ->
                                    BinNums.N -> () -> a1 -> a1) -> (() -> a1) ->
                                    (() -> a1) -> a1
_N__Private_Dec__max_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (_N__compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat n (_N__max n m) __ (hl __);
   _ -> compat m (_N__max n m) __ (hr __)}

_N__Private_Dec__max_case :: BinNums.N -> BinNums.N -> (BinNums.N -> BinNums.N -> ()
                             -> a1 -> a1) -> a1 -> a1 -> a1
_N__Private_Dec__max_case n m x x0 x1 =
  _N__Private_Dec__max_case_strong n m x (\_ -> x0) (\_ -> x1)

_N__Private_Dec__max_dec :: BinNums.N -> BinNums.N -> Prelude.Bool
_N__Private_Dec__max_dec n m =
  _N__Private_Dec__max_case n m (\_ _ _ h0 -> h0) Prelude.True Prelude.False

_N__Private_Dec__min_case_strong :: BinNums.N -> BinNums.N -> (BinNums.N ->
                                    BinNums.N -> () -> a1 -> a1) -> (() -> a1) ->
                                    (() -> a1) -> a1
_N__Private_Dec__min_case_strong n m compat hl hr =
  let {c = Datatypes.coq_CompSpec2Type n m (_N__compare n m)} in
  case c of {
   Datatypes.CompGtT -> compat m (_N__min n m) __ (hr __);
   _ -> compat n (_N__min n m) __ (hl __)}

_N__Private_Dec__min_case :: BinNums.N -> BinNums.N -> (BinNums.N -> BinNums.N -> ()
                             -> a1 -> a1) -> a1 -> a1 -> a1
_N__Private_Dec__min_case n m x x0 x1 =
  _N__Private_Dec__min_case_strong n m x (\_ -> x0) (\_ -> x1)

_N__Private_Dec__min_dec :: BinNums.N -> BinNums.N -> Prelude.Bool
_N__Private_Dec__min_dec n m =
  _N__Private_Dec__min_case n m (\_ _ _ h0 -> h0) Prelude.True Prelude.False

_N__max_case_strong :: BinNums.N -> BinNums.N -> (() -> a1) -> (() -> a1) -> a1
_N__max_case_strong n m x x0 =
  _N__Private_Dec__max_case_strong n m (\_ _ _ x1 -> Logic.eq_rect __ x1 __) x x0

_N__max_case :: BinNums.N -> BinNums.N -> a1 -> a1 -> a1
_N__max_case n m x x0 =
  _N__max_case_strong n m (\_ -> x) (\_ -> x0)

_N__max_dec :: BinNums.N -> BinNums.N -> Prelude.Bool
_N__max_dec =
  _N__Private_Dec__max_dec

_N__min_case_strong :: BinNums.N -> BinNums.N -> (() -> a1) -> (() -> a1) -> a1
_N__min_case_strong n m x x0 =
  _N__Private_Dec__min_case_strong n m (\_ _ _ x1 -> Logic.eq_rect __ x1 __) x x0

_N__min_case :: BinNums.N -> BinNums.N -> a1 -> a1 -> a1
_N__min_case n m x x0 =
  _N__min_case_strong n m (\_ -> x) (\_ -> x0)

_N__min_dec :: BinNums.N -> BinNums.N -> Prelude.Bool
_N__min_dec =
  _N__Private_Dec__min_dec

_N__sqrt_up :: BinNums.N -> BinNums.N
_N__sqrt_up a =
  case _N__compare BinNums.N0 a of {
   Datatypes.Lt -> _N__succ (_N__sqrt (_N__pred a));
   _ -> BinNums.N0}

_N__log2_up :: BinNums.N -> BinNums.N
_N__log2_up a =
  case _N__compare (BinNums.Npos 1) a of {
   Datatypes.Lt -> _N__succ (_N__log2 (_N__pred a));
   _ -> BinNums.N0}

_N__lcm :: BinNums.N -> BinNums.N -> BinNums.N
_N__lcm a b =
  _N__mul a (_N__div b (_N__gcd a b))

_N__eqb_spec :: BinNums.N -> BinNums.N -> Bool.Coq_reflect
_N__eqb_spec x y =
  Bool.iff_reflect (_N__eqb x y)

_N__b2n :: Prelude.Bool -> BinNums.N
_N__b2n b =
  case b of {
   Prelude.True -> BinNums.Npos 1;
   Prelude.False -> BinNums.N0}

_N__setbit :: BinNums.N -> BinNums.N -> BinNums.N
_N__setbit a n =
  _N__lor a (_N__shiftl (BinNums.Npos 1) n)

_N__clearbit :: BinNums.N -> BinNums.N -> BinNums.N
_N__clearbit a n =
  _N__ldiff a (_N__shiftl (BinNums.Npos 1) n)

_N__ones :: BinNums.N -> BinNums.N
_N__ones n =
  _N__pred (_N__shiftl (BinNums.Npos 1) n)

_N__lnot :: BinNums.N -> BinNums.N -> BinNums.N
_N__lnot a n =
  _N__lxor a (_N__ones n)

coq_N_rec_double :: BinNums.N -> a1 -> (BinNums.N -> a1 -> a1) -> (BinNums.N -> a1
                    -> a1) -> a1
coq_N_rec_double a f0 f2 fS2 =
  _N__binary_rec f0 f2 fS2 a

