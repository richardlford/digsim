module Nat where

import qualified Prelude
import qualified Datatypes
import qualified Decimal

type Coq_t = Prelude.Integer

zero :: Prelude.Integer
zero =
  0

one :: Prelude.Integer
one =
  Prelude.succ 0

two :: Prelude.Integer
two =
  Prelude.succ (Prelude.succ 0)

succ :: Prelude.Integer -> Prelude.Integer
succ x =
  Prelude.succ x

pred :: Prelude.Integer -> Prelude.Integer
pred = (\n -> Prelude.max 0 (Prelude.pred n))

double :: Prelude.Integer -> Prelude.Integer
double n =
  (Prelude.+) n n

sub :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
sub = (\n m -> Prelude.max 0 (n Prelude.- m))

eqb :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
eqb n m =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> Prelude.True)
      (\_ -> Prelude.False)
      m)
    (\n' ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> Prelude.False)
      (\m' -> eqb n' m')
      m)
    n

leb :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
leb n m =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> Prelude.True)
    (\n' ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> Prelude.False)
      (\m' -> leb n' m')
      m)
    n

ltb :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
ltb n m =
  leb (Prelude.succ n) m

compare :: Prelude.Integer -> Prelude.Integer -> Datatypes.Coq_comparison
compare n m =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> Datatypes.Eq)
      (\_ -> Datatypes.Lt)
      m)
    (\n' ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> Datatypes.Gt)
      (\m' -> compare n' m')
      m)
    n

even :: Prelude.Integer -> Prelude.Bool
even n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> Prelude.True)
    (\n0 ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> Prelude.False)
      (\n' -> even n')
      n0)
    n

odd :: Prelude.Integer -> Prelude.Bool
odd n =
  Prelude.not (even n)

pow :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
pow n m =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> Prelude.succ 0)
    (\m0 -> (Prelude.*) n (pow n m0))
    m

tail_add :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
tail_add n m =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> m)
    (\n0 -> tail_add n0 (Prelude.succ m))
    n

tail_addmul :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer ->
               Prelude.Integer
tail_addmul r n m =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> r)
    (\n0 -> tail_addmul (tail_add m r) n0 m)
    n

tail_mul :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
tail_mul n m =
  tail_addmul 0 n m

of_uint_acc :: Decimal.Coq_uint -> Prelude.Integer -> Prelude.Integer
of_uint_acc d acc =
  case d of {
   Decimal.Nil -> acc;
   Decimal.D0 d0 ->
    of_uint_acc d0
      (tail_mul (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ 0)))))))))) acc);
   Decimal.D1 d0 ->
    of_uint_acc d0 (Prelude.succ
      (tail_mul (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ 0)))))))))) acc));
   Decimal.D2 d0 ->
    of_uint_acc d0 (Prelude.succ (Prelude.succ
      (tail_mul (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ 0)))))))))) acc)));
   Decimal.D3 d0 ->
    of_uint_acc d0 (Prelude.succ (Prelude.succ (Prelude.succ
      (tail_mul (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ 0)))))))))) acc))));
   Decimal.D4 d0 ->
    of_uint_acc d0 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (tail_mul (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ 0)))))))))) acc)))));
   Decimal.D5 d0 ->
    of_uint_acc d0 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ
      (tail_mul (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ 0)))))))))) acc))))));
   Decimal.D6 d0 ->
    of_uint_acc d0 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ
      (tail_mul (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ 0)))))))))) acc)))))));
   Decimal.D7 d0 ->
    of_uint_acc d0 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ
      (tail_mul (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ 0)))))))))) acc))))))));
   Decimal.D8 d0 ->
    of_uint_acc d0 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (tail_mul (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ 0)))))))))) acc)))))))));
   Decimal.D9 d0 ->
    of_uint_acc d0 (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      (tail_mul (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        (Prelude.succ 0)))))))))) acc))))))))))}

of_uint :: Decimal.Coq_uint -> Prelude.Integer
of_uint d =
  of_uint_acc d 0

to_little_uint :: Prelude.Integer -> Decimal.Coq_uint -> Decimal.Coq_uint
to_little_uint n acc =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> acc)
    (\n0 -> to_little_uint n0 (Decimal._Little__succ acc))
    n

to_uint :: Prelude.Integer -> Decimal.Coq_uint
to_uint n =
  Decimal.rev (to_little_uint n (Decimal.D0 Decimal.Nil))

of_int :: Decimal.Coq_int -> Prelude.Maybe Prelude.Integer
of_int d =
  case Decimal.norm d of {
   Decimal.Pos u -> Prelude.Just (of_uint u);
   Decimal.Neg _ -> Prelude.Nothing}

to_int :: Prelude.Integer -> Decimal.Coq_int
to_int n =
  Decimal.Pos (to_uint n)

divmod :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer ->
          Prelude.Integer -> (,) Prelude.Integer Prelude.Integer
divmod x y q u =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> (,) q u)
    (\x' ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> divmod x' y (Prelude.succ q) y)
      (\u' -> divmod x' y q u')
      u)
    x

div :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
div = (\n m -> if m Prelude.== 0 then 0 else Prelude.div n m)

modulo :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
modulo = (\n m -> if m Prelude.== 0 then 0 else Prelude.mod n m)

gcd :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
gcd a b =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> b)
    (\a' -> gcd (modulo b (Prelude.succ a')) (Prelude.succ a'))
    a

square :: Prelude.Integer -> Prelude.Integer
square n =
  (Prelude.*) n n

sqrt_iter :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer ->
             Prelude.Integer -> Prelude.Integer
sqrt_iter k p q r =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> p)
    (\k' ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ ->
      sqrt_iter k' (Prelude.succ p) (Prelude.succ (Prelude.succ q))
        (Prelude.succ (Prelude.succ q)))
      (\r' -> sqrt_iter k' p q r')
      r)
    k

sqrt :: Prelude.Integer -> Prelude.Integer
sqrt n =
  sqrt_iter n 0 0 0

log2_iter :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer ->
             Prelude.Integer -> Prelude.Integer
log2_iter k p q r =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> p)
    (\k' ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> log2_iter k' (Prelude.succ p) (Prelude.succ q) q)
      (\r' -> log2_iter k' p (Prelude.succ q) r')
      r)
    k

log2 :: Prelude.Integer -> Prelude.Integer
log2 n =
  log2_iter (pred n) 0 (Prelude.succ 0) 0

iter :: Prelude.Integer -> (a1 -> a1) -> a1 -> a1
iter n f x =
  Datatypes.nat_rect x (\_ -> f) n

div2 :: Prelude.Integer -> Prelude.Integer
div2 n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> 0)
    (\n0 ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> 0)
      (\n' -> Prelude.succ (div2 n'))
      n0)
    n

testbit :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
testbit a n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> odd a)
    (\n0 -> testbit (div2 a) n0)
    n

shiftl :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
shiftl a =
  Datatypes.nat_rect a (\_ -> double)

shiftr :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
shiftr a =
  Datatypes.nat_rect a (\_ -> div2)

bitwise :: (Prelude.Bool -> Prelude.Bool -> Prelude.Bool) -> Prelude.Integer
           -> Prelude.Integer -> Prelude.Integer -> Prelude.Integer
bitwise op n a b =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> 0)
    (\n' ->
    (Prelude.+)
      (case op (odd a) (odd b) of {
        Prelude.True -> Prelude.succ 0;
        Prelude.False -> 0})
      ((Prelude.*) (Prelude.succ (Prelude.succ 0))
        (bitwise op n' (div2 a) (div2 b))))
    n

land :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
land a b =
  bitwise (Prelude.&&) a a b

lor :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
lor a b =
  bitwise (Prelude.||) (Prelude.max a b) a b

ldiff :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
ldiff a b =
  bitwise (\b0 b' -> (Prelude.&&) b0 (Prelude.not b')) a a b

lxor :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
lxor a b =
  bitwise Datatypes.xorb (Prelude.max a b) a b

