module Ascii where

import qualified Prelude
import qualified BinNat
import qualified BinNums
import qualified Bool
import qualified Logic
import qualified Data.Char
import qualified Data.Bits

ascii_rect :: (Prelude.Bool -> Prelude.Bool -> Prelude.Bool -> Prelude.Bool ->
              Prelude.Bool -> Prelude.Bool -> Prelude.Bool -> Prelude.Bool -> a1) ->
              Prelude.Char -> a1
ascii_rect f a =
  (\f a -> f (Data.Bits.testBit (Data.Char.ord a) 0)
              (Data.Bits.testBit (Data.Char.ord a) 1)
              (Data.Bits.testBit (Data.Char.ord a) 2)
              (Data.Bits.testBit (Data.Char.ord a) 3)
              (Data.Bits.testBit (Data.Char.ord a) 4)
              (Data.Bits.testBit (Data.Char.ord a) 5)
              (Data.Bits.testBit (Data.Char.ord a) 6)
              (Data.Bits.testBit (Data.Char.ord a) 7))
    (\x x0 x1 x2 x3 x4 x5 x6 -> f x x0 x1 x2 x3 x4 x5 x6)
    a

ascii_rec :: (Prelude.Bool -> Prelude.Bool -> Prelude.Bool -> Prelude.Bool ->
             Prelude.Bool -> Prelude.Bool -> Prelude.Bool -> Prelude.Bool -> a1) ->
             Prelude.Char -> a1
ascii_rec =
  ascii_rect

zero :: Prelude.Char
zero =
  '\000'

one :: Prelude.Char
one =
  '\001'

shift :: Prelude.Bool -> Prelude.Char -> Prelude.Char
shift c a =
  (\f a -> f (Data.Bits.testBit (Data.Char.ord a) 0)
              (Data.Bits.testBit (Data.Char.ord a) 1)
              (Data.Bits.testBit (Data.Char.ord a) 2)
              (Data.Bits.testBit (Data.Char.ord a) 3)
              (Data.Bits.testBit (Data.Char.ord a) 4)
              (Data.Bits.testBit (Data.Char.ord a) 5)
              (Data.Bits.testBit (Data.Char.ord a) 6)
              (Data.Bits.testBit (Data.Char.ord a) 7))
    (\a1 a2 a3 a4 a5 a6 a7 _ ->
    (\b0 b1 b2 b3 b4 b5 b6 b7 -> Data.Char.chr (
      (if b0 then Data.Bits.shiftL 1 0 else 0) Prelude.+
      (if b1 then Data.Bits.shiftL 1 1 else 0) Prelude.+
      (if b2 then Data.Bits.shiftL 1 2 else 0) Prelude.+
      (if b3 then Data.Bits.shiftL 1 3 else 0) Prelude.+
      (if b4 then Data.Bits.shiftL 1 4 else 0) Prelude.+
      (if b5 then Data.Bits.shiftL 1 5 else 0) Prelude.+
      (if b6 then Data.Bits.shiftL 1 6 else 0) Prelude.+
      (if b7 then Data.Bits.shiftL 1 7 else 0)))
    c a1 a2 a3 a4 a5 a6 a7)
    a

eqb_spec :: Prelude.Char -> Prelude.Char -> Bool.Coq_reflect
eqb_spec a b =
  (\f a -> f (Data.Bits.testBit (Data.Char.ord a) 0)
              (Data.Bits.testBit (Data.Char.ord a) 1)
              (Data.Bits.testBit (Data.Char.ord a) 2)
              (Data.Bits.testBit (Data.Char.ord a) 3)
              (Data.Bits.testBit (Data.Char.ord a) 4)
              (Data.Bits.testBit (Data.Char.ord a) 5)
              (Data.Bits.testBit (Data.Char.ord a) 6)
              (Data.Bits.testBit (Data.Char.ord a) 7))
    (\b0 b1 b2 b3 b4 b5 b6 b7 ->
    (\f a -> f (Data.Bits.testBit (Data.Char.ord a) 0)
              (Data.Bits.testBit (Data.Char.ord a) 1)
              (Data.Bits.testBit (Data.Char.ord a) 2)
              (Data.Bits.testBit (Data.Char.ord a) 3)
              (Data.Bits.testBit (Data.Char.ord a) 4)
              (Data.Bits.testBit (Data.Char.ord a) 5)
              (Data.Bits.testBit (Data.Char.ord a) 6)
              (Data.Bits.testBit (Data.Char.ord a) 7))
      (\b8 b9 b10 b11 b12 b13 b14 b15 ->
      case Bool.eqb_spec b0 b8 of {
       Bool.ReflectT ->
        Logic.eq_rec_r b8
          (case Bool.eqb_spec b1 b9 of {
            Bool.ReflectT ->
             Logic.eq_rec_r b9
               (case Bool.eqb_spec b2 b10 of {
                 Bool.ReflectT ->
                  Logic.eq_rec_r b10
                    (case Bool.eqb_spec b3 b11 of {
                      Bool.ReflectT ->
                       Logic.eq_rec_r b11
                         (case Bool.eqb_spec b4 b12 of {
                           Bool.ReflectT ->
                            Logic.eq_rec_r b12
                              (case Bool.eqb_spec b5 b13 of {
                                Bool.ReflectT ->
                                 Logic.eq_rec_r b13
                                   (case Bool.eqb_spec b6 b14 of {
                                     Bool.ReflectT ->
                                      Logic.eq_rec_r b14
                                        (case Bool.eqb_spec b7 b15 of {
                                          Bool.ReflectT ->
                                           Logic.eq_rec_r b15 Bool.ReflectT b7;
                                          Bool.ReflectF -> Bool.ReflectF}) b6;
                                     Bool.ReflectF -> Bool.ReflectF}) b5;
                                Bool.ReflectF -> Bool.ReflectF}) b4;
                           Bool.ReflectF -> Bool.ReflectF}) b3;
                      Bool.ReflectF -> Bool.ReflectF}) b2;
                 Bool.ReflectF -> Bool.ReflectF}) b1;
            Bool.ReflectF -> Bool.ReflectF}) b0;
       Bool.ReflectF -> Bool.ReflectF})
      b)
    a

ascii_of_pos :: Prelude.Integer -> Prelude.Char
ascii_of_pos =
  let {
   loop n p =
     (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
       (\_ -> zero)
       (\n' ->
       (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
         (\p' -> shift Prelude.True (loop n' p'))
         (\p' -> shift Prelude.False (loop n' p'))
         (\_ -> one)
         p)
       n}
  in loop (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
       (Prelude.succ (Prelude.succ (Prelude.succ 0))))))))

ascii_of_N :: BinNums.N -> Prelude.Char
ascii_of_N n =
  case n of {
   BinNums.N0 -> zero;
   BinNums.Npos p -> ascii_of_pos p}

ascii_of_nat :: Prelude.Integer -> Prelude.Char
ascii_of_nat a =
  ascii_of_N (BinNat._N__of_nat a)

coq_N_of_digits :: (([]) Prelude.Bool) -> BinNums.N
coq_N_of_digits l =
  case l of {
   ([]) -> BinNums.N0;
   (:) b l' ->
    BinNat._N__add
      (case b of {
        Prelude.True -> BinNums.Npos 1;
        Prelude.False -> BinNums.N0})
      (BinNat._N__mul (BinNums.Npos ((\x -> 2 Prelude.* x) 1)) (coq_N_of_digits l'))}

coq_N_of_ascii :: Prelude.Char -> BinNums.N
coq_N_of_ascii a =
  (\f a -> f (Data.Bits.testBit (Data.Char.ord a) 0)
              (Data.Bits.testBit (Data.Char.ord a) 1)
              (Data.Bits.testBit (Data.Char.ord a) 2)
              (Data.Bits.testBit (Data.Char.ord a) 3)
              (Data.Bits.testBit (Data.Char.ord a) 4)
              (Data.Bits.testBit (Data.Char.ord a) 5)
              (Data.Bits.testBit (Data.Char.ord a) 6)
              (Data.Bits.testBit (Data.Char.ord a) 7))
    (\a0 a1 a2 a3 a4 a5 a6 a7 ->
    coq_N_of_digits ((:) a0 ((:) a1 ((:) a2 ((:) a3 ((:) a4 ((:) a5 ((:) a6 ((:) a7
      ([]))))))))))
    a

nat_of_ascii :: Prelude.Char -> Prelude.Integer
nat_of_ascii a =
  BinNat._N__to_nat (coq_N_of_ascii a)

coq_Space :: Prelude.Char
coq_Space =
  ' '

coq_DoubleQuote :: Prelude.Char
coq_DoubleQuote =
  '"'

coq_Beep :: Prelude.Char
coq_Beep =
  '\007'

