module Zpower where

import qualified Prelude
import qualified BinPos
import qualified Datatypes
import qualified Logic

__ :: any
__ = Prelude.error "Logical or arity value used"

coq_Zpower_nat :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
coq_Zpower_nat z =
  Datatypes.nat_rect ((\x -> x) 1) (\_ -> (Prelude.*) z)

shift_nat :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
shift_nat n z =
  Datatypes.nat_rect z (\_ x -> (\x -> 2 Prelude.* x) x) n

shift_pos :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
shift_pos n z =
  BinPos._Pos__iter (\x -> (\x -> 2 Prelude.* x) x) z n

shift :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
shift n z =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> z)
    (\p -> BinPos._Pos__iter (\x -> (\x -> 2 Prelude.* x) x) z p)
    (\_ -> z)
    n

two_power_nat :: Prelude.Integer -> Prelude.Integer
two_power_nat n =
  (\x -> x) (shift_nat n 1)

two_power_pos :: Prelude.Integer -> Prelude.Integer
two_power_pos x =
  (\x -> x) (shift_pos x 1)

two_p :: Prelude.Integer -> Prelude.Integer
two_p x =
  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
    (\_ -> (\x -> x) 1)
    (\y -> two_power_pos y)
    (\_ -> 0)
    x

coq_Zdiv_rest_aux :: ((,) ((,) Prelude.Integer Prelude.Integer)
                     Prelude.Integer) -> (,)
                     ((,) Prelude.Integer Prelude.Integer) Prelude.Integer
coq_Zdiv_rest_aux qrd =
  case qrd of {
   (,) p d ->
    case p of {
     (,) q r -> (,)
      ((\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
         (\_ -> (,) 0 r)
         (\p0 ->
         (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
           (\n -> (,) ((\x -> x) n) ((Prelude.+) d r))
           (\n -> (,) ((\x -> x) n) r)
           (\_ -> (,) 0 ((Prelude.+) d r))
           p0)
         (\p0 ->
         (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
           (\n -> (,) ((Prelude.-) (Prelude.negate n) ((\x -> x) 1))
           ((Prelude.+) d r))
           (\n -> (,) (Prelude.negate n) r)
           (\_ -> (,) (Prelude.negate 1) ((Prelude.+) d r))
           p0)
         q) ((Prelude.*) ((\x -> x) ((\x -> 2 Prelude.* x) 1)) d)}}

coq_Zdiv_rest :: Prelude.Integer -> Prelude.Integer -> (,) Prelude.Integer
                 Prelude.Integer
coq_Zdiv_rest x p =
  case BinPos._Pos__iter coq_Zdiv_rest_aux ((,) ((,) x 0) ((\x -> x) 1)) p of {
   (,) qr _ -> qr}

data Zdiv_rest_proofs =
   Zdiv_rest_proof Prelude.Integer Prelude.Integer

coq_Zdiv_rest_proofs_rect :: Prelude.Integer -> Prelude.Integer ->
                             (Prelude.Integer -> Prelude.Integer -> () -> ()
                             -> () -> a1) -> Zdiv_rest_proofs -> a1
coq_Zdiv_rest_proofs_rect _ _ f z =
  case z of {
   Zdiv_rest_proof x x0 -> f x x0 __ __ __}

coq_Zdiv_rest_proofs_rec :: Prelude.Integer -> Prelude.Integer ->
                            (Prelude.Integer -> Prelude.Integer -> () -> ()
                            -> () -> a1) -> Zdiv_rest_proofs -> a1
coq_Zdiv_rest_proofs_rec =
  coq_Zdiv_rest_proofs_rect

coq_Zdiv_rest_correct :: Prelude.Integer -> Prelude.Integer ->
                         Zdiv_rest_proofs
coq_Zdiv_rest_correct x p =
  let {
   p0 = BinPos._Pos__iter coq_Zdiv_rest_aux ((,) ((,) x 0) ((\x -> x) 1)) p}
  in
  case p0 of {
   (,) p1 x0 ->
    case p1 of {
     (,) q r ->
      Logic.eq_rec_r (two_power_pos p) (\_ _ -> Zdiv_rest_proof q r) x0 __ __}}

