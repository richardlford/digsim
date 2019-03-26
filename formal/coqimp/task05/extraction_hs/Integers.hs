module Integers where

import qualified Prelude
import qualified BinInt
import qualified Coqlib
import qualified Zpower

data Coq_comparison =
   Ceq
 | Cne
 | Clt
 | Cle
 | Cgt
 | Cge

_Wordsize_64__wordsize :: Prelude.Integer
_Wordsize_64__wordsize =
  Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
    (Prelude.succ 0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

_Int64__wordsize :: Prelude.Integer
_Int64__wordsize =
  _Wordsize_64__wordsize

_Int64__modulus :: Prelude.Integer
_Int64__modulus =
  Zpower.two_power_nat _Int64__wordsize

_Int64__half_modulus :: Prelude.Integer
_Int64__half_modulus =
  BinInt._Z__div _Int64__modulus ((\x -> x) ((\x -> 2 Prelude.* x) 1))

_Int64__max_signed :: Prelude.Integer
_Int64__max_signed =
  (Prelude.-) _Int64__half_modulus ((\x -> x) 1)

_Int64__min_signed :: Prelude.Integer
_Int64__min_signed =
  BinInt._Z__opp _Int64__half_modulus

type Int64__Coq_int = Prelude.Integer
  -- singleton inductive, whose constructor was mkint
  
_Int64__intval :: Int64__Coq_int -> Prelude.Integer
_Int64__intval i =
  i

_Int64__coq_P_mod_two_p :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
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

