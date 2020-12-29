module List0 where

import qualified Prelude
import qualified Datatypes
import qualified Logic
import qualified Specif

__ :: any
__ = Prelude.error "Logical or arity value used"

hd :: a1 -> (([]) a1) -> a1
hd default0 l =
  case l of {
   ([]) -> default0;
   (:) x _ -> x}

hd_error :: (([]) a1) -> Prelude.Maybe a1
hd_error l =
  case l of {
   ([]) -> Prelude.Nothing;
   (:) x _ -> Prelude.Just x}

tl :: (([]) a1) -> ([]) a1
tl l =
  case l of {
   ([]) -> ([]);
   (:) _ m -> m}

destruct_list :: (([]) a1) -> Prelude.Maybe (Specif.Coq_sigT a1 (([]) a1))
destruct_list l =
  Datatypes.list_rect Prelude.Nothing (\a tail _ -> Prelude.Just (Specif.Coq_existT
    a tail)) l

in_dec :: (a1 -> a1 -> Prelude.Bool) -> a1 -> (([]) a1) -> Prelude.Bool
in_dec h a l =
  Datatypes.list_rec Prelude.False (\a0 _ iHl ->
    let {s = h a0 a} in
    case s of {
     Prelude.True -> Prelude.True;
     Prelude.False -> iHl}) l

nth :: Prelude.Integer -> (([]) a1) -> a1 -> a1
nth n l default0 =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> case l of {
            ([]) -> default0;
            (:) x _ -> x})
    (\m -> case l of {
            ([]) -> default0;
            (:) _ t -> nth m t default0})
    n

nth_ok :: Prelude.Integer -> (([]) a1) -> a1 -> Prelude.Bool
nth_ok n l default0 =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> case l of {
            ([]) -> Prelude.False;
            (:) _ _ -> Prelude.True})
    (\m -> case l of {
            ([]) -> Prelude.False;
            (:) _ t -> nth_ok m t default0})
    n

nth_in_or_default :: Prelude.Integer -> (([]) a1) -> a1 -> Prelude.Bool
nth_in_or_default n l _ =
  Datatypes.list_rec (\_ -> Prelude.False) (\_ _ iHl n0 ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> Prelude.True)
      (\n1 -> iHl n1)
      n0) l n

nth_error :: (([]) a1) -> Prelude.Integer -> Prelude.Maybe a1
nth_error l n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> case l of {
            ([]) -> Prelude.Nothing;
            (:) x _ -> Prelude.Just x})
    (\n0 -> case l of {
             ([]) -> Prelude.Nothing;
             (:) _ l0 -> nth_error l0 n0})
    n

nth_default :: a1 -> (([]) a1) -> Prelude.Integer -> a1
nth_default default0 l n =
  case nth_error l n of {
   Prelude.Just x -> x;
   Prelude.Nothing -> default0}

remove :: (a1 -> a1 -> Prelude.Bool) -> a1 -> (([]) a1) -> ([]) a1
remove eq_dec x l =
  case l of {
   ([]) -> ([]);
   (:) y tl0 ->
    case eq_dec x y of {
     Prelude.True -> remove eq_dec x tl0;
     Prelude.False -> (:) y (remove eq_dec x tl0)}}

last :: (([]) a1) -> a1 -> a1
last l d =
  case l of {
   ([]) -> d;
   (:) a l0 -> case l0 of {
                ([]) -> a;
                (:) _ _ -> last l0 d}}

removelast :: (([]) a1) -> ([]) a1
removelast l =
  case l of {
   ([]) -> ([]);
   (:) a l0 -> case l0 of {
                ([]) -> ([]);
                (:) _ _ -> (:) a (removelast l0)}}

exists_last :: (([]) a1) -> Specif.Coq_sigT (([]) a1) a1
exists_last l =
  Datatypes.list_rect (\_ -> Prelude.error "absurd case") (\a l0 iHl _ ->
    case l0 of {
     ([]) -> Specif.Coq_existT ([]) a;
     (:) a0 l1 ->
      let {s = iHl __} in
      case s of {
       Specif.Coq_existT l' s0 ->
        Logic.eq_rect_r (Datatypes.app l' ((:) s0 ([]))) (Specif.Coq_existT ((:) a
          l') s0) ((:) a0 l1)}}) l __

count_occ :: (a1 -> a1 -> Prelude.Bool) -> (([]) a1) -> a1 -> Prelude.Integer
count_occ eq_dec l x =
  case l of {
   ([]) -> 0;
   (:) y tl0 ->
    let {n = count_occ eq_dec tl0 x} in
    case eq_dec y x of {
     Prelude.True -> Prelude.succ n;
     Prelude.False -> n}}

rev :: (([]) a1) -> ([]) a1
rev l =
  case l of {
   ([]) -> ([]);
   (:) x l' -> Datatypes.app (rev l') ((:) x ([]))}

rev_append :: (([]) a1) -> (([]) a1) -> ([]) a1
rev_append l l' =
  case l of {
   ([]) -> l';
   (:) a l0 -> rev_append l0 ((:) a l')}

rev' :: (([]) a1) -> ([]) a1
rev' l =
  rev_append l ([])

concat :: (([]) (([]) a1)) -> ([]) a1
concat l =
  case l of {
   ([]) -> ([]);
   (:) x l0 -> Datatypes.app x (concat l0)}

list_eq_dec :: (a1 -> a1 -> Prelude.Bool) -> (([]) a1) -> (([]) a1) -> Prelude.Bool
list_eq_dec eq_dec l l' =
  Datatypes.list_rect (\x ->
    case x of {
     ([]) -> Prelude.True;
     (:) _ _ -> Prelude.False}) (\a _ x x0 ->
    case x0 of {
     ([]) -> Prelude.False;
     (:) a0 l1 ->
      Specif.sumbool_rec (\_ ->
        Specif.sumbool_rec (\_ -> Prelude.True) (\_ -> Prelude.False) (x l1)) (\_ ->
        Prelude.False) (eq_dec a a0)}) l l'

map :: (a1 -> a2) -> (([]) a1) -> ([]) a2
map f l =
  case l of {
   ([]) -> ([]);
   (:) a t -> (:) (f a) (map f t)}

flat_map :: (a1 -> ([]) a2) -> (([]) a1) -> ([]) a2
flat_map f l =
  case l of {
   ([]) -> ([]);
   (:) x t -> Datatypes.app (f x) (flat_map f t)}

fold_left :: (a1 -> a2 -> a1) -> (([]) a2) -> a1 -> a1
fold_left f l a0 =
  case l of {
   ([]) -> a0;
   (:) b t -> fold_left f t (f a0 b)}

fold_right :: (a2 -> a1 -> a1) -> a1 -> (([]) a2) -> a1
fold_right f a0 l =
  case l of {
   ([]) -> a0;
   (:) b t -> f b (fold_right f a0 t)}

list_power :: (([]) a1) -> (([]) a2) -> ([]) (([]) ((,) a1 a2))
list_power l l' =
  case l of {
   ([]) -> (:) ([]) ([]);
   (:) x t -> flat_map (\f -> map (\y -> (:) ((,) x y) f) l') (list_power t l')}

existsb :: (a1 -> Prelude.Bool) -> (([]) a1) -> Prelude.Bool
existsb f l =
  case l of {
   ([]) -> Prelude.False;
   (:) a l0 -> (Prelude.||) (f a) (existsb f l0)}

forallb :: (a1 -> Prelude.Bool) -> (([]) a1) -> Prelude.Bool
forallb f l =
  case l of {
   ([]) -> Prelude.True;
   (:) a l0 -> (Prelude.&&) (f a) (forallb f l0)}

filter :: (a1 -> Prelude.Bool) -> (([]) a1) -> ([]) a1
filter f l =
  case l of {
   ([]) -> ([]);
   (:) x l0 ->
    case f x of {
     Prelude.True -> (:) x (filter f l0);
     Prelude.False -> filter f l0}}

find :: (a1 -> Prelude.Bool) -> (([]) a1) -> Prelude.Maybe a1
find f l =
  case l of {
   ([]) -> Prelude.Nothing;
   (:) x tl0 ->
    case f x of {
     Prelude.True -> Prelude.Just x;
     Prelude.False -> find f tl0}}

partition :: (a1 -> Prelude.Bool) -> (([]) a1) -> (,) (([]) a1) (([]) a1)
partition f l =
  case l of {
   ([]) -> (,) ([]) ([]);
   (:) x tl0 ->
    case partition f tl0 of {
     (,) g d ->
      case f x of {
       Prelude.True -> (,) ((:) x g) d;
       Prelude.False -> (,) g ((:) x d)}}}

split :: (([]) ((,) a1 a2)) -> (,) (([]) a1) (([]) a2)
split l =
  case l of {
   ([]) -> (,) ([]) ([]);
   (:) p tl0 ->
    case p of {
     (,) x y ->
      case split tl0 of {
       (,) left right -> (,) ((:) x left) ((:) y right)}}}

combine :: (([]) a1) -> (([]) a2) -> ([]) ((,) a1 a2)
combine l l' =
  case l of {
   ([]) -> ([]);
   (:) x tl0 ->
    case l' of {
     ([]) -> ([]);
     (:) y tl' -> (:) ((,) x y) (combine tl0 tl')}}

list_prod :: (([]) a1) -> (([]) a2) -> ([]) ((,) a1 a2)
list_prod l l' =
  case l of {
   ([]) -> ([]);
   (:) x t -> Datatypes.app (map (\y -> (,) x y) l') (list_prod t l')}

firstn :: Prelude.Integer -> (([]) a1) -> ([]) a1
firstn n l =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> ([]))
    (\n0 -> case l of {
             ([]) -> ([]);
             (:) a l0 -> (:) a (firstn n0 l0)})
    n

skipn :: Prelude.Integer -> (([]) a1) -> ([]) a1
skipn n l =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> l)
    (\n0 -> case l of {
             ([]) -> ([]);
             (:) _ l0 -> skipn n0 l0})
    n

nodup :: (a1 -> a1 -> Prelude.Bool) -> (([]) a1) -> ([]) a1
nodup decA l =
  case l of {
   ([]) -> ([]);
   (:) x xs ->
    case in_dec decA x xs of {
     Prelude.True -> nodup decA xs;
     Prelude.False -> (:) x (nodup decA xs)}}

seq :: Prelude.Integer -> Prelude.Integer -> ([]) Prelude.Integer
seq start len =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> ([]))
    (\len0 -> (:) start (seq (Prelude.succ start) len0))
    len

coq_Exists_dec :: (([]) a1) -> (a1 -> Prelude.Bool) -> Prelude.Bool
coq_Exists_dec l pdec =
  Datatypes.list_rec Prelude.False (\a _ hrec ->
    case hrec of {
     Prelude.True -> Prelude.True;
     Prelude.False -> pdec a}) l

coq_Forall_rect :: a2 -> (a1 -> (([]) a1) -> () -> a2) -> (([]) a1) -> a2
coq_Forall_rect h h' l =
  Datatypes.list_rect (\_ -> h) (\a l0 _ _ -> h' a l0 __) l __

coq_Forall_dec :: (a1 -> Prelude.Bool) -> (([]) a1) -> Prelude.Bool
coq_Forall_dec pdec l =
  Datatypes.list_rec Prelude.True (\a _ hrec ->
    case hrec of {
     Prelude.True -> pdec a;
     Prelude.False -> Prelude.False}) l

coq_Forall_Exists_dec :: (a1 -> Prelude.Bool) -> (([]) a1) -> Prelude.Bool
coq_Forall_Exists_dec =
  coq_Forall_dec

repeat :: a1 -> Prelude.Integer -> ([]) a1
repeat x n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> ([]))
    (\k -> (:) x (repeat x k))
    n

