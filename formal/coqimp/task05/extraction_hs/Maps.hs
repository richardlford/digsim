module Maps where

import qualified Prelude

data PTree__Coq_tree a =
   PTree__Leaf
 | PTree__Node (PTree__Coq_tree a) (Prelude.Maybe a) (PTree__Coq_tree a)

type PTree__Coq_t a = PTree__Coq_tree a

_PTree__empty :: PTree__Coq_t a1
_PTree__empty =
  PTree__Leaf

_PTree__get :: Prelude.Integer -> (PTree__Coq_t a1) -> Prelude.Maybe a1
_PTree__get i m =
  case m of {
   PTree__Leaf -> Prelude.Nothing;
   PTree__Node l o r ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\ii -> _PTree__get ii r)
      (\ii -> _PTree__get ii l)
      (\_ -> o)
      i}

_PTree__set :: Prelude.Integer -> a1 -> (PTree__Coq_t a1) -> PTree__Coq_t a1
_PTree__set i v m =
  case m of {
   PTree__Leaf ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\ii -> PTree__Node PTree__Leaf Prelude.Nothing (_PTree__set ii v PTree__Leaf))
      (\ii -> PTree__Node (_PTree__set ii v PTree__Leaf) Prelude.Nothing PTree__Leaf)
      (\_ -> PTree__Node PTree__Leaf (Prelude.Just v) PTree__Leaf)
      i;
   PTree__Node l o r ->
    (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
      (\ii -> PTree__Node l o (_PTree__set ii v r))
      (\ii -> PTree__Node (_PTree__set ii v l) o r)
      (\_ -> PTree__Node l (Prelude.Just v) r)
      i}

_PTree__remove :: Prelude.Integer -> (PTree__Coq_t a1) -> PTree__Coq_t a1
_PTree__remove i m =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\ii ->
    case m of {
     PTree__Leaf -> PTree__Leaf;
     PTree__Node l o r ->
      case l of {
       PTree__Leaf ->
        case o of {
         Prelude.Just _ -> PTree__Node l o (_PTree__remove ii r);
         Prelude.Nothing ->
          case _PTree__remove ii r of {
           PTree__Leaf -> PTree__Leaf;
           PTree__Node t o0 t0 -> PTree__Node PTree__Leaf Prelude.Nothing (PTree__Node t o0 t0)}};
       PTree__Node _ _ _ -> PTree__Node l o (_PTree__remove ii r)}})
    (\ii ->
    case m of {
     PTree__Leaf -> PTree__Leaf;
     PTree__Node l o r ->
      case o of {
       Prelude.Just _ -> PTree__Node (_PTree__remove ii l) o r;
       Prelude.Nothing ->
        case r of {
         PTree__Leaf ->
          case _PTree__remove ii l of {
           PTree__Leaf -> PTree__Leaf;
           PTree__Node t o0 t0 -> PTree__Node (PTree__Node t o0 t0) Prelude.Nothing PTree__Leaf};
         PTree__Node _ _ _ -> PTree__Node (_PTree__remove ii l) o r}}})
    (\_ ->
    case m of {
     PTree__Leaf -> PTree__Leaf;
     PTree__Node l _ r ->
      case l of {
       PTree__Leaf ->
        case r of {
         PTree__Leaf -> PTree__Leaf;
         PTree__Node _ _ _ -> PTree__Node l Prelude.Nothing r};
       PTree__Node _ _ _ -> PTree__Node l Prelude.Nothing r}})
    i

_PTree__bempty :: (PTree__Coq_t a1) -> Prelude.Bool
_PTree__bempty m =
  case m of {
   PTree__Leaf -> Prelude.True;
   PTree__Node l o r ->
    case o of {
     Prelude.Just _ -> Prelude.False;
     Prelude.Nothing -> (Prelude.&&) (_PTree__bempty l) (_PTree__bempty r)}}

_PTree__beq :: (a1 -> a1 -> Prelude.Bool) -> (PTree__Coq_t a1) -> (PTree__Coq_t a1) -> Prelude.Bool
_PTree__beq beqA m1 m2 =
  case m1 of {
   PTree__Leaf -> _PTree__bempty m2;
   PTree__Node l1 o1 r1 ->
    case m2 of {
     PTree__Leaf -> _PTree__bempty m1;
     PTree__Node l2 o2 r2 ->
      (Prelude.&&)
        ((Prelude.&&)
          (case o1 of {
            Prelude.Just y1 ->
             case o2 of {
              Prelude.Just y2 -> beqA y1 y2;
              Prelude.Nothing -> Prelude.False};
            Prelude.Nothing ->
             case o2 of {
              Prelude.Just _ -> Prelude.False;
              Prelude.Nothing -> Prelude.True}}) (_PTree__beq beqA l1 l2)) (_PTree__beq beqA r1 r2)}}

_PTree__prev_append :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer
_PTree__prev_append i j =
  (\fI fO fH n -> if n Prelude.== 1 then fH () else
                   if Prelude.odd n
                   then fI (n `Prelude.div` 2)
                   else fO (n `Prelude.div` 2))
    (\i' -> _PTree__prev_append i' ((\x -> 2 Prelude.* x Prelude.+ 1) j))
    (\i' -> _PTree__prev_append i' ((\x -> 2 Prelude.* x) j))
    (\_ -> j)
    i

_PTree__prev :: Prelude.Integer -> Prelude.Integer
_PTree__prev i =
  _PTree__prev_append i 1

_PTree__coq_Node' :: (PTree__Coq_t a1) -> (Prelude.Maybe a1) -> (PTree__Coq_t a1) -> PTree__Coq_t a1
_PTree__coq_Node' l x r =
  case l of {
   PTree__Leaf ->
    case x of {
     Prelude.Just _ -> PTree__Node l x r;
     Prelude.Nothing ->
      case r of {
       PTree__Leaf -> PTree__Leaf;
       PTree__Node _ _ _ -> PTree__Node l x r}};
   PTree__Node _ _ _ -> PTree__Node l x r}

_PTree__xcombine_l :: ((Prelude.Maybe a1) -> (Prelude.Maybe a2) -> Prelude.Maybe a3) -> (PTree__Coq_t
                      a1) -> PTree__Coq_t a3
_PTree__xcombine_l f m =
  case m of {
   PTree__Leaf -> PTree__Leaf;
   PTree__Node l o r ->
    _PTree__coq_Node' (_PTree__xcombine_l f l) (f o Prelude.Nothing) (_PTree__xcombine_l f r)}

_PTree__xcombine_r :: ((Prelude.Maybe a1) -> (Prelude.Maybe a2) -> Prelude.Maybe a3) -> (PTree__Coq_t
                      a2) -> PTree__Coq_t a3
_PTree__xcombine_r f m =
  case m of {
   PTree__Leaf -> PTree__Leaf;
   PTree__Node l o r ->
    _PTree__coq_Node' (_PTree__xcombine_r f l) (f Prelude.Nothing o) (_PTree__xcombine_r f r)}

_PTree__combine :: ((Prelude.Maybe a1) -> (Prelude.Maybe a2) -> Prelude.Maybe a3) -> (PTree__Coq_t
                   a1) -> (PTree__Coq_t a2) -> PTree__Coq_t a3
_PTree__combine f m1 m2 =
  case m1 of {
   PTree__Leaf -> _PTree__xcombine_r f m2;
   PTree__Node l1 o1 r1 ->
    case m2 of {
     PTree__Leaf -> _PTree__xcombine_l f m1;
     PTree__Node l2 o2 r2 ->
      _PTree__coq_Node' (_PTree__combine f l1 l2) (f o1 o2) (_PTree__combine f r1 r2)}}

_PTree__xelements :: (PTree__Coq_t a1) -> Prelude.Integer -> (([]) ((,) Prelude.Integer a1)) -> ([])
                     ((,) Prelude.Integer a1)
_PTree__xelements m i k =
  case m of {
   PTree__Leaf -> k;
   PTree__Node l o r ->
    case o of {
     Prelude.Just x ->
      _PTree__xelements l ((\x -> 2 Prelude.* x) i) ((:) ((,) (_PTree__prev i) x)
        (_PTree__xelements r ((\x -> 2 Prelude.* x Prelude.+ 1) i) k));
     Prelude.Nothing ->
      _PTree__xelements l ((\x -> 2 Prelude.* x) i)
        (_PTree__xelements r ((\x -> 2 Prelude.* x Prelude.+ 1) i) k)}}

_PTree__elements :: (PTree__Coq_t a1) -> ([]) ((,) Prelude.Integer a1)
_PTree__elements m =
  _PTree__xelements m 1 ([])

