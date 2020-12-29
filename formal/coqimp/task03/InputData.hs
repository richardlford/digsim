module InputData where

import qualified Prelude
import qualified Advance_States
import qualified Default_Data
import qualified Fin
import qualified Floats
import qualified List0
import qualified State
import qualified String0
import qualified Vector
import qualified Float_text_io
import qualified Data.Char
import qualified Data.Bits

stripCommentsToken :: Prelude.String -> Prelude.Integer -> Prelude.String
stripCommentsToken s parens =
  case s of {
   ([]) -> ([]);
   (:) hd tl ->
    (\f a -> f (Data.Bits.testBit (Data.Char.ord a) 0)
              (Data.Bits.testBit (Data.Char.ord a) 1)
              (Data.Bits.testBit (Data.Char.ord a) 2)
              (Data.Bits.testBit (Data.Char.ord a) 3)
              (Data.Bits.testBit (Data.Char.ord a) 4)
              (Data.Bits.testBit (Data.Char.ord a) 5)
              (Data.Bits.testBit (Data.Char.ord a) 6)
              (Data.Bits.testBit (Data.Char.ord a) 7))
      (\b b0 b1 b2 b3 b4 b5 b6 ->
      case b of {
       Prelude.True ->
        case b0 of {
         Prelude.True ->
          (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
            (\_ -> (:) hd (stripCommentsToken tl parens))
            (\_ -> stripCommentsToken tl parens)
            (\_ -> stripCommentsToken tl parens)
            parens;
         Prelude.False ->
          case b1 of {
           Prelude.True ->
            (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
              (\_ -> (:) hd (stripCommentsToken tl parens))
              (\_ -> stripCommentsToken tl parens)
              (\_ -> stripCommentsToken tl parens)
              parens;
           Prelude.False ->
            case b2 of {
             Prelude.True ->
              case b3 of {
               Prelude.True ->
                (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
                  (\_ -> (:) hd (stripCommentsToken tl parens))
                  (\_ -> stripCommentsToken tl parens)
                  (\_ -> stripCommentsToken tl parens)
                  parens;
               Prelude.False ->
                case b4 of {
                 Prelude.True ->
                  case b5 of {
                   Prelude.True ->
                    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
                      (\_ -> (:) hd (stripCommentsToken tl parens))
                      (\_ -> stripCommentsToken tl parens)
                      (\_ -> stripCommentsToken tl parens)
                      parens;
                   Prelude.False ->
                    case b6 of {
                     Prelude.True ->
                      (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
                        (\_ -> (:) hd (stripCommentsToken tl parens))
                        (\_ -> stripCommentsToken tl parens)
                        (\_ -> stripCommentsToken tl parens)
                        parens;
                     Prelude.False ->
                      stripCommentsToken tl ((Prelude.-) parens ((\x -> x) 1))}};
                 Prelude.False ->
                  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
                    (\_ -> (:) hd (stripCommentsToken tl parens))
                    (\_ -> stripCommentsToken tl parens)
                    (\_ -> stripCommentsToken tl parens)
                    parens}};
             Prelude.False ->
              (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
                (\_ -> (:) hd (stripCommentsToken tl parens))
                (\_ -> stripCommentsToken tl parens)
                (\_ -> stripCommentsToken tl parens)
                parens}}};
       Prelude.False ->
        case b0 of {
         Prelude.True ->
          (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
            (\_ -> (:) hd (stripCommentsToken tl parens))
            (\_ -> stripCommentsToken tl parens)
            (\_ -> stripCommentsToken tl parens)
            parens;
         Prelude.False ->
          case b1 of {
           Prelude.True ->
            (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
              (\_ -> (:) hd (stripCommentsToken tl parens))
              (\_ -> stripCommentsToken tl parens)
              (\_ -> stripCommentsToken tl parens)
              parens;
           Prelude.False ->
            case b2 of {
             Prelude.True ->
              case b3 of {
               Prelude.True ->
                (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
                  (\_ -> (:) hd (stripCommentsToken tl parens))
                  (\_ -> stripCommentsToken tl parens)
                  (\_ -> stripCommentsToken tl parens)
                  parens;
               Prelude.False ->
                case b4 of {
                 Prelude.True ->
                  case b5 of {
                   Prelude.True ->
                    (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
                      (\_ -> (:) hd (stripCommentsToken tl parens))
                      (\_ -> stripCommentsToken tl parens)
                      (\_ -> stripCommentsToken tl parens)
                      parens;
                   Prelude.False ->
                    case b6 of {
                     Prelude.True ->
                      (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
                        (\_ -> (:) hd (stripCommentsToken tl parens))
                        (\_ -> stripCommentsToken tl parens)
                        (\_ -> stripCommentsToken tl parens)
                        parens;
                     Prelude.False ->
                      stripCommentsToken tl ((Prelude.+) parens ((\x -> x) 1))}};
                 Prelude.False ->
                  (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
                    (\_ -> (:) hd (stripCommentsToken tl parens))
                    (\_ -> stripCommentsToken tl parens)
                    (\_ -> stripCommentsToken tl parens)
                    parens}};
             Prelude.False ->
              (\fO fP fN n -> if n Prelude.== 0 then fO () else
                   if n Prelude.> 0 then fP n else
                   fN (Prelude.negate n))
                (\_ -> (:) hd (stripCommentsToken tl parens))
                (\_ -> stripCommentsToken tl parens)
                (\_ -> stripCommentsToken tl parens)
                parens}}}})
      hd}

countParensToken :: Prelude.String -> Prelude.Integer -> Prelude.Integer
countParensToken s p =
  case s of {
   ([]) -> p;
   (:) hd tl ->
    (\f a -> f (Data.Bits.testBit (Data.Char.ord a) 0)
              (Data.Bits.testBit (Data.Char.ord a) 1)
              (Data.Bits.testBit (Data.Char.ord a) 2)
              (Data.Bits.testBit (Data.Char.ord a) 3)
              (Data.Bits.testBit (Data.Char.ord a) 4)
              (Data.Bits.testBit (Data.Char.ord a) 5)
              (Data.Bits.testBit (Data.Char.ord a) 6)
              (Data.Bits.testBit (Data.Char.ord a) 7))
      (\b b0 b1 b2 b3 b4 b5 b6 ->
      case b of {
       Prelude.True ->
        case b0 of {
         Prelude.True -> countParensToken tl p;
         Prelude.False ->
          case b1 of {
           Prelude.True -> countParensToken tl p;
           Prelude.False ->
            case b2 of {
             Prelude.True ->
              case b3 of {
               Prelude.True -> countParensToken tl p;
               Prelude.False ->
                case b4 of {
                 Prelude.True ->
                  case b5 of {
                   Prelude.True -> countParensToken tl p;
                   Prelude.False ->
                    case b6 of {
                     Prelude.True -> countParensToken tl p;
                     Prelude.False ->
                      countParensToken tl ((Prelude.-) p ((\x -> x) 1))}};
                 Prelude.False -> countParensToken tl p}};
             Prelude.False -> countParensToken tl p}}};
       Prelude.False ->
        case b0 of {
         Prelude.True -> countParensToken tl p;
         Prelude.False ->
          case b1 of {
           Prelude.True -> countParensToken tl p;
           Prelude.False ->
            case b2 of {
             Prelude.True ->
              case b3 of {
               Prelude.True -> countParensToken tl p;
               Prelude.False ->
                case b4 of {
                 Prelude.True ->
                  case b5 of {
                   Prelude.True -> countParensToken tl p;
                   Prelude.False ->
                    case b6 of {
                     Prelude.True -> countParensToken tl p;
                     Prelude.False ->
                      countParensToken tl ((Prelude.+) p ((\x -> x) 1))}};
                 Prelude.False -> countParensToken tl p}};
             Prelude.False -> countParensToken tl p}}}})
      hd}

countParensLine :: (([]) Prelude.String) -> Prelude.Integer -> Prelude.Integer
countParensLine ss parens =
  List0.fold_right (Prelude.+) 0 (List0.map (\x0 -> countParensToken x0 parens) ss)

stripCommentsLine :: (([]) Prelude.String) -> Prelude.Integer -> ([]) Prelude.String
stripCommentsLine ss parens =
  case ss of {
   ([]) -> ([]);
   (:) hd tl ->
    case stripCommentsToken hd parens of {
     ([]) -> stripCommentsLine tl (countParensToken hd parens);
     (:) a s0 -> (:) ((:) a s0) (stripCommentsLine tl (countParensToken hd parens))}}

stripCommentsFile :: (([]) (([]) Prelude.String)) -> Prelude.Integer -> ([])
                     (([]) Prelude.String)
stripCommentsFile ss parens =
  case ss of {
   ([]) -> ([]);
   (:) hd tl ->
    case stripCommentsLine hd parens of {
     ([]) -> stripCommentsFile tl (countParensLine hd parens);
     (:) s0 l -> (:) ((:) s0 l) (stripCommentsFile tl (countParensLine hd parens))}}

countParensFile :: (([]) (([]) Prelude.String)) -> Prelude.Integer ->
                   Prelude.Integer
countParensFile ss parens =
  List0.fold_right (Prelude.+) 0 (List0.map (\x0 -> countParensLine x0 parens) ss)

stripComments :: (([]) (([]) Prelude.String)) -> ([]) (([]) Prelude.String)
stripComments ss =
  stripCommentsFile ss 0

endsWith :: Prelude.String -> Prelude.Char -> Prelude.Bool
endsWith s c =
  case s of {
   ([]) -> Prelude.False;
   (:) a tl -> case tl of {
                ([]) -> (Prelude.==) a c;
                (:) _ _ -> endsWith tl c}}

startsWith :: Prelude.String -> Prelude.Char -> Prelude.Bool
startsWith s c =
  case s of {
   ([]) -> Prelude.False;
   (:) a _ -> (Prelude.==) a c}

removeLeading :: Prelude.String -> Prelude.String
removeLeading s =
  case s of {
   ([]) -> ([]);
   (:) hd tl ->
    (\f a -> f (Data.Bits.testBit (Data.Char.ord a) 0)
              (Data.Bits.testBit (Data.Char.ord a) 1)
              (Data.Bits.testBit (Data.Char.ord a) 2)
              (Data.Bits.testBit (Data.Char.ord a) 3)
              (Data.Bits.testBit (Data.Char.ord a) 4)
              (Data.Bits.testBit (Data.Char.ord a) 5)
              (Data.Bits.testBit (Data.Char.ord a) 6)
              (Data.Bits.testBit (Data.Char.ord a) 7))
      (\b b0 b1 b2 b3 b4 b5 b6 ->
      case b of {
       Prelude.True ->
        case b0 of {
         Prelude.True ->
          case b1 of {
           Prelude.True -> s;
           Prelude.False ->
            case b2 of {
             Prelude.True ->
              case b3 of {
               Prelude.True -> s;
               Prelude.False ->
                case b4 of {
                 Prelude.True -> s;
                 Prelude.False ->
                  case b5 of {
                   Prelude.True -> s;
                   Prelude.False ->
                    case b6 of {
                     Prelude.True -> s;
                     Prelude.False -> removeLeading tl}}}};
             Prelude.False -> s}};
         Prelude.False ->
          case b2 of {
           Prelude.True ->
            case b3 of {
             Prelude.True -> s;
             Prelude.False ->
              case b4 of {
               Prelude.True -> s;
               Prelude.False ->
                case b5 of {
                 Prelude.True -> s;
                 Prelude.False ->
                  case b6 of {
                   Prelude.True -> s;
                   Prelude.False -> removeLeading tl}}}};
           Prelude.False -> s}};
       Prelude.False ->
        case b0 of {
         Prelude.True ->
          case b1 of {
           Prelude.True -> s;
           Prelude.False ->
            case b2 of {
             Prelude.True ->
              case b3 of {
               Prelude.True -> s;
               Prelude.False ->
                case b4 of {
                 Prelude.True -> s;
                 Prelude.False ->
                  case b5 of {
                   Prelude.True -> s;
                   Prelude.False ->
                    case b6 of {
                     Prelude.True -> s;
                     Prelude.False -> removeLeading tl}}}};
             Prelude.False -> s}};
         Prelude.False ->
          case b1 of {
           Prelude.True ->
            case b2 of {
             Prelude.True ->
              case b3 of {
               Prelude.True -> s;
               Prelude.False ->
                case b4 of {
                 Prelude.True -> s;
                 Prelude.False ->
                  case b5 of {
                   Prelude.True -> s;
                   Prelude.False ->
                    case b6 of {
                     Prelude.True -> s;
                     Prelude.False -> removeLeading tl}}}};
             Prelude.False -> s};
           Prelude.False ->
            case b2 of {
             Prelude.True -> s;
             Prelude.False ->
              case b3 of {
               Prelude.True -> s;
               Prelude.False ->
                case b4 of {
                 Prelude.True ->
                  case b5 of {
                   Prelude.True -> s;
                   Prelude.False ->
                    case b6 of {
                     Prelude.True -> s;
                     Prelude.False -> removeLeading tl}};
                 Prelude.False -> s}}}}}})
      hd}

isLetter :: Prelude.Char -> Prelude.Bool
isLetter c =
  (\f a -> f (Data.Bits.testBit (Data.Char.ord a) 0)
              (Data.Bits.testBit (Data.Char.ord a) 1)
              (Data.Bits.testBit (Data.Char.ord a) 2)
              (Data.Bits.testBit (Data.Char.ord a) 3)
              (Data.Bits.testBit (Data.Char.ord a) 4)
              (Data.Bits.testBit (Data.Char.ord a) 5)
              (Data.Bits.testBit (Data.Char.ord a) 6)
              (Data.Bits.testBit (Data.Char.ord a) 7))
    (\a b c0 d b0 _ b1 b2 ->
    case a of {
     Prelude.True ->
      case b of {
       Prelude.True ->
        case d of {
         Prelude.True ->
          case b0 of {
           Prelude.True -> Prelude.False;
           Prelude.False ->
            case b1 of {
             Prelude.True ->
              case b2 of {
               Prelude.True -> Prelude.False;
               Prelude.False -> Prelude.True};
             Prelude.False -> Prelude.False}};
         Prelude.False ->
          case b1 of {
           Prelude.True ->
            case b2 of {
             Prelude.True -> Prelude.False;
             Prelude.False -> Prelude.True};
           Prelude.False -> Prelude.False}};
       Prelude.False ->
        case c0 of {
         Prelude.True ->
          case d of {
           Prelude.True ->
            case b0 of {
             Prelude.True -> Prelude.False;
             Prelude.False ->
              case b1 of {
               Prelude.True ->
                case b2 of {
                 Prelude.True -> Prelude.False;
                 Prelude.False -> Prelude.True};
               Prelude.False -> Prelude.False}};
           Prelude.False ->
            case b1 of {
             Prelude.True ->
              case b2 of {
               Prelude.True -> Prelude.False;
               Prelude.False -> Prelude.True};
             Prelude.False -> Prelude.False}};
         Prelude.False ->
          case b1 of {
           Prelude.True ->
            case b2 of {
             Prelude.True -> Prelude.False;
             Prelude.False -> Prelude.True};
           Prelude.False -> Prelude.False}}};
     Prelude.False ->
      case b of {
       Prelude.True ->
        case c0 of {
         Prelude.True ->
          case d of {
           Prelude.True ->
            case b0 of {
             Prelude.True -> Prelude.False;
             Prelude.False ->
              case b1 of {
               Prelude.True ->
                case b2 of {
                 Prelude.True -> Prelude.False;
                 Prelude.False -> Prelude.True};
               Prelude.False -> Prelude.False}};
           Prelude.False ->
            case b1 of {
             Prelude.True ->
              case b2 of {
               Prelude.True -> Prelude.False;
               Prelude.False -> Prelude.True};
             Prelude.False -> Prelude.False}};
         Prelude.False ->
          case b1 of {
           Prelude.True ->
            case b2 of {
             Prelude.True -> Prelude.False;
             Prelude.False -> Prelude.True};
           Prelude.False -> Prelude.False}};
       Prelude.False ->
        case c0 of {
         Prelude.True ->
          case d of {
           Prelude.True ->
            case b0 of {
             Prelude.True -> Prelude.False;
             Prelude.False ->
              case b1 of {
               Prelude.True ->
                case b2 of {
                 Prelude.True -> Prelude.False;
                 Prelude.False -> Prelude.True};
               Prelude.False -> Prelude.False}};
           Prelude.False ->
            case b1 of {
             Prelude.True ->
              case b2 of {
               Prelude.True -> Prelude.False;
               Prelude.False -> Prelude.True};
             Prelude.False -> Prelude.False}};
         Prelude.False ->
          case d of {
           Prelude.True ->
            case b1 of {
             Prelude.True ->
              case b2 of {
               Prelude.True -> Prelude.False;
               Prelude.False -> Prelude.True};
             Prelude.False -> Prelude.False};
           Prelude.False ->
            case b0 of {
             Prelude.True ->
              case b1 of {
               Prelude.True ->
                case b2 of {
                 Prelude.True -> Prelude.False;
                 Prelude.False -> Prelude.True};
               Prelude.False -> Prelude.False};
             Prelude.False -> Prelude.False}}}}})
    c

capitalize :: Prelude.Char -> Prelude.Char
capitalize c =
  case isLetter c of {
   Prelude.True ->
    (\f a -> f (Data.Bits.testBit (Data.Char.ord a) 0)
              (Data.Bits.testBit (Data.Char.ord a) 1)
              (Data.Bits.testBit (Data.Char.ord a) 2)
              (Data.Bits.testBit (Data.Char.ord a) 3)
              (Data.Bits.testBit (Data.Char.ord a) 4)
              (Data.Bits.testBit (Data.Char.ord a) 5)
              (Data.Bits.testBit (Data.Char.ord a) 6)
              (Data.Bits.testBit (Data.Char.ord a) 7))
      (\a b c0 d e _ g h ->
      (\b0 b1 b2 b3 b4 b5 b6 b7 -> Data.Char.chr (
      (if b0 then Data.Bits.shiftL 1 0 else 0) Prelude.+
      (if b1 then Data.Bits.shiftL 1 1 else 0) Prelude.+
      (if b2 then Data.Bits.shiftL 1 2 else 0) Prelude.+
      (if b3 then Data.Bits.shiftL 1 3 else 0) Prelude.+
      (if b4 then Data.Bits.shiftL 1 4 else 0) Prelude.+
      (if b5 then Data.Bits.shiftL 1 5 else 0) Prelude.+
      (if b6 then Data.Bits.shiftL 1 6 else 0) Prelude.+
      (if b7 then Data.Bits.shiftL 1 7 else 0)))
      a b c0 d e Prelude.False g h)
      c;
   Prelude.False -> c}

stringToList :: Prelude.String -> ([]) Prelude.Char
stringToList s =
  case s of {
   ([]) -> ([]);
   (:) hd tl -> (:) hd (stringToList tl)}

listToString :: (([]) Prelude.Char) -> Prelude.String
listToString s =
  case s of {
   ([]) -> ([]);
   (:) hd tl -> (:) hd (listToString tl)}

stringMap :: (Prelude.Char -> Prelude.Char) -> Prelude.String -> Prelude.String
stringMap f s =
  listToString (List0.map f (stringToList s))

reverseString :: Prelude.String -> Prelude.String
reverseString s =
  listToString (List0.rev (stringToList s))

clean :: Prelude.String -> Prelude.String
clean s =
  reverseString
    (removeLeading (reverseString (removeLeading (stringMap capitalize s))))

prepare :: (([]) (([]) Prelude.String)) -> ([]) (([]) Prelude.String)
prepare f =
  List0.map (List0.map clean) (stripComments f)

stringToNatHelper :: Prelude.String -> Prelude.Maybe Prelude.Integer
stringToNatHelper s =
  case s of {
   ([]) -> Prelude.Just 0;
   (:) hd tl ->
    case stringToNatHelper tl of {
     Prelude.Just x0 ->
      (\f a -> f (Data.Bits.testBit (Data.Char.ord a) 0)
              (Data.Bits.testBit (Data.Char.ord a) 1)
              (Data.Bits.testBit (Data.Char.ord a) 2)
              (Data.Bits.testBit (Data.Char.ord a) 3)
              (Data.Bits.testBit (Data.Char.ord a) 4)
              (Data.Bits.testBit (Data.Char.ord a) 5)
              (Data.Bits.testBit (Data.Char.ord a) 6)
              (Data.Bits.testBit (Data.Char.ord a) 7))
        (\b b0 b1 b2 b3 b4 b5 b6 ->
        case b of {
         Prelude.True ->
          case b0 of {
           Prelude.True ->
            case b1 of {
             Prelude.True ->
              case b2 of {
               Prelude.True -> Prelude.Nothing;
               Prelude.False ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just
                        ((Prelude.+)
                          ((Prelude.*) (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ 0))))))))))
                            x0) (Prelude.succ (Prelude.succ (Prelude.succ
                          (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                          0))))))))}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing}};
             Prelude.False ->
              case b2 of {
               Prelude.True -> Prelude.Nothing;
               Prelude.False ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just
                        ((Prelude.+)
                          ((Prelude.*) (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ 0))))))))))
                            x0) (Prelude.succ (Prelude.succ (Prelude.succ 0))))}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing}}};
           Prelude.False ->
            case b1 of {
             Prelude.True ->
              case b2 of {
               Prelude.True -> Prelude.Nothing;
               Prelude.False ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just
                        ((Prelude.+)
                          ((Prelude.*) (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ 0))))))))))
                            x0) (Prelude.succ (Prelude.succ (Prelude.succ
                          (Prelude.succ (Prelude.succ 0))))))}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing}};
             Prelude.False ->
              case b2 of {
               Prelude.True ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just
                        ((Prelude.+)
                          ((Prelude.*) (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ 0))))))))))
                            x0) (Prelude.succ (Prelude.succ (Prelude.succ
                          (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                          (Prelude.succ (Prelude.succ 0))))))))))}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing};
               Prelude.False ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just
                        ((Prelude.+)
                          ((Prelude.*) (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ 0))))))))))
                            x0) (Prelude.succ 0))}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing}}}};
         Prelude.False ->
          case b0 of {
           Prelude.True ->
            case b1 of {
             Prelude.True ->
              case b2 of {
               Prelude.True -> Prelude.Nothing;
               Prelude.False ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just
                        ((Prelude.+)
                          ((Prelude.*) (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ 0))))))))))
                            x0) (Prelude.succ (Prelude.succ (Prelude.succ
                          (Prelude.succ (Prelude.succ (Prelude.succ 0)))))))}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing}};
             Prelude.False ->
              case b2 of {
               Prelude.True -> Prelude.Nothing;
               Prelude.False ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just
                        ((Prelude.+)
                          ((Prelude.*) (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ 0))))))))))
                            x0) (Prelude.succ (Prelude.succ 0)))}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing}}};
           Prelude.False ->
            case b1 of {
             Prelude.True ->
              case b2 of {
               Prelude.True -> Prelude.Nothing;
               Prelude.False ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just
                        ((Prelude.+)
                          ((Prelude.*) (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ 0))))))))))
                            x0) (Prelude.succ (Prelude.succ (Prelude.succ
                          (Prelude.succ 0)))))}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing}};
             Prelude.False ->
              case b2 of {
               Prelude.True ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just
                        ((Prelude.+)
                          ((Prelude.*) (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ 0))))))))))
                            x0) (Prelude.succ (Prelude.succ (Prelude.succ
                          (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                          (Prelude.succ 0)))))))))}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing};
               Prelude.False ->
                case b3 of {
                 Prelude.True ->
                  case b4 of {
                   Prelude.True ->
                    case b5 of {
                     Prelude.True -> Prelude.Nothing;
                     Prelude.False ->
                      case b6 of {
                       Prelude.True -> Prelude.Nothing;
                       Prelude.False -> Prelude.Just
                        ((Prelude.+)
                          ((Prelude.*) (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
                            (Prelude.succ (Prelude.succ (Prelude.succ 0))))))))))
                            x0) 0)}};
                   Prelude.False -> Prelude.Nothing};
                 Prelude.False -> Prelude.Nothing}}}}})
        hd;
     Prelude.Nothing -> Prelude.Nothing}}

stringToNat :: Prelude.String -> Prelude.Maybe Prelude.Integer
stringToNat s =
  case s of {
   ([]) -> Prelude.Nothing;
   (:) _ _ -> stringToNatHelper (reverseString s)}

getIndex :: Prelude.String -> Prelude.Maybe Fin.Coq_t
getIndex index =
  case stringToNat index of {
   Prelude.Just n ->
    case Fin.of_nat n State.coq_Ndes of {
     Prelude.Just x0 -> Prelude.Just x0;
     Prelude.Nothing -> Prelude.Nothing};
   Prelude.Nothing -> Prelude.Nothing}

getArrayField :: Prelude.String -> Fin.Coq_t -> State.State -> Prelude.Maybe
                 Prelude.String
getArrayField f i st =
  case (Prelude.==) f ((:) 'X' ([])) of {
   Prelude.True -> Prelude.Just
    (Float_text_io._FloatIO__float_to_string (Prelude.succ (Prelude.succ
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
      (Prelude.succ 0)))))))))))))))))))))))))))))))))))))))))))))))))))))
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
      (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
      0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      (Vector.nth State.coq_Ndes (State.x st) i));
   Prelude.False ->
    case (Prelude.==) f ((:) 'X' ((:) 'D' ([]))) of {
     Prelude.True -> Prelude.Just
      (Float_text_io._FloatIO__float_to_string (Prelude.succ (Prelude.succ
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
        (Prelude.succ 0)))))))))))))))))))))))))))))))))))))))))))))))))))))
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
        (Prelude.succ (Prelude.succ (Prelude.succ (Prelude.succ
        0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
        (Vector.nth State.coq_Ndes (State.xDot st) i));
     Prelude.False -> Prelude.Nothing}}

getField :: Prelude.String -> State.State -> Prelude.Maybe Prelude.String
getField f st =
  case (Prelude.==) f ((:) 'T' ((:) 'I' ((:) 'M' ((:) 'E' ([]))))) of {
   Prelude.True -> Prelude.Just
    (Float_text_io._FloatIO__float_to_string (Prelude.succ (Prelude.succ
      (Prelude.succ (Prelude.succ 0)))) (Prelude.succ (Prelude.succ (Prelude.succ
      0))) (State.coq_Time st));
   Prelude.False ->
    case (Prelude.==) f ((:) 'T' ((:) 'I' ((:) 'M' ((:) 'E' ((:) '0' ([])))))) of {
     Prelude.True -> Prelude.Just
      (Float_text_io._FloatIO__float_to_string (Prelude.succ (Prelude.succ
        (Prelude.succ (Prelude.succ 0)))) (Prelude.succ (Prelude.succ (Prelude.succ
        0))) (State.coq_Time0 st));
     Prelude.False ->
      case (Prelude.==) f ((:) 'T' ((:) 'S' ((:) 'T' ((:) 'O' ((:) 'P' ([])))))) of {
       Prelude.True -> Prelude.Just
        (Float_text_io._FloatIO__float_to_string (Prelude.succ (Prelude.succ
          (Prelude.succ (Prelude.succ 0)))) (Prelude.succ (Prelude.succ
          (Prelude.succ 0))) (State.coq_Tstop st));
       Prelude.False ->
        case (Prelude.==) f ((:) 'D' ((:) 'T' ([]))) of {
         Prelude.True -> Prelude.Just
          (Float_text_io._FloatIO__float_to_string (Prelude.succ (Prelude.succ
            (Prelude.succ (Prelude.succ 0)))) (Prelude.succ (Prelude.succ
            (Prelude.succ 0))) (State.coq_Dt st));
         Prelude.False ->
          case (Prelude.==) f ((:) 'D' ((:) 'A' ((:) 'M' ((:) 'P' ((:) 'I' ((:) 'N'
                 ((:) 'G' ([])))))))) of {
           Prelude.True -> Prelude.Just
            (Float_text_io._FloatIO__float_to_string (Prelude.succ (Prelude.succ
              (Prelude.succ (Prelude.succ 0)))) (Prelude.succ (Prelude.succ
              (Prelude.succ 0))) (State.coq_Damping_Coefficient st));
           Prelude.False ->
            case (Prelude.==) f ((:) 'G' ((:) 'R' ((:) 'A' ((:) 'V' ((:) 'I' ((:)
                   'T' ((:) 'Y' ([])))))))) of {
             Prelude.True -> Prelude.Just
              (Float_text_io._FloatIO__float_to_string (Prelude.succ (Prelude.succ
                (Prelude.succ (Prelude.succ 0)))) (Prelude.succ (Prelude.succ
                (Prelude.succ 0))) (State.coq_Gravity st));
             Prelude.False ->
              case (Prelude.==) f ((:) 'M' ((:) 'A' ((:) 'S' ((:) 'S' ([]))))) of {
               Prelude.True -> Prelude.Just
                (Float_text_io._FloatIO__float_to_string (Prelude.succ (Prelude.succ
                  (Prelude.succ (Prelude.succ 0)))) (Prelude.succ (Prelude.succ
                  (Prelude.succ 0))) (State.coq_Mass st));
               Prelude.False ->
                case (Prelude.==) f ((:) 'S' ((:) 'P' ((:) 'R' ((:) 'I' ((:) 'N'
                       ((:) 'G' ([]))))))) of {
                 Prelude.True -> Prelude.Just
                  (Float_text_io._FloatIO__float_to_string (Prelude.succ
                    (Prelude.succ (Prelude.succ (Prelude.succ 0)))) (Prelude.succ
                    (Prelude.succ (Prelude.succ 0)))
                    (State.coq_Spring_Coefficient st));
                 Prelude.False -> Prelude.Nothing}}}}}}}}

printer :: (([]) Prelude.String) -> State.State -> Prelude.Either State.State
           Prelude.String
printer args st =
  case args of {
   ([]) -> Prelude.Right
    (String0.append ((:) 'E' ((:) 'R' ((:) 'R' ((:) '0' ((:) '4' ((:) ':' ((:) ' '
      ((:) 'W' ((:) 'r' ((:) 'o' ((:) 'n' ((:) 'g' ((:) ' ' ((:) 'n' ((:) 'u' ((:)
      'm' ((:) 'b' ((:) 'e' ((:) 'r' ((:) ' ' ((:) 'o' ((:) 'f' ((:) ' ' ((:) 'a'
      ((:) 'r' ((:) 'g' ((:) 'u' ((:) 'm' ((:) 'e' ((:) 'n' ((:) 't' ((:) 's' ((:)
      ':' ((:) ' ' ((:) 'P' ((:) 'R' ((:) 'I' ((:) 'N' ((:) 'T' ((:) ' '
      ([])))))))))))))))))))))))))))))))))))))))))
      (List0.fold_right (\x0 y ->
        String0.append x0 (String0.append ((:) ' ' ([])) y)) ([]) args));
   (:) f l ->
    case l of {
     ([]) ->
      case getField f st of {
       Prelude.Just x0 -> Prelude.Left (State.print st x0);
       Prelude.Nothing -> Prelude.Right
        (String0.append ((:) 'E' ((:) 'R' ((:) 'R' ((:) '0' ((:) '3' ((:) ':' ((:)
          ' ' ((:) 'M' ((:) 'a' ((:) 'l' ((:) 'f' ((:) 'o' ((:) 'r' ((:) 'm' ((:)
          'e' ((:) 'd' ((:) ' ' ((:) 'C' ((:) 'o' ((:) 'm' ((:) 'm' ((:) 'a' ((:)
          'n' ((:) 'd' ((:) ':' ((:) ' ' ((:) 'P' ((:) 'R' ((:) 'I' ((:) 'N' ((:)
          'T' ((:) ' ' ([])))))))))))))))))))))))))))))))))
          (List0.fold_right (\x0 y ->
            String0.append x0 (String0.append ((:) ' ' ([])) y)) ([]) args))};
     (:) index l0 ->
      case l0 of {
       ([]) ->
        case getIndex index of {
         Prelude.Just i ->
          case getArrayField f i st of {
           Prelude.Just x0 -> Prelude.Left (State.print st x0);
           Prelude.Nothing -> Prelude.Right
            (String0.append ((:) 'E' ((:) 'R' ((:) 'R' ((:) '0' ((:) '1' ((:) ':'
              ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'l' ((:) 'f' ((:) 'o' ((:) 'r' ((:)
              'm' ((:) 'e' ((:) 'd' ((:) ' ' ((:) 'C' ((:) 'o' ((:) 'm' ((:) 'm'
              ((:) 'a' ((:) 'n' ((:) 'd' ((:) ':' ((:) ' ' ((:) 'P' ((:) 'R' ((:)
              'I' ((:) 'N' ((:) 'T' ((:) ' ' ([])))))))))))))))))))))))))))))))))
              (List0.fold_right (\x0 y ->
                String0.append x0 (String0.append ((:) ' ' ([])) y)) ([]) args))};
         Prelude.Nothing -> Prelude.Right
          (String0.append ((:) 'E' ((:) 'R' ((:) 'R' ((:) '0' ((:) '2' ((:) ':' ((:)
            ' ' ((:) 'M' ((:) 'a' ((:) 'l' ((:) 'f' ((:) 'o' ((:) 'r' ((:) 'm' ((:)
            'e' ((:) 'd' ((:) ' ' ((:) 'C' ((:) 'o' ((:) 'm' ((:) 'm' ((:) 'a' ((:)
            'n' ((:) 'd' ((:) ':' ((:) ' ' ((:) 'P' ((:) 'R' ((:) 'I' ((:) 'N' ((:)
            'T' ((:) ' ' ([])))))))))))))))))))))))))))))))))
            (List0.fold_right (\x0 y ->
              String0.append x0 (String0.append ((:) ' ' ([])) y)) ([]) args))};
       (:) _ _ -> Prelude.Right
        (String0.append ((:) 'E' ((:) 'R' ((:) 'R' ((:) '0' ((:) '4' ((:) ':' ((:)
          ' ' ((:) 'W' ((:) 'r' ((:) 'o' ((:) 'n' ((:) 'g' ((:) ' ' ((:) 'n' ((:)
          'u' ((:) 'm' ((:) 'b' ((:) 'e' ((:) 'r' ((:) ' ' ((:) 'o' ((:) 'f' ((:)
          ' ' ((:) 'a' ((:) 'r' ((:) 'g' ((:) 'u' ((:) 'm' ((:) 'e' ((:) 'n' ((:)
          't' ((:) 's' ((:) ':' ((:) ' ' ((:) 'P' ((:) 'R' ((:) 'I' ((:) 'N' ((:)
          'T' ((:) ' ' ([])))))))))))))))))))))))))))))))))))))))))
          (List0.fold_right (\x0 y ->
            String0.append x0 (String0.append ((:) ' ' ([])) y)) ([]) args))}}}

setArrayField :: Prelude.String -> Fin.Coq_t -> State.State -> Prelude.Maybe
                 (Floats.Coq_float -> State.State)
setArrayField f i st =
  case (Prelude.==) f ((:) 'X' ([])) of {
   Prelude.True -> Prelude.Just (State.coq_Set_x st i);
   Prelude.False ->
    case (Prelude.==) f ((:) 'X' ((:) 'D' ([]))) of {
     Prelude.True -> Prelude.Just (State.coq_Set_xDot st i);
     Prelude.False -> Prelude.Nothing}}

setField :: Prelude.String -> State.State -> Prelude.Maybe
            (Floats.Coq_float -> State.State)
setField f st =
  case (Prelude.==) f ((:) 'T' ((:) 'I' ((:) 'M' ((:) 'E' ([]))))) of {
   Prelude.True -> Prelude.Just (State.coq_Set_Time st);
   Prelude.False ->
    case (Prelude.==) f ((:) 'T' ((:) 'I' ((:) 'M' ((:) 'E' ((:) '0' ([])))))) of {
     Prelude.True -> Prelude.Just (State.coq_Set_Time0 st);
     Prelude.False ->
      case (Prelude.==) f ((:) 'T' ((:) 'S' ((:) 'T' ((:) 'O' ((:) 'P' ([])))))) of {
       Prelude.True -> Prelude.Just (State.coq_Set_Tstop st);
       Prelude.False ->
        case (Prelude.==) f ((:) 'D' ((:) 'T' ([]))) of {
         Prelude.True -> Prelude.Just (State.coq_Set_Dt st);
         Prelude.False ->
          case (Prelude.==) f ((:) 'D' ((:) 'A' ((:) 'M' ((:) 'P' ((:) 'I' ((:) 'N'
                 ((:) 'G' ([])))))))) of {
           Prelude.True -> Prelude.Just (State.coq_Set_Damping_Coefficient st);
           Prelude.False ->
            case (Prelude.==) f ((:) 'G' ((:) 'R' ((:) 'A' ((:) 'V' ((:) 'I' ((:)
                   'T' ((:) 'Y' ([])))))))) of {
             Prelude.True -> Prelude.Just (State.coq_Set_Gravity st);
             Prelude.False ->
              case (Prelude.==) f ((:) 'M' ((:) 'A' ((:) 'S' ((:) 'S' ([]))))) of {
               Prelude.True -> Prelude.Just (State.coq_Set_Mass st);
               Prelude.False ->
                case (Prelude.==) f ((:) 'S' ((:) 'P' ((:) 'R' ((:) 'I' ((:) 'N'
                       ((:) 'G' ([]))))))) of {
                 Prelude.True -> Prelude.Just (State.coq_Set_Spring_Coefficient st);
                 Prelude.False -> Prelude.Nothing}}}}}}}}

setter :: (([]) Prelude.String) -> State.State -> Prelude.Either State.State
          Prelude.String
setter args st =
  case args of {
   ([]) -> Prelude.Right
    (String0.append ((:) 'E' ((:) 'R' ((:) 'R' ((:) '0' ((:) '8' ((:) ':' ((:) ' '
      ((:) 'W' ((:) 'r' ((:) 'o' ((:) 'n' ((:) 'g' ((:) ' ' ((:) 'n' ((:) 'u' ((:)
      'm' ((:) 'b' ((:) 'e' ((:) 'r' ((:) ' ' ((:) 'o' ((:) 'f' ((:) ' ' ((:) 'a'
      ((:) 'r' ((:) 'g' ((:) 'u' ((:) 'm' ((:) 'e' ((:) 'n' ((:) 't' ((:) 's' ((:)
      ':' ((:) ' ' ((:) 'S' ((:) 'E' ((:) 'T' ((:) ' '
      ([])))))))))))))))))))))))))))))))))))))))
      (List0.fold_right (\x0 y ->
        String0.append x0 (String0.append ((:) ' ' ([])) y)) ([]) args));
   (:) f l ->
    case l of {
     ([]) -> Prelude.Right
      (String0.append ((:) 'E' ((:) 'R' ((:) 'R' ((:) '0' ((:) '8' ((:) ':' ((:) ' '
        ((:) 'W' ((:) 'r' ((:) 'o' ((:) 'n' ((:) 'g' ((:) ' ' ((:) 'n' ((:) 'u' ((:)
        'm' ((:) 'b' ((:) 'e' ((:) 'r' ((:) ' ' ((:) 'o' ((:) 'f' ((:) ' ' ((:) 'a'
        ((:) 'r' ((:) 'g' ((:) 'u' ((:) 'm' ((:) 'e' ((:) 'n' ((:) 't' ((:) 's' ((:)
        ':' ((:) ' ' ((:) 'S' ((:) 'E' ((:) 'T' ((:) ' '
        ([])))))))))))))))))))))))))))))))))))))))
        (List0.fold_right (\x0 y ->
          String0.append x0 (String0.append ((:) ' ' ([])) y)) ([]) args));
     (:) val l0 ->
      case l0 of {
       ([]) ->
        case setField f st of {
         Prelude.Just func -> Prelude.Left
          (func (Float_text_io._FloatIO__strToFloat val));
         Prelude.Nothing -> Prelude.Right
          (String0.append ((:) 'E' ((:) 'R' ((:) 'R' ((:) '0' ((:) '7' ((:) ':' ((:)
            ' ' ((:) 'M' ((:) 'a' ((:) 'l' ((:) 'f' ((:) 'o' ((:) 'r' ((:) 'm' ((:)
            'e' ((:) 'd' ((:) ' ' ((:) 'C' ((:) 'o' ((:) 'm' ((:) 'm' ((:) 'a' ((:)
            'n' ((:) 'd' ((:) ':' ((:) ' ' ((:) 'S' ((:) 'E' ((:) 'T' ((:) ' '
            ([])))))))))))))))))))))))))))))))
            (List0.fold_right (\x0 y ->
              String0.append x0 (String0.append ((:) ' ' ([])) y)) ([]) args))};
       (:) val0 l1 ->
        case l1 of {
         ([]) ->
          case getIndex val of {
           Prelude.Just i ->
            case setArrayField f i st of {
             Prelude.Just func -> Prelude.Left
              (func (Float_text_io._FloatIO__strToFloat val0));
             Prelude.Nothing -> Prelude.Right
              (String0.append ((:) 'E' ((:) 'R' ((:) 'R' ((:) '0' ((:) '5' ((:) ':'
                ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'l' ((:) 'f' ((:) 'o' ((:) 'r' ((:)
                'm' ((:) 'e' ((:) 'd' ((:) ' ' ((:) 'C' ((:) 'o' ((:) 'm' ((:) 'm'
                ((:) 'a' ((:) 'n' ((:) 'd' ((:) ':' ((:) ' ' ((:) 'S' ((:) 'E' ((:)
                'T' ((:) ' ' ([])))))))))))))))))))))))))))))))
                (List0.fold_right (\x0 y ->
                  String0.append x0 (String0.append ((:) ' ' ([])) y)) ([]) args))};
           Prelude.Nothing -> Prelude.Right
            (String0.append ((:) 'E' ((:) 'R' ((:) 'R' ((:) '0' ((:) '6' ((:) ':'
              ((:) ' ' ((:) 'M' ((:) 'a' ((:) 'l' ((:) 'f' ((:) 'o' ((:) 'r' ((:)
              'm' ((:) 'e' ((:) 'd' ((:) ' ' ((:) 'C' ((:) 'o' ((:) 'm' ((:) 'm'
              ((:) 'a' ((:) 'n' ((:) 'd' ((:) ':' ((:) ' ' ((:) 'S' ((:) 'E' ((:)
              'T' ((:) ' ' ([])))))))))))))))))))))))))))))))
              (List0.fold_right (\x0 y ->
                String0.append x0 (String0.append ((:) ' ' ([])) y)) ([]) args))};
         (:) _ _ -> Prelude.Right
          (String0.append ((:) 'E' ((:) 'R' ((:) 'R' ((:) '0' ((:) '8' ((:) ':' ((:)
            ' ' ((:) 'W' ((:) 'r' ((:) 'o' ((:) 'n' ((:) 'g' ((:) ' ' ((:) 'n' ((:)
            'u' ((:) 'm' ((:) 'b' ((:) 'e' ((:) 'r' ((:) ' ' ((:) 'o' ((:) 'f' ((:)
            ' ' ((:) 'a' ((:) 'r' ((:) 'g' ((:) 'u' ((:) 'm' ((:) 'e' ((:) 'n' ((:)
            't' ((:) 's' ((:) ':' ((:) ' ' ((:) 'S' ((:) 'E' ((:) 'T' ((:) ' '
            ([])))))))))))))))))))))))))))))))))))))))
            (List0.fold_right (\x0 y ->
              String0.append x0 (String0.append ((:) ' ' ([])) y)) ([]) args))}}}}

execute :: Prelude.String -> (([]) Prelude.String) -> State.State -> Prelude.Either
           State.State Prelude.String
execute cmd args st =
  case (Prelude.==) cmd ((:) 'P' ((:) 'R' ((:) 'I' ((:) 'N' ((:) 'T' ([])))))) of {
   Prelude.True -> printer args st;
   Prelude.False ->
    case (Prelude.==) cmd ((:) 'S' ((:) 'E' ((:) 'T' ([])))) of {
     Prelude.True -> setter args st;
     Prelude.False ->
      case (Prelude.==) cmd ((:) 'R' ((:) 'U' ((:) 'N' ([])))) of {
       Prelude.True -> Prelude.Left (Advance_States.run st);
       Prelude.False ->
        case (Prelude.==) cmd ((:) 'S' ((:) 'T' ((:) 'O' ((:) 'P' ([]))))) of {
         Prelude.True -> Prelude.Left st;
         Prelude.False -> Prelude.Right
          (String0.append ((:) 'E' ((:) 'R' ((:) 'R' ((:) '9' ((:) '9' ((:) ':' ((:)
            ' ' ((:) 'U' ((:) 'n' ((:) 'r' ((:) 'e' ((:) 'c' ((:) 'o' ((:) 'g' ((:)
            'n' ((:) 'i' ((:) 'z' ((:) 'e' ((:) 'd' ((:) ' ' ((:) 'C' ((:) 'o' ((:)
            'm' ((:) 'm' ((:) 'a' ((:) 'n' ((:) 'd' ((:) ':' ((:) ' '
            ([]))))))))))))))))))))))))))))))
            (String0.append cmd
              (String0.append ((:) ' ' ([]))
                (List0.fold_right (\x0 y ->
                  String0.append x0 (String0.append ((:) ' ' ([])) y)) ([]) args))))}}}}

statefulParser :: (([]) (([]) Prelude.String)) -> State.State -> Prelude.String ->
                  (,) State.State Prelude.String
statefulParser input st error =
  case input of {
   ([]) -> (,) st error;
   (:) line ls ->
    case line of {
     ([]) -> statefulParser ls st error;
     (:) cmd cs ->
      case (Prelude.==) cmd ((:) 'S' ((:) 'T' ((:) 'O' ((:) 'P' ([]))))) of {
       Prelude.True -> (,) st error;
       Prelude.False ->
        case execute cmd cs st of {
         Prelude.Left advSt -> statefulParser ls advSt error;
         Prelude.Right e -> (,) st
          (String0.append e (String0.append State.newline error))}}}}

parser :: (([]) (([]) Prelude.String)) -> Prelude.Either State.State Prelude.String
parser input =
  case statefulParser (prepare input) Default_Data.coq_Default_Data ([]) of {
   (,) st err ->
    case err of {
     ([]) -> Prelude.Left st;
     (:) _ _ -> Prelude.Right err}}

