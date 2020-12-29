module InputData where

import qualified Prelude
import qualified Advance_States
import qualified Default_Data
import qualified Events
import qualified List0
import qualified State
import qualified String0
import qualified Float_text_io
import qualified Data.Bits
import qualified Data.Char

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
                      stripCommentsToken tl
                        ((Prelude.-) parens ((\x -> x) 1))}};
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
                      stripCommentsToken tl
                        ((Prelude.+) parens ((\x -> x) 1))}};
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

countParensLine :: (([]) Prelude.String) -> Prelude.Integer ->
                   Prelude.Integer
countParensLine ss parens =
  List0.fold_right (Prelude.+) 0
    (List0.map (\x -> countParensToken x parens) ss)

stripCommentsLine :: (([]) Prelude.String) -> Prelude.Integer -> ([])
                     Prelude.String
stripCommentsLine ss parens =
  case ss of {
   ([]) -> ([]);
   (:) hd tl ->
    case stripCommentsToken hd parens of {
     ([]) -> stripCommentsLine tl (countParensToken hd parens);
     (:) a s0 -> (:) ((:) a s0)
      (stripCommentsLine tl (countParensToken hd parens))}}

stripCommentsFile :: (([]) (([]) Prelude.String)) -> Prelude.Integer -> ([])
                     (([]) Prelude.String)
stripCommentsFile ss parens =
  case ss of {
   ([]) -> ([]);
   (:) hd tl ->
    case stripCommentsLine hd parens of {
     ([]) -> stripCommentsFile tl (countParensLine hd parens);
     (:) s0 l -> (:) ((:) s0 l)
      (stripCommentsFile tl (countParensLine hd parens))}}

countParensFile :: (([]) (([]) Prelude.String)) -> Prelude.Integer ->
                   Prelude.Integer
countParensFile ss parens =
  List0.fold_right (Prelude.+) 0
    (List0.map (\x -> countParensLine x parens) ss)

stripComments :: (([]) (([]) Prelude.String)) -> ([]) (([]) Prelude.String)
stripComments ss =
  stripCommentsFile ss 0

endsWith :: Prelude.String -> Prelude.Char -> Prelude.Bool
endsWith s c =
  case s of {
   ([]) -> Prelude.False;
   (:) a tl ->
    case tl of {
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

clean :: Prelude.String -> Prelude.String
clean s =
  Events.reverseString
    (removeLeading
      (Events.reverseString (removeLeading (Events.stringMap capitalize s))))

prepare :: (([]) (([]) Prelude.String)) -> ([]) (([]) Prelude.String)
prepare f =
  List0.map (List0.map clean) (stripComments f)

execute :: Prelude.String -> (([]) Prelude.String) -> State.State ->
           Prelude.Either State.State Prelude.String
execute cmd args st =
  case (Prelude.==) cmd ((:) 'R' ((:) 'U' ((:) 'N' ([])))) of {
   Prelude.True -> Prelude.Left (Advance_States.run st);
   Prelude.False ->
    case (Prelude.==) cmd ((:) 'S' ((:) 'T' ((:) 'O' ((:) 'P' ([]))))) of {
     Prelude.True -> Prelude.Left st;
     Prelude.False -> Events.executeEvent cmd args st}}

statefulParser :: (([]) (([]) Prelude.String)) -> State.State ->
                  Prelude.String -> (,) State.State Prelude.String
statefulParser input st error =
  case input of {
   ([]) -> (,) st error;
   (:) line ls ->
    case line of {
     ([]) -> statefulParser ls st error;
     (:) cmd cs ->
      case (Prelude.==) cmd ((:) 'S' ((:) 'T' ((:) 'O' ((:) 'P' ([]))))) of {
       Prelude.True -> (,)
        (State.print st
          (String0.append cmd
            (String0.append ((:) '@' ([]))
              (Float_text_io._FloatIO__float_to_string (Prelude.succ
                (Prelude.succ (Prelude.succ (Prelude.succ 0)))) (Prelude.succ
                (Prelude.succ (Prelude.succ 0))) (State.coq_Time st)))))
        error;
       Prelude.False ->
        case execute cmd cs
               (State.print st
                 (String0.append ((:) 'I' ((:) 'n' ((:) 's' ((:) 't' ((:) 'r'
                   ((:) 'u' ((:) 'c' ((:) 't' ((:) 'i' ((:) 'o' ((:) 'n' ((:)
                   ':' ((:) ' ' ([]))))))))))))))
                   (String0.append cmd
                     (String0.append ((:) '@' ([]))
                       (Float_text_io._FloatIO__float_to_string (Prelude.succ
                         (Prelude.succ (Prelude.succ (Prelude.succ 0))))
                         (Prelude.succ (Prelude.succ (Prelude.succ 0)))
                         (State.coq_Time st)))))) of {
         Prelude.Left advSt -> statefulParser ls advSt error;
         Prelude.Right e -> (,) st
          (String0.append e (String0.append State.newline error))}}}}

parser :: (([]) (([]) Prelude.String)) -> Prelude.Either State.State
          Prelude.String
parser input =
  case statefulParser (prepare input) Default_Data.coq_Default_Data ([]) of {
   (,) st err ->
    case err of {
     ([]) -> Prelude.Left st;
     (:) _ _ -> Prelude.Right err}}

