module Main where

import Control.Monad
import Text.Printf

import DigSim

main :: IO ()
main = forM_ digsim $
         \(State { stTime = t, stX = x, stX' = x'}) ->
           putStrLn $ " " ++ concatMap format [t,x,x']

-- |Format a number to look like FORTRAN's 3ES15.5
format :: (Num a, PrintfArg a) => a -> String
format dbl =
  let lbd = case reverse $ printf "%15.5E" dbl of
              ('0':'E':ds)  -> '0':'0':'+':'E':(strip $ strip ds)
              (d:pm:'E':ds) -> d:'0':pm:'E':(strip ds)
              ds            -> ds
      strip [] = ""
      strip (' ':[]) = ""
      strip (x:xs) = x : strip xs
  in reverse lbd
