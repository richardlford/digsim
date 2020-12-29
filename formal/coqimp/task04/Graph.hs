module Graph where

import qualified Digsim
import qualified Fin
import Prelude
import Fappli_IEEE
import qualified Floats
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart
import Data.String
import System.Environment
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart.Backend.Cairo
import Control.Lens
import Text.Read

to_hs_float :: Floats.Coq_float -> Double
to_hs_float (B754_zero b) = (if b then -1.0 else 1.0) * 0.0
to_hs_float (B754_infinity b) = (if b then -1.0 else 1.0)/0.0
to_hs_float (B754_nan b k) = (if b then -0.0 else 0.0)/0.0
to_hs_float (B754_finite b m e) = ((if b then -1.0 else 1.0) * (fromIntegral m)) * (2.0 ** (fromIntegral e))

getMantissa :: Floats.Coq_float -> Maybe Integer
getMantissa (B754_zero b) = Just 0
getMantissa (B754_infinity b) = Nothing
getMantissa (B754_nan b k) = Nothing
getMantissa (B754_finite b m e) = Just m

getExponent :: Floats.Coq_float -> Maybe Integer
getExponent (B754_zero b) = Just 0
getExponent (B754_infinity b) = Nothing
getExponent (B754_nan b k) = Nothing
getExponent (B754_finite b m e) = Just e

getPosData :: [[Maybe Double]] -> [(Double, Double)]
getPosData [] = []
getPosData ([Just x,Just y,_]:ds) = ((x, y) : (getPosData ds))
getPosData (_:ds) = (getPosData ds)

getVelData :: [[Maybe Double]] -> [(Double, Double)]
getVelData [] = []
getVelData ([Just x,_,Just z]:ds) = ((x, z) : (getVelData ds))
getVelData (_:ds) = (getVelData ds)

graph = \f ->  toFile def "Task04_1.png" $ do
   layout_title .= "Task 04"
   setColors [opaque blue]
   plot (line "Position" [getPosData f])

graph_velocity = \f ->  toFile def "Task04_2.png" $ do
   layout_title .= "Task 04"
   setColors [opaque blue]
   plot (line "Velocity" [getVelData f])

toDouble :: String -> Maybe Double
toDouble s = readMaybe s

main = do
          inputStr <- readFile "input.dat"
	  ;let input = map words (lines inputStr)
	  ;let output = (Digsim.dataComments input)
	  ;graph (map (map toDouble) (map words (lines output)))
	  ;graph_velocity (map (map toDouble) (map words (lines output)))
          ;writeFile "task04.dat" output