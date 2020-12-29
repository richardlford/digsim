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

dataPoints :: Digsim.File ->  [(Double, Double)]
dataPoints = \f -> (map (\x -> (to_hs_float (fst x), to_hs_float (snd x))) (Digsim.graph (Fin.F1 1) f))

graph1 = \f -> toFile def "task03_1.png" $ do
    layout_title .= "Task 03"
    setColors [opaque blue]
    plot (line "Position" [dataPoints f])

dataPoints' :: Digsim.File -> [(Double, Double)]
dataPoints' = \f -> (map (\x -> (to_hs_float (fst x), to_hs_float (snd x))) (Digsim.graphxDot (Fin.F1 0) f))

graph2 = \f -> toFile def "task03_2.png" $ do
    layout_title .= "Task 03"
    setColors [opaque blue]
    plot (line "Velocity" [dataPoints' f])

main = do
          inputStr <- readFile "input.dat"
	  ;let input = map words (lines inputStr)
	  ;graph1 input
	  ;graph2 input
          ;writeFile "task03.dat"((foldr (\a -> \b -> a ++ "\CR\LF" ++ b) "" (map (foldr (\a -> \b -> a ++ "\HT" ++ b) "") (map (map show) ((map (map to_hs_float) (Digsim.graphData input)))))) ++ "\CR\LF" ++ (Digsim.dataComments input))