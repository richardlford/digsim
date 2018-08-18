module Main where

-- This program simulates a mass-spring-damper system.

import Control.Monad (forM_)
import Text.Printf

data TimeStep a
  = TimeStep { stepT  :: a  -- ^ Simulation time [sec]
             , stepX  :: a  -- ^ Position of suspended mass [m]
             , stepXd :: a  -- ^ Velocity of suspended mass [m/sec]
             }
  deriving (Show)

-- |Damping force per velocity [Nm/s]
damping_coefficient :: Double
damping_coefficient = 8.88e0

-- |Simulation time step [sec]
dt :: Double
dt                  = 0.01e0

-- |Acceleration due to gravity [m/sec**2]
gravity :: Double
gravity             = 9.88e0

-- |Mass suspended from spring [Kg]
mass :: Double
mass                = 1.0e0

-- |Restoring force per position [N/m]
spring_coefficient :: Double
spring_coefficient  = 39.47e0

-- |Initial position of suspended mass [m]
x₀ :: Double
x₀                  = 0.0e0

-- |Initial velocity of suspended mass [m/sec]
x₀' :: Double
x₀'                 = 0.0e0   -- Initial velocity of suspended mass [m/sec]

-- |Simulation stop time [sec]
tstop :: Double
tstop               = 2.50e0

-- |Acceleration given mass and velocity
accel :: Double -- ^ Position of suspended mass [m]
      -> Double -- ^ Velocity of suspended mass [m/sec]
      -> Double -- ^ Acceleration of suspended mass [m/sec**2]
accel x x' =
  let spring_force  = spring_coefficient  * x
      damping_force = damping_coefficient * x'
  in -(spring_force + damping_force) / mass - gravity

-- |Execute the simulation given initial position and velocity
simulate :: [TimeStep Double]
simulate = loop 0.0e0 x₀ x₀'
  where
    loop :: Double -- ^ Simulation time [sec]
         -> Double -- ^ Position of suspended mass [m]
         -> Double -- ^ Velocity of suspended mass [m/sec]
         -> [TimeStep Double]
    loop t x x' =
      (TimeStep t x x') : loop (t  + dt)
                               (x  + dt * x')
                               (x' + dt * x'')
      where x'' = accel x x'

main :: IO ()
main =
  let samples = takeWhile ((<= tstop) . stepT) simulate
  in forM_ samples $
    \(TimeStep t x xd) ->
      putStrLn $ " " ++ concatMap format [t,x,xd]

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
