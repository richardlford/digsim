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

simulate :: [TimeStep Double]
simulate = loop 0.0e0 x_0 xd_0
  where
    loop :: Double -- ^ Simulation time [sec]
         -> Double -- ^ Position of suspended mass [m]
         -> Double -- ^ Velocity of suspended mass [m/sec]
         -> [TimeStep Double]
    loop t x xd =
      (TimeStep t x xd) : loop ( t +  dt)
                               ( x +  xd * dt)
                               (xd + xdd * dt)
      where
        -- |Acceleration of suspended mass [m/sec**2]
        xdd :: Double
        xdd = -(spring_coefficient  * x +
                damping_coefficient * xd)
              / mass
              - gravity

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
    x_0 :: Double
    x_0                 = 0.0e0

    -- |Initial velocity of suspended mass [m/sec]
    xd_0 :: Double
    xd_0                = 0.0e0   -- Initial velocity of suspended mass [m/sec]

main :: IO ()
main = forM_ (takeWhile ((<= tstop) . stepT) simulate) $
         \(TimeStep t x xd) ->
           putStrLn $ printf " %15.5E%15.5E%15.5E" t x xd
  where
    -- |Simulation stop time [sec]
    tstop :: Double
    tstop               = 2.50e0
