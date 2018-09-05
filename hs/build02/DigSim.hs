module DigSim
( digsim
, State(..)
) where

import DefData

data State = State
  { stTime :: Double
  , stX    :: Double
  , stX'   :: Double
  , stX''  :: Double
  } deriving (Show)

digsim :: [State]
digsim = steps
  where initial  = diffeq state₀
        next     = diffeq . advance
        endOfRun = (> timeStop) . stTime
        (le, (gt:_)) = break endOfRun $ iterate next initial
        steps = le ++ [gt]  -- F77 version includes one past tstop

state₀ :: State
state₀ =  State
  { stTime = time₀
  , stX    = x₀
  , stX'   = x'₀
  , stX''  = x''₀
  }

advance :: State -> State
advance s@(State {stTime = t, stX = x, stX' = x', stX'' = x''}) =
  s { stTime = t  + dT
    , stX    = x  + dT * x'
    , stX'   = x' + dT * x''
    }

diffeq :: State -> State
diffeq s@(State {stX = x, stX' = x', stX'' = x''}) =
  s { stX'' = -(springForce + dampingForce) / mass - gravity }
  where
    springForce  = springCoefficient  * x
    dampingForce = dampingCoefficient * x'
