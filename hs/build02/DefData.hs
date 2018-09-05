module DefData
( dT
, dampingCoefficient
, gravity
, mass
, springCoefficient
, time₀
, timeStop
, x₀
, x'₀
, x''₀
) where

dT :: Double
dT = 0.01

dampingCoefficient :: Double
dampingCoefficient = 8.88

gravity :: Double
gravity = 9.88

mass :: Double
mass = 1.0

time₀ :: Double
time₀ = 0.0

timeStop :: Double
timeStop = 2.5

springCoefficient :: Double
springCoefficient = 39.47

x₀ :: Double
x₀ = 0.0

x'₀ :: Double
x'₀ = 0.0

x''₀ :: Double
x''₀ = 0.0
