module Utils.Constants where

import System.Random (randomRIO)

-- Mathematical Constants
piValue :: Double
piValue = pi -- Haskell's built-in π

infinity :: Double
infinity = 1.0 / 0.0 -- Represents positive infinity

negInfinity :: Double
negInfinity = -1.0 / 0.0

epsilon :: Double
epsilon = 1e-8 -- Small value to avoid floating-point errors

-- Convert degrees to radians
degreesToRadians :: Double -> Double
degreesToRadians degrees = degrees * piValue / 180.0

-- Clamp a value between min and max
clamp :: Double -> Double -> Double -> Double
clamp x minVal maxVal
  | x < minVal = minVal
  | x > maxVal = maxVal
  | otherwise = x

-- Generate a random floating-point number between 0 and 1
randomDouble :: IO Double
randomDouble = randomRIO (0.0, 1.0)

-- Generate a random floating-point number in a given range
randomDoubleInRange :: Double -> Double -> IO Double
randomDoubleInRange minVal maxVal = randomRIO (minVal, maxVal)
