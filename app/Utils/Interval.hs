module Utils.Interval where

import Utils.Constants (infinity)

-- Defines an interval [min, max] to restrict values
data Interval = Interval
    { minVal :: Double
    , maxVal :: Double
    } deriving (Show, Eq)

-- Commonly used intervals
emptyInterval :: Interval
emptyInterval = Interval infinity (-infinity)  -- No valid range

universeInterval :: Interval
universeInterval = Interval (-infinity) infinity  -- Entire range

-- Check if a value is inside an interval
contains :: Interval -> Double -> Bool
contains (Interval minV maxV) x = x >= minV && x <= maxV

-- Clamp a value within an interval
clampInterval :: Interval -> Double -> Double
clampInterval (Interval minV maxV) x
    | x < minV  = minV
    | x > maxV  = maxV
    | otherwise = x
