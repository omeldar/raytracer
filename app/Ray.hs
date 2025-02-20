module Ray (
    Ray(..),
    at
) where

import Vec3

data Ray = Ray
    { origin :: Vec3
    , direction :: Vec3
    } deriving (Show)

at :: Ray -> Double -> Vec3
at (Ray orig dir) t = orig `add` scale t dir