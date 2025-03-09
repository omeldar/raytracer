module Core.Ray
  ( Ray (..),
    at,
  )
where

import Core.Vec3

-- A ray is defined by its origin (a point) and its direction (a vector)
-- both represented as a Vec3
data Ray = Ray
  { origin :: Vec3,
    direction :: Vec3
  }
  deriving (Show)

-- Compute the point at parameter t along the ray
at :: Ray -> Double -> Vec3
at (Ray orig dir) t = orig `add` scale t dir