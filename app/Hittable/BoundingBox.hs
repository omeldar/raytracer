module Hittable.BoundingBox (AABB (..), surroundingBox) where

import Core.Vec3

data AABB = AABB
  { minBounds :: Vec3,
    maxBounds :: Vec3
  }
  deriving (Show)

surroundingBox :: AABB -> AABB -> AABB
surroundingBox (AABB minA maxA) (AABB minB maxB) =
  AABB (minVec3 minA minB) (maxVec3 maxA maxB)
