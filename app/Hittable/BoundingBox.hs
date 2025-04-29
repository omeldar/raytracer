module Hittable.BoundingBox (AABB (..), surroundingBox, hitAABB) where

import Core.Ray (Ray (..))
import Core.Vec3
import Utils.Constants (infinity)
import Utils.Interval (Interval (..))

data AABB = AABB
  { minBounds :: !Vec3,
    maxBounds :: !Vec3
  }
  deriving (Show)

surroundingBox :: AABB -> AABB -> AABB
surroundingBox (AABB minA maxA) (AABB minB maxB) =
  AABB (minVec3 minA minB) (maxVec3 maxA maxB)

hitAABB :: AABB -> Ray -> Interval -> Bool
hitAABB (AABB (Vec3 minX minY minZ) (Vec3 maxX maxY maxZ)) (Ray orig dir) (Interval tMin tMax) =
  let checkAxis o d minB maxB =
        if abs d < 1e-8
          then
            if o < minB || o > maxB
              then (infinity, -infinity) -- miss immediately
              else (-infinity, infinity) -- infinite slab (parallel)
          else
            let invD = 1.0 / d
                t0 = (minB - o) * invD
                t1 = (maxB - o) * invD
             in if invD < 0 then (t1, t0) else (t0, t1)

      (tx0, tx1) = checkAxis (x orig) (x dir) minX maxX
      (ty0, ty1) = checkAxis (y orig) (y dir) minY maxY
      (tz0, tz1) = checkAxis (z orig) (z dir) minZ maxZ

      tEnter = maximum [tx0, ty0, tz0, tMin]
      tExit = minimum [tx1, ty1, tz1, tMax]
   in tEnter <= tExit
