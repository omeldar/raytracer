module Hittable.BoundingBox (AABB (..), surroundingBox, hitAABB) where

import Core.Ray (Ray (..))
import Core.Vec3
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
        let invD = 1.0 / d
            t0 = (minB - o) * invD
            t1 = (maxB - o) * invD
            (tNear, tFar) = if invD < 0 then (t1, t0) else (t0, t1)
         in (tNear, tFar)
      (x0, x1) = checkAxis (x orig) (x dir) minX maxX
      (y0, y1) = checkAxis (y orig) (y dir) minY maxY
      (z0, z1) = checkAxis (z orig) (z dir) minZ maxZ
      tEnter = maximum [x0, y0, z0, tMin]
      tExit = minimum [x1, y1, z1, tMax]
   in tExit > tEnter
