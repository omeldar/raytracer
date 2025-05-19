module Hittable.BoundingBox (AABB (..), surroundingBox, hitAABBInterval) where

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

hitAABBInterval :: AABB -> Ray -> Interval -> Maybe Interval
hitAABBInterval (AABB (Vec3 minX minY minZ) (Vec3 maxX maxY maxZ)) (Ray orig dir) (Interval tMin tMax) =
  let checkAxis o d minB maxB =
        if abs d < 1e-8
          then if o < minB || o > maxB then Nothing else Just (-infinity, infinity)
          else
            let invD = 1.0 / d
                t0 = (minB - o) * invD
                t1 = (maxB - o) * invD
             in Just (min t0 t1, max t0 t1)

      mergeIntervals (Just (a0, a1)) (Just (b0, b1)) (Just (c0, c1)) =
        let tEnter = maximum [a0, b0, c0, tMin]
            tExit = minimum [a1, b1, c1, tMax]
         in if tEnter <= tExit then Just (Interval tEnter tExit) else Nothing
      mergeIntervals _ _ _ = Nothing
   in mergeIntervals
        (checkAxis (x orig) (x dir) minX maxX)
        (checkAxis (y orig) (y dir) minY maxY)
        (checkAxis (z orig) (z dir) minZ maxZ)
