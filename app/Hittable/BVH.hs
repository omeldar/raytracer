module Hittable.BVH where

import Core.Vec3 (Vec3 (..), maxVec3, minVec3)
import Data.Function (on)
import Data.List (sortBy)
import Hittable.BoundingBox (AABB (..))
import Hittable.Class (Hittable (..))
import Hittable.Helpers (getBox)
import Hittable.HittableList (SomeHittable (..))
import Utils.HitHelpers (closestHit)

-- | A node in the Bounding Volume Hierarchy
data BVHNode
  = BVHLeaf [SomeHittable] AABB
  | BVHInternal BVHNode BVHNode AABB

-- | Construct a BVH from a list of arbitrary hittables
constructBVH :: [SomeHittable] -> BVHNode
constructBVH [] = error "Cannot construct BVH from empty list"
constructBVH [obj] =
  let box = getBox obj
   in BVHLeaf [obj] box
constructBVH objs =
  let box =
        AABB
          (foldr (minVec3 . minBounds . getBox) (Vec3 1e30 1e30 1e30) objs)
          (foldr (maxVec3 . maxBounds . getBox) (Vec3 (-1e30) (-1e30) (-1e30)) objs)

      axis = longestAxis box
      sortedObjs = sortBy (compare `on` (getAxisValue axis . minBounds . getBox)) objs
      (left, right) = splitAt (length sortedObjs `div` 2) sortedObjs
   in BVHInternal (constructBVH left) (constructBVH right) box

-- | Axis helpers (X = 0, Y = 1, Z = 2)
getAxisValue :: Int -> Vec3 -> Double
getAxisValue 0 (Vec3 x _ _) = x
getAxisValue 1 (Vec3 _ y _) = y
getAxisValue 2 (Vec3 _ _ z) = z
getAxisValue _ _ = error "Invalid axis"

longestAxis :: AABB -> Int
longestAxis (AABB (Vec3 x0 y0 z0) (Vec3 x1 y1 z1)) =
  let dx = x1 - x0
      dy = y1 - y0
      dz = z1 - z0
   in if dx > dy && dx > dz
        then 0
        else
          if dy > dz
            then 1
            else 2

instance Hittable BVHNode where
  hit (BVHLeaf objs _) ray interval =
    foldr combineHits Nothing objs
    where
      combineHits (SomeHittable o) = closestHit (hit o ray interval)
  hit (BVHInternal left right _) ray interval =
    let hitLeft = hit left ray interval
        hitRight = hit right ray interval
     in closestHit hitLeft hitRight

  boundingBox (BVHLeaf _ box) = box
  boundingBox (BVHInternal _ _ box) = box
