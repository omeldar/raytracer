module Hittable.BVH where

import Core.Vec3 (Vec3 (..), x, y, z)
import qualified Core.Vec3 as V
import qualified Data.List as List
import Hittable.BoundingBox (AABB (..), hitAABB, surroundingBox)
import Hittable.Class (Hittable (..))
import Hittable.Helpers (getBox)
import Hittable.HittableList (SomeHittable (..))
import Utils.Constants
import Utils.HitHelpers (closestHitList)

-- | A node in the Bounding Volume Hierarchy
data BVHNode
  = BVHLeaf [SomeHittable] AABB
  | BVHInternal BVHNode BVHNode AABB

-- | Construct a BVH from a list of arbitrary hittables
constructBVH :: [SomeHittable] -> BVHNode
constructBVH = constructBVHWithLimit maxBound -- no depth limit by default

-- | Construct BVH with depth limit
constructBVHWithLimit :: Int -> [SomeHittable] -> BVHNode
constructBVHWithLimit maxDepth objects = buildBVH objects 0
  where
    buildBVH objs depth
      | length objs <= 16 || depth >= maxDepth =
          BVHLeaf objs (surroundingBoxList getBox objs)
      | otherwise =
          let axis = chooseSplitAxis objs
              sorted = sortHittablesBy axis objs
              (leftObjs, rightObjs) = splitAt (length objs `div` 2) sorted
              left = buildBVH leftObjs (depth + 1)
              right = buildBVH rightObjs (depth + 1)
           in BVHInternal left right (surroundingBoxList boundingBox [left, right])

-- | Choose axis with greatest extent
chooseSplitAxis :: [SomeHittable] -> Int
chooseSplitAxis objs =
  let (minVec, maxVec) = foldr mergeExtents (Vec3 infinity infinity infinity, Vec3 (-infinity) (-infinity) (-infinity)) objs
      extent = V.sub maxVec minVec
   in if x extent > y extent && x extent > z extent
        then 0
        else if y extent > z extent then 1 else 2

-- | Sort objects by the given axis
sortHittablesBy :: Int -> [SomeHittable] -> [SomeHittable]
sortHittablesBy axis =
  List.sortOn
    ( \(SomeHittable h) ->
        case getBox (SomeHittable h) of
          AABB minB _ -> getAxisValue axis minB
    )

-- | Get axis component of vector
getAxisValue :: Int -> Vec3 -> Double
getAxisValue 0 (Vec3 ax _ _) = ax
getAxisValue 1 (Vec3 _ ay _) = ay
getAxisValue 2 (Vec3 _ _ az) = az
getAxisValue _ _ = error "Invalid axis"

-- | Merge object AABBs to get global extent
mergeExtents :: SomeHittable -> (Vec3, Vec3) -> (Vec3, Vec3)
mergeExtents (SomeHittable h) (vmin, vmax) =
  let AABB minB maxB = getBox (SomeHittable h)
   in ( V.Vec3
          (min (V.x minB) (V.x vmin))
          (min (V.y minB) (V.y vmin))
          (min (V.z minB) (V.z vmin)),
        V.Vec3
          (max (V.x maxB) (V.x vmax))
          (max (V.y maxB) (V.y vmax))
          (max (V.z maxB) (V.z vmax))
      )

-- | Compute surrounding box for a list of hittables
surroundingBoxList :: (a -> AABB) -> [a] -> AABB
surroundingBoxList _ [] = error "Cannot compute bounding box of empty list"
surroundingBoxList getBoxFn (bx : xs) = foldr (surroundingBox . getBoxFn) (getBoxFn bx) xs

instance Hittable BVHNode where
  hit (BVHLeaf objs _) ray interval =
    closestHitList (map (\(SomeHittable o) -> hit o ray interval) objs)
  hit (BVHInternal left right _) ray interval =
    let hitLeftBox = hitAABB (boundingBox left) ray interval
        hitRightBox = hitAABB (boundingBox right) ray interval
     in case (hitLeftBox, hitRightBox) of
          (True, True) -> closestHitList [hit left ray interval, hit right ray interval]
          (True, False) -> hit left ray interval
          (False, True) -> hit right ray interval
          (False, False) -> Nothing

  boundingBox (BVHLeaf _ box) = box
  boundingBox (BVHInternal _ _ box) = box
