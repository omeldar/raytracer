{-# LANGUAGE BangPatterns #-}

module Hittable.BVH where

import Core.Vec3 (Vec3 (..), x, y, z)
import qualified Core.Vec3 as V
import qualified Data.List as List
import qualified Data.Vector as DV
import Hittable.BoundingBox (AABB (..), hitAABB, surroundingBox)
import Hittable.Class (HitRecord (t), Hittable (..))
import qualified Hittable.Class as H
import Hittable.Helpers (getBox)
import Hittable.HittableList (SomeHittable (..))
import Utils.Constants
import Utils.HitHelpers (closestHitList)
import Utils.Interval (Interval (..))

-- | A node in the Bounding Volume Hierarchy
data BVHNode
  = BVHLeaf !(DV.Vector SomeHittable) !AABB
  | BVHInternal !BVHNode !BVHNode !AABB

-- | Construct a BVH from a list of arbitrary hittables
constructBVH :: [SomeHittable] -> BVHNode
constructBVH = constructBVHWithLimit 2 maxBound

-- | Construct BVH with depth limit
constructBVHWithLimit :: Int -> Int -> [SomeHittable] -> BVHNode
constructBVHWithLimit leafThreshold maxDepth objects = buildBVH objects 0
  where
    buildBVH :: [SomeHittable] -> Int -> BVHNode
    buildBVH !objs !depth
      | objCount <= leafThreshold || depth >= maxDepth =
          let !bbox = surroundingBoxList getBox objs
           in BVHLeaf (DV.fromList objs) bbox
      | otherwise =
          let !axis = chooseSplitAxis objs
              !sorted = sortByCentroid axis objs
              !(leftObjs, rightObjs) = splitAt (objCount `div` 2) sorted
              !left = buildBVH leftObjs (depth + 1)
              !right = buildBVH rightObjs (depth + 1)
              !bbox = surroundingBoxList boundingBox [left, right]
           in BVHInternal left right bbox
      where
        !objCount = length objs

-- | Choose axis with greatest extent (x = 0, y = 1, z = 2)
chooseSplitAxis :: [SomeHittable] -> Int
chooseSplitAxis objs =
  let (!minVec, !maxVec) = foldr mergeExtents (Vec3 infinity infinity infinity, Vec3 (-infinity) (-infinity) (-infinity)) objs
      !extent = V.sub maxVec minVec
   in if x extent > y extent && x extent > z extent
        then 0
        else if y extent > z extent then 1 else 2

-- | Sort based on centroid along the given axis
sortByCentroid :: Int -> [SomeHittable] -> [SomeHittable]
sortByCentroid axis =
  List.sortOn
    ( \(SomeHittable h) ->
        let AABB minB maxB = getBox (SomeHittable h)
            !centroid = V.scale 0.5 (V.add minB maxB)
         in getAxisValue axis centroid
    )

getAxisValue :: Int -> Vec3 -> Double
getAxisValue 0 (Vec3 ax _ _) = ax
getAxisValue 1 (Vec3 _ ay _) = ay
getAxisValue 2 (Vec3 _ _ az) = az
getAxisValue _ _ = error "Invalid axis"

mergeExtents :: SomeHittable -> (Vec3, Vec3) -> (Vec3, Vec3)
mergeExtents (SomeHittable h) (!vmin, !vmax) =
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

surroundingBoxList :: (a -> AABB) -> [a] -> AABB
surroundingBoxList _ [] = error "Cannot compute bounding box of empty list"
surroundingBoxList getBoxFn (bx : xs) = foldr (surroundingBox . getBoxFn) (getBoxFn bx) xs

instance Hittable BVHNode where
  hit (BVHLeaf objs _) ray interval =
    closestHitList (DV.toList (DV.map (\(SomeHittable o) -> H.hit o ray interval) objs))
  hit (BVHInternal left right _) ray interval =
    let !hitLeftBox = hitAABB (boundingBox left) ray interval
        !hitRightBox = hitAABB (boundingBox right) ray interval
     in case (hitLeftBox, hitRightBox) of
          (False, False) -> Nothing
          (True, False) -> hit left ray interval
          (False, True) -> hit right ray interval
          (True, True) ->
            let !leftHit = hit left ray interval
                !newInterval = maybe interval (Interval (minVal interval) . t) leftHit
                !rightHit = hit right ray newInterval
             in case (leftHit, rightHit) of
                  (Just l, Just r) -> if t l < t r then Just l else Just r
                  (Just l, Nothing) -> Just l
                  (Nothing, Just r) -> Just r
                  _ -> Nothing

  boundingBox (BVHLeaf _ box) = box
  boundingBox (BVHInternal _ _ box) = box
