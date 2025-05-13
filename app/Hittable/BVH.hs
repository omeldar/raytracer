{-# LANGUAGE BangPatterns #-}

module Hittable.BVH where

import qualified Core.Ray as R
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
constructBVHWithLimit leafThreshold maxDepth objects =
  let cachedObjs = map (\obj -> (obj, getBox obj)) objects
   in buildBVH cachedObjs 0
  where
    buildBVH :: [(SomeHittable, AABB)] -> Int -> BVHNode
    buildBVH !objs !depth
      | objCount <= leafThreshold || depth >= maxDepth =
          let !bbox = surroundingBoxList snd objs
           in BVHLeaf (DV.fromList (map fst objs)) bbox
      | otherwise =
          let axis = chooseSplitAxis objs
              sorted = sortByCentroid axis objs
              (leftObjs, rightObjs) = splitAt (objCount `div` 2) sorted
              left = buildBVH leftObjs (depth + 1)
              right = buildBVH rightObjs (depth + 1)
              bbox = surroundingBoxList boundingBox [left, right]
           in BVHInternal left right bbox
      where
        !objCount = length objs

-- | Choose axis with greatest extent (x = 0, y = 1, z = 2)
chooseSplitAxis :: [(SomeHittable, AABB)] -> Int
chooseSplitAxis objs =
  let (minVec, maxVec) =
        foldr
          ( \(_, AABB minB maxB) (vmin, vmax) ->
              ( V.Vec3
                  (min (V.x minB) (V.x vmin))
                  (min (V.y minB) (V.y vmin))
                  (min (V.z minB) (V.z vmin)),
                V.Vec3
                  (max (V.x maxB) (V.x vmax))
                  (max (V.y maxB) (V.y vmax))
                  (max (V.z maxB) (V.z vmax))
              )
          )
          (Vec3 infinity infinity infinity, Vec3 (-infinity) (-infinity) (-infinity))
          objs
      extent = V.sub maxVec minVec
   in if x extent > y extent && x extent > z extent
        then 0
        else if y extent > z extent then 1 else 2

-- | Sort based on centroid along the given axis
sortByCentroid :: Int -> [(SomeHittable, AABB)] -> [(SomeHittable, AABB)]
sortByCentroid axis =
  List.sortOn
    ( \(_, AABB minB maxB) ->
        let centroid = V.scale 0.5 (V.add minB maxB)
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

entryDistance :: AABB -> R.Ray -> Double
entryDistance (AABB (Vec3 minX minY minZ) (Vec3 maxX maxY maxZ)) (R.Ray orig dir) =
  let checkAxis o d minB maxB =
        if abs d < 1e-8
          then if o < minB || o > maxB then (infinity, -infinity) else (-infinity, infinity)
          else
            let invD = 1.0 / d
                t0 = (minB - o) * invD
                t1 = (maxB - o) * invD
             in if invD < 0 then (t1, t0) else (t0, t1)
      (tx0, _) = checkAxis (x orig) (x dir) minX maxX
      (ty0, _) = checkAxis (y orig) (y dir) minY maxY
      (tz0, _) = checkAxis (z orig) (z dir) minZ maxZ
   in maximum [tx0, ty0, tz0]

instance Hittable BVHNode where
  hit (BVHLeaf objs _) ray interval =
    closestHitList (DV.toList (DV.map (\(SomeHittable o) -> H.hit o ray interval) objs))
  hit (BVHInternal left right _) ray interval =
    let !leftBox = boundingBox left
        !rightBox = boundingBox right
        !hitLeft = hitAABB leftBox ray interval
        !hitRight = hitAABB rightBox ray interval
     in case (hitLeft, hitRight) of
          (False, False) -> Nothing
          (True, False) -> hit left ray interval
          (False, True) -> hit right ray interval
          (True, True) ->
            -- Compute which box the ray hits first
            let !leftDist = entryDistance leftBox ray
                !rightDist = entryDistance rightBox ray
                (first, second) =
                  if leftDist < rightDist
                    then (left, right)
                    else (right, left)
                !firstHit = hit first ray interval
                !newInterval = maybe interval (Interval (minVal interval) . t) firstHit
                !secondHit = hit second ray newInterval
             in case (firstHit, secondHit) of
                  (Just f, Just s) -> if t f < t s then Just f else Just s
                  (Just f, Nothing) -> Just f
                  (Nothing, Just s) -> Just s
                  _ -> Nothing

  boundingBox (BVHLeaf _ box) = box
  boundingBox (BVHInternal _ _ box) = box
