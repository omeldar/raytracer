module Hittable.BVH (BVHNode (..), constructBVH, closestHit) where

import Core.Ray (Ray (..))
import Core.Vec3 (Vec3 (..), add, lengthSquared, maxVec3, minVec3, scale, sub, x, y, z)
import Data.Function (on)
import Data.List (sortBy)
import Hittable.Class (HitRecord (..), Hittable (..))
import Hittable.Objects.Triangle (Triangle (..))
import Utils.Interval (Interval (..))

data AABB = AABB
  { minBounds :: Vec3,
    maxBounds :: Vec3
  }

boundingBox :: Triangle -> AABB
boundingBox (Triangle bxv0 bxv1 bxv2 _) =
  let minB = minVec3 (minVec3 bxv0 bxv1) bxv2
      maxB = maxVec3 (maxVec3 bxv0 bxv1) bxv2
   in AABB minB maxB

data BVHNode
  = BVHLeaf [Triangle] AABB
  | BVHInternal BVHNode BVHNode AABB

constructBVH :: [Triangle] -> BVHNode
constructBVH [] = error "Cannot construct BVH from empty list"
constructBVH [tri] = BVHLeaf [tri] (boundingBox tri)
constructBVH tris =
  let box =
        AABB
          (foldr (minVec3 . minBounds . boundingBox) (Vec3 1e30 1e30 1e30) tris)
          (foldr (maxVec3 . maxBounds . boundingBox) (Vec3 (-1e30) (-1e30) (-1e30)) tris)
      axis = longestAxis box
      sortedTris = sortBy (compare `on` (getAxisValue axis . minBounds . boundingBox)) tris
      (left, right) = splitAt (length sortedTris `div` 2) sortedTris
   in BVHInternal (constructBVH left) (constructBVH right) box

longestAxis :: AABB -> Int
longestAxis (AABB (Vec3 xMin yMin zMin) (Vec3 xMax yMax zMax)) =
  let xLen = xMax - xMin
      yLen = yMax - yMin
      zLen = zMax - zMin
   in if xLen > yLen && xLen > zLen then 0 else if yLen > zLen then 1 else 2

getAxisValue :: Int -> Vec3 -> Double
getAxisValue 0 = x
getAxisValue 1 = y
getAxisValue 2 = z
getAxisValue _ = error "Invalid axis value"

instance Hittable BVHNode where
  hit (BVHLeaf triangles _) ray interval =
    foldl (closestHit ray interval) Nothing (map (\tri -> hit tri ray interval) triangles)
  hit (BVHInternal left right aabb) ray interval
    | not (intersectsAABB aabb ray) = Nothing -- Skip if ray doesn't hit the bounding box
    | otherwise =
        let (first, second) = orderByDistance left right ray -- Prioritize closer node
            firstHit = hit first ray interval
         in case firstHit of
              Just hitRec ->
                -- If first hit succeeds, we can reduce the interval for second
                let newInterval = Interval (minVal interval) (t hitRec)
                    secondHit = hit second ray newInterval
                 in closestHit ray interval firstHit secondHit
              Nothing -> hit second ray interval -- If no hit in first, check second

intersectsAABB :: AABB -> Ray -> Bool
intersectsAABB (AABB (Vec3 xMin yMin zMin) (Vec3 xMax yMax zMax)) (Ray rOrigin dir) =
  let tx1 = (xMin - x rOrigin) / x dir
      tx2 = (xMax - x rOrigin) / x dir
      ty1 = (yMin - y rOrigin) / y dir
      ty2 = (yMax - y rOrigin) / y dir
      tz1 = (zMin - z rOrigin) / z dir
      tz2 = (zMax - z rOrigin) / z dir
      tMin = max (max (min tx1 tx2) (min ty1 ty2)) (min tz1 tz2)
      tMax = min (min (max tx1 tx2) (max ty1 ty2)) (max tz1 tz2)
   in tMax >= max 0 tMin

closestHit :: Ray -> Interval -> Maybe HitRecord -> Maybe HitRecord -> Maybe HitRecord
closestHit _ _ Nothing h2 = h2
closestHit _ _ h1 Nothing = h1
closestHit _ _ (Just h1) (Just h2) = if t h1 < t h2 then Just h1 else Just h2

orderByDistance :: BVHNode -> BVHNode -> Ray -> (BVHNode, BVHNode)
orderByDistance left right (Ray orig _) =
  let leftDist = boundingBoxCenterDist left orig
      rightDist = boundingBoxCenterDist right orig
   in if leftDist < rightDist then (left, right) else (right, left)

boundingBoxCenterDist :: BVHNode -> Vec3 -> Double
boundingBoxCenterDist (BVHLeaf _ (AABB minB maxB)) rayOrigin =
  let center = scale 0.5 (add minB maxB) -- Compute box center
   in lengthSquared (sub center rayOrigin) -- Squared distance (no sqrt needed -> more efficient)
boundingBoxCenterDist (BVHInternal _ _ (AABB minB maxB)) rayOrigin =
  let center = scale 0.5 (add minB maxB)
   in lengthSquared (sub center rayOrigin)