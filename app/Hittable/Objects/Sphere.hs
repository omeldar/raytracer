{-# LANGUAGE BangPatterns #-}

module Hittable.Objects.Sphere where

import Core.Ray
import Core.Vec3
import Hittable.BoundingBox (AABB (..))
import Hittable.Class
import Utils.Interval (contains)

data Sphere = Sphere
  { center :: !Vec3,
    radius :: !Double,
    color :: !Vec3,
    materialId :: !Int
  }
  deriving (Show)

instance Hittable Sphere where
  boundingBox (Sphere sCenter sRadius _ _) =
    let !rVec = Vec3 sRadius sRadius sRadius
     in AABB (sub sCenter rVec) (add sCenter rVec)

  hit sphere ray interval
    | discriminant < 0 = Nothing
    | contains interval t1 = Just (makeHitRecord t1)
    | otherwise = Nothing
    where
      !oc = origin ray `sub` center sphere
      !a = lengthSquared $ direction ray
      !h = dot oc $ direction ray
      !discriminant = h * h - a * (lengthSquared oc - radius sphere * radius sphere)
      !sqrtD = sqrt discriminant
      !t1 = (-h - sqrtD) / a

      makeHitRecord t' =
        let !p = ray `at` t'
            !outwardNormal = (1.0 / radius sphere) `scale` (p `sub` center sphere)
            !(faceNormal, front) = setFaceNormal ray outwardNormal
         in HitRecord p faceNormal t' front (Hittable.Objects.Sphere.color sphere) (Hittable.Objects.Sphere.materialId sphere)
