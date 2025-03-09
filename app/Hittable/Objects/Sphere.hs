module Hittable.Objects.Sphere where

import Core.Ray
import Core.Vec3
import Hittable.Class
import Utils.Interval (Interval, contains)

data Sphere = Sphere
  { center :: Vec3,
    radius :: Double
  }
  deriving (Show)

instance Hittable Sphere where
  hit sphere ray interval
    | discriminant < 0 = Nothing
    | contains interval t1 = Just (makeHitRecord t1)
    | contains interval t2 = Just (makeHitRecord t2)
    | otherwise = Nothing
    where
      oc = origin ray `sub` center sphere
      a = lengthSquared $ direction ray
      h = dot oc $ direction ray
      discriminant = h * h - a * (lengthSquared oc - radius sphere * radius sphere)
      sqrtD = sqrt discriminant
      t1 = (-h - sqrtD) / a
      t2 = (-h + sqrtD) / a

      makeHitRecord t' =
        let p = ray `at` t'
            outwardNormal = (1.0 / radius sphere) `scale` (p `sub` center sphere)
            (normal, front) = setFaceNormal ray outwardNormal
         in HitRecord p normal t' front
