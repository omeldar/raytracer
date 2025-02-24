module Hittable.Objects.Sphere where

import Core.Vec3
import Core.Ray
import Hittable.Class

data Sphere = Sphere
    { center :: Vec3
    , radius :: Double
    } deriving (Show)

instance Hittable Sphere where
    hit sphere ray tMin tMax
        | discriminant < 0 = Nothing
        | t1 >= tMin && t1 <= tMax = Just (makeHitRecord t1)
        | t2 >= tMin && t2 <= tMax = Just (makeHitRecord t2)
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
                in HitRecord p outwardNormal t'
