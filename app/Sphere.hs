module Sphere (hitSphere) where

import Vec3 as V
import Ray as R

hitSphere :: V.Vec3 -> Double -> R.Ray -> Double
hitSphere center radius ray
    | discriminant < 0 = -1.0
    | otherwise = (-h - sqrt discriminant) / a
    where 
        oc = R.origin ray `V.sub` center
        a = V.lengthSquared $ R.direction ray
        h = V.dot oc $ R.direction ray
        discriminant = h * h - a * (V.lengthSquared oc - radius * radius)
