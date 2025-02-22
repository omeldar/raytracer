module Sphere (hitSphere) where

import Vec3 as V
import Ray as R
import Color as C

hitSphere :: V.Vec3 -> Double -> R.Ray -> Double
hitSphere center radius ray
    | discriminant < 0 = -1.0
    | t1 > 0.0 = t1
    | t2 > 0.0 = t2
    | otherwise = -1.0
    where 
        oc = R.origin ray `V.sub` center
        a = V.dot (R.direction ray) (R.direction ray)
        b = 2.0 * V.dot (R.direction ray) oc
        c = V.dot oc oc - radius * radius
        discriminant = b * b - 4 * a * c
        sqrtD = sqrt discriminant
        t1 = (-b - sqrtD) / (2.0 * a)  -- Closer intersection
        t2 = (-b + sqrtD) / (2.0 * a)  -- Farther intersection
