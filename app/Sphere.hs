module Sphere (hitSphere) where

import Vec3 as V
import Ray as R

hitSphere :: V.Vec3 -> Double -> R.Ray -> Bool
hitSphere center radius ray =
    let oc = R.origin ray `V.sub` center
        a = V.dot (R.direction ray) (R.direction ray)
        b = 2.0 * V.dot oc (R.direction ray)
        c = V.dot oc oc - radius * radius
        discriminant = b * b - 4 * a * c
    in discriminant > 0