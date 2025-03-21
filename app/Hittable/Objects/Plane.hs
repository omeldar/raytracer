module Hittable.Objects.Plane where

import Core.Ray as R (at, direction, origin)
import Core.Vec3 as V (Vec3, dot, sub)
import Hittable.Class
import Utils.Interval (contains)

data Plane = Plane
  { pointOnPlane :: V.Vec3,
    normal :: V.Vec3,
    color :: V.Vec3
  }
  deriving (Show)

instance Hittable Plane where
  hit (Plane p n c) ray interval =
    let denom = V.dot (R.direction ray) n
     in if abs denom > 1e-6
          then
            let t' = V.dot (V.sub p (R.origin ray)) n / denom
             in if contains interval t'
                  then
                    let hitPoint = R.at ray t'
                        (faceNormal, front) = setFaceNormal ray n
                     in Just $ HitRecord hitPoint faceNormal t' front c -- Store color in HitRecord
                  else Nothing
          else Nothing
