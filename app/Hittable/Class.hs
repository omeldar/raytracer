module Hittable.Class where

import Core.Ray
import Core.Vec3
import Hittable.BoundingBox (AABB)
import Rendering.Material
import Utils.Interval (Interval)

data HitRecord = HitRecord
  { point :: Vec3,
    normal :: Vec3,
    t :: Double,
    frontFace :: Bool,
    color :: Vec3,
    material :: MaterialType
  }
  deriving (Show)

class Hittable a where
  hit :: a -> Ray -> Interval -> Maybe HitRecord
  boundingBox :: a -> AABB

setFaceNormal :: Ray -> Vec3 -> (Vec3, Bool)
setFaceNormal ray outwardNormal =
  let front = dot (direction ray) outwardNormal < 0
      faceNormal = if front then outwardNormal else negateV outwardNormal
   in (faceNormal, front)
