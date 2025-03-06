module Hittable.Class where

import Core.Vec3
import Core.Ray
import Utils.Interval (Interval, contains)

data HitRecord = HitRecord
    { point :: Vec3
    , normal :: Vec3
    , t :: Double
    , frontFace :: Bool
    } deriving (Show)

class Hittable a where
    hit :: a -> Ray -> Interval -> Maybe HitRecord

setFaceNormal :: Ray -> Vec3 -> (Vec3, Bool)
setFaceNormal ray outwardNormal =
    let front = dot (direction ray) outwardNormal < 0
        normal = if front then outwardNormal else negateV outwardNormal
    in (normal, front)
