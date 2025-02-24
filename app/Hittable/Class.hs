module Hittable.Class where

import Core.Vec3
import Core.Ray

data HitRecord = HitRecord
    { point :: Vec3
    , normal :: Vec3
    , t :: Double 
    } deriving (Show)

class Hittable a where
    hit :: a -> Ray -> Double -> Double -> Maybe HitRecord
