module Hittable.Objects.Triangle where

import Core.Ray as R (at, direction, origin)
import Core.Vec3 as V (Vec3, cross, dot, maxVec3, minVec3, normalize, sub)
import Data.Typeable (Typeable)
import Hittable.BoundingBox (AABB (..))
import Hittable.Class as H
import Utils.Interval (contains)

data Triangle = Triangle
  { v0 :: V.Vec3,
    v1 :: V.Vec3,
    v2 :: V.Vec3,
    color :: V.Vec3,
    materialId :: Int
  }
  deriving (Show, Typeable)

-- Möller-Trumbore intersection algorithm
instance Hittable Triangle where
  boundingBox (Triangle a b c _ _) =
    let minB = minVec3 (minVec3 a b) c
        maxB = maxVec3 (maxVec3 a b) c
     in AABB minB maxB

  hit (Triangle p0 p1 p2 col matId) ray interval =
    let epsilon = 1e-8
        e1 = V.sub p1 p0
        e2 = V.sub p2 p0
        h = V.cross (R.direction ray) e2
        a = V.dot e1 h
     in if abs a < epsilon
          then Nothing
          else
            let f = 1.0 / a
                s = V.sub (R.origin ray) p0
                u = f * V.dot s h
             in if u < 0.0 || u > 1.0
                  then Nothing
                  else
                    let q = V.cross s e1
                        v = f * V.dot (R.direction ray) q
                     in if v < 0.0 || u + v > 1.0
                          then Nothing
                          else
                            let intersect = f * V.dot e2 q
                                normalV = normalize $ V.cross e1 e2
                                (faceNormal, front) = setFaceNormal ray normalV
                             in if contains interval intersect
                                  then
                                    Just $
                                      HitRecord
                                        { point = R.at ray intersect,
                                          normal = faceNormal,
                                          t = intersect,
                                          frontFace = front,
                                          H.color = col,
                                          H.materialId = matId
                                        }
                                  else Nothing
