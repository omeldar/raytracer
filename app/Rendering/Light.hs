{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module Rendering.Light where

import qualified Core.Ray as R
import Core.Vec3 as V (Vec3 (..), add, dot, normalize, scale, sub, vLength)
import Hittable.BVH (BVHNode)
import Hittable.Class as H (HitRecord (normal, point), Hittable (..))
import Utils.Interval (Interval (..))

data Light
  = PointLight {position :: V.Vec3, intensity :: V.Vec3}
  | DirectionalLight {direction :: V.Vec3, intensity :: V.Vec3}

computeLighting :: HitRecord -> [Light] -> BVHNode -> V.Vec3
computeLighting hitRecord lights bvh =
  let hitPoint = H.point hitRecord
      hitNormal = V.normalize (H.normal hitRecord)
      lightContribs = map (checkLightVisibility hitPoint hitNormal bvh) lights
   in foldr V.add (V.Vec3 0 0 0) lightContribs

checkLightVisibility :: Vec3 -> Vec3 -> BVHNode -> Light -> Vec3
checkLightVisibility origin lnormal bvh light =
  case light of
    PointLight pos pintensity ->
      let toLight = V.normalize (V.sub pos origin)
          dist = V.vLength (V.sub pos origin)
          shadowRay = R.Ray origin toLight
          isBlocked = case H.hit bvh shadowRay (Interval 0.001 (dist - 0.01)) of
            Just _ -> True
            Nothing -> False
       in if isBlocked
            then V.Vec3 0 0 0
            else
              let lightPower = max 0 (V.dot lnormal toLight)
                  attenuation = 1 / (dist * dist)
               in V.scale (lightPower * attenuation) pintensity
    DirectionalLight dir dintensity ->
      let toLight = V.normalize (V.scale (-1) dir)
          shadowRay = R.Ray origin toLight
          isBlocked = case H.hit bvh shadowRay (Interval 0.001 10000) of
            Just _ -> True
            Nothing -> False
       in if isBlocked
            then V.Vec3 0 0 0
            else
              let lightPower = max 0 (V.dot lnormal toLight)
               in V.scale lightPower dintensity

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = Just x
orElse Nothing y = y

lightContribution :: H.HitRecord -> Light -> V.Vec3
lightContribution hitRecord (PointLight pos lightIntensity) =
  let toLight = V.normalize (V.sub pos (H.point hitRecord))
      cosTheta = max 0 (V.dot toLight (H.normal hitRecord))
   in V.scale cosTheta lightIntensity
lightContribution hitRecord (DirectionalLight dir lightIntensity) =
  let toLight = V.normalize (V.scale (-1) dir) -- Light coming from a direction
      cosTheta = max 0 (V.dot toLight (H.normal hitRecord))
   in V.scale cosTheta lightIntensity
