{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module Rendering.Light where

import qualified Core.Ray as R
import Core.Vec3 as V (Vec3 (..), add, dot, normalize, scale, sub, vLength)
import Hittable.BVH (BVHNode, closestHit)
import Hittable.Class as H (HitRecord (normal, point), Hittable (..))
import Hittable.HittableList (HittableList)
import Utils.Interval (Interval (..))

data Light
  = PointLight {position :: V.Vec3, intensity :: V.Vec3}
  | DirectionalLight {direction :: V.Vec3, intensity :: V.Vec3}

computeLighting :: HitRecord -> [Light] -> (BVHNode, HittableList) -> IO V.Vec3
computeLighting hitRecord lights world = do
  let hitPoint = H.point hitRecord
      hitNormal = V.normalize (H.normal hitRecord)

  lightContribs <- mapM (checkLightVisibility hitPoint hitNormal world) lights
  return $ foldr V.add (Vec3 0 0 0) lightContribs

checkLightVisibility :: Vec3 -> Vec3 -> (BVHNode, HittableList) -> Light -> IO Vec3
checkLightVisibility origin surfaceNormal world light =
  case light of
    PointLight pos pLightIntensity -> do
      let toLight = V.sub pos origin
          distance = V.vLength toLight
          plDirection = V.normalize toLight

          shadowOrigin = V.add origin (V.scale 0.001 plDirection)

          shadowRay = R.Ray shadowOrigin plDirection

          shadowInterval = Interval 0.001 (distance - 1e-3)
          bvhHit = H.hit (fst world) shadowRay shadowInterval
          listHit = H.hit (snd world) shadowRay shadowInterval
          shadowHit = closestHit shadowRay shadowInterval bvhHit listHit

      return $
        case shadowHit of
          Just _ -> Vec3 0 0 0 -- In shadow
          Nothing ->
            let diff = max 0 (V.dot surfaceNormal plDirection)
             in V.scale diff pLightIntensity

    -- If light is a directional light
    DirectionalLight dir dLightIntensity -> do
      let dlDirection = V.normalize (V.scale (-1) dir)

          shadowOrigin = V.add origin (V.scale 0.001 dlDirection)

          shadowRay = R.Ray shadowOrigin dlDirection

          shadowInterval = Interval 0.001 1.0e30
          bvhHit = H.hit (fst world) shadowRay shadowInterval
          listHit = H.hit (snd world) shadowRay shadowInterval
          shadowHit = closestHit shadowRay shadowInterval bvhHit listHit

      return $
        case shadowHit of
          Just _ -> Vec3 0 0 0
          Nothing ->
            let diff = max 0 (V.dot surfaceNormal dlDirection)
             in V.scale diff dLightIntensity

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
