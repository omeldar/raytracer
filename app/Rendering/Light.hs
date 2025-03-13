module Rendering.Light where

import Core.Vec3 as V (Vec3 (..), add, dot, normalize, scale, sub)
import Hittable.Class as H (HitRecord (normal, point))

data Light
  = PointLight {position :: V.Vec3, intensity :: V.Vec3}
  | DirectionalLight {direction :: V.Vec3, intensity :: V.Vec3}

computeLighting :: H.HitRecord -> [Light] -> V.Vec3
computeLighting hitRecord = foldr (V.add . lightContribution hitRecord) (V.Vec3 0 0 0)

lightContribution :: H.HitRecord -> Light -> V.Vec3
lightContribution hitRecord (PointLight pos lightIntensity) =
  let toLight = V.normalize (V.sub pos (H.point hitRecord))
      cosTheta = max 0 (V.dot toLight (H.normal hitRecord))
   in V.scale cosTheta lightIntensity
lightContribution hitRecord (DirectionalLight dir lightIntensity) =
  let toLight = V.normalize (V.scale (-1) dir) -- Light coming from a direction
      cosTheta = max 0 (V.dot toLight (H.normal hitRecord))
   in V.scale cosTheta lightIntensity
