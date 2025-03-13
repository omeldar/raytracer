module Rendering.Light where

import Core.Vec3 as V (Vec3 (..), normalize, sub, dot, scale, add)
import Hittable.Class as H (HitRecord (normal, point))

data Light
  = PointLight { position :: V.Vec3, intensity :: V.Vec3 }
  | DirectionalLight { direction :: V.Vec3, intensity :: V.Vec3 }

computeLighting :: H.HitRecord -> [Light] -> V.Vec3
computeLighting hitRecord lights =
  foldr V.add (V.Vec3 0 0 0) (map (lightContribution hitRecord) lights)

lightContribution :: H.HitRecord -> Light -> V.Vec3
lightContribution hitRecord (PointLight pos intensity) =
  let toLight = V.normalize (V.sub pos (H.point hitRecord))
      cosTheta = max 0 (V.dot toLight (H.normal hitRecord))
   in V.scale cosTheta intensity

lightContribution hitRecord (DirectionalLight dir intensity) =
  let toLight = V.normalize (V.scale (-1) dir) -- Light coming from a direction
      cosTheta = max 0 (V.dot toLight (H.normal hitRecord))
   in V.scale cosTheta intensity
