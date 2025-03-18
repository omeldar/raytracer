{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module Rendering.Light where

import Core.Vec3 as V (Vec3 (..), add, dot, normalize, scale, sub)
import Hittable.Class as H (HitRecord (normal, point))

data Light
  = PointLight {position :: V.Vec3, intensity :: V.Vec3}
  | DirectionalLight {direction :: V.Vec3, intensity :: V.Vec3}

computeLighting :: HitRecord -> [Light] -> V.Vec3
computeLighting hitRecord lights =
  let hitNormal = normalize (H.normal hitRecord) -- Renamed to avoid shadowing
      lightContributions =
        map
          ( \light -> case light of
              PointLight pos localIntensity ->
                let lightDir = normalize (pos `sub` H.point hitRecord)
                    diff = max 0 (dot hitNormal lightDir) -- Increase shadow contrast
                 in scale diff localIntensity
              DirectionalLight dir localIntensity ->
                let lightDir = normalize dir
                    diff = max 0 (dot hitNormal lightDir)
                 in scale diff localIntensity
          )
          lights -- Correct indentation of `) lights`
   in foldr add (Vec3 0 0 0) lightContributions

lightContribution :: H.HitRecord -> Light -> V.Vec3
lightContribution hitRecord (PointLight pos lightIntensity) =
  let toLight = V.normalize (V.sub pos (H.point hitRecord))
      cosTheta = max 0 (V.dot toLight (H.normal hitRecord))
   in V.scale cosTheta lightIntensity
lightContribution hitRecord (DirectionalLight dir lightIntensity) =
  let toLight = V.normalize (V.scale (-1) dir) -- Light coming from a direction
      cosTheta = max 0 (V.dot toLight (H.normal hitRecord))
   in V.scale cosTheta lightIntensity
