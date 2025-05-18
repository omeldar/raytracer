{-# LANGUAGE NumericUnderscores #-}

module Rendering.Trace where

import Core.Ray as R (Ray (..), direction)
import Core.Vec3 as V (Vec3 (..), add, dot, mul, normalize, reflect, refract, scale)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Hittable.BVH (BVHNode)
import Hittable.Class as H (HitRecord (..), Hittable (hit))
import Rendering.Color as Col (Color)
import Rendering.Light (computeLighting)
import qualified Rendering.Light as L (Light (..))
import Rendering.Material (Material (..), defaultMaterial)
import Rendering.SkySphere (SkySphere, sampleSkySphere)
import System.Random (Random (..), StdGen)
import Utils.Interval (Interval (..))

traceRay :: BVHNode -> M.Map Int Material -> Maybe SkySphere -> (R.Ray -> Col.Color) -> [L.Light] -> R.Ray -> Int -> StdGen -> Col.Color
traceRay world materialMap skySphere backgroundFunc sceneLights ray0 maxDepth = traceLoop ray0 maxDepth (V.Vec3 1 1 1)
  where
    traceLoop _ 0 attenuation _ = backgroundSample attenuation ray0
    traceLoop ray depth attenuation rng =
      case H.hit world ray (Interval 0.001 10_000) of
        Nothing -> backgroundSample attenuation ray
        Just rec ->
          let matId = H.materialId rec
              mat = fromMaybe defaultMaterial (M.lookup matId materialMap)
              emitted = fromMaybe (V.Vec3 0 0 0) (emissionColor mat)
              surfaceColor = diffuseColor mat
              outwardNormal = H.normal rec
              unitDir = V.normalize (R.direction ray)
              hitPoint = H.point rec

              directLight = computeLighting rec sceneLights world
              litColor = surfaceColor `V.mul` directLight

              (randX, rng1) = randomR (-1.0, 1.0) rng
              (randY, rng2) = randomR (-1.0, 1.0) rng1
              (randZ, rng3) = randomR (-1.0, 1.0) rng2
              (randD, rng4) = randomR (0.0, 1.0) rng3
              randVec = V.normalize (V.Vec3 randX randY randZ)

              tfrontFace = frontFace rec

              nextRay = case () of
                _
                  | transmission mat == Just 1.0,
                    Just refIdx <- ior mat ->
                      let eta = if tfrontFace then 1.0 / refIdx else refIdx
                          cosTheta = min (negate (V.dot unitDir outwardNormal)) 1.0
                          sinTheta = sqrt (1.0 - cosTheta * cosTheta)
                          cannotRefract = eta * sinTheta > 1.0
                          reflectProb = schlick cosTheta eta
                          bouncedir
                            | cannotRefract = V.reflect unitDir outwardNormal
                            | randD < reflectProb = V.reflect unitDir outwardNormal
                            | otherwise = V.refract unitDir outwardNormal eta
                       in R.Ray hitPoint bouncedir
                  | Just s <- shininess mat,
                    s > 100 ->
                      let reflected = V.reflect unitDir outwardNormal
                          fuzzed = V.add reflected (V.scale 0.05 randVec)
                       in R.Ray hitPoint (V.normalize fuzzed)
                  | otherwise ->
                      let scatterDir = V.add outwardNormal randVec
                       in R.Ray hitPoint (V.normalize scatterDir)

              newAttenuation =
                case transmission mat of
                  Just 1.0 -> attenuation
                  _ -> attenuation `V.mul` surfaceColor

              bounceColor = traceLoop nextRay (depth - 1) newAttenuation rng4
              clampedColor = clamp bounceColor 0 10.0
           in emitted `V.add` litColor `V.add` clampedColor

    backgroundSample attenuation ray =
      case skySphere of
        Just sky ->
          attenuation `V.mul` sampleSkySphere sky (R.direction ray)
        Nothing ->
          attenuation `V.mul` backgroundFunc ray

schlick :: (Floating a) => a -> a -> a
schlick cosine refIdx =
  let r0 = (1 - refIdx) / (1 + refIdx)
      r0sq = r0 * r0
   in r0sq + (1 - r0sq) * ((1 - cosine) ** 5)

clamp :: Vec3 -> Double -> Double -> Vec3
clamp (Vec3 cx cy cz) lo hi = Vec3 (cl cx) (cl cy) (cl cz)
  where
    cl v = max lo (min hi v)

toneMap :: Double -> Vec3 -> Vec3
toneMap toneMapExposure (Vec3 r g b) =
  Vec3
    (r * toneMapExposure / (r * toneMapExposure + 1))
    (g * toneMapExposure / (g * toneMapExposure + 1))
    (b * toneMapExposure / (b * toneMapExposure + 1))

gammaCorrect :: Double -> Vec3 -> Vec3
gammaCorrect gExp (Vec3 r g b) =
  Vec3 (r ** gExp) (g ** gExp) (b ** gExp)