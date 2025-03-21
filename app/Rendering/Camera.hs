module Rendering.Camera
  ( Camera (..),
    defaultCamera,
    generateRay,
  )
where

import qualified Core.Ray as R
import qualified Core.Vec3 as V
import Utils.Constants (degreesToRadians)

-- Define the Camera type
data Camera = Camera
  { origin :: V.Vec3,
    lowerLeftCorner :: V.Vec3,
    horizontal :: V.Vec3,
    vertical :: V.Vec3,
    aperture :: Double,
    u :: V.Vec3,
    v :: V.Vec3,
    w :: V.Vec3
  }
  deriving (Show)

-- Create a configurable camera with FOV, aperture, and focus distance
defaultCamera :: V.Vec3 -> V.Vec3 -> V.Vec3 -> Double -> Double -> Double -> Double -> Camera
defaultCamera lookFrom lookAt vUp vfov aspectRatio camAperture focusDist =
  let theta = degreesToRadians vfov
      h = tan (theta / 2)
      viewportHeight = 2.0 * h * focusDist
      viewportWidth = viewportHeight * aspectRatio

      camW = V.normalize (V.sub lookFrom lookAt) -- Camera direction
      camU = V.normalize (V.cross vUp camW) -- Camera right
      camV = V.cross camW camU -- Camera up
      camOrigin = lookFrom
      camHorizontal = V.scale viewportWidth camU
      camVertical = V.scale viewportHeight camV
      camLowerLeftCorner =
        let temp1 = V.sub camOrigin (V.scale 0.5 camHorizontal)
            temp2 = V.sub temp1 (V.scale 0.5 camVertical)
            temp3 = V.sub temp2 (V.scale focusDist camW)
         in temp3
   in Camera camOrigin camLowerLeftCorner camHorizontal camVertical camAperture camU camV camW

-- Generate a ray with depth of field (DOF) effect
generateRay :: Camera -> Int -> Int -> Int -> Int -> Double -> Double -> IO R.Ray
generateRay camera i j width height uOffset vOffset = do
  lensRand <- V.randomInUnitDisk
  let lensOffset = V.scale (aperture camera / 2) (V.add (V.scale (V.x lensRand) (u camera)) (V.scale (V.y lensRand) (v camera)))
      origin' = V.add (origin camera) lensOffset
      ww = fromIntegral width
      hh = fromIntegral height
      rayu = (fromIntegral i + uOffset) / ww
      rayv = (fromIntegral j + vOffset) / hh
      direction =
        V.sub
          (V.add (V.add (lowerLeftCorner camera) (V.scale rayu (horizontal camera))) (V.scale rayv (vertical camera)))
          origin'
  return (R.Ray origin' (V.normalize direction))
