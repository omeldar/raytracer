module Rendering.Camera
  ( Camera (..),
    defaultCamera,
    generateRay,
  )
where

import qualified Core.Ray as R
import qualified Core.Vec3 as V

-- Define the Camera type
data Camera = Camera
  { origin :: V.Vec3,
    lowerLeftCorner :: V.Vec3,
    horizontal :: V.Vec3,
    vertical :: V.Vec3
  }
  deriving (Show)

-- Create a default camera
defaultCamera :: Int -> Int -> Camera
defaultCamera width height =
  let aspectRatio = fromIntegral width / fromIntegral height
      viewportHeight = 2.0
      viewportWidth = viewportHeight * aspectRatio

      focalLength = 5.0

      cOrigin = V.Vec3 0.0 0.0 5.0
      cHorizontal = V.Vec3 viewportWidth 0.0 0.0
      cVertical = V.Vec3 0.0 viewportHeight 0.0
      cLowerLeftCorner =
        cOrigin
          `V.sub` V.scale 0.5 cHorizontal
          `V.sub` V.scale 0.5 cVertical
          `V.sub` V.Vec3 0.0 0.0 focalLength
   in Camera cOrigin cLowerLeftCorner cHorizontal cVertical

-- Generate a ray for pixel (i, j)
generateRay :: Camera -> Int -> Int -> Int -> Int -> Double -> Double -> R.Ray
generateRay camera i j width height uOffset vOffset =
  let u = (fromIntegral i + uOffset) / fromIntegral (width - 1)
      v = (fromIntegral j + vOffset) / fromIntegral (height - 1)
      direction =
        ( lowerLeftCorner camera
            `V.add` V.scale u (horizontal camera)
            `V.add` V.scale v (vertical camera)
        )
          `V.sub` origin camera
   in R.Ray (origin camera) (V.normalize direction)
