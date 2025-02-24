module Rendering.Camera (
    Camera(..),
    defaultCamera,
    generateRay
) where

import qualified Core.Vec3 as V
import qualified Core.Ray as R

-- Define the Camera type
data Camera = Camera
    { origin :: V.Vec3
    , lowerLeftCorner :: V.Vec3
    , horizontal :: V.Vec3
    , vertical :: V.Vec3
    } deriving (Show)

-- Create a default camera
defaultCamera :: Int -> Int -> Camera
defaultCamera width height =
    let aspectRatio = fromIntegral width / fromIntegral height
        viewportHeight = 2.0
        viewportWidth = viewportHeight * aspectRatio

        focalLength = 1.0

        origin = V.Vec3 0.0 0.0 0.0
        horizontal = V.Vec3 viewportWidth 0.0 0.0
        vertical = V.Vec3 0.0 viewportHeight 0.0
        lowerLeftCorner = origin `V.sub` V.scale 0.5 horizontal
                                `V.sub` V.scale 0.5 vertical
                                `V.sub` V.Vec3 0.0 0.0 focalLength
    in Camera origin lowerLeftCorner horizontal vertical

-- Generate a ray for pixel (i, j)
generateRay :: Camera -> Int -> Int -> Int -> Int -> R.Ray
generateRay camera i j width height =
    let u = fromIntegral i / fromIntegral (width - 1)
        v = fromIntegral j / fromIntegral (height - 1)
        direction = (lowerLeftCorner camera `V.add`
                     V.scale u (horizontal camera) `V.add`
                     V.scale v (vertical camera))
                    `V.sub` origin camera
    in R.Ray (origin camera) (V.normalize direction)