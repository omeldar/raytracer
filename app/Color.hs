module Color where

import Vec3 ( Vec3(..), scale, add)

type Color = Vec3  -- Define Color as an alias for Vec3

writeColor :: Color -> String
writeColor (Vec3 r g b) =
    unwords [show (truncate (255.999 * r) :: Int),
             show (truncate (255.999 * g) :: Int),
             show (truncate (255.999 * b) :: Int)]

lerp :: Double -> Color -> Color -> Color
lerp t start end = scale (1.0 - t) start `add` scale t end