module Rendering.Color where

import Core.Vec3 ( Vec3(..), scale, add)
import Utils.Constants (clamp)

type Color = Vec3  -- Define Color as an alias for Vec3

writeColor :: Color -> String
writeColor (Vec3 r g b) =
    unwords [show (truncate (255.999 * clamp r 0.0 1.0) :: Int),
             show (truncate (255.999 * clamp g 0.0 1.0) :: Int),
             show (truncate (255.999 * clamp b 0.0 1.0) :: Int)]

lerp :: Double -> Color -> Color -> Color
lerp t start end = scale (1.0 - t) start `add` scale t end