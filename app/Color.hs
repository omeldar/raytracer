module Color where

import Vec3 ( Vec3(..) )

writeColor :: Vec3 -> String
writeColor (Vec3 r g b) =
    unwords [show (truncate (255.999 * r) :: Int),
             show (truncate (255.999 * g) :: Int),
             show (truncate (255.999 * b) :: Int)]