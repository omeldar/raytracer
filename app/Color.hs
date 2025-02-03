module Color
    ( Color
    , toColor
    , colorToString
    ) where

import Vec3 ( Vec3(..), x, y, z )

type Color = Vec3

toColor :: Double -> Double -> Double -> Color
toColor = Vec3

colorToString :: Color -> String
colorToString color =
    let rByte = toByte $ x color
        gByte = toByte $ y color
        bByte = toByte $ z color
    in unwords $ map show [rByte, gByte, bByte]

toByte :: Double -> Int
toByte component = floor $ 255.999 * component