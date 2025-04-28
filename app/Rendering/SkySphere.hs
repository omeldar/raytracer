module Rendering.SkySphere
  ( SkySphere (..),
    loadSkySphere,
    sampleSkySphere,
  )
where

import Codec.Picture (DynamicImage (..), Image (..), PixelRGB8 (..), PixelRGBF (..), convertRGB8, pixelAt, readImage)
import Core.Vec3
import Data.List (isSuffixOf)

-- Define sky types: 8-bit (LDR) or floating-point (HDR)
data SkySphere
  = LDRSkySphere {skyWidth :: !Int, skyHeight :: !Int, skyPixels :: Image PixelRGB8}
  | HDRSkySphere {skyWidth :: !Int, skyHeight :: !Int, skyHDRPixels :: Image PixelRGBF}

-- Load a sky sphere from a file
loadSkySphere :: FilePath -> IO SkySphere
loadSkySphere path = do
  eimg <- readImage path
  case eimg of
    Left err -> error ("Failed to load sky texture: " ++ err)
    Right dynimg ->
      if ".hdr" `isSuffixOf` path
        then case dynimg of
          ImageRGBF hdr -> return $ HDRSkySphere (imageWidth hdr) (imageHeight hdr) hdr
          _ -> error "Expected HDR image (.hdr) but got different format!"
        else case convertRGB8 dynimg of
          img -> return $ LDRSkySphere (imageWidth img) (imageHeight img) img

-- Sample a color from sky sphere based on ray direction
sampleSkySphere :: SkySphere -> Vec3 -> Vec3
sampleSkySphere sky dir =
  let Vec3 dx dy dz = normalize dir
      u = 0.5 + atan2 dz dx / (2 * pi)
      v = 0.5 - asin dy / pi
      w = skyWidth sky
      h = skyHeight sky
      i = clampInt (floor (u * fromIntegral w)) 0 (w - 1)
      j = clampInt (floor (v * fromIntegral h)) 0 (h - 1)
   in case sky of
        LDRSkySphere _ _ img ->
          let PixelRGB8 r g b = pixelAt img i j
           in Vec3 (fromIntegral r / 255) (fromIntegral g / 255) (fromIntegral b / 255)
        HDRSkySphere _ _ img ->
          let PixelRGBF r g b = pixelAt img i j
           in Vec3 (realToFrac r) (realToFrac g) (realToFrac b)

clampInt :: Int -> Int -> Int -> Int
clampInt theInt minV maxV = max minV (min theInt maxV)