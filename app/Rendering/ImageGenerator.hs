module Rendering.ImageGenerator
  ( -- types
    Pixel,
    Row,
    Image,
    -- functions
    createPPM,
    ppmToStr,
    createAndWriteFile,
  )
where

import Core.Ray as R
import Core.Vec3 as V
import Hittable.Class as H
import Hittable.HittableList as HL
import Hittable.Objects.Sphere as S
import Rendering.Camera as Cam
import Rendering.Color as Col
import Utils.Constants (randomDouble)
import Utils.Interval (Interval (..))

-- Define Pixel as Vec3 (representing RGB color)
type Pixel = V.Vec3

type Row = [Pixel]

type Image = [Row]

createPPM :: Int -> Int -> Int -> IO Image
createPPM width height samplesPerPixel =
  mapM (\j -> mapM (\i -> pixelColor i (height - 1 - j)) [0 .. width - 1]) [0 .. height - 1]
  where
    camera = Cam.defaultCamera width height
    pixelColor i j = do
      sampledColors <-
        mapM
          ( \_ -> do
              uOffset <- randomDouble
              vOffset <- randomDouble
              let ray = Cam.generateRay camera i j width height uOffset vOffset
              return $ traceRay ray
          )
          [1 .. samplesPerPixel]
      return $ averageColor sampledColors

averageColor :: [Color] -> Color
averageColor colors = scale (1.0 / fromIntegral (length colors)) (foldr add (V.Vec3 0 0 0) colors)

traceRay :: R.Ray -> Col.Color
traceRay ray =
  let spheres =
        HittableList
          [ S.Sphere (V.Vec3 (-1.2) 0 (-1)) 0.5,
            S.Sphere (V.Vec3 0.0 0 (-1)) 0.5,
            S.Sphere (V.Vec3 (1.2) 0 (-1)) 0.5
          ]
      interval = Interval 0.001 100 -- Avoids shadow acne
   in case H.hit spheres ray interval of
        Just hitRec ->
          let normalizedNormal = V.normalize (H.normal hitRec)
           in if H.frontFace hitRec
                then 0.5 `V.scale` (normalizedNormal `V.add` V.Vec3 1 1 1)
                else V.Vec3 1.0 0.0 0.0
        Nothing ->
          Col.lerp
            (0.5 * (V.y (V.normalize (R.direction ray)) + 1.0))
            (V.Vec3 1 1 1)
            (V.Vec3 0.5 0.7 1.0)

-- Convert an Image to a PPM string
ppmToStr :: Image -> String
ppmToStr image =
  let height = length image
      width = if null image then 0 else length (head image)
      header = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n"
      pixelData = unlines $ map (unwords . map showPixel) image
   in header ++ pixelData
  where
    -- Convert a Vec3 (Pixel) to a PPM format string
    showPixel :: Pixel -> String
    showPixel (V.Vec3 r g b) =
      unwords $ map (show . (truncate :: Double -> Int) . (* 255.999)) [r, g, b]

-- Write the PPM image to a file
createAndWriteFile :: String -> String -> IO ()
createAndWriteFile = writeFile