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

import qualified Control.Monad
import Core.Ray as R (Ray (Ray, direction))
import Core.Vec3 as V (Vec3 (..), add, normalize, randomInUnitSphere, scale, y)
import Hittable.Class as H
  ( HitRecord (normal, point),
    Hittable (hit),
  )
import Hittable.HittableList as HL (HittableList (HittableList))
import Hittable.Objects.Sphere as S (Sphere (Sphere))
import Rendering.Camera as Cam (defaultCamera, generateRay)
import Rendering.Color as Col (Color, lerp)
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
    maxDepth = 50 -- Set the maximum depth for recursion in traceRay
    pixelColor :: Int -> Int -> IO Col.Color
    pixelColor i j = do
      sampledColors <- Control.Monad.replicateM samplesPerPixel (samplePixel i j)
      return $ averageColor sampledColors

    samplePixel :: Int -> Int -> IO Col.Color
    samplePixel i j = do
      uOffset <- randomDouble
      vOffset <- randomDouble
      let ray = Cam.generateRay camera i j width height uOffset vOffset
      traceRay ray maxDepth

averageColor :: [Color] -> Color
averageColor colors = scale (1.0 / fromIntegral (length colors)) (foldr add (V.Vec3 0 0 0) colors)

traceRay :: R.Ray -> Int -> IO Col.Color
traceRay ray depth
  | depth <= 0 = return (V.Vec3 0 0 0) -- Return black if recursion depth is exceeded
  | otherwise = do
      let spheres =
            HL.HittableList
              [ S.Sphere (V.Vec3 (-1.2) 0 (-1)) 0.5,
                S.Sphere (V.Vec3 0 0 (-1)) 0.5,
                S.Sphere (V.Vec3 1.2 0 (-1)) 0.5
              ]
          interval = Interval 0.001 100 -- Avoid shadow acne by ignoring self-intersections
      case H.hit spheres ray interval of
        Just hitRecord -> do
          randomVec <- V.randomInUnitSphere -- Generate a random bounce direction
          let newDirection = V.add (H.normal hitRecord) randomVec
              scatteredRay = R.Ray (H.point hitRecord) newDirection

          bounceColor <- traceRay scatteredRay (depth - 1) -- Recursive bounce
          return $ V.scale 0.5 bounceColor -- Attenuate the bounced color
        Nothing -> do
          -- Compute background color gradient
          let unitDir = V.normalize (R.direction ray)
              tHit = 0.5 * (V.y unitDir + 1.0)
          return $ Col.lerp tHit (V.Vec3 1 1 1) (V.Vec3 0.5 0.7 1) -- Linear interpolation for sky color

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