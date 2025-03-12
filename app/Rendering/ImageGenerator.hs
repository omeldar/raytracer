module Rendering.ImageGenerator
  ( -- types
    Pixel,
    Row,
    Image,
    -- functions
    createPPM,
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
import System.IO (BufferMode (BlockBuffering), Handle, IOMode (WriteMode), hPutStr, hSetBuffering, withFile)
import Utils.Constants (randomDouble)
import Utils.Interval (Interval (..))

-- Define Pixel as Vec3 (representing RGB color)
type Pixel = V.Vec3

type Row = [Pixel]

type Image = [Row]

createPPM :: Int -> Int -> Int -> Bool -> String -> IO ()
createPPM width height samplesPerPixel aa filename =
  withFile filename WriteMode $ \handle -> do
    hSetBuffering handle (BlockBuffering (Just (1024 * 512))) -- Enable buffering
    hPutStr handle ("P3\n" ++ show width ++ " " ++ show height ++ "\n255\n") -- Write header
    mapM_ (`processRow` handle) [0 .. height - 1]
    putStrLn $ "Image saved to " ++ filename -- Print confirmation
  where
    lookFrom = V.Vec3 0 0 5 -- Camera position
    lookAt = V.Vec3 0 0 (-1) -- Point the camera is looking at
    vUp = V.Vec3 0 1 0 -- "Up" direction
    vfov = 30.0 -- Field of view (in degrees)
    aperture = 0.0 -- Small aperture for defocus blur
    focusDist = 1.0
    camera = Cam.defaultCamera lookFrom lookAt vUp vfov (fromIntegral width / fromIntegral height) aperture focusDist

    processRow :: Int -> Handle -> IO ()
    processRow j handle = do
      row <- mapM (\i -> pixelColor i (height - 1 - j)) [0 .. width - 1]
      hPutStr handle (unlines (map showPixel row) ++ "\n")

    maxDepth = 50 -- Set the maximum depth for recursion in traceRay
    pixelColor :: Int -> Int -> IO Col.Color
    pixelColor i j = do
      sampledColors <- Control.Monad.replicateM samplesPerPixel (samplePixel i j)
      return $ averageColor sampledColors

    samplePixel :: Int -> Int -> IO Col.Color
    samplePixel i j = do
      uOffset <- if aa then randomDouble else return 0.5
      vOffset <- if aa then randomDouble else return 0.5
      ray <- Cam.generateRay camera i j width height uOffset vOffset
      traceRay ray maxDepth

    showPixel :: Pixel -> String
    showPixel (V.Vec3 r g b) = unwords $ map (show . truncate . (* 255.999)) [r, g, b]

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
                S.Sphere (V.Vec3 1.2 0.3 (-1.6)) 0.5
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