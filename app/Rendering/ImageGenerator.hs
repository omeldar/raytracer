module Rendering.ImageGenerator
  ( -- types
    Pixel,
    Image,
    -- functions
    createPPM,
  )
where

import Control.Monad (forM, replicateM, when)
import Control.Monad.IO.Class (liftIO)
import Core.Ray as R (Ray (Ray, direction))
import Core.Vec3 as V (Vec3 (..), add, normalize, randomInUnitSphere, scale, x, y, z)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List.Split (chunksOf)
import Data.Massiv.Array (compute)
import Data.Massiv.Array as A (Array, Comp (Seq), Ix2, Sz (..), U, compute, flatten, fromLists', mapM_, size)
import Hittable.Class as H (HitRecord (normal, point), Hittable (hit))
import Hittable.HittableList as HL (HittableList (HittableList), SomeHittable (SomeHittable))
import Hittable.Objects.Plane as P (Plane (Plane))
import Hittable.Objects.Sphere as S (Sphere (Sphere))
import Hittable.Objects.Triangle as T (Triangle (..))
import Rendering.Camera as Cam (Camera, defaultCamera, generateRay)
import Rendering.Color as Col (Color, lerp, writeColor)
import Rendering.Light as L (Light (PointLight), computeLighting)
import System.IO (BufferMode (BlockBuffering), IOMode (WriteMode), hPutStr, hSetBuffering, withFile)
import Utils.Constants (clamp, randomDouble)
import Utils.Interval (Interval (..))
import Utils.ProgressBar as PB (ProgressBar, newProgressBar, updateMessage, updateProgress)

-- Define Pixel as Vec3 (representing RGB color)
type Pixel = V.Vec3

type Image = Array U Ix2 Pixel

createPPM :: Int -> Int -> Int -> Bool -> String -> IO ()
createPPM width height samplesPerPixel aa filename = do
  -- Initialize progress bar
  progressBar <- PB.newProgressBar height
  rowsProcessedRef <- newIORef (0 :: Int)

  -- Setup Camera
  let lookFrom = V.Vec3 0 0 5
      lookAt = V.Vec3 0 0 (-1)
      vUp = V.Vec3 0 1 0
      vfov = 30.0
      aperture = 0.0
      focusDist = 1.0
      camera = Cam.defaultCamera lookFrom lookAt vUp vfov (fromIntegral width / fromIntegral height) aperture focusDist

  -- Compute the image in parallel using `massiv`
  imageData <- forM [(j, i) | j <- [0 .. height - 1], i <- [0 .. width - 1]] (\(j, i) -> generatePixel samplesPerPixel aa progressBar rowsProcessedRef camera width height (j, i))
  let image = compute $ A.fromLists' Seq (chunksOf width imageData) :: Array U Ix2 Pixel

  -- Write the computed image in bulk
  writePPM filename image

generatePixel :: Int -> Bool -> PB.ProgressBar -> IORef Int -> Cam.Camera -> Int -> Int -> (Int, Int) -> IO Pixel
generatePixel samplesPerPixel aa progressBar rowsProcessedRef camera width height (j, i) = do
  sampledColors <- replicateM samplesPerPixel (samplePixel i j camera width height aa)
  let pixel = averageColor sampledColors

  -- If we are at the last column of a row, update progress
  when (i == width - 1) $ do
    atomicModifyIORef' rowsProcessedRef (\x -> (x + 1, ()))
    newRows <- readIORef rowsProcessedRef
    PB.updateMessage progressBar ("Rendering row " ++ show newRows)
    PB.updateProgress progressBar newRows

  return pixel

writePPM :: FilePath -> Image -> IO ()
writePPM filename image = do
  withFile filename WriteMode $ \handle -> do
    hSetBuffering handle (BlockBuffering (Just (1024 * 512)))
    hPutStr handle ("P3\n" ++ show (size image) ++ "\n255\n")
    A.mapM_ (hPutStr handle . Col.writeColor) (flatten image)

samplePixel :: Int -> Int -> Cam.Camera -> Int -> Int -> Bool -> IO Col.Color
samplePixel i j camera width height aa = do
  uOffset <- if aa then randomDouble else return 0.5
  vOffset <- if aa then randomDouble else return 0.5
  ray <- Cam.generateRay camera i j width height uOffset vOffset
  traceRay ray 50

averageColor :: [Color] -> Color
averageColor colors = scale (1.0 / fromIntegral (length colors)) (foldr add (V.Vec3 0 0 0) colors)

traceRay :: R.Ray -> Int -> IO Col.Color
traceRay ray depth
  | depth <= 0 = return (V.Vec3 0 0 0)
  | otherwise = do
      let world =
            HL.HittableList
              [ HL.SomeHittable (S.Sphere (V.Vec3 (-1.2) 0 (-1)) 0.5),
                HL.SomeHittable (S.Sphere (V.Vec3 0 0 (-1)) 0.5),
                HL.SomeHittable (S.Sphere (V.Vec3 1.2 0.3 (-1.6)) 0.5),
                HL.SomeHittable (P.Plane (V.Vec3 0 (-0.5) 0) (V.Vec3 0 1 0)),
                HL.SomeHittable (T.Triangle (V.Vec3 (-0.5) 0 0) (V.Vec3 0.5 0 0) (V.Vec3 0 0.5 (-0.5)))
              ]
          interval = Interval 0.001 100
          lights = [PointLight (V.Vec3 (-10) 5 0.5) (V.Vec3 0.3 0.3 0.3)]

      case H.hit world ray interval of
        Just hitRecord -> do
          randomVec <- V.randomInUnitSphere
          let newDirection = V.add (H.normal hitRecord) randomVec
              scatteredRay = R.Ray (H.point hitRecord) newDirection
              directLight = L.computeLighting hitRecord lights
              clampedLight = V.Vec3 (clamp (V.x directLight) 0 1) (clamp (V.y directLight) 0 1) (clamp (V.z directLight) 0 1)

          bounceColor <- traceRay scatteredRay (depth - 1)
          return $ V.add (V.scale 0.5 bounceColor) clampedLight
        Nothing -> do
          let unitDir = V.normalize (R.direction ray)
              tHit = 0.5 * (V.y unitDir + 1.0)
          return $ Col.lerp tHit (V.Vec3 1 1 1) (V.Vec3 0.5 0.7 1)
