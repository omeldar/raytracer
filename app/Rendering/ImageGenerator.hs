module Rendering.ImageGenerator
  ( -- types
    Pixel,
    Row,
    Image,
    -- functions
    createPPM,
  )
where

import Config
  ( BackgroundSettings (..),
    CameraSettings (..),
    Config (..),
    ImageSettings (..),
    LightSettings (..),
    RaytracerSettings (..),
    RussianRouletteSettings (..),
    SceneObject (..),
    SceneSettings (..),
  )
import qualified Control.Monad
import Core.Ray as R (Ray (Ray, direction))
import Core.Vec3 as V (Vec3 (..), add, normalize, randomInUnitSphere, scale, x, y, z)
import Hittable.Class as H (HitRecord (normal, point), Hittable (hit))
import Hittable.HittableList as HL (HittableList (HittableList), SomeHittable (SomeHittable))
import Hittable.Objects.Plane as P (Plane (Plane))
import Hittable.Objects.Sphere as S (Sphere (Sphere))
import Hittable.Objects.Triangle as T (Triangle (..))
import qualified Rendering.Camera as Cam (defaultCamera, generateRay)
import Rendering.Color as Col (Color, lerp)
import qualified Rendering.Light as L (Light (..), computeLighting)
import System.IO (BufferMode (BlockBuffering), Handle, IOMode (WriteMode), hPutStr, hSetBuffering, withFile)
import Utils.Constants (clamp, randomDouble)
import Utils.Interval (Interval (..))
import Utils.ProgressBar as PB (ProgressBar, newProgressBar, updateMessage, updateProgress)

-- Define Pixel as Vec3 (representing RGB color)
type Pixel = V.Vec3

type Row = [Pixel]

type Image = [Row]

createPPM :: Config -> FilePath -> IO ()
createPPM config filename =
  withFile filename WriteMode $ \handle -> do
    progressBar <- PB.newProgressBar (height (image config))
    hSetBuffering handle (BlockBuffering (Just (1024 * 512))) -- Enable buffering
    hPutStr handle ("P3\n" ++ show (width (image config)) ++ " " ++ show (height (image config)) ++ "\n255\n") -- Write header
    mapM_ (processRow config progressBar handle) [0 .. height (image config) - 1]

processRow :: Config -> PB.ProgressBar -> Handle -> Int -> IO ()
processRow config progressBar handle j = do
  row <- mapM (\i -> pixelColor config i (height (image config) - 1 - j)) [0 .. width (image config) - 1]
  hPutStr handle (unlines (map showPixel row) ++ "\n")
  PB.updateMessage progressBar ("Rendering row " ++ show (j + 1))
  PB.updateProgress progressBar (j + 1)

pixelColor :: Config -> Int -> Int -> IO Col.Color
pixelColor config i j = do
  sampledColors <- Control.Monad.replicateM (samplesPerPixel (image config)) (samplePixel config i j)
  return $ averageColor sampledColors

samplePixel :: Config -> Int -> Int -> IO Col.Color
samplePixel config i j = do
  uOffset <- if antialiasing (image config) then randomDouble else return 0.5
  vOffset <- if antialiasing (image config) then randomDouble else return 0.5
  let cameraObj =
        Cam.defaultCamera
          (lookFrom (camera config))
          (lookAt (camera config))
          (vUp (camera config))
          (vfov (camera config))
          (fromIntegral (width (image config)) / fromIntegral (height (image config)))
          (aperture (camera config))
          (focusDist (camera config))
  ray <- Cam.generateRay cameraObj i j (width (image config)) (height (image config)) uOffset vOffset
  traceRay config ray (maxBounces (raytracer config))

showPixel :: Pixel -> String
showPixel (V.Vec3 r g b) = unwords $ map (show . (truncate :: Double -> Int) . (* 255.999)) [r, g, b]

averageColor :: [Color] -> Color
averageColor colors = scale (1.0 / fromIntegral (length colors)) (foldr add (V.Vec3 0 0 0) colors)

traceRay :: Config -> R.Ray -> Int -> IO Col.Color
traceRay config ray depth
  | depth <= 0 = return (V.Vec3 0 0 0)
  | otherwise = do
      let world = parseSceneObjects (scene config)
          interval = Interval 0.001 100

      case H.hit world ray interval of
        Just hitRecord -> do
          randomVec <- V.randomInUnitSphere
          let newDirection = V.add (H.normal hitRecord) randomVec
              scatteredRay = R.Ray (H.point hitRecord) newDirection
              sceneLights = map convertLight (lights config)
              directLight = L.computeLighting hitRecord sceneLights
              clampedLight =
                V.Vec3 (clamp (V.x directLight) 0 1) (clamp (V.y directLight) 0 1) (clamp (V.z directLight) 0 1)

          -- Russian Roulette
          let rrEnabled = enabled (russianRoulette (raytracer config))
              rrProbability = probability (russianRoulette (raytracer config))

          terminate <-
            if rrEnabled && depth > 3
              then do
                randVal <- randomDouble
                return (randVal < rrProbability)
              else return False

          if terminate
            then return (V.Vec3 0 0 0)
            else do
              bounceColor <- traceRay config scatteredRay (depth - 1)
              return $ V.add (V.scale 0.5 bounceColor) clampedLight
        Nothing -> return $ getBackgroundColor ray (background config)

convertLight :: LightSettings -> L.Light
convertLight (PointLight pos lIntensity) = L.PointLight pos lIntensity
convertLight (DirectionalLight dir lIntensity) = L.DirectionalLight dir lIntensity

parseSceneObjects :: SceneSettings -> HittableList
parseSceneObjects sceneConfig =
  let parsedObjects = map toHittable (objects sceneConfig)
   in HL.HittableList parsedObjects

toHittable :: SceneObject -> SomeHittable
toHittable (SphereObj center radius) = HL.SomeHittable (S.Sphere center radius)
toHittable (PlaneObj pointOnPlane planeNormal) = HL.SomeHittable (P.Plane pointOnPlane planeNormal)
toHittable (TriangleObj v0' v1' v2') = HL.SomeHittable (T.Triangle v0' v1' v2')

getBackgroundColor :: R.Ray -> BackgroundSettings -> Col.Color
getBackgroundColor ray (Gradient c1 c2) =
  let unitDir = V.normalize (R.direction ray)
      tHit = 0.5 * (V.y unitDir + 1.0)
   in Col.lerp tHit c1 c2
getBackgroundColor _ (SolidColor solidColor) = solidColor
