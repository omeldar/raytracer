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
  ( AdaptiveMethod (..),
    BackgroundSettings (..),
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
import ObjParser (loadObj)
import qualified Rendering.Camera as Cam (defaultCamera, generateRay)
import Rendering.Color as Col (Color, lerp)
import qualified Rendering.Light as L (Light (..), computeLighting)
import System.Directory (createDirectoryIfMissing)
import System.IO (BufferMode (BlockBuffering), Handle, IOMode (WriteMode), hPutStr, hSetBuffering, withFile)
import Utils.Constants (clamp, randomDouble)
import Utils.Interval (Interval (..))
import Utils.ProgressBar as PB (ProgressBar, newProgressBar, updateMessage, updateProgress)

-- Define Pixel as Vec3 (representing RGB color)
type Pixel = V.Vec3

type Row = [Pixel]

type Image = [Row]

createPPM :: Config -> FilePath -> IO ()
createPPM config filename = do
  let outputDir = "out/"
  createDirectoryIfMissing True outputDir

  world <- parseSceneObjects (scene config)

  withFile filename WriteMode $ \handle -> do
    progressBar <- PB.newProgressBar (height (image config))
    hSetBuffering handle (BlockBuffering (Just (1024 * 512))) -- Enable buffering
    hPutStr handle ("P3\n" ++ show (width (image config)) ++ " " ++ show (height (image config)) ++ "\n255\n") -- Write header
    mapM_ (processRow config progressBar handle world) [0 .. height (image config) - 1]

processRow :: Config -> PB.ProgressBar -> Handle -> HittableList -> Int -> IO ()
processRow config progressBar handle world j = do
  row <- mapM (\i -> pixelColor config world i (height (image config) - 1 - j)) [0 .. width (image config) - 1]
  hPutStr handle (unlines (map showPixel row) ++ "\n")
  PB.updateMessage progressBar ("Rendering row " ++ show (j + 1))
  PB.updateProgress progressBar (j + 1)

pixelColor :: Config -> HittableList -> Int -> Int -> IO Col.Color
pixelColor config world i j = do
  sampledColors <- Control.Monad.replicateM (samplesPerPixel (image config)) (samplePixel config world i j)
  return $ averageColor sampledColors

samplePixel :: Config -> HittableList -> Int -> Int -> IO Col.Color
samplePixel config world i j = do
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
  traceRay config world ray (maxBounces (raytracer config))

showPixel :: Pixel -> String
showPixel (V.Vec3 r g b) = unwords $ map (show . (truncate :: Double -> Int) . (* 255.999)) [r, g, b]

averageColor :: [Color] -> Color
averageColor colors = scale (1.0 / fromIntegral (length colors)) (foldr add (V.Vec3 0 0 0) colors)

traceRay :: Config -> HittableList -> R.Ray -> Int -> IO Col.Color
traceRay config world ray depth
  | depth <= 0 = return (V.Vec3 0 0 0)
  | otherwise = do
      let interval = Interval 0.001 100

      case H.hit world ray interval of
        Just hitRecord -> do
          randomVec <- V.randomInUnitSphere
          let newDirection = V.add (H.normal hitRecord) randomVec
              scatteredRay = R.Ray (H.point hitRecord) newDirection
              sceneLights = map convertLight (lights config)
              directLight = L.computeLighting hitRecord sceneLights
              clampedLight =
                V.Vec3 (clamp (V.x directLight) 0 1) (clamp (V.y directLight) 0 1) (clamp (V.z directLight) 0 1)

          -- Russian Roulette, using adaptive probability
          let rrSettings = russianRoulette (raytracer config)
              rrEnabled = enabled rrSettings
              baseProb = probability rrSettings
              adaptivity = adaptivityFactor rrSettings
              adaptiveProb = case adaptiveMethod rrSettings of
                Linear -> min 1.0 (baseProb * (fromIntegral depth / adaptivity)) -- Slower growth
                Exponential -> min 1.0 (1.0 - exp (-fromIntegral depth / adaptivity)) -- Rapid increase
                Sqrt -> min 1.0 (baseProb * sqrt (fromIntegral depth / adaptivity)) -- Smooth scaling
          terminate <-
            if rrEnabled && depth > 5
              then do
                randVal <- randomDouble
                return (randVal < adaptiveProb)
              else return False

          if terminate
            then return clampedLight
            else do
              bounceColor <- traceRay config world scatteredRay (depth - 1)
              return $ V.add (V.scale 0.5 bounceColor) clampedLight
        Nothing -> return $ getBackgroundColor ray (background config)

convertLight :: LightSettings -> L.Light
convertLight (PointLight pos lIntensity) = L.PointLight pos lIntensity
convertLight (DirectionalLight dir lIntensity) = L.DirectionalLight dir lIntensity

parseSceneObjects :: SceneSettings -> IO HittableList
parseSceneObjects sceneConfig = do
  let parsedObjects = maybe [] (map toHittable) (objects sceneConfig)

  case objFile sceneConfig of
    Just filePath -> do
      HittableList objModels <- loadObj filePath
      return $ HittableList (parsedObjects ++ objModels)
    Nothing -> do
      return $ HittableList parsedObjects

toHittable :: SceneObject -> SomeHittable
toHittable (SphereObj center radius hColor) = HL.SomeHittable (S.Sphere center radius hColor)
toHittable (PlaneObj pointOnPlane planeNormal hColor) = HL.SomeHittable (P.Plane pointOnPlane planeNormal hColor)
toHittable (TriangleObj v0' v1' v2' hColor) = HL.SomeHittable (T.Triangle v0' v1' v2' hColor)

getBackgroundColor :: R.Ray -> BackgroundSettings -> Col.Color
getBackgroundColor ray (Gradient c1 c2) =
  let unitDir = V.normalize (R.direction ray)
      tHit = 0.5 * (V.y unitDir + 1.0)
   in Col.lerp tHit c1 c2
getBackgroundColor _ (SolidColor solidColor) = solidColor
