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
    ObjFileEntry (..),
    RaytracerSettings (..),
    RussianRouletteSettings (..),
    SceneObject (..),
    SceneSettings (..),
  )
import qualified Control.Monad
import Core.Ray as R (Ray (Ray, direction))
import Core.Vec3 as V (Vec3 (..), add, normalize, randomInUnitSphere, scale, x, y, z)
import Data.Typeable (cast)
import Hittable.BVH (BVHNode, closestHit, constructBVH)
import Hittable.Class as H (HitRecord (normal, point), Hittable (hit))
import Hittable.HittableList as HL (HittableList (HittableList), SomeHittable (SomeHittable))
import Hittable.Objects.Plane as P (Plane (Plane))
import Hittable.Objects.Sphere as S (Sphere (Sphere))
import Hittable.Objects.Triangle as T (Triangle (..))
import ObjParser (loadObjWithOffset)
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

processRow :: Config -> PB.ProgressBar -> Handle -> (BVHNode, HittableList) -> Int -> IO ()
processRow config progressBar handle world j = do
  row <- mapM (\i -> pixelColor config world i (height (image config) - 1 - j)) [0 .. width (image config) - 1]
  hPutStr handle (unlines (map showPixel row) ++ "\n")
  PB.updateMessage progressBar ("Rendering row " ++ show (j + 1))
  PB.updateProgress progressBar (j + 1)

pixelColor :: Config -> (BVHNode, HittableList) -> Int -> Int -> IO Col.Color
pixelColor config world i j = do
  sampledColors <- Control.Monad.replicateM (samplesPerPixel (image config)) (samplePixel config world i j)
  return $ averageColor sampledColors

samplePixel :: Config -> (BVHNode, HittableList) -> Int -> Int -> IO Col.Color
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

traceRay :: Config -> (BVHNode, HittableList) -> R.Ray -> Int -> IO Col.Color
traceRay config world@(bvh, hittableList) ray depth
  | depth <= 0 = return (V.Vec3 0 0 0)
  | otherwise = do
      let interval = Interval 0.001 100

      let bvhHit = H.hit bvh ray interval
      let listHit = H.hit hittableList ray interval

      case closestHit ray interval bvhHit listHit of
        Just hitRecord -> do
          randomVec <- V.randomInUnitSphere
          let newDirection = V.add (H.normal hitRecord) randomVec
              scatteredRay = R.Ray (H.point hitRecord) newDirection
              sceneLights = map convertLight (lights config)

          directLight <- L.computeLighting hitRecord sceneLights world
          let clampedLight =
                V.Vec3
                  (clamp (V.x directLight) 0 1)
                  (clamp (V.y directLight) 0 1)
                  (clamp (V.z directLight) 0 1)

          -- Russian Roulette, using adaptive probability
          let rrSettings = russianRoulette (raytracer config)
              rrEnabled = enabled rrSettings
              baseProb = probability rrSettings
              adaptivity = adaptivityFactor rrSettings
              adaptiveProb = case adaptiveMethod rrSettings of
                Linear -> min 1.0 (baseProb * (fromIntegral depth / adaptivity))
                Exponential -> min 1.0 (1.0 - exp (-fromIntegral depth / adaptivity))
                Sqrt -> min 1.0 (baseProb * sqrt (fromIntegral depth / adaptivity))

          terminate <-
            if rrEnabled && depth > 5
              then do
                randVal <- randomDouble
                return (randVal < adaptiveProb)
              else return False

          if terminate
            then return clampedLight
            else do
              bounceColor <- traceRay config (bvh, hittableList) scatteredRay (depth - 1)
              return $ V.add (V.scale 0.5 bounceColor) clampedLight
        Nothing -> return $ getBackgroundColor ray (background config)

convertLight :: LightSettings -> L.Light
convertLight (PointLight pos lIntensity) = L.PointLight pos lIntensity
convertLight (DirectionalLight dir lIntensity) = L.DirectionalLight dir lIntensity

parseSceneObjects :: SceneSettings -> IO (BVHNode, HittableList)
parseSceneObjects sceneConfig = do
  putStrLn $ "DEBUG: objFiles = " ++ show (objFiles sceneConfig)
  let hittables = case objects sceneConfig of
        Just objs -> map toHittable objs
        Nothing -> []

  objTriangles <- case objFiles sceneConfig of
    Just entries -> do
      allObjTrias <- mapM (\entry -> loadObjWithOffset (path entry) (objposition entry)) entries
      return $ concatMap (\(HittableList hs) -> extractTriangles hs) allObjTrias
    Nothing -> return []

  let onlyTriangles = extractTriangles hittables
  let otherObjects = HittableList (filter isNotTriangle hittables) -- Keep planes & spheres seperately
  return (constructBVH (onlyTriangles ++ objTriangles), otherObjects)

isNotTriangle :: SomeHittable -> Bool
isNotTriangle (SomeHittable obj) = case cast obj :: Maybe T.Triangle of
  Just _ -> False
  Nothing -> True

extractTriangles :: [SomeHittable] -> [T.Triangle]
extractTriangles [] = []
extractTriangles (SomeHittable obj : rest) =
  case cast obj of
    Just triangle -> triangle : extractTriangles rest
    Nothing -> extractTriangles rest

toHittable :: SceneObject -> SomeHittable
toHittable (SphereObj center radius sColor) =
  SomeHittable (S.Sphere center radius sColor)
toHittable (PlaneObj pointOnPlane pnormal pColor) =
  SomeHittable (P.Plane pointOnPlane pnormal pColor)
toHittable (TriangleObj tv0 tv1 tv2 tColor) =
  SomeHittable (T.Triangle tv0 tv1 tv2 tColor)

getBackgroundColor :: R.Ray -> BackgroundSettings -> Col.Color
getBackgroundColor ray (Gradient c1 c2) =
  let unitDir = V.normalize (R.direction ray)
      tHit = 0.5 * (V.y unitDir + 1.0)
   in Col.lerp tHit c1 c2
getBackgroundColor _ (SolidColor solidColor) = solidColor
