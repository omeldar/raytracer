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
import Core.Ray as R (Ray (..), origin, direction)
import Core.Vec3 as V (Vec3 (..), add, dot, mul, negateV, normalize, randomInUnitSphere, reflect, refract, scale, x, y, z, sub, vLength)
import Data.Typeable (cast)
import Hittable.BVH (BVHNode, constructBVH)
import Hittable.Class as H (HitRecord (normal, point), Hittable (hit), color, material)
import Hittable.HittableList as HL (HittableList (HittableList), SomeHittable (SomeHittable))
import Hittable.Objects.Plane as P (Plane (Plane))
import Hittable.Objects.Sphere as S (Sphere (Sphere))
import Hittable.Objects.Triangle as T (Triangle (..))
import ObjParser (loadObjWithOffset)
import qualified Rendering.Camera as Cam (defaultCamera, generateRay)
import Rendering.Color as Col (Color, lerp)
import qualified Rendering.Light as L (Light (..), computeLighting)
import Rendering.Material (MaterialType (..))
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
    PB.updateMessage progressBar ("Rendering row " ++ show (height (image config)))

processRow :: Config -> PB.ProgressBar -> Handle -> BVHNode -> Int -> IO ()
processRow config progressBar handle world j = do
  row <- mapM (\i -> pixelColor config world i (height (image config) - 1 - j)) [0 .. width (image config) - 1]
  hPutStr handle (unlines (map showPixel row) ++ "\n")
  PB.updateMessage progressBar ("Rendering row " ++ show (j + 1))
  PB.updateProgress progressBar (j + 1)

pixelColor :: Config -> BVHNode -> Int -> Int -> IO Col.Color
pixelColor config world i j = do
  sampledColors <- Control.Monad.replicateM (samplesPerPixel (image config)) (samplePixel config world i j)
  return $ averageColor sampledColors

samplePixel :: Config -> BVHNode -> Int -> Int -> IO Col.Color
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

traceRay :: Config -> BVHNode -> R.Ray -> Int -> IO Col.Color
traceRay config bvh ray depth
  | depth <= 0 = return (V.Vec3 0 0 0)
  | otherwise = do
      let interval = Interval 0.001 100
          hitResult = H.hit bvh ray interval

      case hitResult of
        Just hitRecord -> do
          let hitMaterial = H.material hitRecord
              surfaceColor = H.color hitRecord
              sceneLights = map convertLight (lights config)
              p = H.point hitRecord
              n = H.normal hitRecord
              dir = R.direction ray

          -- Handle material scattering
          scatterResult <- case hitMaterial of
            Lambertian -> do
              rand <- V.randomInUnitSphere
              let target = V.add n rand
              return $ Right (R.Ray p target)

            Metal hitfuzz -> do
              rand <- V.randomInUnitSphere
              let reflected = V.reflect (V.normalize dir) n
                  scattered = V.add reflected (V.scale hitfuzz rand)
              return $ Right (R.Ray p scattered)

            Dielectric hitrefIdx -> do
              let unitDir = V.normalize dir
                  cosTheta = min 1.0 (V.dot (V.negateV unitDir) n)
                  reflectProb = schlick cosTheta hitrefIdx

                  reflectDir = V.reflect unitDir n
                  refractDir = V.refract unitDir n (1 / hitrefIdx)

                  reflectRay = R.Ray p reflectDir
                  refractRay = R.Ray p refractDir

              reflectCol <- traceRay config bvh reflectRay (depth - 1)
              refractCol <- do
                rawRefract <- traceRay config bvh refractRay (depth - 1)
                let absorption = V.Vec3 0.02 0.02 0.02
                    distance = V.vLength (V.sub p (R.origin ray))
                    attenuation = V.Vec3
                      (exp (-V.x absorption * distance))
                      (exp (-V.y absorption * distance))
                      (exp (-V.z absorption * distance))
                return (V.mul attenuation rawRefract)

              let blended = V.add (V.scale reflectProb reflectCol) (V.scale (1 - reflectProb) refractCol)
              return $ Left blended

          -- Direct lighting (only for non-glass)
          clampedLight <- case hitMaterial of
            Dielectric _ -> return (V.Vec3 0 0 0)
            _ -> do
              directLight <- L.computeLighting hitRecord sceneLights bvh
              let litColor = V.mul directLight surfaceColor
              return $
                V.Vec3
                  (clamp (V.x litColor) 0 1)
                  (clamp (V.y litColor) 0 1)
                  (clamp (V.z litColor) 0 1)

          -- Russian Roulette termination
          let rr = russianRoulette (raytracer config)
              baseProb = probability rr
              adaptivity = adaptivityFactor rr
              adaptiveProb = case adaptiveMethod rr of
                Linear -> min 1.0 (baseProb * fromIntegral depth / adaptivity)
                Exponential -> min 1.0 (1.0 - exp (-fromIntegral depth / adaptivity))
                Sqrt -> min 1.0 (baseProb * sqrt (fromIntegral depth / adaptivity))

          terminate <-
            if enabled rr && depth > 5
              then (< adaptiveProb) <$> randomDouble
              else return False

          if terminate
            then return clampedLight
            else do
              bounceColor <- case scatterResult of
                Right scatteredRay -> traceRay config bvh scatteredRay (depth - 1)
                Left blended -> return blended

              let baseColor = case scatterResult of
                    Right _ -> V.scale 0.5 (V.mul surfaceColor bounceColor)
                    Left blended -> blended

                  finalColor = V.add baseColor clampedLight

              return finalColor

        Nothing -> return $ getBackgroundColor ray (background config)

schlick :: Double -> Double -> Double
schlick cosine refidx =
  let r0 = ((1 - refidx) / (1 + refidx)) ^ (2 :: Integer)
   in r0 + (1 - r0) * ((1 - cosine) ** 5)

convertLight :: LightSettings -> L.Light
convertLight (PointLight pos lIntensity) = L.PointLight pos lIntensity
convertLight (DirectionalLight dir lIntensity) = L.DirectionalLight dir lIntensity

parseSceneObjects :: SceneSettings -> IO BVHNode
parseSceneObjects sceneConfig = do
  let hittables = case objects sceneConfig of
        Just objs -> map toHittable objs
        Nothing -> []

  objTriangles <- case objFiles sceneConfig of
    Just entries -> do
      allObjTrias <- mapM (\entry -> loadObjWithOffset (path entry) (objposition entry) (overrideColor entry) (overrideMaterial entry)) entries
      return $ concatMap (\(HittableList hs) -> extractTriangles hs) allObjTrias
    Nothing -> return []

  let configTriangles = extractTriangles hittables
      totalTriangles = configTriangles ++ objTriangles
      triangleObjects = map SomeHittable totalTriangles
      allObjects = triangleObjects ++ hittables

  putStrLn $ "Loaded " ++ show (length totalTriangles) ++ " triangles into BVH."
  putStrLn $ "Loaded " ++ show (length allObjects - length totalTriangles) ++ " other objects into BVH."

  return $ constructBVH allObjects

extractTriangles :: [SomeHittable] -> [T.Triangle]
extractTriangles [] = []
extractTriangles (SomeHittable obj : rest) =
  case cast obj of
    Just triangle -> triangle : extractTriangles rest
    Nothing -> extractTriangles rest

toHittable :: SceneObject -> SomeHittable
toHittable (SphereObj center radius sColor mat) =
  SomeHittable (S.Sphere center radius sColor mat)
toHittable (PlaneObj pointOnPlane pnormal pColor mat) =
  SomeHittable (P.Plane pointOnPlane pnormal pColor mat)
toHittable (TriangleObj tv0 tv1 tv2 tColor mat) =
  SomeHittable (T.Triangle tv0 tv1 tv2 tColor mat)

getBackgroundColor :: R.Ray -> BackgroundSettings -> Col.Color
getBackgroundColor ray (Gradient c1 c2) =
  let unitDir = V.normalize (R.direction ray)
      tHit = 0.5 * (V.y unitDir + 1.0)
   in Col.lerp tHit c1 c2
getBackgroundColor _ (SolidColor solidColor) = solidColor
