{-# LANGUAGE BangPatterns #-}

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
import Core.Ray as R (Ray (..), direction, origin)
import Core.Vec3 as V (Vec3 (..), add, dot, mul, negateV, normalize, randomInUnitSphere, reflect, refract, scale, sub, vLength, x, y, z)
import Data.List (foldl', isPrefixOf)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Typeable (cast)
import Hittable.BVH (BVHNode, constructBVHWithLimit)
import Hittable.Class as H (HitRecord (..), Hittable (hit))
import Hittable.HittableList as HL (HittableList (HittableList), SomeHittable (SomeHittable))
import Hittable.Objects.Plane as P (Plane (Plane))
import Hittable.Objects.Sphere as S (Sphere (Sphere))
import Hittable.Objects.Triangle as T (Triangle (..))
import Parser.Material (assignMaterialIds)
import qualified Parser.Material as PM
import Parser.Object (loadObjWithOffset)
import qualified Rendering.Camera as Cam (defaultCamera, generateRay)
import Rendering.Color as Col (Color, lerp)
import qualified Rendering.Light as L (Light (..), computeLighting)
import Rendering.Material (Material (..), defaultMaterial)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))
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

  (world, materialMap) <- parseSceneObjects config

  withFile filename WriteMode $ \handle -> do
    progressBar <- PB.newProgressBar (height (image config))
    hSetBuffering handle (BlockBuffering (Just (1024 * 512))) -- Enable buffering
    hPutStr handle ("P3\n" ++ show (width (image config)) ++ " " ++ show (height (image config)) ++ "\n255\n") -- Write header
    mapM_ (processRow config progressBar handle world materialMap) [0 .. height (image config) - 1]
    PB.updateMessage progressBar ("Rendering row " ++ show (height (image config)))

processRow :: Config -> PB.ProgressBar -> Handle -> BVHNode -> M.Map Int Material -> Int -> IO ()
processRow config progressBar handle world materialMap j = do
  row <- mapM (\i -> pixelColor config world materialMap i (height (image config) - 1 - j)) [0 .. width (image config) - 1]
  hPutStr handle (unlines (map showPixel row) ++ "\n")
  PB.updateMessage progressBar ("Rendering row " ++ show (j + 1))
  PB.updateProgress progressBar (j + 1)

pixelColor :: Config -> BVHNode -> M.Map Int Material -> Int -> Int -> IO Col.Color
pixelColor config world materialMap i j = do
  sampledColors <- Control.Monad.replicateM (samplesPerPixel (image config)) (samplePixel config world materialMap i j)
  let !avg = averageColor sampledColors
  return avg

samplePixel :: Config -> BVHNode -> M.Map Int Material -> Int -> Int -> IO Col.Color
samplePixel config world materialMap i j = do
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
  traceRay config world materialMap ray (maxBounces (raytracer config))

showPixel :: Pixel -> String
showPixel (V.Vec3 r g b) = unwords $ map (show . (truncate :: Double -> Int) . (* 255.999)) [r, g, b]

averageColor :: [Color] -> Color
averageColor colors =
  let !colsum = foldl' add (V.Vec3 0 0 0) colors
   in V.scale (1.0 / fromIntegral (length colors)) colsum

traceRay :: Config -> BVHNode -> M.Map Int Material -> R.Ray -> Int -> IO Col.Color
traceRay config bvh materialMap ray depth
  | depth <= 0 = return (Vec3 0 0 0)
  | otherwise = do
      let interval = Interval 0.001 100
          hitResult = H.hit bvh ray interval

      case hitResult of
        Just hitRecord -> do
          let matId = H.materialId hitRecord
              mat = fromMaybe defaultMaterial (M.lookup matId materialMap)
              surfaceColor = diffuseColor mat
              p = H.point hitRecord
              n = H.normal hitRecord
              dir = R.direction ray
              unitDir = V.normalize dir
              cosTheta = min 1.0 (V.dot (V.negateV unitDir) n)
              sceneLights = maybe [] (map convertLight) (lights (scene config))

          -- Compute emission (if any)
          let emitted = fromMaybe (Vec3 0 0 0) (emissionColor mat)

          -- Decide scattering behavior
          scatterResult <-
            case ior mat of
              Just refIdx -> do
                let reflectDir = V.reflect unitDir n
                    refractDir = V.refract unitDir n (1 / refIdx)
                    reflectRay = R.Ray p reflectDir
                    refractRay = R.Ray p refractDir
                    rProb = schlick cosTheta refIdx
                reflectCol <- traceRay config bvh materialMap reflectRay (depth - 1)
                refractCol <- do
                  raw <- traceRay config bvh materialMap refractRay (depth - 1)
                  let absorption = Vec3 0.02 0.02 0.02
                      dist = V.vLength (V.sub p (R.origin ray))
                      atten =
                        Vec3
                          (exp (-x absorption * dist))
                          (exp (-y absorption * dist))
                          (exp (-z absorption * dist))
                  return (V.mul atten raw)
                return $ Left (V.add (V.scale rProb reflectCol) (V.scale (1 - rProb) refractCol))
              Nothing -> case shininess mat of
                Just s | s > 100 -> do
                  -- Metal-like
                  rand <- V.randomInUnitSphere
                  let reflected = V.reflect unitDir n
                      fuzzed = V.add reflected (V.scale 0.05 rand)
                  return $ Right (R.Ray p fuzzed)
                _ -> do
                  -- Lambertian
                  rand <- V.randomInUnitSphere
                  let scatterDir = V.add n rand
                  return $ Right (R.Ray p scatterDir)

          -- Direct lighting (optional)
          directLight <- L.computeLighting hitRecord sceneLights bvh
          let litColor = V.mul directLight surfaceColor
              clampedLight = Vec3 (clamp (x litColor) 0 1) (clamp (y litColor) 0 1) (clamp (z litColor) 0 1)

          -- Russian Roulette
          let rr = russianRoulette (raytracer config)
              baseProb = probability rr
              adaptiveProb = case adaptiveMethod rr of
                Linear -> min 1.0 (baseProb * fromIntegral depth / adaptivityFactor rr)
                Exponential -> min 1.0 (1.0 - exp (-fromIntegral depth / adaptivityFactor rr))
                Sqrt -> min 1.0 (baseProb * sqrt (fromIntegral depth / adaptivityFactor rr))
          terminate <-
            if enabled rr && depth > 5
              then (< adaptiveProb) <$> randomDouble
              else return False

          if terminate
            then return (V.add emitted clampedLight)
            else do
              bounceColor <- case scatterResult of
                Right scattered -> traceRay config bvh materialMap scattered (depth - 1)
                Left blended -> return blended
              let finalColor = case scatterResult of
                    Right _ -> V.add (V.scale 0.5 (V.mul surfaceColor bounceColor)) clampedLight
                    Left _ -> V.add bounceColor clampedLight
              return (V.add finalColor emitted)
        Nothing -> return $ getBackgroundColor ray (background config)

-- Schlick approximation for reflectivity
schlick :: Double -> Double -> Double
schlick cosine refIdx =
  let r0 = ((1 - refIdx) / (1 + refIdx)) ^ (2 :: Integer)
   in r0 + (1 - r0) * ((1 - cosine) ** 5)

convertLight :: LightSettings -> L.Light
convertLight (PointLight pos lIntensity) = L.PointLight pos lIntensity
convertLight (DirectionalLight dir lIntensity) = L.DirectionalLight dir lIntensity

parseSceneObjects :: Config -> IO (BVHNode, M.Map Int Material)
parseSceneObjects config = do
  -- Phase 0: Load materials from config (JSON)
  let sceneConfig = scene config
      (jsonNameToId, jsonIdToMat) = maybe (M.empty, M.empty) assignMaterialIds (materials sceneConfig)

  -- Phase 1: Parse materials from .mtl files
  (mtlNameToId, mtlIdToMat) <- case objFiles sceneConfig of
    Just entries -> do
      matTexts <- mapM (tryReadMtlFile . path) entries
      let parsed = map PM.parseMaterial matTexts
          mergedNameToId = M.unions (map fst parsed)
          mergedIdToMat = M.unions (map snd parsed)
      return (mergedNameToId, mergedIdToMat)
    Nothing -> return (M.empty, M.empty)

  -- Phase 2: Merge material maps
  let nameToIdMap = M.union jsonNameToId mtlNameToId
      idToMatMap = M.union jsonIdToMat mtlIdToMat

  -- Phase 3: Load built-in scene objects
  let hittables = case objects sceneConfig of
        Just objs -> map (toHittable nameToIdMap) objs
        Nothing -> []

  -- Phase 4: Load OBJ geometry with correct material IDs
  objTrianglesWithMats <- case objFiles sceneConfig of
    Just entries ->
      mapM
        ( \entry ->
            loadObjWithOffset
              (path entry)
              (objposition entry)
              (overrideColor entry)
              Nothing
              nameToIdMap
              idToMatMap
        )
        entries
    Nothing -> return []

  let objTrianglesOnly = map fst objTrianglesWithMats
      allObjTriangles = concatMap (\(HittableList hs) -> extractTriangles hs) objTrianglesOnly

      configTriangles = extractTriangles hittables
      totalTriangles = configTriangles ++ allObjTriangles
      triangleObjects = map SomeHittable totalTriangles
      allObjects = triangleObjects ++ hittables

  putStrLn $ "Loaded " ++ show (length totalTriangles) ++ " triangles into BVH."
  putStrLn $ "Loaded " ++ show (length allObjects - length totalTriangles) ++ " other objects into BVH."

  let maxDepth = bvhMaxDepth (raytracer config)
  return (constructBVHWithLimit maxDepth allObjects, idToMatMap)

-- Attempt to locate and read .mtl file referenced in an .obj file
tryReadMtlFile :: FilePath -> IO String
tryReadMtlFile mtlpath = do
  let baseDir = takeDirectory mtlpath
  content <- readFile mtlpath
  let mtlLine = case filter ("mtllib" `isPrefixOf`) (lines content) of
        (l : _) -> l
        [] -> ""
  case words mtlLine of
    ["mtllib", name] -> do
      let mtlPath = baseDir </> name
      exists <- doesFileExist mtlPath
      if exists then readFile mtlPath else return ""
    _ -> return ""

-- Extract only triangles from the list of hittables
extractTriangles :: [SomeHittable] -> [T.Triangle]
extractTriangles [] = []
extractTriangles (SomeHittable obj : rest) =
  case cast obj of
    Just triangle -> triangle : extractTriangles rest
    Nothing -> extractTriangles rest

-- Resolve material name via map and construct object with material ID
toHittable :: M.Map String Int -> SceneObject -> SomeHittable
toHittable nameToIdMap (SphereObj center radius sColor mname) =
  let matId = fromMaybe 0 (mname >>= (`M.lookup` nameToIdMap))
   in SomeHittable (S.Sphere center radius sColor matId)
toHittable nameToIdMap (PlaneObj ppoint pnormal pColor mname) =
  let matId = fromMaybe 0 (mname >>= (`M.lookup` nameToIdMap))
   in SomeHittable (P.Plane ppoint pnormal pColor matId)
toHittable nameToIdMap (TriangleObj tv0 tv1 tv2 tColor mname) =
  let matId = fromMaybe 0 (mname >>= (`M.lookup` nameToIdMap))
   in SomeHittable (T.Triangle tv0 tv1 tv2 tColor matId)

getBackgroundColor :: R.Ray -> BackgroundSettings -> Col.Color
getBackgroundColor ray (Gradient c1 c2) =
  let unitDir = V.normalize (R.direction ray)
      tHit = 0.5 * (V.y unitDir + 1.0)
   in Col.lerp tHit c1 c2
getBackgroundColor _ (SolidColor solidColor) = solidColor
