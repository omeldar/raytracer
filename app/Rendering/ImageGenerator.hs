{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}

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
    ObjFileEntry (..),
    RaytracerSettings (..),
    SceneObject (..),
    SceneSettings (..),
  )
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM
  ( atomically,
    newTQueue,
    tryReadTQueue,
    writeTQueue,
  )
import Control.Concurrent.STM.TQueue (TQueue)
import Control.Monad (forM, forM_, replicateM, replicateM_, when, (>=>))
import Core.Ray as R (Ray (..), direction, origin)
import Core.Vec3 as V (Vec3 (..), add, dot, mul, negateV, normalize, randomInUnitSphere, reflect, refract, scale, sub, vLength, x, y, z)
import Data.Array.IO (IOArray, newArray_, readArray, writeArray)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
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
import qualified Rendering.Camera as Cam (Camera (..), defaultCamera, generateRay)
import Rendering.Color as Col (Color, lerp)
import qualified Rendering.Light as L (Light (..), computeLighting)
import Rendering.Material (Material (..), defaultMaterial)
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, (</>))
import System.IO (BufferMode (BlockBuffering), IOMode (WriteMode), hPutStr, hSetBuffering, withFile)
import Utils.Constants (clamp, randomDouble)
import Utils.Interval (Interval (..))
import Utils.ProgressBar as PB (newProgressBar, updateMessage, updateProgress)

-- Define Pixel as Vec3 (representing RGB color)
type Pixel = V.Vec3

type Row = [Pixel]

type Image = [Row]

type JitterTable = [[[(Double, Double)]]]

createPPM :: Config -> FilePath -> IO ()
createPPM config filename = do
  let imgW = width (image config)
      imgH = height (image config)
      spp = samplesPerPixel (image config)
      numWorkers = 24

  (bvh, materialMap) <- parseSceneObjects config
  progressBar <- PB.newProgressBar imgH
  progressCounter <- newIORef 0

  let camSettings = camera config
      aspectRatio = fromIntegral imgW / fromIntegral imgH
      cam = Cam.defaultCamera (lookFrom camSettings) (lookAt camSettings) (vUp camSettings) (vfov camSettings) aspectRatio (aperture camSettings) (focusDist camSettings)

  jitterTable <- generateJitterTable imgW imgH spp

  rowQueue <- atomically newTQueue
  resultArray <- newArray_ (0, imgH - 1) :: IO (IOArray Int String)
  mapM_ (atomically . writeTQueue rowQueue) [0 .. imgH - 1]

  _ <- forkIO $ do
    let loop = do
          count <- readIORef progressCounter
          PB.updateProgress progressBar count
          PB.updateMessage progressBar ("Rendered rows: " ++ show count ++ "/" ++ show imgH)
          when (count < imgH) $ threadDelay 200_000 >> loop
    loop

  doneSignal <- newEmptyMVar
  replicateM_ numWorkers $ forkIO $ worker rowQueue resultArray config cam jitterTable bvh materialMap imgW imgH progressCounter doneSignal
  replicateM_ numWorkers (takeMVar doneSignal)

  withFile filename WriteMode $ \handle -> do
    hSetBuffering handle (BlockBuffering (Just (1024 * 512)))
    hPutStr handle ("P3\n" ++ show imgW ++ " " ++ show imgH ++ "\n255\n")
    forM_ [0 .. imgH - 1] (readArray resultArray Control.Monad.>=> hPutStr handle)

worker :: TQueue Int -> IOArray Int String -> Config -> Cam.Camera -> JitterTable -> BVHNode -> M.Map Int Material -> Int -> Int -> IORef Int -> MVar () -> IO ()
worker queue resultArray config cam jitTable bvh matMap imgW imgH counter doneSignal = do
  let loop = do
        mRow <- atomically $ tryReadTQueue queue
        case mRow of
          Nothing -> putMVar doneSignal ()
          Just j -> do
            rowStr <- renderRow config cam (jitTable !! j) bvh matMap imgW imgH counter j
            writeArray resultArray j rowStr
            loop
  loop

renderRow :: Config -> Cam.Camera -> [[(Double, Double)]] -> BVHNode -> M.Map Int Material -> Int -> Int -> IORef Int -> Int -> IO String
renderRow config cam pixelOffsets world matMap imgW imgH counter j = do
  let rowIdx = imgH - 1 - j
  rowPixels <- mapM (\i -> pixelColor config cam (pixelOffsets !! i) world matMap i rowIdx) [0 .. imgW - 1]
  modifyIORef' counter (+ 1)
  return $ unlines (map showPixel rowPixels)

pixelColor :: Config -> Cam.Camera -> [(Double, Double)] -> BVHNode -> M.Map Int Material -> Int -> Int -> IO Col.Color
pixelColor config cam offsets world matMap i j = do
  colors <- mapM (\(u, v) -> samplePixel config cam u v world matMap i j) offsets
  return $ averageColor colors

samplePixel :: Config -> Cam.Camera -> Double -> Double -> BVHNode -> M.Map Int Material -> Int -> Int -> IO Col.Color
samplePixel config cam u v world matMap i j = do
  ray <- Cam.generateRay cam i j (width $ image config) (height $ image config) u v
  traceRay config world matMap ray (maxBounces $ raytracer config)

averageColor :: [Col.Color] -> Col.Color
averageColor cs = let !sumColor = foldl' V.add (Vec3 0 0 0) cs in V.scale (1 / fromIntegral (length cs)) sumColor

showPixel :: V.Vec3 -> String
showPixel (Vec3 r g b) = unwords $ map (show . floor . (* 255.999)) [r, g, b]

generateJitterTable :: Int -> Int -> Int -> IO JitterTable
generateJitterTable w h spp = forM [0 .. h - 1] $ \_ -> forM [0 .. w - 1] $ \_ -> replicateM spp ((,) <$> randomDouble <*> randomDouble)

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
