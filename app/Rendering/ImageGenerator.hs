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
import Control.Monad (forM_, replicateM_, when)
import qualified Control.Monad
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
import qualified Rendering.Camera as Cam (defaultCamera, generateRay)
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

createPPM :: Config -> FilePath -> IO ()
createPPM config filename = do
  let imgW = width (image config)
      imgH = height (image config)
      numWorkers = 24

  (bvh, materialMap) <- parseSceneObjects config
  progressBar <- PB.newProgressBar imgH
  progressCounter <- newIORef 0

  rowQueue <- atomically newTQueue
  resultArray <- newArray_ (0, imgH - 1) :: IO (IOArray Int String)

  mapM_ (atomically . writeTQueue rowQueue) [0 .. imgH - 1]

  _ <-
    forkIO $
      let loop = do
            count <- readIORef progressCounter
            PB.updateProgress progressBar count
            PB.updateMessage progressBar ("Rendered rows: " ++ show count ++ "/" ++ show imgH)
            when (count < imgH) $ threadDelay 200_000 >> loop
       in loop

  doneSignal <- newEmptyMVar
  replicateM_ numWorkers $ forkIO $ worker rowQueue resultArray config bvh materialMap imgW imgH progressCounter doneSignal
  replicateM_ numWorkers (takeMVar doneSignal)

  withFile filename WriteMode $ \handle -> do
    hSetBuffering handle (BlockBuffering (Just (1024 * 512)))
    hPutStr handle ("P3\n" ++ show imgW ++ " " ++ show imgH ++ "\n255\n")
    forM_ [0 .. imgH - 1] $ \j -> do
      str <- readArray resultArray j
      hPutStr handle str

worker :: TQueue Int -> IOArray Int String -> Config -> BVHNode -> M.Map Int Material -> Int -> Int -> IORef Int -> MVar () -> IO ()
worker queue resultArray config bvh matMap imgW imgH counter doneSignal = do
  let loop = do
        mRow <- atomically $ tryReadTQueue queue
        case mRow of
          Nothing -> putMVar doneSignal ()
          Just j -> do
            row <- renderRow config bvh matMap imgW imgH counter j
            writeArray resultArray j (snd row)
            loop
  loop

renderRow :: Config -> BVHNode -> M.Map Int Material -> Int -> Int -> IORef Int -> Int -> IO (Int, String)
renderRow config bvh materialMap imgW imgH progressCounter j = do
  let rowIdx = imgH - 1 - j
  pixels <- mapM (pixelColor config bvh materialMap `flip` rowIdx) [0 .. imgW - 1]
  let !rowStr = unlines (map showPixel pixels)
  modifyIORef' progressCounter (+ 1)
  return (j, rowStr)

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
traceRay config bvh materialMap ray0 maxDepth = traceLoop ray0 maxDepth (Vec3 1 1 1)
  where
    -- Pre-convert all lights from config
    sceneLights :: [L.Light]
    sceneLights = maybe [] (map convertLight) (lights (scene config))

    -- Main ray tracing loop with attenuation
    traceLoop :: R.Ray -> Int -> Col.Color -> IO Col.Color
    traceLoop ray 0 attenuation =
      return $ attenuation `mul` backgroundColorFunc (background config) ray
    traceLoop ray depth attenuation = do
      let interval = Interval 0.001 100
      case H.hit bvh ray interval of
        Nothing ->
          return $ attenuation `mul` backgroundColorFunc (background config) ray
        Just rec -> do
          let matId = H.materialId rec
              mat = fromMaybe defaultMaterial (M.lookup matId materialMap)
              emitted = fromMaybe (Vec3 0 0 0) (emissionColor mat)
              surfaceColor = diffuseColor mat
              normalVec = H.normal rec
              unitDir = V.normalize (R.direction ray)
              hitPoint = H.point rec

          -- Compute direct lighting contribution
          directLight <- L.computeLighting rec sceneLights bvh
          let litColor = V.mul directLight surfaceColor
              clampedLight =
                Vec3
                  (clamp (x litColor) 0 1)
                  (clamp (y litColor) 0 1)
                  (clamp (z litColor) 0 1)

          -- Decide scattering behavior
          scatterRay <- case ior mat of
            Just refIdx -> do
              let cosTheta = min 1.0 (V.dot (V.negateV unitDir) normalVec)
                  reflectDir = V.reflect unitDir normalVec
                  refractDir = V.refract unitDir normalVec (1 / refIdx)
                  rProb = schlick cosTheta refIdx
                  blendedDir = V.add (V.scale rProb reflectDir) (V.scale (1 - rProb) refractDir)
              return $ R.Ray hitPoint blendedDir
            Nothing -> case shininess mat of
              Just s | s > 100 -> do
                rand <- V.randomInUnitSphere
                let reflected = V.reflect unitDir normalVec
                    fuzzed = V.add reflected (V.scale 0.05 rand)
                return $ R.Ray hitPoint fuzzed
              _ -> do
                rand <- V.randomInUnitSphere
                let scatterDir = V.add normalVec rand
                return $ R.Ray hitPoint scatterDir

          let newAttenuation = attenuation `mul` surfaceColor
          bounceColor <- traceLoop scatterRay (depth - 1) newAttenuation

          -- Combine everything: emission + lighting + scattered
          return $ emitted `add` clampedLight `add` bounceColor

backgroundColorFunc :: BackgroundSettings -> R.Ray -> Col.Color
backgroundColorFunc (SolidColor c) _ = c
backgroundColorFunc (Gradient top bot) ray =
  let unitDir = V.normalize (R.direction ray)
      bgt = 0.5 * (y unitDir + 1.0)
   in Col.lerp bgt bot top

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
