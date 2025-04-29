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
import Control.Monad (forM_, replicateM, replicateM_, when)
import Core.Ray as R (Ray (..), direction)
import Core.Vec3 as V (Vec3 (..), add, dot, mul, normalize, reflect, refract, scale, y)
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
import qualified Rendering.Camera as Cam (Camera, defaultCamera, generateRay)
import Rendering.Color as Col (Color, lerp)
import Rendering.Light (lightContribution)
import qualified Rendering.Light as L (Light (..))
import Rendering.Material (Material (..), defaultMaterial)
import Rendering.SkySphere (SkySphere, loadSkySphere, sampleSkySphere)
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, (</>))
import System.IO (BufferMode (BlockBuffering), IOMode (WriteMode), hPutStr, hSetBuffering, withFile)
import System.Random (Random (..), StdGen, mkStdGen)
import qualified System.Random.MWC as MWC
import Utils.Interval (Interval (..))
import Utils.ProgressBar as PB (newProgressBar, updateMessage, updateProgress)
import Utils.RandomHelper (getThreadRNG, randomDouble)

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

  skySphere <- case skyTexture (scene config) of
    Just pathToTexture -> Just <$> loadSkySphere pathToTexture
    Nothing -> return Nothing

  let cameraObj =
        Cam.defaultCamera
          (lookFrom (camera config))
          (lookAt (camera config))
          (vUp (camera config))
          (vfov (camera config))
          (fromIntegral (width (image config)) / fromIntegral (height (image config)))
          (aperture (camera config))
          (focusDist (camera config))
      sceneLights = maybe [] (map convertLight) (lights (scene config))

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

  let backgroundFunc = backgroundColor (background config)

  replicateM_ numWorkers $ forkIO $ worker rowQueue resultArray config bvh materialMap skySphere backgroundFunc cameraObj sceneLights imgW imgH progressCounter doneSignal
  replicateM_ numWorkers (takeMVar doneSignal)

  withFile filename WriteMode $ \handle -> do
    hSetBuffering handle (BlockBuffering (Just (1024 * 512)))
    hPutStr handle ("P3\n" ++ show imgW ++ " " ++ show imgH ++ "\n255\n")
    forM_ [0 .. imgH - 1] $ \j -> do
      str <- readArray resultArray j
      hPutStr handle str

worker :: TQueue Int -> IOArray Int String -> Config -> BVHNode -> M.Map Int Material -> Maybe SkySphere -> (R.Ray -> Col.Color) -> Cam.Camera -> [L.Light] -> Int -> Int -> IORef Int -> MVar () -> IO ()
worker queue resultArray config bvh matMap skySphere backgroundFunc cameraObj sceneLights imgW imgH counter doneSignal = do
  let loop = do
        mRow <- atomically $ tryReadTQueue queue
        case mRow of
          Nothing -> putMVar doneSignal ()
          Just j -> do
            row <- renderRow config bvh matMap skySphere backgroundFunc cameraObj sceneLights imgW imgH counter j
            writeArray resultArray j (snd row)
            loop
  loop

renderRow :: Config -> BVHNode -> M.Map Int Material -> Maybe SkySphere -> (R.Ray -> Col.Color) -> Cam.Camera -> [L.Light] -> Int -> Int -> IORef Int -> Int -> IO (Int, String)
renderRow config bvh materialMap skySphere backgroundFunc camObj sceneLights imgW imgH progressCounter j = do
  let rowIdx = imgH - 1 - j
  pixels <- mapM (pixelColor config bvh materialMap skySphere backgroundFunc camObj sceneLights rowIdx) [0 .. imgW - 1]
  let !rowStr = unlines (map showPixel pixels)
  modifyIORef' progressCounter (+ 1)
  return (j, rowStr)

pixelColor :: Config -> BVHNode -> M.Map Int Material -> Maybe SkySphere -> (R.Ray -> Col.Color) -> Cam.Camera -> [L.Light] -> Int -> Int -> IO Col.Color
pixelColor config world materialMap skySphere backgroundFunc camObj sceneLights j i = do
  sampledColors <- replicateM (samplesPerPixel (image config)) (samplePixel config world materialMap skySphere backgroundFunc camObj sceneLights i j)
  return (averageColor sampledColors)

-- Sampling a pixel
samplePixel ::
  Config ->
  BVHNode ->
  M.Map Int Material ->
  Maybe SkySphere ->
  (R.Ray -> Col.Color) ->
  Cam.Camera ->
  [L.Light] ->
  Int ->
  Int ->
  IO Col.Color
samplePixel config world materialMap skySphere backgroundFunc camObj sceneLights i j = do
  let imgW = fromIntegral (width (image config)) :: Double
      imgH = fromIntegral (height (image config)) :: Double
  uOffset <- if antialiasing (image config) then randomDouble else pure 0.5
  vOffset <- if antialiasing (image config) then randomDouble else pure 0.5
  let imgW' = round imgW :: Int
      imgH' = round imgH :: Int
  ray <- Cam.generateRay camObj i j imgW' imgH' uOffset vOffset
  gen <- getThreadRNG
  seed <- MWC.uniform gen :: IO Int
  let rng = mkStdGen seed
  return $ traceRay world materialMap skySphere backgroundFunc sceneLights ray (maxBounces (raytracer config)) rng

showPixel :: Pixel -> String
showPixel (V.Vec3 r g b) = unwords $ map (show . (truncate :: Double -> Int) . (* 255.999)) [r, g, b]

averageColor :: [Color] -> Color
averageColor colors =
  let !colsum = foldl' add (V.Vec3 0 0 0) colors
   in V.scale (1.0 / fromIntegral (length colors)) colsum

-- Pure traceRay using StdGen
traceRay ::
  BVHNode ->
  M.Map Int Material ->
  Maybe SkySphere ->
  (R.Ray -> Col.Color) ->
  [L.Light] ->
  R.Ray ->
  Int ->
  StdGen ->
  Col.Color
traceRay world materialMap skySphere backgroundFunc sceneLights ray0 maxDepth = traceLoop ray0 maxDepth (V.Vec3 1 1 1)
  where
    traceLoop _ 0 attenuation _ = backgroundSample attenuation ray0
    traceLoop ray depth attenuation rng =
      case H.hit world ray (Interval 0.001 10_000) of
        Nothing -> backgroundSample attenuation ray
        Just rec ->
          let matId = H.materialId rec
              mat = fromMaybe defaultMaterial (M.lookup matId materialMap)
              emitted = fromMaybe (V.Vec3 0 0 0) (emissionColor mat)
              surfaceColor = diffuseColor mat
              normalVec = H.normal rec
              unitDir = V.normalize (R.direction ray)
              hitPoint = H.point rec

              directLight = foldl V.add (V.Vec3 0 0 0) (map (lightContribution rec) sceneLights)
              litColor = surfaceColor `V.mul` directLight

              (randX, rng1) = randomR (-1.0, 1.0) rng
              (randY, rng2) = randomR (-1.0, 1.0) rng1
              (randZ, rng3) = randomR (-1.0, 1.0) rng2
              (randD, rng4) = randomR (0.0, 1.0) rng3
              randVec = V.normalize (V.Vec3 randX randY randZ)

              cosTheta = min (negate (V.dot unitDir normalVec)) 1.0
              sinTheta = sqrt (1.0 - cosTheta * cosTheta)
              reflectProb = maybe 1.0 (schlick cosTheta) (ior mat)

              nextRay =
                case (ior mat, shininess mat) of
                  (Just refIdx, _)
                    | dissolve mat < Just 1.0 ->
                        if randD < reflectProb || (refIdx * sinTheta > 1.0)
                          then R.Ray hitPoint (V.reflect unitDir normalVec)
                          else R.Ray hitPoint (V.refract unitDir normalVec (1.0 / refIdx))
                  (_, Just s)
                    | s > 100 ->
                        let reflected = V.reflect unitDir normalVec
                            fuzzed = V.add reflected (V.scale 0.05 randVec)
                         in R.Ray hitPoint (V.normalize fuzzed)
                  _ ->
                    let scatterDir = V.add normalVec randVec
                     in R.Ray hitPoint (V.normalize scatterDir)

              newAttenuation = attenuation `V.mul` surfaceColor
              bounceColor = traceLoop nextRay (depth - 1) newAttenuation rng4
              clampedColor = clamp bounceColor 0 4.0
           in emitted `V.add` litColor `V.add` clampedColor

    backgroundSample attenuation ray =
      case skySphere of
        Just sky -> attenuation `V.mul` sampleSkySphere sky (R.direction ray)
        Nothing -> attenuation `V.mul` backgroundFunc ray

    schlick cosine refIdx =
      let r0 = (1 - refIdx) / (1 + refIdx)
          r0sq = r0 * r0
       in r0sq + (1 - r0sq) * ((1 - cosine) ** 5)

clamp :: Vec3 -> Double -> Double -> Vec3
clamp (Vec3 cx cy cz) lo hi = Vec3 (cl cx) (cl cy) (cl cz)
  where
    cl v = max lo (min hi v)

-- somewhere at the top of ImageGenerator.hs or in Rendering.Color
backgroundColor :: BackgroundSettings -> (R.Ray -> Col.Color)
backgroundColor (SolidColor c) = const c
backgroundColor (Gradient c1 c2) = \ray ->
  let unitDir = V.normalize (R.direction ray)
      tval = 0.5 * (y unitDir + 1.0)
   in Col.lerp tval c2 c1

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
