{-# LANGUAGE BangPatterns #-}

module Rendering.RenderWorker where

import Config
  ( Config (..),
    ImageSettings (..),
    RaytracerSettings (..),
  )
import Control.Concurrent (putMVar)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM
  ( atomically,
    tryReadTQueue,
  )
import Control.Concurrent.STM.TQueue (TQueue)
import Control.DeepSeq (deepseq)
import Control.Monad (forM, replicateM)
import Core.Ray as R (Ray (..))
import Core.Vec3 as V (Vec3 (..), add, scale)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef')
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Map.Strict as MS
import Hittable.BVH (BVHNode)
import qualified Rendering.Camera as Cam (Camera, generateRay)
import Rendering.Color as Col (Color)
import qualified Rendering.Light as L (Light (..))
import Rendering.Material (Material (..))
import Rendering.SkySphere (SkySphere)
import Rendering.Trace (gammaCorrect, toneMap, traceRay)
import System.Random (mkStdGen)
import qualified System.Random.MWC as MWC
import Utils.RandomHelper (getThreadRNG, randomDouble)

type Pixel = V.Vec3

worker ::
  TQueue Int ->
  IORef (M.Map Int String) ->
  Config ->
  BVHNode ->
  M.Map Int Material ->
  Maybe SkySphere ->
  (R.Ray -> Col.Color) ->
  Cam.Camera ->
  [L.Light] ->
  Int ->
  Int ->
  IORef Int ->
  MVar () ->
  IO ()
worker queue rowBuffer config bvh matMap skySphere backgroundFunc cameraObj sceneLights imgW imgH counter doneSignal = do
  let loop = do
        mRow <- atomically $ tryReadTQueue queue
        case mRow of
          Nothing -> putMVar doneSignal ()
          Just j -> do
            (rowIdx, rowStr) <- renderRow config bvh matMap skySphere backgroundFunc cameraObj sceneLights imgW imgH counter j
            rowStr `deepseq` atomicModifyIORef' rowBuffer (\buf -> (MS.insert rowIdx rowStr buf, ()))
            loop
  loop

renderRow :: Config -> BVHNode -> M.Map Int Material -> Maybe SkySphere -> (R.Ray -> Col.Color) -> Cam.Camera -> [L.Light] -> Int -> Int -> IORef Int -> Int -> IO (Int, String)
renderRow config bvh materialMap skySphere backgroundFunc camObj sceneLights imgW imgH progressCounter j = do
  let rowIdx = imgH - 1 - j
  pixels <- forM [0 .. imgW - 1] $ \i -> do
    pixel <- pixelColor config bvh materialMap skySphere backgroundFunc camObj sceneLights rowIdx i
    pixel `seq` return pixel
  let !rowStr = unlines (map showPixel pixels)
  modifyIORef' progressCounter (+ 1)
  return (j, rowStr)

pixelColor :: Config -> BVHNode -> MS.Map Int Material -> Maybe SkySphere -> (Ray -> Color) -> Cam.Camera -> [L.Light] -> Int -> Int -> IO Vec3
pixelColor config world materialMap skySphere backgroundFunc camObj sceneLights j i = do
  sampledColors <- replicateM (samplesPerPixel (image config)) (samplePixel config world materialMap skySphere backgroundFunc camObj sceneLights i j)
  let averaged = averageColor sampledColors
      toned = toneMap (exposure (image config)) averaged
      gammaed = gammaCorrect (gamma (image config)) toned
  return gammaed

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