{-# LANGUAGE NumericUnderscores #-}

module Rendering.ImageGenerator (createPPM) where

import Config
  ( BackgroundSettings (..),
    CameraSettings (..),
    Config (..),
    ImageSettings (..),
    LightSettings (..),
    SceneSettings (..),
  )
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.STM
  ( atomically,
    newTQueue,
    writeTQueue,
  )
import Control.Monad (replicateM_, when)
import Core.Ray as R (Ray (..), direction)
import Core.Vec3 as V (normalize, y)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map as M
import qualified Rendering.Camera as Cam (defaultCamera)
import Rendering.Color as Col (Color, lerp)
import qualified Rendering.Light as L (Light (..))
import Rendering.RenderWorker (worker)
import Rendering.SceneParser (parseSceneObjects)
import Rendering.SkySphere (loadSkySphere)
import System.IO (BufferMode (BlockBuffering), IOMode (WriteMode), hPutStr, hSetBuffering, withFile)
import Utils.ProgressBar as PB (newProgressBar, updateMessage, updateProgress)

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
          (fromIntegral imgW / fromIntegral imgH)
          (aperture (camera config))
          (focusDist (camera config))

      sceneLights = maybe [] (map convertLight) (lights (scene config))

  progressBar <- PB.newProgressBar imgH
  progressCounter <- newIORef 0

  rowQueue <- atomically newTQueue
  rowBuffer <- newIORef M.empty
  nextRowRef <- newIORef 0
  doneSignal <- newEmptyMVar
  writerDoneSignal <- newEmptyMVar -- new
  let backgroundFunc = backgroundColor (background config)

  mapM_ (atomically . writeTQueue rowQueue) [0 .. imgH - 1]

  replicateM_ numWorkers $
    forkIO $
      worker
        rowQueue
        rowBuffer
        config
        bvh
        materialMap
        skySphere
        backgroundFunc
        cameraObj
        sceneLights
        imgW
        imgH
        progressCounter
        doneSignal

  -- Writer thread
  _ <- forkIO $ do
    withFile filename WriteMode $ \handle -> do
      hSetBuffering handle (BlockBuffering (Just (1024 * 512)))
      hPutStr handle ("P3\n" ++ show imgW ++ " " ++ show imgH ++ "\n255\n")
      let loop = do
            next <- readIORef nextRowRef
            buffer <- readIORef rowBuffer
            if next >= imgH
              then putMVar writerDoneSignal () -- done writing
              else case M.lookup next buffer of
                Just rowStr -> do
                  hPutStr handle rowStr
                  atomicModifyIORef' rowBuffer (\m -> (M.delete next m, ()))
                  atomicModifyIORef' nextRowRef (\n -> (n + 1, ()))
                  loop
                Nothing -> do
                  threadDelay 10_000
                  loop
      loop

  -- Progress bar thread
  _ <- forkIO $ do
    let loop = do
          count <- readIORef progressCounter
          PB.updateProgress progressBar count
          PB.updateMessage progressBar ("Rendered rows: " ++ show count ++ "/" ++ show imgH)
          when (count < imgH) $ threadDelay 200_000 >> loop
    loop

  replicateM_ numWorkers (takeMVar doneSignal)
  takeMVar writerDoneSignal

backgroundColor :: BackgroundSettings -> (R.Ray -> Col.Color)
backgroundColor (SolidColor c) = const c
backgroundColor (Gradient c1 c2) = \ray ->
  let unitDir = V.normalize (R.direction ray)
      tval = 0.5 * (y unitDir + 1.0)
   in Col.lerp tval c2 c1

convertLight :: LightSettings -> L.Light
convertLight (PointLight pos lIntensity) = L.PointLight pos lIntensity
convertLight (DirectionalLight dir lIntensity) = L.DirectionalLight dir lIntensity
