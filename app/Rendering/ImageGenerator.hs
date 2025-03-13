module Rendering.ImageGenerator
  ( createPPM,
  )
where

import Control.Concurrent (forkIO, newMVar, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, readMVar)
import Control.DeepSeq (deepseq)
import Control.Monad (forM_, replicateM)
import Core.Ray as R (Ray (Ray, direction))
import Core.Vec3 as V (Vec3 (..), add, normalize, randomInUnitSphere, scale, y)
import qualified Data.Map.Strict as Map
import Hittable.Class as H (HitRecord (normal, point), Hittable (hit))
import Hittable.HittableList as HL (HittableList (HittableList))
import Hittable.Objects.Sphere as S (Sphere (Sphere))
import Rendering.Camera as Cam (defaultCamera, generateRay)
import Rendering.Color as Col (Color, lerp, writeColor)
import System.IO (BufferMode (BlockBuffering), Handle, IOMode (WriteMode), hPutStr, hSetBuffering, withFile)
import Utils.Constants (randomDouble)
import Utils.Interval (Interval (..))
import Utils.ProgressBar (ProgressBar (..), newProgressBar, renderProgressBar, updateProgress)

-- Number of threads to use
numThreads :: Int
numThreads = 8

-- Chunk size (number of rows per chunk)
chunkSize :: Int
chunkSize = 50

createPPM :: Int -> Int -> Int -> Bool -> String -> IO ()
createPPM width height samplesPerPixel aa filename = do
  let numChunks = (height + chunkSize - 1) `div` chunkSize

  -- Shared state
  workQueue <- newMVar [0 .. numChunks - 1] -- List of chunks to process
  completedChunks <- newMVar Map.empty -- Store results indexed by chunk
  progressBar <- newProgressBar numChunks -- Progress bar
  withFile filename WriteMode $ \handle -> do
    hSetBuffering handle (BlockBuffering (Just (1024 * 1024)))
    hPutStr handle ("P3\n" ++ show width ++ " " ++ show height ++ "\n255\n")

    -- Spawn worker threads
    forM_ [1 .. numThreads] $ \_ ->
      forkIO $
        workerThread width height samplesPerPixel aa workQueue completedChunks progressBar

    -- Collect results and write them in order
    collectAndWrite handle completedChunks numChunks progressBar

workerThread :: Int -> Int -> Int -> Bool -> MVar [Int] -> MVar (Map.Map Int String) -> ProgressBar -> IO ()
workerThread width height samplesPerPixel aa workQueue completedChunks progressBar = do
  putStrLn "Worker thread started"
  let processChunk chunk = do
        putStrLn $ "Processing chunk: " ++ show chunk
        let startRow = chunk * chunkSize
        let endRow = min (startRow + chunkSize) height
        rows <-
          mapM
            ( \j -> do
                renderRow width height samplesPerPixel aa j
            )
            [height - 1, height - 2 .. 0]
        let chunkData = unlines rows
        chunkData `deepseq` modifyMVar_ completedChunks (return . Map.insert chunk chunkData)
        updateProgress progressBar 1
        renderProgressBar progressBar
        putStrLn $ "Finished chunk: " ++ show chunk

  let loop = do
        maybeChunk <- modifyMVar workQueue $ \queue -> case queue of
          [] -> return (queue, Nothing)
          (c : cs) -> return (cs, Just c)

        case maybeChunk of
          Just chunk -> processChunk chunk >> loop
          Nothing -> putStrLn "Worker thread finished"

  loop

collectAndWrite :: Handle -> MVar (Map.Map Int String) -> Int -> ProgressBar -> IO ()
collectAndWrite handle completedChunks numChunks progressBar = go 0
  where
    go chunk
      | chunk >= numChunks = putStrLn "All chunks written" >> return ()
      | otherwise = do
          chunkMap <- readMVar completedChunks
          case Map.lookup chunk chunkMap of
            Just chunkData -> do
              putStrLn $ "Writing chunk: " ++ show chunk
              hPutStr handle chunkData
              modifyMVar_ completedChunks (return . Map.delete chunk)
              updateProgress progressBar 1
              renderProgressBar progressBar
              go (chunk + 1)
            Nothing -> do
              threadDelay 1000 -- Wait a little before retrying
              go chunk

renderRow :: Int -> Int -> Int -> Bool -> Int -> IO String
renderRow width height samplesPerPixel aa j = do
  pixels <- mapM (\i -> renderPixel i j width height samplesPerPixel aa) [0 .. width - 1]
  return $ unlines pixels

renderPixel :: Int -> Int -> Int -> Int -> Int -> Bool -> IO String
renderPixel i j width height samplesPerPixel aa = do
  sampledColors <- replicateM samplesPerPixel (samplePixel i j width height aa)
  return $ writeColor (averageColor sampledColors)

samplePixel :: Int -> Int -> Int -> Int -> Bool -> IO Col.Color
samplePixel i j width height aa = do
  let lookFrom = V.Vec3 0 0 5
      lookAt = V.Vec3 0 0 (-1)
      vUp = V.Vec3 0 1 0
      vfov = 30.0
      aperture = 0.0
      focusDist = 1.0
      camera = Cam.defaultCamera lookFrom lookAt vUp vfov (fromIntegral width / fromIntegral height) aperture focusDist

  uOffset <- if aa then randomDouble else return 0.5
  vOffset <- if aa then randomDouble else return 0.5
  ray <- Cam.generateRay camera i j width height uOffset vOffset
  traceRay ray 50

traceRay :: R.Ray -> Int -> IO Col.Color
traceRay ray depth
  | depth <= 0 = return (V.Vec3 0 0 0)
  | otherwise = do
      let spheres = HL.HittableList [S.Sphere (V.Vec3 (-1.2) 0 (-1)) 0.5, S.Sphere (V.Vec3 0 0 (-1)) 0.5, S.Sphere (V.Vec3 1.2 0.3 (-1.6)) 0.5]
          interval = Interval 0.001 100
      case H.hit spheres ray interval of
        Just hitRecord -> do
          randomVec <- V.randomInUnitSphere
          let newDirection = V.add (H.normal hitRecord) randomVec
              scatteredRay = R.Ray (H.point hitRecord) newDirection
          bounceColor <- traceRay scatteredRay (depth - 1)
          return $ V.scale 0.5 bounceColor
        Nothing -> do
          let unitDir = V.normalize (R.direction ray)
              tHit = 0.5 * (V.y unitDir + 1.0)
          return $ Col.lerp tHit (V.Vec3 1 1 1) (V.Vec3 0.5 0.7 1)

averageColor :: [Col.Color] -> Col.Color
averageColor colors = V.scale (1.0 / fromIntegral (length colors)) (foldr V.add (V.Vec3 0 0 0) colors)
