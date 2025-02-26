module Rendering.ImageGenerator (
    -- types
    Pixel, Row, Image,

    -- functions
    createPPM, ppmToStr, createAndWriteFile
) where

import Control.Parallel (par, pseq)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, writeTVar)
import Control.Monad(forM_, replicateM_)

import Core.Vec3 as V

import Core.Ray as R
import Rendering.Camera as Cam
import Rendering.Color as Col

import Hittable.Objects.Sphere as S
import Hittable.Class as H

-- Define Pixel as Vec3 (representing RGB color)
type Pixel = V.Vec3
type Row = [Pixel]
type Image = [Row]

createPPM :: Int -> Int -> Int -> (Int -> IO ()) -> IO Image
createPPM width height threadCount updateProgress = do
    let rowsPerThread = height `div` threadCount
        rowRanges = zip [0..] [(i * rowsPerThread, min height ((i+1) * rowsPerThread)) | i <- [0..threadCount-1]]

    results <- newTVarIO (replicate threadCount [])
    doneSignal <- newEmptyMVar

    forM_ rowRanges $ \(index, (startY, endY)) -> forkIO $ do
        rows <- generateRows width height (startY, endY) updateProgress
        atomically $ modifyTVar' results (\xs -> take index xs ++ [rows] ++ drop (index+1) xs)
        putMVar doneSignal ()

    replicateM_ threadCount (takeMVar doneSignal)

    orderedRows <- atomically $ readTVar results
    return (concat orderedRows)

generateRows :: Int -> Int -> (Int, Int) -> (Int -> IO ()) -> IO [Row]
generateRows width height (startY, endY) updateProgress =
    mapM (\j -> do
        row <- mapM (\i -> return (pixelColor i (height - 1 - j))) [0 .. width - 1]
        updateProgress 1
        return row  
    ) [startY .. endY - 1]
  where
    camera = Cam.defaultCamera width height
    pixelColor i j = 
        let ray = Cam.generateRay camera i j width height 
        in traceRay ray

traceRay :: R.Ray -> Col.Color
traceRay ray =
    let sphere = S.Sphere (V.Vec3 0 0 (-1)) 0.5
        tMin = 0.0
        tMax = 100
    in case H.hit sphere ray tMin tMax of
        Just hitRec -> 0.5 `V.scale` (H.normal hitRec `V.add` V.Vec3 1 1 1)

        Nothing ->  -- Background gradient
            Col.lerp (0.5 * (V.y (V.normalize (R.direction ray)) + 1.0))
                     (V.Vec3 1 1 1)
                     (V.Vec3 0.5 0.7 1.0)


-- Convert an Image to a PPM string
ppmToStr :: Image -> String
ppmToStr image =
    let height = length image
        width = if null image then 0 else length (head image)
        header = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n"
        pixelData = unlines $ map (unwords . map showPixel) image
    in header ++ pixelData
    where
        -- Convert a Vec3 (Pixel) to a PPM format string
        showPixel :: Pixel -> String
        showPixel (V.Vec3 r g b) =
            unwords $ map (show . (truncate :: Double -> Int) . (* 255.999)) [r, g, b]

-- Write the PPM image to a file
createAndWriteFile :: String -> String -> IO ()
createAndWriteFile = writeFile