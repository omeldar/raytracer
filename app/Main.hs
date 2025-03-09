module Main where

import System.Environment (getArgs)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale, diffUTCTime)

import Utils.ProgressBar
import Control.Concurrent (threadDelay)

import Rendering.ImageGenerator as IG
    ( createPPM, ppmToStr, createAndWriteFile )

main :: IO ()
main = do
    args <- getArgs -- Retrieve command-line args
    let (width, height, samplesPerPixel) = parseArgs args
    putStrLn "-----------------------------------"
    putStrLn $ "Width: " ++ show width ++ ", Height: " ++ show height ++ ", AASize: " ++ show samplesPerPixel

    currentTime <- getCurrentTime
    let timestamp = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" currentTime
    let filename = "out/" ++ timestamp ++ ".ppm"

    startFileCreation <- getCurrentTime
    image <- createPPM width height samplesPerPixel
    IG.createAndWriteFile filename $ IG.ppmToStr image
    endFileCreation <- getCurrentTime

    let timeToCreate = diffUTCTime endFileCreation startFileCreation
    putStrLn ""
    putStrLn $ "Time taken for create and write file: " ++ show timeToCreate
    putStrLn $ "File written to: " ++ filename

simTask :: ProgressBar -> IO()
simTask pb = do
    updateMessage pb "Starting tasks..."

    threadDelay 100000
    updateMessage pb "Running..."
    updateProgress pb 313

    threadDelay 100000
    updateMessage pb "Finalizing"
    updateProgress pb 32417

    threadDelay 100000
    updateMessage pb "Task complete!"
    updateProgress pb 32418

    putStrLn ""

-- Parse the command-line arguments
parseArgs :: [String] -> (Int, Int, Int)
parseArgs [] = (640, 360, 10)  -- assign default with aspect ratio 16:9
parseArgs [a] = 
    let width = read a
        height = (width * 9) `div` 16 -- Calculate height based on 16:9 aspect ratio
    in (width, height, 10)
parseArgs [a, b] =
    let width = read a
        height = read b
        -- Ensure the aspect ratio is 16:9
        expectedHeight = (width * 9) `div` 16
    in if height == expectedHeight
        then (width, height, 10)
        else error $ "Invalid aspect ratio. For width " ++ show width ++ ", height must be " ++ show expectedHeight
parseArgs [a, b, c] = (read a, read b, read c)
parseArgs _ = error "Too many arguments. Provide at most two arguments."