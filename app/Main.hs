module Main where

import System.Environment (getArgs)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale, diffUTCTime)

import ProgressBar
import Control.Concurrent (threadDelay)

import ImageGenerator as IG
    ( createPPM, ppmToStr, createAndWriteFile )

main :: IO ()
main = do
    args <- getArgs -- Retrieve command-line args
    let (width, height) = parseArgs args
    putStrLn $ "Width: " ++ show width ++ ", Height: " ++ show height
    putStrLn ""

    currentTime <- getCurrentTime
    let timestamp = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" currentTime
    let filename = "out/" ++ timestamp ++ ".ppm"

    startFileCreation <- getCurrentTime
    IG.createAndWriteFile filename $ IG.ppmToStr $ IG.createPPM width height
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
parseArgs :: [String] -> (Int, Int)
parseArgs [] = (250, 250)  -- assign default
parseArgs [a] = let val = read a in (val, val)  -- if only one value: img becomes square with size of that value
parseArgs [a, b] = (read a, read b)  -- image size fixed on both sides by args
parseArgs _ = error "Too many arguments. Provide at most two arguments."