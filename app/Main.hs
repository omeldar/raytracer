module Main where

import System.Environment (getArgs)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale, diffUTCTime)

import Utils.ProgressBar

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, modifyTVar', readTVarIO)

import Control.Monad (replicateM_)

import Rendering.ImageGenerator as IG
    ( createPPM, ppmToStr, createAndWriteFile )

main :: IO ()
main = do
    args <- getArgs
    let (width, height, threads) = parseArgs args
    putStrLn "-----------------------------------"
    putStrLn $ "Rendering " ++ show width ++ "x" ++ show height ++ " with " ++ show threads ++ " threads."

    progressBar <- newProgressBar (width * height)
    progress <- newTVarIO (0 :: Int)

    let updateProgress steps = do
          atomically $ modifyTVar' progress (+ steps)
          currentProgress <- readTVarIO progress
          renderProgressBar progressBar

    startTime <- getCurrentTime

    -- Pass `updateProgress` correctly as `Int -> IO ()`
    ppmImage <- createPPM width height threads updateProgress

    currentTime <- getCurrentTime
    let timestamp = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" currentTime
    let filename = "out/" ++ timestamp ++ ".ppm"

    IG.createAndWriteFile filename $ IG.ppmToStr ppmImage

    endTime <- getCurrentTime
    putStrLn "\nRendering complete!"
    putStrLn $ "Time taken: " ++ show (diffUTCTime endTime startTime)

    putStrLn $ "File written to: " ++ filename

parseArgs :: [String] -> (Int, Int, Int)
parseArgs [] = (640, 360, 2)
parseArgs [a] =
    let width = read a
        height = (width * 9) `div` 16
    in (width, height, 4)
parseArgs [a, b] =
    let width = read a
        height = read b
        expectedHeight = (width * 9) `div` 16
    in if height == expectedHeight
        then (width, height, 4)
        else error $ "Invalid aspect ratio. For width " ++ show width ++ ", height must be " ++ show expectedHeight
parseArgs [a, b, c] =
    let width = read a
        height = read b
        threads = read c
        expectedHeight = (width * 9) `div` 16
    in if height == expectedHeight
        then (width, height, threads)
        else error $ "Invalid aspect ratio. For width " ++ show width ++ ", height must be " ++ show expectedHeight
parseArgs _ = error "Too many arguments. Provide at most two arguments."
