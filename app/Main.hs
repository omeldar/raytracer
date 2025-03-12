module Main where

import Data.Time (defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime)
import Rendering.ImageGenerator as IG
  ( createPPM,
  )
import System.Environment (getArgs)
import Utils.ProgressBar
  ( ProgressBar,
    updateMessage,
    updateProgress,
  )

main :: IO ()
main = do
  args <- getArgs -- Retrieve command-line args
  let (width, height, samplesPerPixel, aa) = parseArgs args
  putStrLn "-----------------------------------"
  if aa
    then putStrLn $ "Width: " ++ show width ++ ", Height: " ++ show height ++ ", AASize: " ++ show samplesPerPixel
    else putStrLn $ "Width: " ++ show width ++ ", Height: " ++ show height ++ ", No AA:"

  currentTime <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" currentTime
  let filename = "out/" ++ timestamp ++ ".ppm"

  startFileCreation <- getCurrentTime
  createPPM width height samplesPerPixel aa filename
  endFileCreation <- getCurrentTime

  let timeToCreate = diffUTCTime endFileCreation startFileCreation
  putStrLn ""
  putStrLn $ "Time taken for create and write file: " ++ show timeToCreate
  putStrLn $ "File written to: " ++ filename

-- Parse the command-line arguments
parseArgs :: [String] -> (Int, Int, Int, Bool)
parseArgs [] = (640, 360, 10, True) -- Default: 640x360 with AA
parseArgs ["--no-aa"] = (640, 360, 1, False) -- Disable AA with default resolution
parseArgs [a] =
  let width = read a
      height = (width * 9) `div` 16 -- Maintain 16:9 aspect ratio
   in (width, height, 10, True)
parseArgs [a, b]
  | a == "--no-aa" = (read b, (read b * 9) `div` 16, 1, False) -- --no-aa width
  | otherwise =
      let width = read a
          height = read b
          expectedHeight = (width * 9) `div` 16
       in if height == expectedHeight
            then (width, height, 10, True)
            else error $ "Invalid aspect ratio. For width " ++ show width ++ ", height must be " ++ show expectedHeight
parseArgs [a, b, c]
  | a == "--no-aa" = (read b, read c, 1, False) -- --no-aa width height
  | otherwise = (read a, read b, read c, True) -- Normal width height samples
parseArgs _ = error "Invalid arguments. Usage: <width> <height> <samples> or --no-aa <width> <height>"