module Main where

import Config
  ( Config (..),
    ImageSettings (..),
    loadConfig,
  )
import Data.Time (defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime)
import Rendering.ImageGenerator as IG (createPPM)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  putStrLn "-----------------------------------------"
  putStrLn "[Raytracer] Starting Haskell Raytracer..."
  args <- getArgs -- Retrieve command-line args
  case args of
    [configFile] -> do
      putStrLn $ "[Scene Setup] Loading config file: " ++ configFile
      maybeConfig <- loadConfig configFile
      case maybeConfig of
        Just config -> runRaytracer config
        Nothing -> do
          putStrLn "Error: Failed to load config file."
          exitFailure
    _ -> do
      putStrLn "Error: Invalid arguments. Usage: raytracer <config-file.json>"

runRaytracer :: Config -> IO ()
runRaytracer config = do
  let imgSettings = image config
      imgWidth = width imgSettings
      imgHeight = height imgSettings
      imgSamples = samplesPerPixel imgSettings
      imgAA = antialiasing imgSettings
      imgExposure = exposure imgSettings
      imgGamma = gamma imgSettings

  putStrLn $ "[Configuration] Resolution: " ++ show imgWidth ++ "x" ++ show imgHeight

  if imgAA
    then putStrLn $ "[Configuration] AA-" ++ show imgSamples
    else putStrLn "[Configuration] No AA"

  putStrLn $ "[Configuration] Exposure: " ++ show imgExposure ++ ", Gamma: " ++ show imgGamma

  currentTime <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" currentTime
  let filename = "out/" ++ timestamp ++ ".ppm"

  startFileCreation <- getCurrentTime
  createPPM config filename
  endFileCreation <- getCurrentTime

  let timeToCreate = diffUTCTime endFileCreation startFileCreation
  putStrLn ""
  putStrLn $ "Time taken to create and write file: " ++ show timeToCreate
  putStrLn $ "File written to: " ++ filename
