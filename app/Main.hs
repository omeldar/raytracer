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
  args <- getArgs -- Retrieve command-line args
  case args of
    [configFile] -> do
      putStrLn $ "Loading config file: " ++ configFile
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

  putStrLn "---------------------------------"
  putStrLn "Raytracer"
  if imgAA
    then putStrLn $ "Resolution: " ++ show imgWidth ++ "x" ++ show imgHeight ++ ", AASize: " ++ show imgSamples
    else putStrLn $ "Resolution: " ++ show imgWidth ++ "x" ++ show imgHeight ++ ", AA disabled!"

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
