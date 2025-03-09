module Utils.ProgressBar
  ( ProgressBar,
    newProgressBar,
    updateProgress,
    updateMessage,
    renderProgressBar,
  )
where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.Clock (getMonotonicTime)
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), setSGR, setTitle)
import System.IO (hFlush, stdout)

progressBarWidth :: Int
progressBarWidth = 50

data ProgressBar = ProgressBar
  { progress :: IORef Float,
    message :: IORef String,
    startTime :: IORef Double,
    currentSteps :: IORef Int,
    totalSteps :: IORef Int
  }

newProgressBar :: Int -> IO ProgressBar
newProgressBar total = do
  progressRef <- newIORef 0.0
  messageRef <- newIORef ""
  startTimeRef <- newIORef =<< getMonotonicTime
  currentStepsRef <- newIORef 0
  totalStepsRef <- newIORef total
  return $ ProgressBar progressRef messageRef startTimeRef currentStepsRef totalStepsRef

updateProgress :: ProgressBar -> Int -> IO ()
updateProgress pb steps = do
  writeIORef (currentSteps pb) steps
  total <- readIORef (totalSteps pb)
  writeIORef (progress pb) (fromIntegral steps / fromIntegral total)
  renderProgressBar pb

updateMessage :: ProgressBar -> String -> IO ()
updateMessage pb newMessage = do
  writeIORef (message pb) newMessage
  renderProgressBar pb

renderProgressBar :: ProgressBar -> IO ()
renderProgressBar pb = do
  currentProgressVal <- readIORef (progress pb)
  currentMessageVal <- readIORef (message pb)
  currentStepsVal <- readIORef (currentSteps pb)
  totalStepsVal <- readIORef (totalSteps pb)
  startTimeVal <- readIORef (startTime pb)
  currentTime <- getMonotonicTime

  let elapsedTime = currentTime - startTimeVal
      stepsPerSecond = fromIntegral currentStepsVal / elapsedTime
      filledWidth = round (currentProgressVal * fromIntegral progressBarWidth)
      filled = replicate filledWidth '▓'
      empty = replicate (progressBarWidth - filledWidth) '░'

  setTitle "Raytracer"
  setSGR [SetColor Foreground Vivid Blue]
  putStr $ "\r[" ++ filled ++ empty ++ "] "

  let progressPercent = (floor (currentProgressVal * 10000) :: Int) `div` 100
      stepsPerSec = (round (stepsPerSecond * 100) :: Int) `div` 100

  setSGR [SetColor Foreground Vivid Green]
  putStr $ show (progressPercent :: Int) ++ "% "

  setSGR [SetColor Foreground Vivid Yellow]
  putStr $ show (currentStepsVal :: Int) ++ "/" ++ show (totalStepsVal :: Int) ++ " "

  setSGR [SetColor Foreground Vivid Red]
  putStr $ show (stepsPerSec :: Int) ++ "/s "

  setSGR [SetColor Foreground Vivid Cyan]
  putStr $ formatTime elapsedTime ++ " "

  setSGR [SetColor Foreground Vivid Black]
  putStr currentMessageVal
  setSGR [Reset]

  hFlush stdout

formatTime :: Double -> String
formatTime seconds =
  let hours :: Int
      hours = floor (seconds / 3600)
      minutes :: Int
      minutes = floor ((seconds - fromIntegral hours * 3600) / 60)
      secs :: Int
      secs = floor (seconds - fromIntegral hours * 3600 - fromIntegral minutes * 60)
   in show hours ++ ":" ++ show minutes ++ ":" ++ show secs

withProgressBar :: Int -> (ProgressBar -> IO a) -> IO a
withProgressBar total action = do
  pb <- newProgressBar total
  action pb