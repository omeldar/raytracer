module Utils.ProgressBar (
    ProgressBar,
    newProgressBar,
    updateProgress,
    renderProgressBar
) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO (hFlush, stdout)
import System.Console.ANSI (setSGR, SGR(..), ConsoleLayer(..), Color(..), ColorIntensity(..), setTitle)
import GHC.Clock (getMonotonicTime)

progressBarWidth :: Int
progressBarWidth = 50

data ProgressBar = ProgressBar 
    { progress      :: IORef Float
    , totalSteps    :: IORef Int  
    , startTime     :: IORef Double
    }

newProgressBar :: Int -> IO ProgressBar
newProgressBar total = do
    progressRef <- newIORef 0.0
    totalStepsRef <- newIORef total
    startTimeRef <- newIORef =<< getMonotonicTime
    return $ ProgressBar progressRef totalStepsRef startTimeRef 

updateProgress :: ProgressBar -> Int -> IO ()
updateProgress pb steps = do
    total <- readIORef (totalSteps pb)
    writeIORef (progress pb) (fromIntegral steps / fromIntegral total)
    renderProgressBar pb

renderProgressBar :: ProgressBar -> IO ()
renderProgressBar pb = do
    currentProgressVal <- readIORef (progress pb)
    let filledWidth = round (currentProgressVal * fromIntegral progressBarWidth)  
        filled = replicate filledWidth '▓'  
        empty = replicate (progressBarWidth - filledWidth) '░'

    setTitle "Raytracer"
    setSGR [SetColor Foreground Vivid Green]
    putStr $ "\r[" ++ filled ++ empty ++ "] " ++ show (round (currentProgressVal * 100)) ++ "%"

    setSGR [Reset]
    hFlush stdout