module Utils.RandomHelper where

import Control.Concurrent (ThreadId, myThreadId)
import Core.Vec3 as V
import Data.IORef
import qualified Data.Map.Strict as M
import System.IO.Unsafe (unsafePerformIO)
import System.Random.MWC (GenIO)
import qualified System.Random.MWC as MWC

{-# NOINLINE rngMapRef #-}
rngMapRef :: IORef (M.Map ThreadId GenIO)
rngMapRef = unsafePerformIO (newIORef M.empty)

getThreadRNG :: IO GenIO
getThreadRNG = do
  tid <- myThreadId
  rngMap <- readIORef rngMapRef
  case M.lookup tid rngMap of
    Just gen -> return gen
    Nothing -> do
      gen <- MWC.createSystemRandom
      atomicModifyIORef' rngMapRef (\m -> (M.insert tid gen m, ()))
      return gen

randomDouble :: IO Double
randomDouble = do
  gen <- getThreadRNG
  MWC.uniform gen

randomInUnitSphere :: IO V.Vec3
randomInUnitSphere = do
  let loop = do
        x1 <- randomDoubleRange (-1, 1)
        y1 <- randomDoubleRange (-1, 1)
        z1 <- randomDoubleRange (-1, 1)
        let p = V.Vec3 x1 y1 z1
        if V.lengthSquared p >= 1 then loop else return p
  loop

randomDoubleRange :: (Double, Double) -> IO Double
randomDoubleRange (lo, hi) = do
  d <- randomDouble
  return $ lo + (hi - lo) * d