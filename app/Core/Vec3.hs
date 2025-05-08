module Core.Vec3
  ( Vec3 (..), -- Export types and constructors
    x, -- Extractor for x
    y, -- Extractor for y
    z, -- Extractor for z
    add, -- Vector addition
    sub, -- Vector subtraction
    dot, -- Dot product
    cross, -- Cross product
    scale, -- Scalar multiplication
    vLength, -- Vector length (magnitude)
    normalize, -- Normalize vector
    negateV, -- Negate a vector
    mul, -- Component-wise multiplication
    divV, -- Component-wise division
    nearZero, -- Check if vector is near zero
    reflect, -- Reflect a vector
    refract, -- Refract a vector
    lengthSquared, -- Dot product of the same vector squaring its length
    randomInUnitSphere, -- Generate a random vector in unit sphere
    randomInUnitDisk, -- Generate a random vector in unit disk
    toList, -- Convert Vec3 to list
    maxVec3, -- Component-wise maximum
    minVec3, -- Component-wise minimum
  )
where

import Data.Aeson (FromJSON, Value (..), parseJSON, withArray)
import qualified Data.Foldable as F
import Utils.Constants (randomDoubleInRange)

data Vec3 = Vec3 {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double deriving (Show, Eq)

-- Implement FromJSON instance for Vec3
instance FromJSON Vec3 where
  parseJSON = withArray "Vec3" $ \arr ->
    let values = take 3 (map extractDouble (F.toList arr))
     in case values of
          [Just xJ, Just yJ, Just zJ] -> return (Vec3 xJ yJ zJ)
          _ -> fail "Failed parsing Vec3 from JSON config. Vec3 must contain exactly three valid numbers."
    where
      extractDouble :: Value -> Maybe Double
      extractDouble (Number n) = Just (realToFrac n)
      extractDouble _ = Nothing

toList :: Vec3 -> [Double]
toList (Vec3 xV yV zV) = [xV, yV, zV]

{-# INLINE add #-}
add :: Vec3 -> Vec3 -> Vec3
add (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)

{-# INLINE sub #-}
sub :: Vec3 -> Vec3 -> Vec3
sub (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)

{-# INLINE dot #-}
dot :: Vec3 -> Vec3 -> Double
dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

{-# INLINE cross #-}
cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

{-# INLINE scale #-}
scale :: Double -> Vec3 -> Vec3
scale s (Vec3 xV yV zV) = Vec3 (s * xV) (s * yV) (s * zV)

{-# INLINE vLength #-}
vLength :: Vec3 -> Double
vLength v = sqrt (dot v v)

{-# INLINE normalize #-}
normalize :: Vec3 -> Vec3
normalize v = scale (1 / vLength v) v

{-# INLINE negateV #-}
negateV :: Vec3 -> Vec3
negateV (Vec3 xV yV zV) = Vec3 (-xV) (-yV) (-zV)

{-# INLINE mul #-}
mul :: Vec3 -> Vec3 -> Vec3
mul (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 * x2) (y1 * y2) (z1 * z2)

{-# INLINE divV #-}
divV :: Vec3 -> Vec3 -> Vec3
divV (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 / x2) (y1 / y2) (z1 / z2)

{-# INLINE nearZero #-}
nearZero :: Vec3 -> Bool
nearZero (Vec3 xV yV zV) = abs xV < 1e-8 && abs yV < 1e-8 && abs zV < 1e-8

{-# INLINE reflect #-}
reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = sub v (scale (2 * dot v n) n)

{-# INLINE refract #-}
refract :: Vec3 -> Vec3 -> Double -> Vec3
refract v n eta =
  let cosTheta = negate (dot v n)
      rOutPerp = scale eta (v `add` scale cosTheta n)
      rOutParallel = scale (-sqrt (abs (1.0 - dot rOutPerp rOutPerp))) n
   in rOutPerp `add` rOutParallel

{-# INLINE lengthSquared #-}
lengthSquared :: Vec3 -> Double
lengthSquared v = dot v v

{-# INLINE randomInUnitSphere #-}
randomInUnitSphere :: IO Vec3
randomInUnitSphere = do
  randx <- randomDoubleInRange (-1) 1
  randy <- randomDoubleInRange (-1) 1
  randz <- randomDoubleInRange (-1) 1
  let p = Vec3 randx randy randz
  if lengthSquared p < 1
    then return p
    else randomInUnitSphere

-- Generate random points in a unit disk (for defocus blur)
{-# INLINE randomInUnitDisk #-}
randomInUnitDisk :: IO Vec3
randomInUnitDisk = do
  randx <- randomDoubleInRange (-1.0) 1.0
  randy <- randomDoubleInRange (-1.0) 1.0
  let p = Vec3 randx randy 0
  if lengthSquared p < 1.0
    then return p
    else randomInUnitDisk

{-# INLINE maxVec3 #-}
maxVec3 :: Vec3 -> Vec3 -> Vec3
maxVec3 (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (max x1 x2) (max y1 y2) (max z1 z2)

{-# INLINE minVec3 #-}
minVec3 :: Vec3 -> Vec3 -> Vec3
minVec3 (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (min x1 x2) (min y1 y2) (min z1 z2)

{-# INLINE x #-}
x :: Vec3 -> Double
x (Vec3 xV _ _) = xV

{-# INLINE y #-}
y :: Vec3 -> Double
y (Vec3 _ yV _) = yV

{-# INLINE z #-}
z :: Vec3 -> Double
z (Vec3 _ _ zV) = zV