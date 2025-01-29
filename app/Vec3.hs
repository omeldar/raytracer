module Vec3
    ( Vec3 (..)         -- Export types and constructors
    , x                 -- Extractor for x    
    , y                 -- Extractor for y
    , z                 -- Extractor for z
    , add               -- Vector addition
    , sub               -- Vector subtraction
    , dot               -- Dot product
    , cross             -- Cross product
    , scale             -- Scalar multiplication
    , vLength           -- Vector length (magnitude)
    , normalize         -- Normalize vector
    , negateV           -- Negate a vector
    , mul               -- Component-wise multiplication
    , divV              -- Component-wise division
    , nearZero          -- Check if vector is near zero
    , reflect           -- Reflect a vector
    , refract           -- Refract a vector
    ) where

data Vec3 = Vec3 Double Double Double deriving (Show, Eq)

add :: Vec3 -> Vec3 -> Vec3
add (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)

sub :: Vec3 -> Vec3 -> Vec3
sub (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)

dot :: Vec3 -> Vec3 -> Double
dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

scale :: Double -> Vec3 -> Vec3
scale s (Vec3 x y z) = Vec3 (s * x) (s * y) (s * z)

vLength :: Vec3 -> Double
vLength v = sqrt (dot v v)

normalize :: Vec3 -> Vec3
normalize v = scale (1 / vLength v) v

negateV :: Vec3 -> Vec3
negateV (Vec3 x y z) = Vec3 (-x) (-y) (-z)

mul :: Vec3 -> Vec3 -> Vec3
mul (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 * x2) (y1 * y2) (z1 * z2)

divV :: Vec3 -> Vec3 -> Vec3
divV (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 / x2) (y1 / y2) (z1 / z2)

nearZero :: Vec3 -> Bool
nearZero (Vec3 x y z) = abs x < 1e-8 && abs y < 1e-8 && abs z < 1e-8

reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = sub v (scale (2 * dot v n) n)

refract :: Vec3 -> Vec3 -> Double -> Vec3
refract uv n etaiOverEtat =
    let cosTheta = min (dot (negateV uv) n) 1.0
        rOutPerp = scale etaiOverEtat (add uv (scale cosTheta n))
        rOutParallel = scale (-sqrt (abs (1.0 - vLength rOutPerp ** 2))) n
    in add rOutPerp rOutParallel

x :: Vec3 -> Double
x (Vec3 x _ _) = x

y :: Vec3 -> Double
y (Vec3 _ y _) = y

z :: Vec3 -> Double
z (Vec3 _ _ z) = z