module Rendering.ImageGenerator (
    -- types
    Pixel, Row, Image,

    -- functions
    createPPM, ppmToStr, createAndWriteFile
) where

import Core.Vec3 as V

import Core.Ray as R
import Rendering.Camera as Cam
import Rendering.Color as Col

import Hittable.Objects.Sphere as S
import Hittable.HittableList as HL
import Hittable.Class as H

-- Define Pixel as Vec3 (representing RGB color)
type Pixel = V.Vec3
type Row = [Pixel]
type Image = [Row]

-- Create a PPM image with a red circle using ray tracing
createPPM :: Int -> Int -> Image
createPPM width height =
    [[pixelColor i (height - 1 - j) | i <- [0..width-1]] | j <- [0..height-1]]
    where
        camera = Cam.defaultCamera width height
        pixelColor i j =
            let ray = Cam.generateRay camera i j width height
            in traceRay ray

traceRay :: R.Ray -> Col.Color
traceRay ray =
    let spheres = HittableList [ 
            S.Sphere (V.Vec3 0 0 (-1)) 0.5,  -- Sphere 1
            S.Sphere (V.Vec3 1.5 0 (-1)) 0.5,  -- Sphere 2
            S.Sphere (V.Vec3 (-1.5) 0 (-1)) 0.5 -- Sphere 3
            ]
        tMin = 0.0
        tMax = 100
    in case H.hit spheres ray tMin tMax of
        Just hitRec ->
            let normal = V.normalize (H.normal hitRec)  -- Normalize normal to prevent artifacts
            in 0.5 `V.scale` (normal `V.add` V.Vec3 1 1 1)

        Nothing ->  -- Background gradient
            Col.lerp (0.5 * (V.y (V.normalize (R.direction ray)) + 1.0))
                    (V.Vec3 1 1 1)
                    (V.Vec3 0.5 0.7 1.0)


-- Convert an Image to a PPM string
ppmToStr :: Image -> String
ppmToStr image =
    let height = length image
        width = if null image then 0 else length (head image)
        header = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n"
        pixelData = unlines $ map (unwords . map showPixel) image
    in header ++ pixelData
    where
        -- Convert a Vec3 (Pixel) to a PPM format string
        showPixel :: Pixel -> String
        showPixel (V.Vec3 r g b) =
            unwords $ map (show . (truncate :: Double -> Int) . (* 255.999)) [r, g, b]

-- Write the PPM image to a file
createAndWriteFile :: String -> String -> IO ()
createAndWriteFile = writeFile