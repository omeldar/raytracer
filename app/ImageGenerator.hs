module ImageGenerator (
    -- types
    Pixel, Row, Image,

    -- functions
    createPPM, ppmToStr, createAndWriteFile
) where

import qualified Vec3 as V
import qualified Ray as R
import qualified Camera as Cam
import qualified Color as Col

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
    let V.Vec3 _ y _ = V.normalize (R.direction ray)  -- Normalize direction
        t = 0.5 * (y + 1.0)
        white = V.Vec3 1.0 1.0 1.0
        blue  = V.Vec3 0.5 0.7 1.0
    in Col.lerp t white blue  -- Use lerp for smooth background

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