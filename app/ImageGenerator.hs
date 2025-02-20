module ImageGenerator (
    -- types
    Pixel, Row, Image,

    -- functions
    createPPM, ppmToStr, createAndWriteFile
) where

import Vec3 -- Import your Vec3 module

-- Define Pixel as Vec3 (representing RGB color)
type Pixel = Vec3
type Row = [Pixel]
type Image = [Row]

-- Create a PPM image with a gradient or other pattern
createPPM :: Int -> Int -> Image
createPPM width height =
    [[pixelColor i j | i <- [0..width-1]] | j <- [0..height-1]]
    where
        cx = width `div` 2
        cy = height `div` 2
        radius = min cx cy `div` 2
        pixelColor i j =
            if (i - cx) ^ (2 :: Integer) + (j - cy) ^ (2 :: Integer) <= radius ^ (2 :: Integer)
                then Vec3 1.0 0.0 0.0 -- Red
                else Vec3 1.0 1.0 1.0 -- White

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
        showPixel (Vec3 r g b) =
            unwords $ map (show . (truncate :: Double -> Int) . (* 255.999)) [r, g, b]

-- Write the PPM image to a file
createAndWriteFile :: String -> String -> IO ()
createAndWriteFile = writeFile