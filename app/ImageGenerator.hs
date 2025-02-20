module ImageGenerator (
    -- types
    Pixel, Row, Image,

    -- functions
    createPPM, ppmToStr, createAndWriteFile
) where

type Pixel = (Int, Int, Int)   -- (R,G,B)
type Row = [Pixel]
type Image = [Row]

createPPM :: Int -> Int -> Image
createPPM width height =
    [[pixelColor x y | x <- [0..width-1]] | y <- [0..height-1]]
    where
        scale val maxVal = round (((255.999 :: Double) * fromIntegral val) / fromIntegral (maxVal - 1))

        radius = min (width `div` 2) (height `div` 2) `div` 2

        pixelColor x y =
            if (x - cx) ** 2  + (y - cy) ** 2 <= radius ** 2
                then (255, 0, 0)
                else (255, 255, 255)

ppmToStr :: Image -> String
ppmToStr image =
        let height = length image
            width = if null image then 0 else length (head image)
            header = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n"
            pixelData = unlines $ map (unwords . map showPixel) image
        in header ++ pixelData
    where
        showPixel :: Pixel -> String
        showPixel (r, g, b) = unwords $ map show [r, g, b]

createAndWriteFile :: String -> String -> IO ()
createAndWriteFile = writeFile