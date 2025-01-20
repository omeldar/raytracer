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
    [[(r, g, b) | x <- [0..width-1],
                  let r = scale x width,
                  let g = 0,
                  let b = scale y height]
                | y <- [0..height-1]]
    where
        scale val maxVal = round (((255.999 :: Double) * fromIntegral val) / fromIntegral (maxVal - 1))

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