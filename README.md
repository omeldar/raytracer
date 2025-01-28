# Raytracer

A simple raytracer built with haskell.

🔴 NOT WORKING YET!

## Usage

Use the raytracer like this:

```
cabal run raytracer res_width res_height
```

So for example (for a 1920x1080 image):

```
cabal run raytracer 1920 1080
```

## Progress

In this chapter, I will track the progress of the raytracer everytime I continue the work on it. I will explain why certain things are done and the technical/mathematical explanations (oversimplified) too.

### Creating an image

When we want to build a raytracer, the first thing we need is to create an image. A simple image format to create is the [ppm image format](https://de.wikipedia.org/wiki/Portable_Anymap).

<div style="text-align: center;">
    <img src="docs/ppm_example.png" alt="PPM Image Example" style="width: 70%" />
</div>


We can use that to create a simple picture using some code:

```haskell
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
```

This functions creates a picture based on a given width and height, creating a gradient from bottom left (blue) to top right (red), which looks like this:

<div style="text-align: center;">
    <img src="docs/first_generated_images.png" alt="First generated image in the PPM format" style="width: 70%" />
</div>

This image has been created 1920x1080. I allow the user to choose a resolution by providing arguments to the raytracer:

```
cabal run raytracer 1920 1080
```

You can see this in the code in the `Main.hs`

```haskell
import ImageGenerator as IG -- ImageGenerator is an own module in this repository

main = do
    args <- getArgs -- Retrieve command-line args
    let (width, height) = parseArgs args -- parse them
    ...
    IG.createAndWriteFile filename $ IG.ppmToStr $ IG.createPPM width height
```

### Creating the Progress Bar

When building a computationally intense application like a raytracer, we excpect longer computation times. It can take forever to render complex scenes. That's why we're adding a progress bar. It gives us a clear sense that things are moving along, helps you catch if something's stuck and shows performance stats so you can see the effect of optimizations.

#### Setting Up the Progress Bar

```haskell
newProgressBar :: Int -> IO ProgressBar
newProgressBar total = do
    progressRef     <- newIORef 0.0
    messageRef      <- newIORef ""
    startTimeRef    <- newIORef =<< getMonotonicTime
    currentStepsRef <- newIORef 0
    totalStepsRef   <- newIORef total
    return $ ProgressBar progressRef messageRef startTimeRef currentStepsRef totalStepsRef
```

We need this function to set up everything for the progress bar. It tracks:

- How much progress we've made
- The total number of steps (e.g. how many rays or pixels we're calculating)
- The time we started, so we know how long we're computing.

#### Updating Progress

```haskell
updateProgress :: ProgressBar -> Int -> IO ()
updateProgress pb steps = do
    writeIORef (currentSteps pb) steps
    total <- readIORef (totalSteps pb)
    writeIORef (progress pb) (fromIntegral steps / fromIntegral total)
    renderProgressBar pb
```

Updating the progress as the raytracer does its thing. Every time certain number of steps are completed, we update how many steps are done, we calculate how for along we are (percentage) and we refresh the progress bar on the screen.

#### Rendering the bar

```haskell
renderProgressBar :: ProgressBar -> IO ()
renderProgressBar pb = do
    ... -- Read IORefs to grab progress, message, etc.
    let filled = replicate (progressPercent * 50 `div` 100) '▓'
    let empty = replicate (50 - length filled) '░'
    putStr $ "\r[" ++ filled ++ empty ++ "] " ++ show progressPercent ++ "% ..."
    hFlush stdout
```

This is where the magic happens: it takes all the data - like how for along we are, how fast it's going and how long it's been running - and renders it as a nice progress bar. We use `▓` for the filled part and `░` for the empty part, so it looks slick.

Here is a screenshot of what the bar looks like when we start the raytracer:

![Starting Progress Bar](./docs/starting_pb.png)

Here is a screenshot of what the bar looks like when it's about 3/4 done.:

![Middle Progress Bar](./docs/pb_middle.png)

And here is what it looks when the program ended:

![End Progress Bar](./docs/pb_end.png)

### Vectors

Ongoing