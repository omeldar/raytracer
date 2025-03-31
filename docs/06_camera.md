[🔗 Back to Chapters](/README.md#-chapters)

# Chapter 6: Adding a Camera

A ray tracer sends rays through pixels and computes the color seen in the direction of those rays. The general idea is simple:

1. Calculate the ray from the \"eye\" through the pixel
2. Determine which object(s) the ray hits
3. Compute the color at the closest intersection point

To start that, we need a camera in the scene. We'll define a standard camera positioned at `(0, 0, 0)` and then cast rays through a virtual viewport in front of it. Later on, we’ll experiment with moving the camera and observing the effect on the scene.

Here’s how we define a basic camera:

```haskell
defaultCamera :: Int -> Int -> Camera
defaultCamera width height =
    let aspectRatio = fromIntegral width / fromIntegral height
        viewportHeight = 2.0
        viewportWidth = viewportHeight * aspectRatio

        focalLength = 1.0

        origin = V.Vec3 0.0 0.0 0.0
        horizontal = V.Vec3 viewportWidth 0.0 0.0
        vertical = V.Vec3 0.0 viewportHeight 0.0
        lowerLeftCorner = origin `V.sub` V.scale 0.5 horizontal
                                `V.sub` V.scale 0.5 vertical
                                `V.sub` V.Vec3 0.0 0.0 focalLength
    in Camera origin lowerLeftCorner horizontal vertical
```

This defines a viewport (or screen) in front of the camera. The camera sits at the origin, and the lower-left corner of the screen is calculated by stepping back in the X and Y directions by half the screen’s width and height, and then stepping forward in Z by the focal length.

Before we add any objects like spheres, we update the background first — getting rid of the circle we rendered earlier. This means changing our traceRay function so it produces a vertical gradient instead of solid color.

```haskell
traceRay :: R.Ray -> Col.Color
traceRay ray =
    let V.Vec3 _ y _ = V.normalize (R.direction ray)  -- Normalize direction
        t = 0.5 * (y + 1.0)
        white = V.Vec3 1.0 1.0 1.0
        blue  = V.Vec3 0.5 0.7 1.0
    in Col.lerp t white blue  -- Use lerp for smooth background

```

This produces a smooth vertical blend from white (at the top of the image) to blue (at the bottom), creating the look of a basic sky gradient.

Here's the resulting image:

![Fade background](./media/06/scene_background_fade.png)

With this, we've taken a big step forward tracing rays through a scene: the camera is generating rays ocrrectly, and we're producing a gradient background based on the ray direction. The next step is to let rays interact with geometry.

<div style="display: flex; justify-content: space-between;">
  <a href="./05_ray.md">← All Chapters</a>
  <a href="./07_sphere.md">Next Chapter →</a>
</div>
