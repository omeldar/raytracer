[🔗 Back to Chapters](/README.md#-chapters)

# Chapter 5: The Ray Module

To render 3D scenes, we need rays. Rays are the core abstraction in ray tracing: they originate from the camera and travel through the scene, interacting with objects along the way.

The `Ray` module defines a simple `Ray` type and provides utility functions to work with it. A ray consists of two things: an origin point (where the ray starts) and a direction vector (where the ray points). Both of these are represented using our `Vec3` type:

```haskell
data Ray = Ray
  { origin :: Vec3
  , direction :: Vec3
  } deriving (Show)
```

Once we have a ray, we often want to compute where it is at a given time `t`. That means starting at the origin and moving `t` units along the direction. This is handled by the `at` function:

```haskell
at :: Ray -> Double -> Vec3
at (Ray orig dir) t = orig `add` scale t dir
```

This gives us a point along the ray at parameter `t`. It’s simple, but fundamental. For example, later when we cast rays into the scene, we’ll want to know where those rays intersect objects. That intersection point will be computed using this exact function.

Thanks to Haskell’s flexibility, we can even use at as an infix function for better readability:

```haskell
-- Define a ray
let rayOrigin = Vec3 1.0 2.0 3.0
let rayDirection = Vec3 0.0 1.0 0.0
let ray = Ray rayOrigin rayDirection

-- Compute the point at t = 2.0 using infix notation
let pointAtT = ray `at` 2.0
```

This module is intentionally minimal right now. Its main job is to give us rays we can aim into the scene, evaluate, and check for intersections. It will grow over time — as we add reflection, refraction, time-varying rays, and more — but for now, it gives us the foundation we need to start tracing.

We now have:

- A way to represent a ray
- A function to evaluate it at any `t`
- Vector math to support manipulating its components

With this, we're ready to actually start shooting rays from our camera into the world.

<div align="center">
  <a href="./04_circle.md">← All Chapters</a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
  <a href="./06_camera.md">Next Chapter →</a>
</div>
