[🔗 Back to Chapters](/README.md#-chapters)

# Chapter 15: Diffuse Materials

Until now, our spheres just returned a color based on the surface normal. That helped us visualize hits and shape curvature, but it wasn’t realistic. Real-world materials reflect light — some of it bounces off in many directions. That’s the essence of **diffuse reflection**.

A Lambertian (diffuse) surface scatters incoming light uniformly in all directions. To simulate this, we modify our raytracing logic to _scatter_ a new ray at the hit point in a random direction.

## 🌌 Lambertian Reflection

When a ray hits a diffuse surface, we generate a random direction in the hemisphere around the hit point's normal:

```haskell
scatterDirection = normal `add` randomUnitVector
```

This biases the scatter toward the normal, resulting in realistic diffuse bounce.

We then trace this new scattered ray recursively, accumulating color. The more bounces, the more indirect lighting we capture.

## 🎲 Random Unit Vector

To get a random direction on a unit sphere:

```haskell
randomUnitVector :: IO Vec3
randomUnitVector = normalize <$> randomInUnitSphere
```

Where `randomInUnitSphere` keeps generating random vectors until one lies within the unit sphere.

This keeps all scattered directions constrained and unbiased.

## ⚡ Recursive Ray Tracing

Our `traceRay` function now becomes recursive:

```haskell
traceRay :: Ray -> Int -> IO Color
traceRay ray depth
  | depth <= 0 = return $ Vec3 0 0 0
  | otherwise = case hit scene ray tMin tMax of
      Just rec -> do
          scatterDir <- randomHemisphere (normal rec)
          let scattered = Ray (point rec) scatterDir
          c <- traceRay scattered (depth - 1)
          return $ 0.5 `scale` c
      Nothing -> return $ backgroundColor ray
```

Each hit spawns a new ray, and we trace it recursively up to some depth. We multiply the color by `0.5` to simulate energy loss on bounce.

This is the first glimpse of global illumination.

## ⚖️ Depth Limiting

To avoid infinite recursion, we add a depth limit (e.g. 10). This also helps control performance.

When depth hits 0, we return black:

```haskell
traceRay _ 0 = return $ Vec3 0 0 0
```

Otherwise, each recursive call simulates one light bounce.

## 💎 First Material

The sphere is no longer just a geometric primitive — it now has **material behavior**. Each object will carry a material reference, and that material determines how it reflects or refracts light.

For now, everything is diffuse, but this sets the stage for:

- Metal
- Glass
- Emissive surfaces
- Texture mapping

Diffuse shading is our first material. And it already gives a stunning difference in realism.

With anti-aliasing and recursion in place, scenes start to glow with soft, natural lighting.

<div align="center">
  <a href="./14_anti_aliasing.md">← All Chapters</a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
  <a href="./16_buffered_writing.md">Next Chapter →</a>
</div>
