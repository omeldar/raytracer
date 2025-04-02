[🔗 Back to Chapters](/README.md#-chapters)

# Chapter 11: Multiple Objects

Now that we have a `Hittable` abstraction in place, we can easily extend our scenes to contain multiple objects. This chapter demonstrates how we build a small scene and trace rays through it using the `HittableList` structure.

## 🏛️ Building a Scene

Let’s define a few spheres and group them in a `HittableList`:

```haskell
traceRay :: R.Ray -> Col.Color
traceRay ray =
    let scene = HittableList
            [ S.Sphere (V.Vec3 0 0 (-1)) 0.5
            , S.Sphere (V.Vec3 1.5 0 (-1)) 0.5
            , S.Sphere (V.Vec3 (-1.5) 0 (-1)) 0.5
            ]
        tMin = 0.001  -- avoid hits extremely close to origin (e.g. shadow acne)
        tMax = 1000.0
    in case H.hit scene ray tMin tMax of
        Just hitRec ->
            let normal = V.normalize (H.normal hitRec)
            in 0.5 `V.scale` (normal `V.add` V.Vec3 1 1 1)
        Nothing ->
            Col.lerp (0.5 * (V.y (V.normalize (R.direction ray)) + 1.0))
                     (V.Vec3 1 1 1)
                     (V.Vec3 0.5 0.7 1.0)
```

- Each sphere has the same radius but is placed at different horizontal positions.
- Rays are traced through the entire list.
- The result is the normal-based shading we established earlier.

## 📸 Render Output

With this logic, the rendered scene should now contain **three spheres** side by side, all shaded based on their surface normals:

![Multiple Spheres](./media/11/multiple_spheres.png)

The shading is a visualization of the surface normal at each hit point.

## 🔄 Future Extensions

Because of the abstraction we've created, we can easily:

- Add different shapes like **planes** or **triangles**
- Compose complex scenes with hundreds or thousands of primitives
- Swap out a `HittableList` for a BVH (Bounding Volume Hierarchy) without changing the rendering code

This is the real power of generalizing ray-object intersection logic.

We now have our first real scene.

<div align="center">
  <a href="./10_hittable_abstraction.md">← Previous Chapter</a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
  <a href="./12_fixing_perspective.md">Next Chapter →</a>
</div>
