[🔗 Back to Chapters](/README.md#-chapters)

# Chapter 19: Monkey Render

After adding .obj file support and implementing acceleration structures, it was time to try rendering a proper 3D model. The go-to test subject: **Blender's Suzanne monkey**.

Suzanne is a classic benchmark mesh — simple yet detailed enough to reveal raytracing bugs, performance issues, and shading artifacts.

## 📖 Importing the Model

The monkey model was exported from Blender as a `.obj` file containing roughly **1000 triangles**.

So we parse the file using our custom OBJ loader and convert the triangles into hittable objects:

```haskell
loadObj :: FilePath -> Vec3 -> IO [Triangle]
```

The triangles are optionally offset (e.g. moving the model up or back in the scene), then placed into a `BVHNode`.

## 🚀 BVH Acceleration

To handle the large number of triangles efficiently, we insert them into a Bounding Volume Hierarchy (BVH). This reduces ray-triangle intersection checks from O(n) to O(log n).

The monkey render was tested with various BVH settings:

- BVH depth 10
- BVH depth 32
- Heuristic for sorting along longest axis
- Child ordering based on hit distance

The BVH depth is configurable within the configuration. Heuristic sorting along longest axis and child ordering are implemented without option to disable it - because why would we?

## 🎨 Final Render

The monkey was rendered at:

- Resolution: 1920x1080
- Samples per pixel: 50
- Approx. 1000 triangles

![Rendering the Blender Monkey](./media/19/monkey.png)

You can learn more about how long that took with and without BVH in the next chapter: [Chapter 20 - BVH](./20_bvh.md)

<div align="center">
  <a href="./18_optimizations.md">← All Chapters</a>
  <a href="./20_bvh.md">Next Chapter →</a>
</div>
