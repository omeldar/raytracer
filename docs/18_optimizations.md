[🔗 Back to Chapters](/README.md#-chapters)

# Chapter 18: Optimizations

As our raytracer begins handling more complex scenes and higher resolutions, render times start growing rapidly. This chapter documents some of the **key optimizations** implemented to keep performance manageable without sacrificing (that much) quality.

## 🔄 Russian Roulette Termination

Recursive ray tracing can quickly lead to performance issues due to exponential growth in bounce rays. Russian Roulette helps terminate rays **probabilistically** when their contribution becomes negligible.

We introduce a small chance to terminate each recursive ray after a certain bounce depth:

```haskell
shouldTerminate <- randomDouble < probability
```

The remaining color is then scaled appropriately to account for this statistical early exit.

This reduces the number of deep recursive calls, greatly improving performance for scenes with many bounces.

## 🔢 Object Import via .OBJ

Instead of manually defining geometry, we want support for loading triangle meshes from **.obj files**.

Benefits:

- Import from Blender, Maya, or any modeling software
- Enables complex scenes with thousands of triangles
- Supports normals and indexed geometry

We parse the `.obj` into a flat list of triangles and insert them into the BVH structure.

## 🔍 Precision Cleanup

- Avoiding `t = 0.0` for intersections (prevents shadow acne)
- Normalizing vectors only when necessary
- Consolidating per-ray computations to reduce overhead

Even small math optimizations add up in a renderer that runs millions of rays per image.
