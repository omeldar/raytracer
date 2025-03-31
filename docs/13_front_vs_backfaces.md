[🔗 Back to Chapters](/README.md#-chapters)

# Chapter 13: Front Faces vs. Back Faces

With surface normals now in play, it’s time to address a subtle but important design choice: **which way should a surface normal point?**

This affects:

- Lighting calculations
- Material behavior (like refraction)
- Shading and rendering correctness

## 🪗 Inside vs Outside

Let’s take the example of a sphere. For any point of intersection:

- If the ray hits **from outside**, the normal points **outward** from the surface.
- If the ray hits **from inside**, the normal points **inward** toward the center.

This means:

- Sometimes the normal faces **against** the ray (what we usually want)
- Other times, it faces **with** the ray (when ray starts inside the object)

## 🚧 Use Case: Differentiating Surface Sides

This is important for:

- **Two-sided materials** (e.g., paper with different text on each side)
- **Refraction/transparency** (e.g., glass: need to know when we enter vs exit)
- **Backface culling** in rasterization (which doesn’t apply to raytracing but is conceptually related)

![Front Faces vs Back Faces](./media/13/frontvsbackfaces.jpg)

_[Source: Ray Tracing in One Weekend](https://raytracing.github.io/books/RayTracingInOneWeekend.html#surfacenormalsandmultipleobjects/frontfacesversusbackfaces)_

---

## 🧹 Our Current Approach

Currently, we calculate the normal as:

```haskell
let normal = V.normalize (R.at ray t `V.sub` center)
```

This **always points outward** from the sphere. That’s fine for opaque objects from outside. But once we add transmissive materials or two-sided lighting, we’ll need to correct the normal to **always point against the ray**.

The fix (to be implemented later):

```haskell
let frontFace = V.dot ray.direction outwardNormal < 0
    normal = if frontFace then outwardNormal else V.negate outwardNormal
```

This ensures consistent normal direction relative to the ray.

<div style="display: flex; justify-content: space-between;">
  <a href="./12_fixing_perspective.md">← All Chapters</a>
  <a href="./14_anti_aliasing.md">Next Chapter →</a>
</div>
