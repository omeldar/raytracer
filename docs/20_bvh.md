[🔗 Back to Chapters](/README.md#-chapters)

# Chapter 20: BVH - Bounding Volume Hierarchies

As our scenes grew from a few spheres to thousands of triangles (via .obj imports), ray-object intersection checks became the major performance bottleneck. Without any spatial partitioning, every ray had to test against every object.

The solution: a **Bounding Volume Hierarchy (BVH)** — a binary tree that groups nearby objects into axis-aligned bounding boxes.


## 📦 What is a BVH?

A BVH recursively divides a set of objects into two subsets and wraps each subset in a bounding box. These boxes are then tested for ray intersections:

- If a ray misses a box, all contained objects are skipped.
- If it hits, we recurse down into its children.

This greatly reduces the number of intersection checks.


## 🛠️ Implementation Details

- We use **axis-aligned bounding boxes** (AABB) for performance.
- During construction, objects are **sorted by their center** along the longest axis.
- The resulting binary tree is built top-down, depth-limited if configured.
- Boxes are tested with a fast slab-intersection algorithm.

Each node either contains:
- Two child BVH nodes (interior node)
- A single hittable object (leaf node)


## 🔢 Configuration Example

```json
"bvh": {
  "enabled": true,
  "maxDepth": 32,
  "childSorting": true,
  "preferCloserChild": true
}
```

- `maxDepth`: limits how deep the hierarchy can grow
- `childSorting`: controls sorting objects before splitting
- `preferCloserChild`: prioritize intersecting the child closer to the ray origin first


## 🧪 Observed Performance

We tested BVH on the monkey model (~1000 triangles).

| BVH Config | Render Time |
|------------|-------------|
| No BVH     | 42m 04s     |
| BVH Depth 10 | 1m 44s     |
| BVH Depth 32 | 1m 36s     |

With BVH enabled, only a fraction of the triangle intersections are tested.


## 🪛 Current TODO / Bugs

- Without anti-aliasing, a faint **blank vertical line** appears in the center of some renders — possibly a box test edge case.
- Improve BVH node balancing heuristics
- Visualize bounding boxes for debugging


## 🧠 Why This Matters

BVH is the cornerstone of modern raytracers. Without it, render times become unusable as scenes grow.

We now render complex triangle meshes at interactive speeds. Every model, every light bounce, every feature added going forward — benefits from the performance boost BVH brings.

