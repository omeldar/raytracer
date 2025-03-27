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

With BVH enabled, only a fraction of the triangle intersections are tested. Instead of $O(n)$ we get a complexity of $O(log n)$.

| Triangle Count | Checks (no BVH) | Checks (BVH) | Decrease (%) |
|------------|-------------|------------|-------------|
| 10 | 10 | 3 | 70% |
| 100 | 100 | 6 | 94% |
| 1'000 | 1'000 | 9 | 99.1% |
| 10'000 | 10'000 | 13 | 99.87% |
| 100'000 | 100'000 | 16 | 99.984% |
| 1'000'000 | 1'000'000 | 19 | 99.9981% |
| 10'000'000 | 10'000'000 | 23 | 99.999770% |
| 100'000'000 | 100'000'000 | 26 | 99.999974% |
| 1'000'000'000 | 1'000'000'000 | 29 | 99.999997% |