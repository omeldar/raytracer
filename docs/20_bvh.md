[🔗 Back to Chapters](/README.md#-chapters)

# Chapter 20: BVH - Bounding Volume Hierarchies

As our scenes keep growing from a few spheres to thousands of triangles (via .obj imports), ray-object intersection checks become the major performance bottleneck. Without any spatial partitioning, every ray has to test against every object.

The solution: a **Bounding Volume Hierarchy (BVH)** — a binary tree that groups nearby objects into axis-aligned bounding boxes.

## 📦 What is a BVH?

A BVH recursively divides a set of objects into two subsets and wraps each subset in a bounding box. These boxes are then tested for ray intersections:

- If a ray misses a box, all contained objects are skipped.
- If it hits, we recurse down into its children.

This greatly reduces the number of intersection checks.

## 🛠️ Implementation Details

- We use **axis-aligned bounding boxes** (AABB) for performance.
- During construction, objects are **sorted by their center** along the longest axis.
- The resulting binary tree is built top-down, depth-limited as configured.
- Boxes are tested with a fast slab-intersection algorithm.

Each node either contains:

- Two child BVH nodes (interior node)
- A single hittable object (leaf node)

## 🧪 Observed Performance

Now lets test BVH on the monkey model (~1000 triangles).

| BVH Config   | Render Time |
| ------------ | ----------- |
| No BVH       | 42m 04s     |
| BVH Depth 10 | 1m 44s      |
| BVH Depth 32 | 1m 36s      |

With BVH enabled, only a fraction of the triangle intersections are tested. Instead of $O(n)$ we get a complexity of $O(log n)$. This means, that for 1'000 triangles, we need 1'000 checks without BVH and only 9 checks with BVH-10.

| Triangle Count | Checks (no BVH) | Checks (BVH-10) | Decrease (%) |
| -------------- | --------------- | --------------- | ------------ |
| 10             | 10              | 3               | 70%          |
| 100            | 100             | 6               | 94%          |
| 1'000          | 1'000           | 9               | 99.1%        |
| 10'000         | 10'000          | 13              | 99.87%       |
| 100'000        | 100'000         | 16              | 99.984%      |
| 1'000'000      | 1'000'000       | 19              | 99.9981%     |
| 10'000'000     | 10'000'000      | 23              | 99.999770%   |
| 100'000'000    | 100'000'000     | 26              | 99.999974%   |
| 1'000'000'000  | 1'000'000'000   | 29              | 99.999997%   |

<div style="display: flex; justify-content: space-between;">
  <a href="./19_monkey_render.md">← All Chapters</a>
  <a href="#">Next Chapter (WIP) →</a>
</div>
