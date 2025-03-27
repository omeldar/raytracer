# Raytracer

A raytracer built in Haskell.

## 🚀 Usage

Run the raytracer with:

```bash
cabal run raytracer <config-file.json>
```

The config file lets you customize:

- Resolution & sampling
- Camera & scene setup
- Lighting
- Raytracer settings (e.g. BVH, Russian Roulette)

## 📚 Chapters

This project is documented as a progressive journey. The steps are grouped in own files:

1. [Creating the Image](./docs/01_image.md) - Basic PPM output & gradients
2. [Progress Bar](./docs/02_progress_bar.md) - Rendering feedback & performance tracking
3. [Vec3 Module](./docs/03_vec3.md) - First vector operations and math foundations
4. [Drawing a Circle](./docs/04_circle.md) - First shape before proper ray-sphere logic
5. [The Ray Module](./docs/05_ray.md) - Definition of rays and ray evaluation
6. [Adding a Camera](./docs/06_camera.md) - Camera and background gradient
7. [Sphere Intersection](./docs/07_sphere.md) - Algebra and rendering a sphere
8. [Surface Normals & Perspective Projection](./docs/08_normals_and_perspective.md) - Visualization of normals and perspective
9. [Hittable Abstraction]() - Polymorphism for objects
10. [Multiple Objects]() - Scene with many spheres
11. [Fixing Perspective]() - Distortion & focal length
12. [Front vs Back Faces]() - Handling ray-side normal direction
13. [Anti-Aliasing]() - Reducing jagged edges
14. [Diffuse Materials]() - (WIP) Lambertian surfaces
15. [Buffered Writing]() - Optimizing memory usage
16. [Config Files]() - Switching from CLI args to JSON configs
17. [Optimizations]() - Russian Roulette, .OBJ import
18. [Monkey Render]() - Rendering Blender monkey with triangles
19. [BVH (Bounding Volume Hierarchy)]() - Huge speedup via spatial acceleration

## 🖼️ Render Gallery

| ![](docs/img1.png) | ![](docs/img2.png) | ![](docs/img3.png) |
| :----------------: | :----------------: | :----------------: |
|    First Render    |   Normal Mapping   | Perspective Fixed  |

| ![](docs/img4.png) | ![](docs/img5.png) | ![](docs/img6.png) |
| :----------------: | :----------------: | :----------------: |
|    Monkey Head     |  Multiple Spheres  |  BVH Optimization  |
