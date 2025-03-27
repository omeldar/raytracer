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
9. [Ray-Sphere Simplification & Profiling](./docs/09_simplifications_and_profiling.md) - Simplyfing the ray-sphere intersection and profiling the Raytracer
10. [Hittable Abstraction](./docs/10_hittable_abstraction.md) - Polymorphism for objects
11. [Multiple Objects](./docs/11_multiple_objects.md) - Scene with many spheres
12. [Fixing Perspective](./docs/12_fixing_perspective.md) - Distortion & focal length
13. [Front vs Back Faces]() - Handling ray-side normal direction
14. [Anti-Aliasing]() - Reducing jagged edges
15. [Diffuse Materials]() - (WIP) Lambertian surfaces
16. [Buffered Writing]() - Optimizing memory usage
17. [Config Files]() - Switching from CLI args to JSON configs
18. [Optimizations]() - Russian Roulette, .OBJ import
19. [Monkey Render]() - Rendering Blender monkey with triangles
20. [BVH (Bounding Volume Hierarchy)]() - Huge speedup via spatial acceleration

## 🖼️ Render Gallery

| ![](./docs/media/other/complicated_scene.png) |
| :----------------: |
| Shadowing in a 988 triangles scene  |

| ![](./docs/media/) |
| :----------------: |
| Shadowing in a 63432 triangles scene |
