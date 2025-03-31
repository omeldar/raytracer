[🔗 Back to Chapters](/README.md#-chapters)

# Chapter 14: Anti-Aliasing

So far, every ray we traced per pixel came from a single, exact coordinate at the center of that pixel. This results in very sharp but also very **jagged edges** — especially visible when rendering spheres or diagonals.

To improve this, we now implement **anti-aliasing** by **sampling multiple rays per pixel**, each slightly jittered within the pixel area, and averaging their colors.

This makes the rendered image smoother, less noisy, and more visually realistic.

## 🔢 The Core Idea

Instead of one ray per pixel, we shoot _n_ rays per pixel with random offsets:

```haskell
let u = (fromIntegral i + randomDouble) / fromIntegral (width - 1)
    v = (fromIntegral j + randomDouble) / fromIntegral (height - 1)
```

This spreads the ray samples across the pixel. The more samples per pixel (spp), the smoother the final image.

Each color is accumulated, then averaged:

```haskell
let scale = 1.0 / fromIntegral samplesPerPixel
    finalColor = scale `V.scale` accumulatedColor
```

Gamma correction is also applied before writing the final value.

## 🌍 Configuration

Anti-aliasing is now controlled via the config file:

```json
"image": {
  "width": 1920,
  "height": 1080,
  "samplesPerPixel": 50,
  "antialiasing": true
}
```

This allows tuning render quality vs speed.

## 📈 Visual Results

Without AA:

- Jagged contours
- Harsh transitions
- Pixelation especially around curved geometry

With AA:

- Smooth edges
- Subtle gradients
- Sharper overall realism

Even 10-20 samples per pixel makes a big difference. At 50–500, results become photorealistic depending on lighting and material complexity.

## 🪤 Performance Impact

Anti-aliasing increases render time linearly with the number of samples per pixel. But it also opens the door to more advanced techniques like:

- Depth of field
- Soft shadows
- Motion blur
- Path tracing

So this small change unlocks a big step toward realism.

The image renderer now benefits from variance reduction and smoother color blending, especially across object boundaries.

Anti-aliasing: essential, and now supported.

<div style="display: flex; justify-content: space-between;">
  <a href="./13_front_vs_backfaces.md">← All Chapters</a>
  <a href="./15_diffuse_materials.md">Next Chapter →</a>
</div>
