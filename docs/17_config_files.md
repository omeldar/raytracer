[🔗 Back to Chapters](/README.md#-chapters)

# Chapter 17: Config Files

As the raytracer grew more capable, the number of command-line arguments became unmanageable. Resolution, samples per pixel, recursion depth, scene setup... passing these by CLI quickly became a headache.

To make the project more scalable and flexible, we moved to using **JSON configuration files**.

## 🔢 Why JSON?

- Human-readable and editable
- Easily parsed in Haskell (via Aeson)
- Allows deeply nested config (camera, materials, lights, etc.)
- Enables saving and sharing entire scene setups

## 📑 Sample Config Structure

```json
{
  "image": {
    "width": 1920,
    "height": 1080,
    "samplesPerPixel": 50,
    "antialiasing": true
  },
  "camera": {
    "origin": [0.0, 0.0, 5.0],
    "lookAt": [0.0, 0.0, 0.0],
    "focalLength": 5.0
  },
  "scene": {
    "objects": [
      { "type": "sphere", "center": [0, 0, -1], "radius": 0.5 },
      { "type": "sphere", "center": [1.5, 0, -1], "radius": 0.5 }
    ]
  },
  "render": {
    "maxDepth": 10,
    "russianRoulette": true
  },
  "bvh": {
    "enabled": true,
    "maxDepth": 32
  }
}
```

This format allows us to fully describe a scene and all relevant render parameters in one place.

## 💡 CLI Usage

The program now expects one argument: the path to the config file.

```bash
cabal run raytracer configs/scene1.json
```

All previous CLI flags are removed. If the file is missing or invalid, we fail with a clear message.

## ⚖️ Advantages

- Easy to create new render setups
- Enables reproducibility of renders
- Ideal for automation, scripting, or batch rendering
- Clean separation between UI and engine

This change simplifies everything going forward. From this point on, any new feature (e.g. materials, lights, animation) can simply extend the config file schema.