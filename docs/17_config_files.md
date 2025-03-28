[🔗 Back to Chapters](/README.md#-chapters)

# Chapter 17: Config Files

As the raytracer grew more capable, the number of command-line arguments became unmanageable. Resolution, samples per pixel, recursion depth, scene setup... passing these by CLI quickly became a headache.

To make the project more scalable and flexible, we move to using **JSON configuration files**.

## 🔢 Why JSON?

- Human-readable and editable
- (more or less) easily parsed in Haskell (via Aeson)
- Allows deeply nested config (camera, materials, lights, etc.)
- Enables saving and sharing entire scene setups

## 📑 Sample Config Structure

```json
{
  "image": {
    "width": 640,
    "height": 380,
    "samplesPerPixel": 50,
    "antialiasing": true
  },
  "background": {
    "tag": "Gradient",
    "color1": [1.0, 1.0, 1.0],
    "color2": [0.5, 0.7, 1.0]
  },
  "camera": {
    "lookFrom": [0, 0, 2],
    "lookAt": [0, 0.0, -1],
    "vUp": [0, 1, 0],
    "vfov": 30.0,
    "aperture": 0.0,
    "focusDist": 3.0
  },
  "raytracer": {
    "maxBounces": 5,
    "useBVH": true,
    "bvhMaxDepth": 10,
    "russianRoulette": {
      "enabled": false,
      "probability": 0.05,
      "adaptive": true,
      "adaptivityFactor": 2,
      "adaptiveMethod": "sqrt"
    }
  },
  "lights": [
    {
      "tag": "PointLight",
      "position": [-7, 1, 5],
      "intensity": [0.5, 0.5, 0.5]
    }
  ],
  "scene": {
    "tag": "FileImport",
    "objects": [
      {
        "type": "plane",
        "pointOnPlane": [0, -2, 0],
        "normal": [0, 1, 0],
        "color": [0.8, 0.8, 0.8]
      }
    ],
    "objFiles": [
      {
        "path": "pathto/monkey_def_size.obj",
        "objposition": [0.5, -0.2, -8]
      },
      {
        "path": "pathto/monkey_def_size.obj",
        "objposition": [-0.5, 0.5, -5]
      }
    ]
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
