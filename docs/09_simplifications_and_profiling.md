[🔗 Back to Chapters](/README.md#-chapters)

# Chapter 9: Ray-Sphere Simplification & Profiling

As the raytracer grows in complexity, performance begins to matter. One of the most frequently called functions is the ray-sphere intersection. Before moving on to support more object types, it's worth optimizing this logic.

---

## 🧮 Simplifying the Ray-Sphere Intersection Code

Let’s look at the original implementation:

```haskell
hitSphere :: V.Vec3 -> Double -> R.Ray -> Double
hitSphere center radius ray
    | discriminant < 0 = -1.0
    | t1 > 0.0 = t1
    | t2 > 0.0 = t2
    | otherwise = -1.0
  where
    oc = R.origin ray `V.sub` center
    a = V.dot (R.direction ray) (R.direction ray)
    b = 2.0 * V.dot (R.direction ray) oc
    c = V.dot oc oc - radius * radius
    discriminant = b * b - 4 * a * c
    sqrtD = sqrt discriminant
    t1 = (-b - sqrtD) / (2.0 * a)
    t2 = (-b + sqrtD) / (2.0 * a)
```

We can simplify this using the substitution:

$$
b = -2h
\Rightarrow t = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}
= \frac{2h \pm 2\sqrt{h^2 - ac}}{2a}
= \frac{h \pm \sqrt{h^2 - ac}}{a}
$$

So we define:

$$ h = \vec{oc} \cdot \vec{d} $$

$$ a = \vec{d} \cdot \vec{d} $$

This gives us a much cleaner implementation:

```haskell
hitSphere :: V.Vec3 -> Double -> R.Ray -> Double
hitSphere center radius ray
    | discriminant < 0 = -1.0
    | otherwise = (-h - sqrt discriminant) / a
  where
    oc = R.origin ray `V.sub` center
    a = V.lengthSquared $ R.direction ray
    h = V.dot oc $ R.direction ray
    discriminant = h * h - a * (V.lengthSquared oc - radius * radius)
```

This version:

- Reduces redundant math
- Avoids unnecessary multiplications and divisions
- Keeps the function compact and efficient

---

## 📈 Profiling with GHC

To verify performance or detect bottlenecks, we use Haskell’s built-in GHC profiling tools.

### 🔧 Enabling Profiling

1. In `cabal.project.local`, add:

   ```
   package *
     profiling: True
   ```

2. Clean and rebuild:

   ```bash
   cabal clean
   cabal build --enable-profiling
   ```

3. Run the raytracer with profiling enabled:

   ```bash
   cabal run profraytracer 1920 1080 -- +RTS -p
   ```

   This will generate a `.prof` file (e.g., `profraytracer.prof`).

---

### 📊 Example Profiling Output

```
Mon Feb 24 23:23 2025 Time and Allocation Profiling Report  (Final)

   profraytracer +RTS -p -RTS 1920 1080

total time  =        2.15 secs   (2152 ticks @ 1000 us, 1 processor)
total alloc = 7,983,032,104 bytes

COST CENTRE                     MODULE                   SRC                                             %time %alloc

ppmToStr.pixelData              Rendering.ImageGenerator app/Rendering/ImageGenerator.hs:53:9-65          28.6   36.7
ppmToStr.showPixel              Rendering.ImageGenerator app/Rendering/ImageGenerator.hs:(58,9)-(59,86)   14.7   24.9
generateRay                     Rendering.Camera         app/Rendering/Camera.hs:(37,1)-(44,52)            9.6   12.7
createAndWriteFile              Rendering.ImageGenerator app/Rendering/ImageGenerator.hs:63:1-30           9.4    0.1
generateRay.direction           Rendering.Camera         app/Rendering/Camera.hs:(40,9)-(43,41)            7.3    1.2
traceRay                        Rendering.ImageGenerator app/Rendering/ImageGenerator.hs:(34,1)-(44,41)    6.9    7.5
hit                             Hittable.Objects.Sphere  app/Hittable/Objects/Sphere.hs:(13,5)-(29,47)     5.9    0.5
lerp                            Rendering.Color          app/Rendering/Color.hs:14:1-58                    4.4    4.8
```

### 📌 Interpretation

- `ppmToStr.pixelData` and `showPixel` take a huge portion of time and allocation — consider streaming output instead of building full strings.
- `generateRay` and `traceRay` are expected hotspots.
- The sphere `hit` logic is now relatively fast — our optimization is paying off.

---

### 🪟 Disabling Profiling

Once done:

```haskell
package *
  profiling: False
```

Then rebuild:

```bash
cabal build
```

You may see harmless exceptions during the switch. You can now run normally again:

```bash
cabal run raytracer <config-file.json>
```

---

## 💡 Takeaway

Optimization is only valuable when guided by data. Simplifying the math gave a theoretical improvement, and profiling confirmed where real time was being spent. These tools give you control over both precision and performance — a key skill for writing a performant raytracer.

<div align="center">
  <a href="./08_normals_and_perspective.md">← All Chapters</a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
  <a href="./10_hittable_abstraction.md">Next Chapter →</a>
</div>
