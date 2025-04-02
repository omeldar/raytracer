[🔗 Back to Chapters](/README.md#-chapters)

# Chapter 16: Buffered Writing

When rendering large images with many samples per pixel, we quickly ran into a major bottleneck: **RAM usage**.

The original approach stored the entire rendered image in memory before writing it to a `.ppm` file. For high resolutions (1920x1080) and large sample counts (e.g. 500), this consumed **gigabytes of memory**, sometimes crashing the program.

## ⚠️ The Problem

- Resolution: 1920x1080
- Anti-aliasing: 500 samples per pixel
- Memory spike: 64 GB RAM used
- Result: crash, or the system becomes unresponsive

We need a way to write the image incrementally to disk instead of holding it in memory.

## 📁 The Fix: Streaming Output

We now **buffer the writing process**, flushing pixels line by line (or block by block) directly to the output file.

Haskell's `hSetBuffering` is used to control output buffering, and we only store a small chunk of pixels in memory at any one time.

Instead of building up a giant string:

```haskell
writeFile "output.ppm" (ppmToStr image)
```

We now do something like:

```haskell
withFile "output.ppm" WriteMode $ \handle -> do
    hSetBuffering handle LineBuffering
    ...
    forM_ rows $ \row -> do
        forM_ row $ \pixel -> hPutStr handle (formatPixel pixel)
```

This drastically reduces RAM usage.

## 📊 Results

| Resolution / AA | Before (RAM/Time) | After (RAM/Time)    |
| --------------- | ----------------- | ------------------- |
| 1920x1080 / 50  | ~75% RAM / 68s    | ~0.8% RAM / 58s     |
| 1920x1080 / 500 | ~100% RAM / crash | ~1.1% RAM / 603.69s |

Total RAM on the system: 64GB

<div align="center">
  <a href="./15_diffuse_materials.md">← Previous Chapter</a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
  <a href="./17_config_files.md">Next Chapter →</a>
</div>
