[🔗 Back to Chapters](/README.md#-chapters)

# Chapter 10: Hittable Abstraction

Our raytracer can currently detect intersections with a single sphere using hardcoded logic. But we’re going to need many different kinds of objects in a scene — and we want to support all of them in a modular, scalable way.

That’s where **abstraction** comes in.

We'll define a common interface called `Hittable`, which lets any object determine whether it was hit by a ray.

## 🔢 Defining the `Hittable` Typeclass

We start with a new module:

```haskell
module Hittable.Class where

import Core.Vec3
import Core.Ray

-- Data structure to return hit information
data HitRecord = HitRecord
    { point  :: Vec3
    , normal :: Vec3
    , t      :: Double
    } deriving (Show)

-- Typeclass for any object that can be hit by a ray
class Hittable a where
    hit :: a -> Ray -> Double -> Double -> Maybe HitRecord
```

- `HitRecord` captures where the ray hit, what the surface normal was, and the distance `t` from the ray origin.
- The `hit` method tells us: given a ray and `tMin`/`tMax` bounds, does the object get hit? And if so, return the `HitRecord`.

This abstraction will be the foundation for **spheres**, **planes**, **triangles**, and later on: **BVHs**, **meshes**, and **volumes**.

---

## ⚫ Implementing `Hittable` for a Sphere

We start by turning our sphere into an instance of `Hittable`.

```haskell
instance Hittable Sphere where
    hit sphere ray tMin tMax
        | discriminant < 0 = Nothing
        | t1 >= tMin && t1 <= tMax = Just (makeHitRecord t1)
        | t2 >= tMin && t2 <= tMax = Just (makeHitRecord t2)
        | otherwise = Nothing
      where
        oc = origin ray `sub` center sphere
        a = lengthSquared $ direction ray
        h = dot oc $ direction ray
        discriminant = h * h - a * (lengthSquared oc - radius sphere * radius sphere)
        sqrtD = sqrt discriminant
        t1 = (-h - sqrtD) / a
        t2 = (-h + sqrtD) / a

        makeHitRecord t' =
            let p = ray `at` t'
                outwardNormal = (1.0 / radius sphere) `scale` (p `sub` center sphere)
            in HitRecord p outwardNormal t'
```

This is the same optimized logic as before, now integrated into a reusable and type-safe interface.

Note: we calculate the **outward normal**, assuming we always want normals pointing against the incoming ray (which we can flip later if needed).

## 🗺️ Hittable List: A Scene with Multiple Objects

We now want to support **scenes with many objects**. For that, we introduce a new wrapper type:

```haskell
newtype HittableList = HittableList [Sphere]
```

> In this version, the list only stores `Sphere`s. Later, we’ll generalize this to store any `Hittable` object.

### ⚖️ Instance for `HittableList`

```haskell
instance Hittable HittableList where
    hit (HittableList objects) ray tMin tMax =
        foldr checkHit Nothing objects
      where
        checkHit obj acc =
            case hit obj ray tMin tMax of
                Nothing -> acc
                Just rec -> case acc of
                    Nothing -> Just rec
                    Just prev -> if t rec < t prev then Just rec else acc
```

We fold over the list of objects and keep the **closest valid hit**, if any. This gives us the first intersection along the ray.

## ✨ Example Usage

Here's how we update `traceRay` to use the new `HittableList`:

```haskell
traceRay :: R.Ray -> Col.Color
traceRay ray =
    let scene = HittableList
            [ S.Sphere (V.Vec3 0 0 (-1)) 0.5
            , S.Sphere (V.Vec3 1.5 0 (-1)) 0.5
            , S.Sphere (V.Vec3 (-1.5) 0 (-1)) 0.5
            ]
        tMin = 0.001  -- prevent shadow acne
        tMax = 1000.0
    in case H.hit scene ray tMin tMax of
        Just hitRec ->
            let normal = V.normalize (H.normal hitRec)
            in 0.5 `V.scale` (normal `V.add` V.Vec3 1 1 1)
        Nothing ->
            Col.lerp (0.5 * (V.y (V.normalize (R.direction ray)) + 1.0))
                     (V.Vec3 1 1 1)
                     (V.Vec3 0.5 0.7 1.0)
```

Each sphere returns a hit if intersected, and we use the closest one to determine the color. If no hit occurs, we render the background.

## 💡 Summary

We now have a unified `Hittable` abstraction:

- Any shape can implement `hit`
- We can group objects into a list
- We can now build entire scenes with arbitrary geometry

From this point on, we can introduce **planes**, **triangles**, **meshes**, and **acceleration structures** without rewriting our core raytracing loop.

<div align="center">
  <a href="./09_simplifications_and_profiling.md">← All Chapters</a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
  <a href="./11_multiple_objects.md">Next Chapter →</a>
</div>
