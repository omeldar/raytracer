[🔗 Back to Chapters](/README.md#-chapters)

# Chapter 9: Ray-Sphere Simplification & Profiling

We’ve already implemented ray-sphere intersection, but the current code can be optimized further. This matters a lot, because in any complex scene, this function will be called **millions of times**.

Let’s revisit the current form:

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

We notice here that the b coefficient has a `2.0` in it. So let’s replace `b` with `-2h`, and simplify the math using this substitution:

## The Math

Start with the quadratic formula:

$$
t = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}
$$

$$
= \frac{-(-2h) \pm \sqrt{(-2h)^2 - 4ac}}{2a}
$$

$$
= \frac{2h \pm 2 \sqrt{h^2 - ac}}{2a}
$$

$$
= \frac{h \pm \sqrt{h^2 - ac}}{a}
$$

Solving for h, we get:

$$
h = \frac{b}{-2} = d \cdot (C - Q)
$$
