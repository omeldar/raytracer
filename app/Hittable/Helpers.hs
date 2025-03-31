module Hittable.Helpers (getBox) where

import Hittable.BoundingBox (AABB)
import Hittable.Class (Hittable (..))
import Hittable.Wrapped (SomeHittable (..))

getBox :: SomeHittable -> AABB
getBox (SomeHittable obj) = boundingBox obj
