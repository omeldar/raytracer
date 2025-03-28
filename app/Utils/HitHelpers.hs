module Utils.HitHelpers (closestHit) where

import Hittable.Class (HitRecord (..))

closestHit :: Maybe HitRecord -> Maybe HitRecord -> Maybe HitRecord
closestHit (Just a) (Just b) = if t a < t b then Just a else Just b
closestHit (Just a) Nothing = Just a
closestHit Nothing (Just b) = Just b
closestHit Nothing Nothing = Nothing
