module Utils.HitHelpers (closestHit, closestHitList) where

import Hittable.Class (HitRecord (..))

closestHit :: Maybe HitRecord -> Maybe HitRecord -> Maybe HitRecord
closestHit (Just a) (Just b) = if t a < t b then Just a else Just b
closestHit (Just a) Nothing = Just a
closestHit Nothing (Just b) = Just b
closestHit Nothing Nothing = Nothing

closestHitList :: [Maybe HitRecord] -> Maybe HitRecord
closestHitList = foldr closer Nothing
  where
    closer Nothing acc = acc
    closer (Just x) Nothing = Just x
    closer (Just x) (Just y) = if t x < t y then Just x else Just y