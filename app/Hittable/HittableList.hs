{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

module Hittable.HittableList
  ( HittableList (HittableList),
    SomeHittable (SomeHittable),
  )
where

import Hittable.BoundingBox (AABB (..), surroundingBox)
import Hittable.Class (HitRecord (t), Hittable (..))
import Hittable.Helpers (getBox)
import Hittable.Wrapped (SomeHittable (..))

newtype HittableList = HittableList [SomeHittable]

instance Hittable HittableList where
  boundingBox :: HittableList -> AABB
  boundingBox (HittableList []) = error "Empty HittableList has no bounding box"
  boundingBox (HittableList (obj : objs)) =
    foldr
      (surroundingBox . getBox)
      (getBox obj)
      objs
  hit (HittableList objects) ray interval =
    foldr
      ( \(SomeHittable obj) acc -> case hit obj ray interval of
          Nothing -> acc
          Just rec -> case acc of
            Nothing -> Just rec
            Just prevRec -> if t rec < t prevRec then Just rec else acc
      )
      Nothing
      objects
