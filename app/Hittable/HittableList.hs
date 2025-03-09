module Hittable.HittableList where

import Hittable.Class
import Hittable.Objects.Sphere

newtype HittableList = HittableList [Sphere]

instance Hittable HittableList where
  hit (HittableList objects) ray interval =
    foldr
      ( \obj acc -> case hit obj ray interval of
          Nothing -> acc
          Just rec -> case acc of
            Nothing -> Just rec
            Just prevRec -> if t rec < t prevRec then Just rec else acc
      )
      Nothing
      objects
