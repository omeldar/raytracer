module Hittable.HittableList where

import Hittable.Objects.Sphere
import Core.Ray as R
import Hittable.Class

newtype HittableList = HittableList [Sphere]

instance Hittable HittableList where
    hit (HittableList objects) ray tMin tMax =
        foldr (\obj acc -> case hit obj ray tMin tMax of
            Nothing -> acc
            Just rec -> case acc of
                Nothing -> Just rec
                Just prevRec -> if t rec < t prevRec then Just rec else acc
        ) Nothing objects