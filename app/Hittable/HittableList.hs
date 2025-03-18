{-# LANGUAGE ExistentialQuantification #-}

module Hittable.HittableList
  ( HittableList (HittableList),
    SomeHittable (SomeHittable),
  )
where

import Hittable.Class (HitRecord (t), Hittable (..))

-- Existential wrapper for any Hittable type
data SomeHittable = forall a. (Hittable a) => SomeHittable a

newtype HittableList = HittableList [SomeHittable]

instance Hittable HittableList where
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
