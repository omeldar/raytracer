{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}

module Hittable.Wrapped (SomeHittable (..)) where

import Data.Typeable (Typeable)
import Hittable.Class (Hittable)

-- Exists to hold any Hittable
data SomeHittable = forall a. (Hittable a, Typeable a) => SomeHittable a
