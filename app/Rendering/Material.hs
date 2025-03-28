module Rendering.Material where

data MaterialType
  = Lambertian
  | Metal { fuzz :: Double }
  | Dielectric { refIdx :: Double }
  deriving (Show, Eq)
