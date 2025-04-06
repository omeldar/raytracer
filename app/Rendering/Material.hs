{-# LANGUAGE OverloadedStrings #-}

module Rendering.Material
  ( Material (..),
    defaultMaterial,
  )
where

import Core.Vec3 (Vec3 (..))
import Data.Aeson (FromJSON (parseJSON), withObject, (.!=), (.:?))

-- A flexible physical material model
data Material = Material
  { diffuseColor :: Vec3, -- Kd
    specularColor :: Maybe Vec3, -- Ks
    emissionColor :: Maybe Vec3, -- Ke
    shininess :: Maybe Double, -- Ns
    ior :: Maybe Double, -- Ni (index of refraction)
    dissolve :: Maybe Double -- d (transparency, 1 = opaque)
  }
  deriving (Show, Eq)

-- Reasonable fallback
defaultMaterial :: Material
defaultMaterial =
  Material
    { diffuseColor = Vec3 1 1 1,
      specularColor = Nothing,
      emissionColor = Nothing,
      shininess = Nothing,
      ior = Nothing,
      dissolve = Nothing
    }

instance FromJSON Material where
  parseJSON = withObject "Material" $ \v ->
    Material
      <$> v .:? "diffuseColor" .!= Vec3 1 1 1
      <*> v .:? "specularColor"
      <*> v .:? "emissionColor"
      <*> v .:? "shininess"
      <*> v .:? "ior"
      <*> v .:? "dissolve"
