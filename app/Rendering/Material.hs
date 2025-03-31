{-# LANGUAGE OverloadedStrings #-}

module Rendering.Material where

import Data.Aeson

data MaterialType
  = Lambertian
  | Metal {fuzz :: Double}
  | Dielectric {refIdx :: Double}
  deriving (Show, Eq)

instance FromJSON MaterialType where
  parseJSON = withObject "MaterialType" $ \v -> do
    matType <- v .: "type"
    case (matType :: String) of
      "lambertian" -> return Lambertian
      "metal" -> Metal <$> v .:? "fuzz" .!= 0.0
      "dielectric" -> Dielectric <$> v .: "refIdx"
      _ -> fail $ "Unknown material type: " ++ matType
