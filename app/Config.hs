{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config (..),
    ImageSettings (..),
    BackgroundSettings (..),
    CameraSettings (..),
    RaytracerSettings (..),
    SceneSettings (..),
    LightSettings (..),
    SceneObject (..),
    RussianRouletteSettings (..),
    AdaptiveMethod (..),
    loadConfig,
  )
where

import Core.Vec3 (Vec3 (..))
import Data.Aeson (FromJSON, eitherDecode, parseJSON, withObject, (.:))
import Data.Aeson.Types (withText)
import qualified Data.ByteString.Lazy as B
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.IO (hPutStrLn, stderr)

data ImageSettings = ImageSettings
  { width :: Int,
    height :: Int,
    samplesPerPixel :: Int,
    antialiasing :: Bool
  }
  deriving (Generic, Show)

data BackgroundSettings
  = Gradient {color1 :: Vec3, color2 :: Vec3}
  | SolidColor {color :: Vec3}
  deriving (Show, Generic)

data CameraSettings = CameraSettings
  { lookFrom :: Vec3,
    lookAt :: Vec3,
    vUp :: Vec3,
    vfov :: Double,
    aperture :: Double,
    focusDist :: Double
  }
  deriving (Generic, Show)

data RaytracerSettings = RaytracerSettings
  { maxBounces :: Int,
    russianRoulette :: RussianRouletteSettings,
    useBVH :: Bool
  }
  deriving (Show, Generic)

data AdaptiveMethod = Linear | Exponential | Sqrt deriving (Show, Generic)

instance FromJSON AdaptiveMethod where
  parseJSON = withText "AdaptiveMethod" $ \case
    "linear" -> return Linear
    "exponential" -> return Exponential
    "sqrt" -> return Sqrt
    _ -> fail "Unknown adaptive method. Use 'linear', 'exponential', or 'sqrt'."

data RussianRouletteSettings = RussianRouletteSettings
  { enabled :: Bool,
    probability :: Double,
    adaptive :: Bool,
    adaptivityFactor :: Double,
    adaptiveMethod :: AdaptiveMethod
  }
  deriving (Show, Generic)

data LightSettings
  = PointLight {position :: Vec3, intensity :: Vec3}
  | DirectionalLight {direction :: Vec3, intensity :: Vec3}
  deriving (Show, Generic)

data SceneObject
  = SphereObj Vec3 Double
  | PlaneObj Vec3 Vec3
  | TriangleObj Vec3 Vec3 Vec3
  deriving (Show, Generic)

instance FromJSON SceneObject where
  parseJSON = withObject "SceneObject" $ \v -> do
    objType <- v .: "type"
    case (objType :: String) of
      "sphere" -> SphereObj <$> v .: "center" <*> v .: "radius"
      "plane" -> PlaneObj <$> v .: "pointOnPlane" <*> v .: "normal"
      "triangle" -> TriangleObj <$> v .: "v0" <*> v .: "v1" <*> v .: "v2"
      _ -> fail $ "Unknown object type: " ++ objType

data SceneSettings = SceneSettings
  { tag :: String,
    objects :: [SceneObject]
  }
  deriving (Show, Generic)

data Config = Config
  { image :: ImageSettings,
    background :: BackgroundSettings,
    camera :: CameraSettings,
    raytracer :: RaytracerSettings,
    lights :: [LightSettings],
    scene :: SceneSettings
  }
  deriving (Show, Generic)

instance FromJSON ImageSettings

instance FromJSON BackgroundSettings

instance FromJSON CameraSettings

instance FromJSON RaytracerSettings

instance FromJSON RussianRouletteSettings

instance FromJSON LightSettings

instance FromJSON SceneSettings

instance FromJSON Config

loadConfig :: FilePath -> IO (Maybe Config)
loadConfig filePath = do
  fileExists <- doesFileExist filePath
  if not fileExists
    then do
      hPutStrLn stderr $ "Error: Config file not found -> " ++ filePath
      return Nothing
    else do
      content <- B.readFile filePath
      case eitherDecode content of
        Left err -> do
          hPutStrLn stderr $ "Error parsing config file -> " ++ filePath
          hPutStrLn stderr $ "JSON Error: " ++ err
          return Nothing
        Right config -> return (Just config)