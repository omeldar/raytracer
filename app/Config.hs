{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Config
  ( Config (..),
    ImageSettings (..),
    BackgroundSettings (..),
    CameraSettings (..),
    RaytracerSettings (..),
    SceneSettings (..),
    LightSettings (..),
    SceneObject (..),
    ObjFileEntry (..),
    loadConfig,
  )
where

import Core.Vec3 (Vec3 (..))
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import GHC.Generics (Generic)
import Rendering.Material (Material)
import System.Directory (doesFileExist)
import System.IO (hPutStrLn, stderr)

-- Image configuration with new gamma and exposure options
data ImageSettings = ImageSettings
  { width :: Int,
    height :: Int,
    samplesPerPixel :: Int,
    antialiasing :: Bool,
    gamma :: Double,
    exposure :: Double
  }
  deriving (Show, Generic)

-- Background can be a gradient or a solid color
data BackgroundSettings
  = Gradient {color1 :: Vec3, color2 :: Vec3}
  | SolidColor {color :: Vec3}
  deriving (Show, Generic)

-- Camera parameters
data CameraSettings = CameraSettings
  { lookFrom :: Vec3,
    lookAt :: Vec3,
    vUp :: Vec3,
    vfov :: Double,
    aperture :: Double,
    focusDist :: Double
  }
  deriving (Show, Generic)

-- Raytracer options
-- Russian roulette removed; leafThreshold added
data RaytracerSettings = RaytracerSettings
  { maxBounces :: Int,
    useBVH :: Bool,
    bvhMaxDepth :: Int,
    leafThreshold :: Int
  }
  deriving (Show, Generic)

-- Light types
data LightSettings
  = PointLight {position :: Vec3, intensity :: Vec3}
  | DirectionalLight {direction :: Vec3, intensity :: Vec3}
  deriving (Show, Generic)

-- Primitive scene objects with optional material name
data SceneObject
  = SphereObj Vec3 Double Vec3 (Maybe String)
  | PlaneObj Vec3 Vec3 Vec3 (Maybe String)
  | TriangleObj Vec3 Vec3 Vec3 Vec3 (Maybe String)
  deriving (Show, Generic)

-- Entry for loading .obj files
data ObjFileEntry = ObjFileEntry
  { path :: FilePath,
    objposition :: Vec3,
    overrideColor :: Maybe Vec3,
    overrideMaterial :: Maybe Material
  }
  deriving (Show, Generic)

-- Entire scene configuration
data SceneSettings = SceneSettings
  { objects :: Maybe [SceneObject],
    objFiles :: Maybe [ObjFileEntry],
    lights :: Maybe [LightSettings],
    materials :: Maybe [(String, Material)],
    skyTexture :: Maybe FilePath
  }
  deriving (Show, Generic)

-- Top-level config
data Config = Config
  { image :: ImageSettings,
    background :: BackgroundSettings,
    camera :: CameraSettings,
    raytracer :: RaytracerSettings,
    scene :: SceneSettings
  }
  deriving (Show, Generic)

-- JSON parsing instances
deriving instance FromJSON ImageSettings

instance FromJSON BackgroundSettings

instance FromJSON CameraSettings

instance FromJSON RaytracerSettings

instance FromJSON LightSettings

instance FromJSON SceneObject

instance FromJSON ObjFileEntry

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