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
    ObjFileEntry (..),
    loadConfig,
  )
where

import Core.Vec3 (Vec3 (..))
import Data.Aeson (FromJSON, eitherDecode, parseJSON, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (withText)
import qualified Data.ByteString.Lazy as B
import GHC.Generics (Generic)
import Rendering.Material
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
    useBVH :: Bool,
    bvhMaxDepth :: Int
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
  = SphereObj Vec3 Double Vec3 (Maybe String)
  | PlaneObj Vec3 Vec3 Vec3 (Maybe String)
  | TriangleObj Vec3 Vec3 Vec3 Vec3 (Maybe String)
  deriving (Show, Generic)

data ObjFileEntry = ObjFileEntry
  { path :: FilePath,
    objposition :: Vec3,
    overrideColor :: Maybe Vec3,
    overrideMaterial :: Maybe Material
  }
  deriving (Show, Generic)

instance FromJSON ObjFileEntry where
  parseJSON = withObject "ObjFileEntry" $ \v -> do
    ObjFileEntry
      <$> v .: "path"
      <*> v .: "objposition"
      <*> v .:? "overrideColor"
      <*> v .:? "overrideMaterial"

instance FromJSON SceneObject where
  parseJSON = withObject "SceneObject" $ \v -> do
    objType <- v .: "type"
    scolor <- v .:? "color" .!= Vec3 1 1 1
    matName <- v .:? "material" -- Optional, stays as Maybe String
    case objType of
      "sphere" ->
        SphereObj
          <$> v .: "center"
          <*> v .: "radius"
          <*> pure scolor
          <*> pure matName
      "plane" ->
        PlaneObj
          <$> v .: "pointOnPlane"
          <*> v .: "normal"
          <*> pure scolor
          <*> pure matName
      "triangle" ->
        TriangleObj
          <$> v .: "v0"
          <*> v .: "v1"
          <*> v .: "v2"
          <*> pure scolor
          <*> pure matName
      _ -> fail $ "Unknown scene object type: " ++ objType

data SceneSettings = SceneSettings
  { objects :: Maybe [SceneObject],
    objFiles :: Maybe [ObjFileEntry],
    lights :: Maybe [LightSettings],
    materials :: Maybe [(String, Material)]
  }
  deriving (Show, Generic)

instance FromJSON SceneSettings where
  parseJSON = withObject "SceneSettings" $ \v ->
    SceneSettings
      <$> v .:? "objects"
      <*> v .:? "objFiles"
      <*> v .:? "lights"
      <*> v .:? "materials"

data Config = Config
  { image :: ImageSettings,
    background :: BackgroundSettings,
    camera :: CameraSettings,
    raytracer :: RaytracerSettings,
    scene :: SceneSettings
  }
  deriving (Show, Generic)

instance FromJSON ImageSettings

instance FromJSON BackgroundSettings

instance FromJSON CameraSettings

instance FromJSON RaytracerSettings

instance FromJSON RussianRouletteSettings

instance FromJSON LightSettings

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