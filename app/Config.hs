{-# LANGUAGE DeriveGeneric #-}

module Config
  ( Config (..),
    ImageSettings (..),
    BackgroundSettings (..),
    CameraSettings (..),
    RaytracerSettings (..),
    SceneSettings (..),
    LightSettings (..),
    loadConfig,
  )
where

import Core.Vec3 (Vec3 (..))
import Data.Aeson (FromJSON, eitherDecode)
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

data RussianRouletteSettings = RussianRouletteSettings
  { enabled :: Bool,
    probability :: Double
  }
  deriving (Show, Generic)

data LightSettings = PointLight {position :: Vec3, intensity :: Vec3}
  deriving (Show, Generic)

data SceneSettings = InternalObjects | FileImport {objFile :: String} deriving (Show, Generic)

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