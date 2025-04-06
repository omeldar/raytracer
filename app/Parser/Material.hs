module Parser.Material where

import Core.Vec3 as V (Vec3 (..))
import qualified Data.Map as M
import Rendering.Material

type MaterialMap = M.Map String Material

parseMaterial :: String -> MaterialMap
parseMaterial content = go (lines content) Nothing M.empty
  where
    go :: [String] -> Maybe (String, Material) -> MaterialMap -> MaterialMap
    go [] (Just (name, mat)) acc = M.insert name mat acc
    go [] Nothing acc = acc
    go (l : ls) current acc =
      case words l of
        ("newmtl" : name : _) ->
          let acc' = maybe acc (\(n, m) -> M.insert n m acc) current
           in go ls (Just (name, defaultMaterial)) acc'
        ("Kd" : r : g : b : _) -> updateCurrent (\m -> m {diffuseColor = Vec3 (read r) (read g) (read b)})
        ("Ks" : r : g : b : _) -> updateCurrent (\m -> m {specularColor = Just (Vec3 (read r) (read g) (read b))})
        ("Ke" : r : g : b : _) -> updateCurrent (\m -> m {emissionColor = Just (Vec3 (read r) (read g) (read b))})
        ("Ns" : n : _) -> updateCurrent (\m -> m {shininess = Just (read n)})
        ("Ni" : iorVal : _) -> updateCurrent (\m -> m {ior = Just (read iorVal)})
        ("d" : dVal : _) -> updateCurrent (\m -> m {dissolve = Just (read dVal)})
        _ -> go ls current acc
      where
        updateCurrent :: (Material -> Material) -> MaterialMap
        updateCurrent f = case current of
          Just (n, m) -> go ls (Just (n, f m)) acc
          Nothing -> go ls Nothing acc