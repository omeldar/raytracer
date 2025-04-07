module Parser.Material where

import Core.Vec3 as V (Vec3 (..))
import qualified Data.Map as M
import Rendering.Material

type MaterialNameToId = M.Map String Int

type IdToMaterialMap = M.Map Int Material

parseMaterial :: String -> (MaterialNameToId, IdToMaterialMap)
parseMaterial content =
  let rawMap = go (lines content) Nothing M.empty
      numbered = zip [0 ..] (M.toList rawMap)
      nameToId = M.fromList [(name, i) | (i, (name, _)) <- numbered]
      idToMat = M.fromList [(i, mat) | (i, (_, mat)) <- numbered]
   in (nameToId, idToMat)
  where
    go :: [String] -> Maybe (String, Material) -> M.Map String Material -> M.Map String Material
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
        updateCurrent :: (Material -> Material) -> M.Map String Material
        updateCurrent f = case current of
          Just (n, m) -> go ls (Just (n, f m)) acc
          Nothing -> go ls Nothing acc

assignMaterialIds :: [(String, Material)] -> (M.Map String Int, M.Map Int Material)
assignMaterialIds namedMats =
  let indexed = zip [1 ..] namedMats
      nameToId = M.fromList [(name, i) | (i, (name, _)) <- indexed]
      idToMat = M.fromList [(i, mat) | (i, (_, mat)) <- indexed]
   in (nameToId, idToMat)
