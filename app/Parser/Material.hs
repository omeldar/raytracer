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
    go :: [String] -> Maybe (String, Material, Maybe Int) -> M.Map String Material -> M.Map String Material
    go [] (Just (name, mat, illum)) acc = M.insert name (finalizeMaterial mat illum) acc
    go [] Nothing acc = acc
    go (l : ls) current acc =
      case words l of
        ("newmtl" : name : _) ->
          let acc' = maybe acc (\(n, m, illum) -> M.insert n (finalizeMaterial m illum) acc) current
           in go ls (Just (name, defaultMaterial, Nothing)) acc'
        ("Kd" : r : g : b : _) -> updateCurrent (\(m, i) -> (m {diffuseColor = Vec3 (read r) (read g) (read b)}, i))
        ("Ks" : r : g : b : _) -> updateCurrent (\(m, i) -> (m {specularColor = Just (Vec3 (read r) (read g) (read b))}, i))
        ("Ke" : r : g : b : _) -> updateCurrent (\(m, i) -> (m {emissionColor = Just (Vec3 (read r) (read g) (read b))}, i))
        ("Ns" : n : _) -> updateCurrent (\(m, i) -> (m {shininess = Just (read n)}, i))
        ("Ni" : iorVal : _) -> updateCurrent (\(m, i) -> (m {ior = Just (read iorVal)}, i))
        ("d" : dVal : _) -> updateCurrent (\(m, i) -> (m {dissolve = Just (read dVal)}, i))
        ("illum" : n : _) -> updateCurrent (\(m, _) -> (m, Just (read n)))
        _ -> go ls current acc
      where
        updateCurrent :: ((Material, Maybe Int) -> (Material, Maybe Int)) -> M.Map String Material
        updateCurrent f = case current of
          Just (n, m, illum) ->
            let (m', illum') = f (m, illum)
             in go ls (Just (n, m', illum')) acc
          Nothing -> go ls Nothing acc

    finalizeMaterial :: Material -> Maybe Int -> Material
    finalizeMaterial mat illum =
      let refIdx = ior mat
          transmissionVal =
            case (illum, refIdx) of
              (Just n, Just i) | n `elem` [7, 9], i > 1.0 -> Just 1.0
              _ -> Nothing
       in mat {transmission = transmissionVal}

assignMaterialIds :: [(String, Material)] -> (M.Map String Int, M.Map Int Material)
assignMaterialIds namedMats =
  let indexed = zip [1 ..] namedMats
      nameToId = M.fromList [(name, i) | (i, (name, _)) <- indexed]
      idToMat = M.fromList [(i, mat) | (i, (_, mat)) <- indexed]
   in (nameToId, idToMat)
