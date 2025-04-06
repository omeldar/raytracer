module Parser.Object (loadObjWithOffset) where

import Core.Vec3
import Data.List (isPrefixOf)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Hittable.HittableList
  ( HittableList (..),
    SomeHittable (SomeHittable),
  )
import Hittable.Objects.Triangle
import qualified Parser.Material as PM
import Rendering.Material
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, (</>))

parseObj :: String -> Maybe Vec3 -> Maybe Material -> PM.MaterialMap -> [Triangle]
parseObj content overrideColor overrideMaterial matMap =
  let ls = lines content
      verts = [parseVertex l | l <- ls, "v " `isPrefixOf` l]
      (_, tris) = parseLines ls verts Nothing []
   in concat (reverse tris)
  where
    parseLines [] _ _ acc = (Nothing, acc)
    parseLines (l : ls) verts currentMat acc =
      case words l of
        ("usemtl" : name : _) ->
          parseLines ls verts (Just name) acc
        ("f" : _) ->
          let mat = case currentMat >>= (`M.lookup` matMap) of
                Just m -> m
                Nothing -> fromMaybe defaultMaterial overrideMaterial
              matcolor = fromMaybe (diffuseColor mat) overrideColor
              tris = parseFace l verts matcolor mat
           in parseLines ls verts currentMat (tris : acc)
        _ -> parseLines ls verts currentMat acc

parseVertex :: String -> Vec3
parseVertex line =
  case words line of
    ["v", vx, vy, vz] -> Vec3 (read vx) (read vy) (read vz)
    _ -> error "Invalid vertex format in .obj file"

parseFace :: String -> [Vec3] -> Vec3 -> Material -> [Triangle]
parseFace line verts inColor inMaterial =
  case words line of
    ("f" : indices)
      | length indices >= 3 ->
          let faceVerts = map (read . takeWhile (/= '/')) indices
              vertexCount = length faceVerts
           in if vertexCount == 3
                then
                  [ Triangle
                      (verts !! (head faceVerts - 1))
                      (verts !! (faceVerts !! 1 - 1))
                      (verts !! (faceVerts !! 2 - 1))
                      inColor
                      inMaterial
                  ]
                else
                  [ Triangle
                      (verts !! (head faceVerts - 1))
                      (verts !! (faceVerts !! (i + 1) - 1))
                      (verts !! (faceVerts !! i - 1))
                      inColor
                      inMaterial
                    | i <- [1 .. vertexCount - 2]
                  ]
    _ -> error "Invalid face format in .obj file"

loadObjWithOffset :: FilePath -> Vec3 -> Maybe Vec3 -> Maybe Material -> IO HittableList
loadObjWithOffset path offset overrideColor overrideMaterial = do
  content <- readFile path
  if null content
    then do
      putStrLn "Error: OBJ file is empty or could not be read."
      return $ HittableList []
    else do
      let mtlLine = case filter ("mtllib" `isPrefixOf`) (lines content) of
            (l : _) -> l
            [] -> ""
          mtlFile = case words mtlLine of
            ["mtllib", name] -> name
            _ -> ""
          baseDir = takeDirectory path
          mtlPath = baseDir </> mtlFile

      matMap <-
        if null mtlFile
          then return M.empty
          else do
            exists <- doesFileExist mtlPath
            if exists
              then PM.parseMaterial <$> readFile mtlPath
              else do
                putStrLn $ "Warning: .mtl file not found at " ++ mtlPath
                return M.empty

      let triangles = parseObj content overrideColor overrideMaterial matMap
          offsetTriangle (Triangle a b c col mat) =
            Triangle (add a offset) (add b offset) (add c offset) col mat

      return $ HittableList [SomeHittable t | t <- map offsetTriangle triangles]
