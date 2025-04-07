module Parser.Object (loadObjWithOffset) where

import Control.Applicative ((<|>))
import Core.Vec3
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Debug.Trace
import Hittable.HittableList
  ( HittableList (..),
    SomeHittable (SomeHittable),
  )
import Hittable.Objects.Triangle
import Rendering.Material

parseObj :: String -> Maybe Vec3 -> Maybe Int -> Map String Int -> [Triangle]
parseObj content overrideColor overrideMaterialId nameToIdMap =
  let ls = lines content
      verts = [parseVertex l | l <- ls, "v " `isPrefixOf` l]
      (_, tris) = parseLines ls verts Nothing []
   in concat (reverse tris)
  where
    parseLines :: [String] -> [Vec3] -> Maybe String -> [[Triangle]] -> (Maybe String, [[Triangle]])
    parseLines [] _ _ acc = (Nothing, acc)
    parseLines (l : ls) verts currentMatName acc =
      case words l of
        ("usemtl" : name : _) ->
          trace ("Switching material to: " ++ name) $
            parseLines ls verts (Just name) acc
        ("f" : _) ->
          let matId = fromMaybe 0 $ (currentMatName >>= (`M.lookup` nameToIdMap)) <|> overrideMaterialId
              matColor = fromMaybe (Vec3 1 1 1) overrideColor
              tris = parseFace l verts matColor matId
           in parseLines ls verts currentMatName (tris : acc)
        _ -> parseLines ls verts currentMatName acc

parseVertex :: String -> Vec3
parseVertex line =
  case words line of
    ["v", vx, vy, vz] -> Vec3 (read vx) (read vy) (read vz)
    _ -> error "Invalid vertex format in .obj file"

parseFace :: String -> [Vec3] -> Vec3 -> Int -> [Triangle]
parseFace line verts inColor matId =
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
                      matId
                  ]
                else
                  [ Triangle
                      (verts !! (head faceVerts - 1))
                      (verts !! (faceVerts !! (i + 1) - 1))
                      (verts !! (faceVerts !! i - 1))
                      inColor
                      matId
                    | i <- [1 .. vertexCount - 2]
                  ]
    _ -> error "Invalid face format in .obj file"

loadObjWithOffset ::
  FilePath ->
  Vec3 ->
  Maybe Vec3 ->
  Maybe Int ->
  Map String Int ->
  Map Int Material ->
  IO (HittableList, Map Int Material)
loadObjWithOffset path offset overrideColor overrideMaterialId nameToIdMap idToMatMap = do
  content <- readFile path
  if null content
    then do
      putStrLn "Error: OBJ file is empty or could not be read."
      return (HittableList [], M.empty)
    else do
      let triangles = parseObj content overrideColor overrideMaterialId nameToIdMap
          offsetTriangle (Triangle a b c col mid) =
            Triangle (add a offset) (add b offset) (add c offset) col mid
          hittables = [SomeHittable t | t <- map offsetTriangle triangles]
      return (HittableList hittables, idToMatMap)
