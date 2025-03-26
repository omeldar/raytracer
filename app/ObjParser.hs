module ObjParser (loadObjWithOffset) where

import Core.Vec3
import Data.List (isPrefixOf)
import Hittable.HittableList
import Hittable.Objects.Triangle

parseObj :: String -> ([Vec3], [Triangle])
parseObj content =
  let ls = lines content
      verts = [parseVertex l | l <- ls, "v " `isPrefixOf` l]
      faces = [parseFace l verts (Vec3 1.0 1.0 1.0) | l <- ls, "f " `isPrefixOf` l] -- Default white color
   in (verts, concat faces)

parseVertex :: String -> Vec3
parseVertex line =
  case words line of
    ["v", vx, vy, vz] -> Vec3 (read vx) (read vy) (read vz)
    _ -> error "Invalid vertex format in .obj file"

parseFace :: String -> [Vec3] -> Vec3 -> [Triangle]
parseFace line verts inColor =
  case words line of
    ("f" : indices)
      | length indices >= 3 ->
          let faceVerts = map (read . takeWhile (/= '/')) indices -- Extract vertex indices
              vertexCount = length faceVerts
           in if vertexCount == 3
                then
                  [ Triangle
                      (verts !! (head faceVerts - 1))
                      (verts !! (faceVerts !! 1 - 1)) -- Swapped order
                      (verts !! (faceVerts !! 2 - 1))
                      inColor -- Swapped order
                  ]
                else
                  [ Triangle
                      (verts !! (head faceVerts - 1))
                      (verts !! (faceVerts !! (i + 1) - 1))
                      (verts !! (faceVerts !! i - 1))
                      inColor
                    | i <- [1 .. vertexCount - 2]
                  ]
    _ -> error "Invalid face format in .obj file"

loadObjWithOffset :: FilePath -> Vec3 -> IO HittableList
loadObjWithOffset path offset = do
  content <- readFile path
  if null content
    then do
      putStrLn "Error: OBJ file is empty or could not be read."
      return $ HittableList []
    else do
      let (_, triangles) = parseObj content
          offsetTriangle (Triangle a b c col) =
            Triangle (add a offset) (add b offset) (add c offset) col
          colors = cycle [Vec3 1 0 0, Vec3 0 1 0, Vec3 0 0 1, Vec3 1 1 0, Vec3 1 0 1, Vec3 0 1 1]
          coloredTriangles = zipWith (\t c -> offsetTriangle t {color = c}) triangles colors
      return $ HittableList [SomeHittable t | t <- coloredTriangles]