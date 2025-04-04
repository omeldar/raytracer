module Parser.Object (loadObjWithOffset) where

import Core.Vec3
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Hittable.HittableList
import Hittable.Objects.Triangle
import Rendering.Material

parseObj :: String -> Vec3 -> MaterialType -> ([Vec3], [Triangle])
parseObj content inColor inMaterial =
  let ls = lines content
      verts = [parseVertex l | l <- ls, "v " `isPrefixOf` l]
      faces = [parseFace l verts inColor inMaterial | l <- ls, "f " `isPrefixOf` l]
   in (verts, concat faces)

parseVertex :: String -> Vec3
parseVertex line =
  case words line of
    ["v", vx, vy, vz] -> Vec3 (read vx) (read vy) (read vz)
    _ -> error "Invalid vertex format in .obj file"

parseFace :: String -> [Vec3] -> Vec3 -> MaterialType -> [Triangle]
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

loadObjWithOffset :: FilePath -> Vec3 -> Maybe Vec3 -> Maybe MaterialType -> IO HittableList
loadObjWithOffset path offset overrideColor overrideMaterial = do
  content <- readFile path
  if null content
    then do
      putStrLn "Error: OBJ file is empty or could not be read."
      return $ HittableList []
    else do
      let inColor = fromMaybe (Vec3 1 1 1) overrideColor
          inMaterial = fromMaybe Lambertian overrideMaterial
          (_, triangles) = parseObj content inColor inMaterial
          offsetTriangle (Triangle a b c col mat) =
            Triangle (add a offset) (add b offset) (add c offset) col mat
      return $ HittableList [SomeHittable t | t <- map offsetTriangle triangles]