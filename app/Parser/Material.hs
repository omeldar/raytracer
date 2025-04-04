module Parser.Material where

import qualified Data.Map as M
import Rendering.Material
import Core.Vec3 (Vec3 (..))

type MaterialMap = M.Map String (V.Vec3, MaterialType)

parseMaterial :: String -> MaterialMap
parseMaterial content = parseLines (lines content) M.empty Nothing
    where
        parseLines [] acc _ = acc
        parseLines (l:ls) acc currentName =
            case worlds l of
                ("newmtl" : name : _) -> parseLines ls acc (Just name)
                ("Kd" : r : g : b : _) -> 
                    case currentName of
                        Just name -> 
                            let color = Vec3 (read r) (read g) (read b)
                            in parseLines ls (M.insert name (color, Lambertian) acc) name
                        Nothing -> parseLines ls acc currentName
                _ -> parseLines ls acc currentName