{-# LANGUAGE ViewPatterns #-}

module Transform.Extract.VertexNormals (
    extractor
) where


import Hoops.Match
import Hoops.SyntaxToken
import Transform.Extract.Common
import Transform.Extract.Util


extractor :: BlockKind -> ExtractFunc
extractor kind = case kind of
    "HC_MSet_Vertex_Normals" -> evalListsExtractor . extract
    _ -> const $ return Nothing


extract :: ListsExtractFunc VectorsT
extract = let
    normal = match "normals[$!int].x = $num; normals[$!int].y = $num; normals[$!int].z = $num;"
    args = match "HC_MSet_Vertex_Normals(LOOKUP($key), $int, $int, normals);"
    in \tokens -> case tokens of
        (normal -> CapturesRest [double -> Just x, double -> Just y, double -> Just z] rest) -> do
            modifyVectors (Vector x y z :)
            extract rest
        (args -> Captures [Ext (Key key), Integer offset, Integer count]) -> do
            ns <- gets vectors
            path <- lift $ scopedEntry "vertex-normals" "txt" $ \entry -> do
                tellEntry entry $ showVectors ns
                return $ entryPath entry
            return $ Just $ cgsMsetVertexNormals path key offset count
        _ : rest -> extract rest
        [] -> return Nothing


showVector :: Vector -> String
showVector (Vector x y z) = unwords $ map show [x, y, z]


showVectors :: [Vector] -> String
showVectors = unlines . map showVector






