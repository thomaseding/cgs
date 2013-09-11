{-# LANGUAGE ViewPatterns #-}

module Transform.Extract.Camera (
    extractor
) where


import Hoops.Match
import Hoops.SyntaxToken
import Transform.Extract.Common
import Transform.Extract.Util


extractor :: BlockKind -> ExtractFunc
extractor kind = case kind of
    "HC_Set_Camera" -> extract
    _ -> const $ return Nothing


extract :: ExtractFunc
extract = let
    point var = match $ var ++ ".x = $num;" ++ var ++ ".y = $num;" ++ var ++ ".z = $num"
    pos = point "position"
    tar = point "target"
    up = point "up_vector"
    fieldProj = match "HC_Set_Camera(&$!var, &$!var, &$!var, $num, $num, $str);"
    --
    go :: Entry -> ExtractFunc
    go entry tokens = case tokens of
        (pos -> CapturesRest [double -> Just x, double -> Just y, double -> Just z] rest) -> do
            tellEntry entry $ showPoint $ Point x y z
            go entry rest
        (tar -> CapturesRest [double -> Just x, double -> Just y, double -> Just z] rest) -> do
            tellEntry entry $ showPoint $ Point x y z
            go entry rest
        (up -> CapturesRest [double -> Just x, double -> Just y, double -> Just z] rest) -> do
            tellEntry entry $ showPoint $ Point x y z
            go entry rest
        (fieldProj -> Captures [double -> Just w, double -> Just h, String proj]) -> do
            tellEntry entry $ "\n\t" ++ show w ++ "\n\t" ++ show h ++ "\n\t" ++ show proj ++ ")"
            return $ Just $ cgsReadMetafile (entryPath entry) Nothing
        _ : rest -> go entry rest
        [] -> return Nothing
    in \tokens -> scopedEntry "camera" "hmf" $ \entry -> do
        tellEntry entry $ "(Camera "
        go entry tokens


showPoint :: Point -> String
showPoint (Point x y z) = "\n\t(" ++ unwords (map show [x, y, z]) ++ ")"






