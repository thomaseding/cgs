{-# LANGUAGE ViewPatterns #-}

module Transform.Extract.Shell (
    extractor
) where


import Hoops.Match
import Hoops.SyntaxToken
import Transform.Extract.Common
import Transform.Extract.Util


extractor :: BlockKind -> ExtractFunc
extractor kind = case kind of
    "HC_Insert_Shell" -> evalListsExtractor . extract
    _ -> const $ return Nothing


extract :: [SyntaxToken Hoops] -> ListsExtractor PointT FaceT (Maybe [SyntaxToken Hoops])
extract = let
    point = match "points[$!int].x = $num; points[$!int].y = $num; points[$!int].z = $num;"
    list = match "list[$!int] = $int;"
    define = match "DEFINE($!var($!args),$key);"
    in \tokens -> case tokens of
        (point -> CapturesRest [double -> Just x, double -> Just y, double -> Just z] rest) -> do
            modifyPoints ((x, y, z) :)
            extract rest
        (list -> CapturesRest [Integer idx] rest) -> do
            modifyFaces $ (idx :)
            extract rest
        (define -> Captures [Ext (Key key)]) -> do
            ps <- getList points
            fs <- getList faces
            path <- lift $ scopedEntry "shell" "hmf" $ \entry -> do
                tellEntry entry $ toHmf ps fs
                return $ entryPath entry
            return $ Just $ cgsReadMetafile path $ Just key
        _ : rest -> extract rest
        [] -> return Nothing


toHmf :: [Point] -> [FaceIndex] -> String
toHmf ps fs = "(Shell (" ++ showPoints ps ++ ")\n\t(" ++ showFaces fs ++ ")"
    where
        showPoint (x, y, z) = "\n\t(" ++ unwords (map show [x, y, z]) ++ ")"
        showPoints = concatMap showPoint
        showFaces = unwords . map show






