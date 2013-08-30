{-# LANGUAGE ViewPatterns #-}

module Transform.Extract.Glyph (
    extractor
) where


import Hoops.Match
import Hoops.SyntaxToken
import Transform.Extract.Common
import Transform.Extract.Util


extractor :: BlockKind -> ExtractFunc
extractor kind = case kind of
    "HC_Define_Glyph" -> extract
    _ -> const $ return Nothing


extract :: ExtractFunc
extract = let
    definition = match "definition[$!int] = (char) $int;"
    glyph = match "HC_Define_Glyph($str, $!int, $!var);"
    --
    go :: Entry -> ListsExtractFunc IntegersT
    go entry tokens = case tokens of
        (definition -> CapturesRest [Integer num] rest) -> do
            modifyIntegers $ (num :)
            go entry rest
        (glyph -> CapturesRest [String name] rest) -> do
            lift $ tellEntry entry $ "Glyph (" ++ show name ++ " ("
            go entry rest
        _ : rest -> go entry rest
        [] -> return $ Just $ cgsReadMetafile (entryPath entry) Nothing
    in \tokens -> scopedEntry "glyph" "hmf" $ \entry -> evalListsExtractor $ do
        res <- go entry tokens
        vals <- fmap reverse $ getList integers
        lift $ do
            tellEntry entry $ unwords $ map show vals
            tellEntry entry "))"
        return res





