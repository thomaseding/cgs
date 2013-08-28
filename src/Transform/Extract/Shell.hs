{-# LANGUAGE ViewPatterns #-}

module Transform.Extract.Shell (
      extractor
    ) where


import Control.Monad.State.Lazy
import Hoops.Match
import Hoops.SyntaxToken
import Language.Cpp.Pretty
import Transform.Extract.Common


extractor :: BlockKind -> ExtractFunc
extractor kind = case kind of
    "HC_Insert_Shell" -> runShellExtractor . extract
    _ -> const $ return Nothing


type Number = SyntaxToken Hoops
type Point = (Number, Number, Number)
type Index = Integer


data ShellState = ShellState {
      points :: [Point]
    , faces :: [Index]
    }


type ShellExtractor = StateT ShellState Extractor


runShellExtractor :: ShellExtractor a -> Extractor a
runShellExtractor = flip evalStateT st
    where 
        st = ShellState {
              points = []
            , faces = []
            }


extract :: [SyntaxToken Hoops] -> ShellExtractor (Maybe [SyntaxToken Hoops])
extract = let
    point = match "points[$int].x = $num; points[$int].y = $num; points[$int].z = $num;"
    list = match "list[$!int] = $int;"
    define = match "DEFINE($!var($!args),$key);"
    in \tokens -> case tokens of
        (point -> CapturesRest [Integer i, x, Integer j, y, Integer k, z] rest) -> do
            if i /= j || j /= k
                then return Nothing
                else do
                    let p = (x, y, z)
                    modify $ \st -> st { points = p : points st }
                    extract rest
        (list -> CapturesRest [Integer idx] rest) -> do
            modify $ \st -> st { faces = idx : faces st }
            extract rest
        (define -> Captures [Ext (Key key)]) -> do
            ps <- gets points
            fs <- gets faces
            path <- lift $ scopedEntry "shell" "hmf" $ \entry -> do
                tellEntry entry $ toHmf ps fs
                return $ entryPath entry
            return $ Just $ cgsReadMetafile path $ Just key
        _ : rest -> extract rest
        [] -> return Nothing


toHmf :: [Point] -> [Index] -> String
toHmf ps fs = "(Shell (" ++ showPoints ps ++ ")\n\t(" ++ showFaces fs ++ ")"
    where
        showNum x = filter (/= '\n') $ pretty expandHoops [x]
        showPoint (x, y, z) = "\n\t(" ++ unwords (map showNum [x, y, z]) ++ ")"
        showPoints = concatMap showPoint
        showFaces = unwords . map show



