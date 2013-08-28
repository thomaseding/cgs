{-# LANGUAGE ViewPatterns #-}

module Transform.Extract.Shell (
      extractor
    ) where


import Control.Exception (assert)
import Control.Monad.State.Lazy
import Hoops.Match
import Hoops.SyntaxToken
import Language.Cpp.Pretty
import Transform.Extract.Common


extractor :: BlockKind -> ExtractFunc
extractor kind = case kind of
    "HC_Insert_Shell" -> Just . runShellExtractor . extract
    _ -> const Nothing


type Number = SyntaxToken Hoops
type Point = (Number, Number, Number)
type Index = Integer


data ShellState = ShellState {
      points :: [Point]
    , faces :: [Index]
    , failure :: Bool
    }


type ShellExtractor = StateT ShellState Extractor


runShellExtractor :: ShellExtractor a -> Extractor a
runShellExtractor = flip evalStateT $ ShellState {
      points = []
    , faces = []
    , failure = False
    }


extract :: [SyntaxToken Hoops] -> ShellExtractor [SyntaxToken Hoops]
extract = let
    point = match "points[$int].x = $num; points[$int].y = $num; points[$int].z = $num;"
    list = match "list[$!int] = $int;"
    define = match "DEFINE($!var($!args),$key);"
    in \tokens -> case tokens of
        (point -> CapturesRest [Integer i, x, Integer j, y, Integer k, z] rest) -> do
            if i /= j || j /= k
                then undefined
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
            path <- lift $ scopedEntry "shell" $ \entry -> do
                tellEntry entry $ toHmf ps fs
                return $ entryPath entry
            return $ cgsReadMetafile path $ Just key
        _ : rest -> extract rest


toHmf :: [Point] -> [Index] -> String
toHmf ps fs = "(Shell (" ++ showPoints ps ++ ")(" ++ showFaces fs ++ ")"
    where
        showNum x = pretty expandHoops [x]
        showPoint (x, y, z) = unwords $ map showNum [x, y, z]
        showPoints = concatMap showPoint
        showFaces = unwords . map show



