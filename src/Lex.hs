{-# LANGUAGE ViewPatterns #-}

module Lex (
      runLexer
    , C.ParseError
    ) where


import Control.Monad.State.Strict
import Data.Char
import Data.List
import Hoops
import qualified Language.Cpp.Lex as C
import Language.Cpp.SyntaxToken
import Match
import SegPath


runLexer :: Code -> Either C.ParseError [SyntaxToken Hoops]
runLexer = fmap massage . C.runLexer


massage :: [SyntaxToken Hoops] -> [SyntaxToken Hoops]
massage = segPaths . keys . map removeK . filter (/= Comment)


viewpunc :: SyntaxToken Hoops -> String
viewpunc tok = case tok of
    Punctuation p -> unpunc p
    _ -> ""


i :: String -> SyntaxToken Hoops
i = Identifier


p :: String -> SyntaxToken Hoops
p = Punctuation . punc


segPaths :: [SyntaxToken Hoops] -> [SyntaxToken Hoops]
segPaths = let
    matchers = [
        matchBy "HC_Begin_Contents_Search($str, $str)" [0]
      , matchBy "HC_Conditional_Include($str, $str)" [0]
      , matchBy "HC_Conditional_Style($str, $str)" [0]
      , matchBy "HC_Create_Segment($str)" [0]
      , matchBy "HC_Define_Alias($str, $str)" [0, 1]
      , matchBy "HC_Delete_Segment($str)" [0]
      , matchBy "HC_Flush_Contents($str, $str)" [0]
      , matchBy "HC_Include_Segment($str)" [0]
      , matchBy "HC_Move_Segment($str, $str)" [0]
      , matchBy "HC_Open_Segment($str)" [0]
      , matchBy "HC_Rename_Segment($str, $str)" [0]
      , matchBy "HC_Style_Segment($str)" [0]
      , matchBy "HC_Update_One_Display($str)" [0]
      ]
    matchBy str idxs = (match str, nub $ sort idxs)
    undetail idxs d = case d of
        Lit t -> t
        Wild idx t -> if idx `elem` idxs
            then Ext $ SegPath $ mkSegPath $ case t of
                String str -> str
            else t
    go _ [] = []
    go [] (t:ts) = t : go matchers ts
    go ((m, idxs) : ms) ts = case m ts of
        NoDetailedRest -> go ms ts
        DetailedRest ds rest -> let
            ts' = map (undetail idxs) ds
            in ts' ++ go matchers rest
    qSet = let
        m = match "$var($str"
        in \toks -> case toks of
            [] -> []
            t : ts -> case m toks of
                NoWildsRest -> t : qSet ts
                WildsRest [Identifier var, String str] rest -> if any (`isPrefixOf` var) ["HC_QSet_", "HC_QUnSet_"]
                    then Identifier var : p "(" : Ext (SegPath $ mkSegPath str) : qSet rest
                    else t : qSet ts
    in \ts -> qSet $ go matchers ts


keys :: [SyntaxToken Hoops] -> [SyntaxToken Hoops]
keys ts0 = let
    continue = case ts0 of
        t : ts -> t : keys ts
        [] -> []
    (ts1, name) = case ts0 of
        Identifier name' : ts -> case name' of
            "DEFINE" -> (ts, name')
            "LOOKUP" -> (ts, name')
            _ -> ([], undefined)
        _ -> ([], undefined)
    in case ts1 of
        (viewpunc -> "(") : ts2 -> case ts2 of
            Integer x : ts3 -> let
                mKeyRest = case ts3 of
                    (viewpunc -> ",") : Integer y : (viewpunc -> ")") : rest -> Just (mkKey (x, y), rest)
                    (viewpunc -> ")") : rest -> Just (mkKey x, rest)
                    _ -> Nothing
                in case mKeyRest of
                    Just (key, rest) -> i name : p "(" : Ext (Key key) : p ")" : keys rest
                    Nothing -> continue
            _ -> continue
        _ -> continue


removeK :: SyntaxToken Hoops -> SyntaxToken Hoops
removeK tok = case tok of
    Identifier "HC_KEY" -> tok
    Identifier ('H':'C':'_':'K':c:cs) -> if isUpper c
        then Identifier $ "HC_" ++ [c] ++ cs
        else tok
    _ -> tok



