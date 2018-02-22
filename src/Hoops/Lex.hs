{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Hoops.Lex (
    runLexer,
    C.ParseError
) where


import Control.Exception (assert)
import Data.Char (isUpper)
import Data.List (sortBy, find, isPrefixOf)
import Data.Ord (comparing)
import Hoops.Match
import Hoops.SyntaxToken
import qualified Language.Cpp.Lex as C (runLexer, ParseError)


runLexer :: Code -> Either C.ParseError [SyntaxToken Hoops]
runLexer = fmap massage . C.runLexer


massage :: [SyntaxToken Hoops] -> [SyntaxToken Hoops]
massage = segPaths . keys . map removeK . filter (/= Comment)


i :: String -> SyntaxToken Hoops
i = Identifier


p :: String -> SyntaxToken Hoops
p = Punctuation . punc


data PathRule = Infer | ForceRelative


segPaths :: [SyntaxToken Hoops] -> [SyntaxToken Hoops]
segPaths = let
    matchers = [
        matchBy "HC_Begin_Contents_Search($str, $!str)" [(0, Infer)]
      , matchBy "HC_Conditional_Include($str, $!str)" [(0, Infer)]
      , matchBy "HC_Conditional_Style($str, $!str)" [(0, Infer)]
      , matchBy "HC_Create_Segment_Key_By_Key($!key,$str)" [(0, ForceRelative)]
      , matchBy "HC_Create_Segment($str)" [(0, Infer)]
      , matchBy "HC_Define_Alias($!str, $str)" [(1, Infer)]
      , matchBy "HC_Delete_Segment($str)" [(0, Infer)]
      , matchBy "HC_Flush_Contents($str, $!str)" [(0, Infer)]
      , matchBy "HC_Include_Segment($str)" [(0, Infer)]
      , matchBy "HC_Move_Segment($str, $!str)" [(0, Infer)] -- TODO: If moved by wildcard, things can get tricky. Currently not handling that case
      , matchBy "HC_Open_Segment($str)" [(0, Infer)]
      , matchBy "HC_Open_Segment_Key_By_Key($!key,$str)" [(0, ForceRelative)]
      , matchBy "HC_Rename_Segment($str, $!str)" [(0, Infer)] -- TODO: If renamed by wildcard, things can get tricky. Currently not handling that case
      , matchBy "HC_Style_Segment($str)" [(0, Infer)]
      , matchBy "HC_Update_One_Display($str)" [(0, Infer)]
      ]
    matchBy str idxs = (match str, sortBy (comparing fst) idxs)
    undetail idxs d = case d of
        Lit t -> t
        Wild idx t -> case find (\(idx', _) -> idx == idx') idxs of
            Just (_, rule) -> Ext $ SegPath $ mkSegPath $ case t of
                String str -> case rule of
                    Infer -> str
                    ForceRelative -> case str of
                        '/' : str' -> str'
                        _ -> str
                _ -> assert False undefined
            Nothing -> t
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
                NoCapturesRest -> t : qSet ts
                CapturesRest [Identifier var, String str] rest -> if any (`isPrefixOf` var) ["HC_QSet_", "HC_QUnSet_"]
                    then Identifier var : p "(" : Ext (SegPath $ mkSegPath str) : qSet rest
                    else t : qSet ts
                _ -> assert False undefined
    in \ts -> qSet $ go matchers ts


dropEnd :: Int -> [a] -> [a]
dropEnd n xs = take (length xs - n) xs


keys :: [SyntaxToken Hoops] -> [SyntaxToken Hoops]
keys = let
    lookup1 = match "LOOKUP($int)"
    lookup2 = match "LOOKUP($int,$int)"
    define1 = match "DEFINE($!var($!args),$int)"
    define2 = match "DEFINE($!var($!args),$int,$int)"
    mkKey' x my = mkKey (x, my `asTypeOf` Just x)
    mkLookup k1 mK2 = i "LOOKUP" : p "(" : Ext (Key $ mkKey' k1 mK2) : p ")" : []
    mkDefine prefix k1 mK2 = let
        dropAmount = maybe 2 (const 4) mK2
        in keys $ dropEnd dropAmount prefix ++ [Ext $ Key $ mkKey' k1 mK2, p ")"]
    in \toks -> case toks of
        (lookup1 -> CapturesRest [Integer k] rest) -> mkLookup k Nothing ++ keys rest
        (lookup2 -> CapturesRest [Integer k1, Integer k2] rest) -> mkLookup k1 (Just k2) ++ keys rest
        (define1 -> PrefixCapturesRest prefix [Integer k] rest) -> mkDefine prefix k Nothing ++ keys rest
        (define2 -> PrefixCapturesRest prefix [Integer k1, Integer k2] rest) -> mkDefine prefix k1 (Just k2) ++ keys rest
        t : ts -> t : keys ts
        [] -> []


removeK :: SyntaxToken Hoops -> SyntaxToken Hoops
removeK tok = case tok of
    Identifier name -> case name of
        "HC_KEY" -> tok
        'H':'C':'_':'K':c:cs -> if isUpper c
            then Identifier $ "HC_" ++ [c] ++ cs
            else tok
        _ -> tok
    _ -> tok



