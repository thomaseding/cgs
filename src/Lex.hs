{-# LANGUAGE ViewPatterns #-}

module Lex (
      runLexer
    , C.ParseError
    ) where


import Data.Char
import Hoops
import qualified Language.Cpp.Lex as C
import Language.Cpp.SyntaxToken


runLexer :: Code -> Either C.ParseError [SyntaxToken Hoops]
runLexer = fmap massage . C.runLexer


massage :: [SyntaxToken Hoops] -> [SyntaxToken Hoops]
massage = keys . map removeK . filter (/= Comment)


viewpunc :: SyntaxToken Hoops -> String
viewpunc tok = case tok of
    Punctuation p -> unpunc p
    _ -> ""


i :: String -> SyntaxToken Hoops
i = Identifier


p :: String -> SyntaxToken Hoops
p = Punctuation . punc


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


removeK :: SyntaxToken Hoops -> SyntaxToken Hoops
removeK tok = case tok of
    Identifier ('H':'C':'_':'K':c:cs) -> if isUpper c
        then Identifier $ "HC_" ++ [c] ++ cs
        else tok
    _ -> tok



