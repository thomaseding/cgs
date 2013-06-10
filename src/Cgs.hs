module Cgs (
      main
    ) where


import Data.Tagged
import Hoops.Lex
import Hoops.SyntaxToken
import Language.Cpp.Pretty
import System.Environment
import System.Exit
import Text.Indent
import Transform.Expand
import Transform.Flatten
import Transform.Merge
import Transform.Nop


main :: IO ()
main = do
    args <- getArgs
    toks <- case args of
        [] -> getContents >>= lexCode
        [file] -> readFile file >>= lexCode
        _ -> exitFailure
    putStrLn $ additionalIndent $ pretty expandHoops $ cgs toks


additionalIndent :: String -> String
additionalIndent = untag . (indent KeepOldTabs :: String -> Tagged CodeGen String)


cgs :: [SyntaxToken Hoops] -> [SyntaxToken Hoops]
cgs = id
    . iterateMaybe (fmap merge . removeNopPairs)
    . merge
    . flatten
    . removeUnusedDefines
    . iterateMaybe removeNopPairs
    . expand


iterateMaybe :: (a -> Maybe a) -> a -> a
iterateMaybe f x = case f x of
    Nothing -> x
    Just y -> iterateMaybe f y


lexCode :: Code -> IO [SyntaxToken Hoops]
lexCode code = do
    case runLexer code of
        Left err -> print err >> exitFailure
        Right ts -> return ts




