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
import Transform.ExpandKeys
import Transform.Flatten
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
cgs = removeNopPairs . flatten . removeUnusedDefines . removeNopPairs . expandKeys


lexCode :: Code -> IO [SyntaxToken Hoops]
lexCode code = do
    case runLexer code of
        Left err -> print err >> exitFailure
        Right ts -> return ts




