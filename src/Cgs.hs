module Cgs (
      main
    ) where


import Data.Char
import Data.Tagged
import ExpandKeys
import Hoops
import Language.Cpp.Pretty
import Language.Cpp.SyntaxToken
import Lex
import Nop
import System.Environment
import System.Exit
import Text.Indent


main :: IO ()
main = do
    args <- getArgs
    toks <- case args of
        [] -> getContents >>= lexCode
        [file] -> readFile file >>= lexCode
    putStrLn $ additionalIndent $ pretty expandHoops $ cgs toks


additionalIndent :: String -> String
additionalIndent = untag . (indent KeepOldTabs :: String -> Tagged CodeGen String)


cgs :: [SyntaxToken Hoops] -> [SyntaxToken Hoops]
cgs = removeNopPairs . removeUnusedDefines . removeNopPairs . expandKeys


lexCode :: Code -> IO [SyntaxToken Hoops]
lexCode code = do
    case runLexer code of
        Left err -> print err >> exitFailure
        Right ts -> return ts




