module Cgs (
      main
    ) where


import Data.Char
import Data.Tagged
import Hoops
import Language.Cpp.Lex hiding (main)
import Language.Cpp.Pretty
import Language.Cpp.SyntaxToken
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
cgs = removeNopPairs . removeUnusedDefines . removeNopPairs


lexCode :: Code -> IO [SyntaxToken Hoops]
lexCode code = do
    case runLexer code of
        Left err -> print err >> exitFailure
        Right ts -> return $ massage ts


massage :: [SyntaxToken Hoops] -> [SyntaxToken Hoops]
massage = map removeK . filter (/= Comment)
    where
        removeK tok = case tok of
            Identifier ('H':'C':'_':'K':c:cs) -> if isUpper c
                then Identifier $ "HC_" ++ [c] ++ cs
                else tok
            _ -> tok



