module Cgs (
    main
) where


import Cgs.Args (parseArgsIO, CgsOptions(..))
import Control.Monad.Identity (runIdentity)
import Data.Tagged (untag)
import Hoops.Lex (runLexer)
import Hoops.SyntaxToken
import Language.Cpp.Pretty (pretty)
import System.Directory (getCurrentDirectory, createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import Text.Indent (indent, IndentMode(KeepOldTabs), Tagged, CodeGen)
import Transform.Expand (expand)
import Transform.Extract (extract, ExtractOptions)
import Transform.Flatten (flatten)
import Transform.Merge (merge)
import Transform.Nop (removeNopPairs, removeUnusedDefines)


main :: IO ()
main = do
    mCgsOpts <- parseArgsIO
    case mCgsOpts of
        Nothing -> badArgs
        Just cgsOpts -> do
            let getOpt f = runIdentity $ f cgsOpts
            toks <- readFile (getOpt cgsFile) >>= lexCode
            toks' <- runExtract (cgsExtractOpts cgsOpts) toks
            putStrLn $ additionalIndent $ pretty expandHoops $ cgs toks'


badArgs :: IO ()
badArgs = putStrLn "Could not parse command line arguments"


additionalIndent :: String -> String
additionalIndent = untag . (indent KeepOldTabs :: String -> Tagged CodeGen String)


runExtract :: ExtractOptions -> [SyntaxToken Hoops] -> IO [SyntaxToken Hoops]
runExtract opts toks = do
    cwd <- getCurrentDirectory
    let extractDir = cwd </> "extract"
    createDirectoryIfMissing True extractDir
    extract opts extractDir toks


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
lexCode code = case runLexer code of
    Left err -> print err >> exitFailure
    Right ts -> return ts




