{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Cgs (
    main
) where


import Cgs.Args (runParseArgsIO, CgsOptions(..))
import Control.Exception (assert)
import Control.Monad ((<=<))
import Control.Monad.Identity (runIdentity)
import Control.Monad.ListM (takeWhileM)
import Control.Monad.State.Lazy (evalState, modify, gets)
import Data.List (isPrefixOf)
import Data.Tagged (untag)
import Hoops.Lex (runLexer)
import Hoops.Match
import Hoops.SyntaxToken
import Language.Cpp.Pretty (pretty)
import System.Directory (getCurrentDirectory, createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (stderr, hPutStrLn)
import Text.Indent (indent, IndentMode(DropOldTabs), Tagged, CodeGen)
import Text.Parsec (ParseError)
import Transform.Expand (expand)
import Transform.Extract (extract, ExtractOptions)
import Transform.Flatten (flatten)
import Transform.Merge (merge)
import Transform.Nop (removeNopPairs, removeUnusedDefines)


main :: IO ()
main = do
    mCgsOpts <- runParseArgsIO
    case mCgsOpts of
        Left err -> badArgs err
        Right cgsOpts -> do
            let getOpt f = runIdentity $ f cgsOpts
            tokss <- mapM (lexCode <=< readFile) $ getOpt cgsFiles
            let content = catMainContent tokss
            content' <- runExtract (cgsExtractOpts cgsOpts) content
            let cgsContent = cgs content'
                cgsContents = chunkStmts (cgsChunkSize cgsOpts) cgsContent
                tus = zipWith compileTranslationUnit [0 ..] cgsContents
            mapM_ (putStrLn . reindent . pretty expandHoops) tus


badArgs :: ParseError -> IO ()
badArgs err = hPutStrLn stderr $ show err


compileTranslationUnit :: Int -> [SyntaxToken Hoops] -> [SyntaxToken Hoops]
compileTranslationUnit unitId tokens = prelude ++ tokens ++ prologue
    where
        code = either (const $ assert False undefined) id . runLexer
        codeChain = "code_chain_" ++ show unitId
        prelude = code $ unlines $ [
            "#include \"cgs.h\"\n",
            "int " ++ codeChain ++ " () {\n" ]
        prologue = code $ unlines [
            "return 0;",
            "}\n" ]


data ChunkState = ChunkState {
    consumedStmts :: Int,
    braceBalance :: Int
}


chunkByM :: (Monad m) => (a -> m Bool) -> [a] -> m [[a]]
chunkByM _ [] = return []
chunkByM p (x:xs) = do
    keep <- p x
    xss <- chunkByM p xs
    let xss' = case xss of
            ys : yss -> (x:ys) : yss
            [] -> [[x]]
    return $ if keep
        then xss'
        else [] : xss'


chunkStmts :: Int -> [SyntaxToken Hoops] -> [[SyntaxToken Hoops]]
chunkStmts chunkSize = flip evalState baseSt . chunkByM chunker
    where
        baseSt = ChunkState {
            consumedStmts = 0,
            braceBalance = 0 }
        chunker token = do
            balance <- gets braceBalance
            numConsumed <- gets consumedStmts
            placeInCurrChunk <- if balance == 0 && numConsumed >= chunkSize
                then do
                    modify $ \st -> st { consumedStmts = 0 }
                    return False
                else return True
            case token of
                Punctuation (unpunc -> ";") -> do
                    modify $ \st -> st { consumedStmts = consumedStmts st + 1 }
                Punctuation (unpunc -> "{") -> do
                    modify $ \st -> st { braceBalance = braceBalance st + 1 }
                Punctuation (unpunc -> "}") -> do
                    modify $ \st -> st { braceBalance = braceBalance st - 1 }
                _ -> return ()
            return placeInCurrChunk


reindent :: String -> String
reindent = untag . (indent DropOldTabs :: String -> Tagged CodeGen String)


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


catMainContent :: [[SyntaxToken Hoops]] -> [SyntaxToken Hoops]
catMainContent = concatMap extractMainContent


extractMainContent :: [SyntaxToken Hoops] -> [SyntaxToken Hoops]
extractMainContent = let
    mainFunc = match "int main ($!args) {"
    codeChainSig = match "int $var ($!args) {"
    codeChainVar = isPrefixOf "code_chain_"
    codeChainFunc tokens = case tokens of
        (codeChainSig -> CapturesRest [Identifier (codeChainVar -> True)] rest) -> Rest rest
        _ -> NoRest
    dropBoringDecls = match $ concat [
        "char string_buffer[256];",
        "float ff;",
        "int ii;",
        "long ll;",
        "HC_KEY key;",
        "float matrix[16];" ]
    go tokens = case extractBody tokens of
        (dropBoringDecls -> Rest rest) -> rest
        ts -> ts
    in \tokens -> case tokens of
        (mainFunc -> Rest rest) -> go rest
        (codeChainFunc -> Rest rest) -> go rest
        _ : rest -> extractMainContent rest
        [] -> []


extractBody :: [SyntaxToken Hoops] -> [SyntaxToken Hoops]
extractBody = flip evalState (0 :: Int) . takeWhileM f
    where
        f token = case token of
            Punctuation (unpunc -> sym) -> case sym of
                "{" -> modify (+ 1) >> return True
                "}" -> modify (subtract 1) >> gets (>= 0)
                _ -> return True
            Keyword (unkw -> name) -> return $ case name of
                "return" -> False
                _ -> True
            Identifier name -> return $ not $ "code_chain_" `isPrefixOf` name
            _ -> return True







