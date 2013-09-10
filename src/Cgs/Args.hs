module Cgs.Args (
    CgsOptions(..),
    parseArgs,
    parseArgsIO
) where


import Control.Monad.Identity (Identity(Identity))
import Control.Monad.State (State, execState, modify)
import Data.Char (isDigit)
import Data.Function (on)
import Data.List (groupBy, sortBy)
import System.Environment (getArgs)
import System.FilePath (takeBaseName)
import Text.Parsec hiding (satisfy)
import Transform.Extract (ExtractOptions(..))


data CgsOptions a = CgsOptions {
    cgsFiles :: a [FilePath],
    cgsChunkSize :: Int,
    cgsExtractOpts :: ExtractOptions }


type Parser a = Parsec [String] (CgsOptions Maybe) a


collapse :: (CgsOptions Maybe) -> Maybe (CgsOptions Identity)
collapse CgsOptions {
    cgsFiles = Just files,
    cgsChunkSize = chunkSize,
    cgsExtractOpts = opts } = Just $ CgsOptions {
        cgsFiles = Identity files,
        cgsChunkSize = chunkSize,
        cgsExtractOpts = opts }
collapse _ = Nothing


parseArgsIO :: IO (Either ParseError (CgsOptions Identity))
parseArgsIO = fmap runParseArgs getArgs


runParseArgs :: [String] -> Either ParseError (CgsOptions Identity)
runParseArgs = runParser parseArgs st source
    where
        source = ""
        st = CgsOptions {
            cgsFiles = Nothing,
            cgsChunkSize = 4000,
            cgsExtractOpts = extractOpts }
        extractOpts = ExtractOptions {
            minStmtRequirement = 0 }


parseArgs :: Parser (CgsOptions Identity)
parseArgs = do
    parseTaggedOpts
    parseUntaggedOpts
    mState <- fmap collapse getState
    case mState of
        Nothing -> parserFail "Missing arguments"
        Just state -> return state


parseTaggedOpts :: Parser ()
parseTaggedOpts = return ()


parseUntaggedOpts :: Parser ()
parseUntaggedOpts = parseFiles


parseFiles :: Parser ()
parseFiles = do
    files <- many1 anyArg <?> "Missing input files"
    let files' = sortBy (humanOrdering `on` takeBaseName) files
    modifyState $ \st -> st { cgsFiles = Just files' }


data Human
    = NonDigit Char
    | Number Int
    deriving (Show, Eq, Ord)


toHuman :: [Char] -> [Human]
toHuman str = case span isDigit str of
    (ds, rest) -> case rest of
        c : cs -> case ds of
            [] -> NonDigit c : toHuman cs
            _ -> Number (read ds) : toHuman rest
        [] -> case ds of
            [] -> []
            _ -> [Number $ read ds]


humanOrdering :: String -> String -> Ordering
humanOrdering s1 s2 = compare h1 h2
    where
        grouper (NonDigit _) (NonDigit _) = True
        grouper (Number _) (Number _) = True
        grouper _ _ = False
        massage = groupBy grouper . toHuman
        h1 = massage s1
        h2 = massage s2


updatePosArg :: SourcePos -> String -> SourcePos
updatePosArg pos _ = incSourceColumn pos 1


satisfy :: (String -> Bool) -> Parser String
satisfy f = tokenPrim showArg nextPos testArg
    where
        showArg = show
        testArg arg = if f arg then Just arg else Nothing
        nextPos pos arg args = updatePosArg pos arg


anyArg :: Parser String
anyArg = satisfy $ const True







