{-# LANGUAGE ViewPatterns #-}

module Cgs.Args (
    CgsOptions(..),
    runParseArgs,
    runParseArgsIO
) where


import Control.Exception (assert)
import Control.Monad.Identity (Identity(Identity))
import Data.Char (isDigit, isAlphaNum)
import Data.Function (on)
import Data.List (groupBy, sortBy, isPrefixOf, stripPrefix)
import Data.Maybe (isJust)
import System.Environment (getArgs)
import System.FilePath (takeBaseName)
import Text.Parsec hiding (satisfy)
import Transform.Extract (ExtractOptions(..))


data CgsOptions a = CgsOptions {
    cgsFiles :: a [FilePath],
    cgsChunkSize :: Int,
    cgsExtractOpts :: ExtractOptions
}


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


runParseArgsIO :: IO (Either ParseError (CgsOptions Identity))
runParseArgsIO = fmap runParseArgs getArgs


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
parseTaggedOpts = do
    res <- parseTaggedOpt
    case res of
        DoesNotExist -> return ()
        Exists -> parseTaggedOpts


parseTaggedOpt :: Parser Existence
parseTaggedOpt = parseChunkSize


parseChunkSize :: Parser Existence
parseChunkSize = optionalArg HasAssign "--chunk-size" $ \val -> do
    case tryReadInt val of
        Nothing -> failure
        Just size -> if size > 0
            then do
                modifyState $ \st -> st { cgsChunkSize = size }
                return ()
            else failure
    where
        failure = parserFail "Invalid --chunk-size value"


parseUntaggedOpts :: Parser ()
parseUntaggedOpts = parseFiles


parseFiles :: Parser ()
parseFiles = do
    files <- many1 anyArg <?> "Missing input files"
    let unknownOpts = filter ("-" `isPrefixOf`) files
    case unknownOpts of
        opt : _ -> parserFail $ "Unknown option: " ++ opt
        [] -> return ()
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
        nextPos pos arg _ = updatePosArg pos arg


tryRead :: (Read a) => String -> Maybe a
tryRead s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing


tryReadInt :: String -> Maybe Int
tryReadInt s = case tryRead s :: Maybe Integer of
    Nothing -> Nothing
    Just n -> if fromIntegral (minBound :: Int) <= n && n <= fromIntegral (maxBound :: Int)
        then Just $ fromInteger n
        else Nothing


anyArg :: Parser String
anyArg = satisfy $ const True


optionBool :: Parser a -> Parser Bool
optionBool = fmap isJust . optionMaybe


data ArgKind = HasAssign
data Success = Success | Failure
data Existence = Exists | DoesNotExist


spanOption :: String -> Maybe (String, String)
spanOption str = case span p str of
    res @ ('-' : _, _) -> Just res
    _ -> Nothing
    where
        p x = isAlphaNum x || x == '-'


optionalArg :: ArgKind -> String -> (String -> Parser ()) -> Parser Existence
optionalArg kind name f = do
    mStr <- optionMaybe $ satisfy ((== Just name) . fmap fst . spanOption)
    case mStr of
        Just (spanOption -> Just (_, suffix)) -> case kind of
            HasAssign -> case suffix of
                '=' : value -> f value >> return Exists
                _ -> parserFail $ "Expected `=' after " ++ name
        _ -> return DoesNotExist






