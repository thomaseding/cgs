{-# LANGUAGE FlexibleInstances #-}

module SegPath (
      SegPath
    , mkSegPath
    , toString
    , isAbsolute
    , isRelative
    , isFromRoot
    , isFromAlias
    , isFromKey
    ) where


import Data.Char
import Data.List
import Data.Monoid
import Text.Parsec
import Text.Parsec.String


data SegPath
    = Absolute AbsoluteType
    | Relative [PathPart]
    deriving (Show, Eq, Ord)


type Key = String


data AbsoluteType
    = ByAlias String [PathPart]
    | ByRoot [PathPart]
    | ByKey Key [PathPart]
    deriving (Show, Eq, Ord)


data Name
    = Anonymous
    | Name String
    deriving (Show, Eq, Ord)


data PathPart
    = Child Name
    | Self
    | Parent
    deriving (Show, Eq, Ord)


mkSegPath :: String -> SegPath
mkSegPath str = case runParseSegPath str of
    Left err -> error $ show (str, err)
    Right sp -> sp


class Stringify a where
    stringify :: a -> String

instance Stringify SegPath where
    stringify s = case s of
        Relative ps -> case ps of
            [] -> "."
            _ -> join ps
        Absolute at -> case at of
            ByRoot ps -> '/' : join ps
            ByAlias alias ps -> '?' : alias ++ joinWith ps
            ByKey key ps -> '@' : stringify key ++ joinWith ps
        where
            join = intercalate "/" . map stringify
            joinWith ps = case ps of
                [] -> ""
                _ -> '/' : join ps


instance Stringify PathPart where
    stringify p = case p of
        Child name -> case name of
            Anonymous -> "{anon}"
            Name str -> str
        Parent -> ".."
        Self -> "."


instance Stringify Key where
    stringify = show


toString :: SegPath -> String
toString = stringify


isAbsolute :: SegPath -> Bool
isAbsolute s = case s of
    Absolute {} -> True
    Relative {} -> False


isRelative :: SegPath -> Bool
isRelative = not . isAbsolute


isFromAlias :: SegPath -> Bool
isFromAlias s = case s of
    Absolute (ByAlias {}) -> True
    _ -> False


isFromRoot :: SegPath -> Bool
isFromRoot s = case s of
    Absolute (ByRoot {}) -> True
    _ -> False


isFromKey :: SegPath -> Bool
isFromKey s = case s of
    Absolute (ByKey {}) -> True
    _ -> False


instance Monoid SegPath where
    mempty = Relative []
    mappend _ s@(Absolute {}) = s
    mappend (Relative ps) (Relative qs) = Relative $ simplify $ ps ++ qs
    mappend (Absolute at) (Relative qs) = Absolute $ case at of
        ByAlias alias ps -> ByAlias alias $ simplify ps ++ qs
        ByRoot ps -> ByRoot $ ps ++ qs
        ByKey key ps -> ByKey key $ ps ++ qs


class Simplify a where
    simplify :: a -> a


instance Simplify SegPath where
    simplify s = case s of
        Relative ps -> Relative $ simplify ps
        Absolute absType -> Absolute $ case absType of
            ByRoot ps -> ByRoot $ simplify ps
            ByAlias alias ps -> ByAlias alias $ simplify ps
            ByKey key ps -> ByKey key $ simplify ps


instance Simplify [PathPart] where
    simplify parts = case parts of
        [] -> []
        Self : ps -> simplify ps
        _ : Parent : ps -> simplify ps
        p : ps -> p : simplify ps


type SegParser a = Parser a


trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace


optionBool :: SegParser a -> SegParser Bool
optionBool = fmap (maybe False $ const True) . optionMaybe


runParseSegPath :: String -> Either ParseError SegPath
runParseSegPath s = case trim s of
    "" -> Right $ Relative [Child Anonymous]
    "/"-> Right $ Absolute $ ByRoot []
    s' -> runParser parseSegPath () "" $ trim s'


parseSegPath :: SegParser SegPath
parseSegPath = fmap simplify $ parseRelative <|> parseAbsolute


parseRelative :: SegParser SegPath
parseRelative = fmap Relative parsePathParts


parseAbsolute :: SegParser SegPath
parseAbsolute = fmap Absolute parseAbsoluteType


parseAbsoluteType :: SegParser AbsoluteType
parseAbsoluteType = parseByRoot <|> parseByAlias <|> parseByKey


parseByRoot :: SegParser AbsoluteType
parseByRoot = char '/' >> fmap ByRoot parsePathParts


parseByAlias :: SegParser AbsoluteType
parseByAlias = do
    char '?'
    alias <- parseUnquoted
    hasKids <- optionBool $ char '/'
    if hasKids
        then fmap (ByAlias alias) parsePathParts
        else return $ ByAlias alias []


parseByKey :: SegParser AbsoluteType
parseByKey = do
    char '@'
    key <- fmap (map toLower) $ many1 hexDigit
    hasKids <- optionBool $ char '/'
    if hasKids
        then fmap (ByKey key) parsePathParts
        else return $ ByKey key []



parsePathParts :: SegParser [PathPart]
parsePathParts = parsePathPart `sepBy1` char '/'

parsePathPart :: SegParser PathPart
parsePathPart = try dots <|> try parseParent <|> try parseSelf <|> parseChild
    where
        dots = do
            string ".."
            ds <- many1 $ char '.'
            return $ Child $ Name $ ".." ++ ds


parseChild :: SegParser PathPart
parseChild = fmap Child parseName


parseName :: SegParser Name -- Intentionally does not parse anonymous segments
parseName = fmap (Name . concat) $ many1 $ parseUnquoted <|> parseQuoted '\'' <|> parseQuoted '"' <|> parseQuoted '`'


parseUnquoted :: SegParser String
parseUnquoted = fmap (map toLower . unwords . words) $ many1 $ satisfy $ \c -> isAlphaNum c || c `elem` " #+-$_.:"


parseQuoted :: Char -> SegParser String
parseQuoted delim = do
    char delim
    res <- many $ satisfy (/= delim) <|> try (string [delim, delim] >> return delim)
    char delim
    return res


parseParent :: SegParser PathPart
parseParent = do
    done <- optionBool $ char '^'
    if done
        then return Parent
        else do
            string ".."
            notFollowedBy $ char '.'
            return Parent


parseSelf :: SegParser PathPart
parseSelf = char '.' >> return Self





