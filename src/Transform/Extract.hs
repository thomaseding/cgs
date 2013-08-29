{-# LANGUAGE ViewPatterns #-}

module Transform.Extract (
      extract
    ) where


import Data.Maybe (mapMaybe)
import Hoops.Match
import Hoops.SyntaxToken
import Language.Cpp.SyntaxToken
import Transform.Extract.Common


import qualified Transform.Extract.Shell


type Index = Integer


data Tokens
    = Free [SyntaxToken Hoops]
    | Block (Maybe BlockKind) [SyntaxToken Hoops]
    deriving (Show)


extractors :: [BlockKind -> ExtractFunc]
extractors = [
      Transform.Extract.Shell.extractor
    ]


extract :: FilePath -> [SyntaxToken Hoops] -> IO [SyntaxToken Hoops]
extract basePath = fmap unblockify . runExtractor basePath . mapM extractM . blockify 


extractM :: Tokens -> Extractor Tokens
extractM tokens = case tokens of
    Free ts -> return $ Free ts
    block@(Block mKind toks) -> case mKind of
        Nothing -> return block
        Just kind -> do
            mToks' <- firstM (map uncurry extractors) (kind, toks)
            case mToks' of
                Nothing -> return block
                Just toks' -> return $ Block Nothing toks'


firstM :: Monad m => [a -> m (Maybe b)] -> (a -> m (Maybe b))
firstM fs x = case fs of
    f : fs' -> do
        mY <- f x
        case mY of
            Nothing -> firstM fs' x
            Just _ -> return mY
    [] -> return Nothing


braceIndices :: [SyntaxToken Hoops] -> [Either Index Index]
braceIndices = mapMaybe f . zip [0 ..]
    where
        f (idx, tok) = case tok of
            Punctuation symbol -> case unpunc symbol of
                "{" -> Just $ Left idx
                "}" -> Just $ Right idx
                _ -> Nothing
            _ -> Nothing


deepBraceIndices :: [SyntaxToken Hoops] -> [(Index, Index)]
deepBraceIndices toks = mapMaybe f $ zip idxs (drop 1 idxs)
    where
        idxs = braceIndices toks
        f pair = case pair of
            (Left i, Right j) -> Just (i, j)
            _ -> Nothing


blockify :: [SyntaxToken Hoops] -> [Tokens]
blockify toks = blockify' idxs $ zip [0 ..] toks
    where
        idxs = deepBraceIndices toks


blockify' :: [(Index, Index)] -> [(Index, SyntaxToken Hoops)] -> [Tokens]
blockify' idxs toks = case idxs of
    [] -> [free toks]
    (leftIdx, rightIdx) : idxs' -> let
        (beforeLeftBrace, afterLeftBrace) = split leftIdx toks
        (betweenBraces, afterRightBrace) = split (rightIdx - 1) afterLeftBrace
        in free beforeLeftBrace : block betweenBraces : blockify' idxs' afterRightBrace
    where
        split idx = span ((<= idx) . fst)
        free = Free . map snd
        block ts = let
            ts' = map snd ts
            in Block (blockKind ts') ts'


blockKind :: [SyntaxToken Hoops] -> Maybe BlockKind
blockKind = let
    defVar = match "DEFINE($var"
    in \tokens -> case tokens of
        (defVar -> Captures [Identifier name]) -> Just name
        _ : rest -> blockKind rest
        [] -> Nothing


unblockify :: [Tokens] -> [SyntaxToken Hoops]
unblockify tokens = case tokens of
    Free ts : rest -> ts ++ unblockify rest -- TODO : Should I be using a difference list?
    Block _ ts : rest -> ts ++ unblockify rest
    [] -> []





