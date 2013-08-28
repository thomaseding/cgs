{-# LANGUAGE ViewPatterns #-}

module Extract (
      extract
    ) where


import Control.Monad.State.Lazy
import Data.Maybe (mapMaybe)
import Hoops.Match
import Hoops.SyntaxToken
import Language.Cpp.SyntaxToken


type BlockKind = String
type Index = Integer
type ExtractFunc = BlockKind -> [SyntaxToken Hoops] -> Maybe ([SyntaxToken Hoops], ExtractedData)
type Extractor = State ExtractState


data ExtractedData = ExtractedData {
      tagName :: String -- uniquely identifies this extracted data
    , theData :: String
    }
    deriving (Show)


data ExtractState = ExtractState {
      extractedDatas :: [ExtractedData]
    }


data Tokens
    = Free [SyntaxToken Hoops]
    | Block (Maybe BlockKind) [SyntaxToken Hoops]
    deriving (Show)


extractors :: [ExtractFunc]
extractors = []


extract :: [SyntaxToken Hoops] -> ([SyntaxToken Hoops], [ExtractedData])
extract = massage . flip runState st . mapM extractM . blockify 
    where
        massage (tokens, state) = (unblockify tokens, extractedDatas state)
        st = ExtractState {
              extractedDatas = []
            }


extractM :: Tokens -> Extractor Tokens
extractM tokens = case tokens of
    Free ts -> return $ Free ts
    block@(Block mKind ts) -> case mKind of
        Nothing -> return block
        Just kind -> case runExtractors kind ts extractors of
            Nothing -> return block
            Just (newToks, extractedData) -> do
                modify $ \st -> st { extractedDatas = extractedData : extractedDatas st }
                return $ Block Nothing newToks


runExtractors :: BlockKind -> [SyntaxToken Hoops] -> [ExtractFunc] -> Maybe ([SyntaxToken Hoops], ExtractedData)
runExtractors kind tokens es = case es of
    e : es' -> case e kind tokens of
        Just res -> Just res
        Nothing -> runExtractors kind tokens es'
    [] -> Nothing


braceIndices :: [SyntaxToken Hoops] -> [Either Index Index]
braceIndices = braceIndices' . zip [0 ..]


braceIndices' :: [(Index, SyntaxToken Hoops)] -> [Either Index Index]
braceIndices' toks = case toks of
    (idx, Punctuation symbol) : rest -> case unpunc symbol of
        "{" -> Left idx : braceIndices' rest
        "}" -> Right idx : braceIndices' rest
        _ -> braceIndices' rest
    _ : rest -> braceIndices' rest
    [] -> []


deepBraceIndices :: [SyntaxToken Hoops] -> [(Index, Index)]
deepBraceIndices toks = mapMaybe massage $ zip idxs (drop 1 idxs)
    where
        idxs = braceIndices toks
        massage pair = case pair of
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





