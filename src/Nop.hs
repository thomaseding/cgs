{-# LANGUAGE ViewPatterns #-}

module Nop (
      removeNopPairs
    , removeUnusedDefines
    ) where


import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Cpp.Lex
import Language.Cpp.SyntaxToken
import Match
import UsedKeys


defOpenCloseSeg :: Matcher WildsRest
defOpenCloseSeg = match "DEFINE(HC_Open_Segment($str),$int);HC_Close_Segment();"

openCloseSeg :: Matcher WildsRest
openCloseSeg = match "HC_Open_Segment($str);HC_Close_Segment();"

openCloseSegByKey :: Matcher Rest
openCloseSegByKey = match "HC_Open_Segment_By_Key(LOOKUP($int));HC_Close_Segment();"

openCloseGeom :: Matcher Rest
openCloseGeom = match "HC_Open_Geometry(LOOKUP($int));HC_Close_Geometry();"

openCloseFace :: Matcher Rest
openCloseFace = match "HC_Open_Face(LOOKUP($int));HC_Close_Face();"

openCloseVertex :: Matcher Rest
openCloseVertex = match "HC_Open_Vertex(LOOKUP($int));HC_Close_Vertex();"

openCloseEdge :: Matcher Rest
openCloseEdge = match "HC_Open_Edge(LOOKUP($int,$int));HC_Close_Edge();"


defVarByStr :: Matcher WildsRest
defVarByStr = match "DEFINE($var($str),$int)"


i :: String -> SyntaxToken
i = Identifier


p :: String -> SyntaxToken
p = Punctuation . punc


data Status = Changed | Unchanged


removeNopPairs :: [SyntaxToken] -> [SyntaxToken]
removeNopPairs toks = case flip runState Unchanged $ removeNopPairsM toks of
    (newToks, Changed) -> removeNopPairs newToks
    (newToks, Unchanged) -> newToks


removeNopPairsM :: [SyntaxToken] -> State Status [SyntaxToken]
removeNopPairsM (defOpenCloseSeg -> WildsRest [seg, key] ts) = do
    put Changed
    ts' <- removeNopPairsM ts
    return $ i "DEFINE" : p "(" : i "HC_Create_Segment" : p "(" : seg : p ")" : p "," : key : p ")" : p ";" : ts'
removeNopPairsM (openCloseSeg -> WildsRest [seg] ts) = do
    put Changed
    ts' <- removeNopPairsM ts
    return $ i "HC_Create_Segment" : p "(" : seg : p ")" : p ";" : ts'
removeNopPairsM (openCloseSegByKey -> Rest ts) = put Changed >> removeNopPairsM ts
removeNopPairsM (openCloseGeom -> Rest ts) = put Changed >> removeNopPairsM ts
removeNopPairsM (openCloseFace -> Rest ts) = put Changed >> removeNopPairsM ts
removeNopPairsM (openCloseVertex -> Rest ts) = put Changed >> removeNopPairsM ts
removeNopPairsM (openCloseEdge -> Rest ts) = put Changed >> removeNopPairsM ts
removeNopPairsM (t:ts) = fmap (t :) $ removeNopPairsM ts
removeNopPairsM [] = return []


removeUnusedDefines :: [SyntaxToken] -> [SyntaxToken]
removeUnusedDefines toks = flip runReader (Set.fromList $ usedKeys toks) $ removeUnusedDefinesM toks


removeUnusedDefinesM :: [SyntaxToken] -> Reader (Set Integer) [SyntaxToken]
removeUnusedDefinesM (defVarByStr -> WildsRest [func, seg, key@(Integer keyNum)] rest) = do
    used <- asks $ Set.member keyNum
    rest' <- removeUnusedDefinesM rest
    if used
        then return $ i "DEFINE" : p "(" : func : p "(" : seg : p ")" : p ")" : rest'
        else return $ func : p "(" : seg : p ")" : rest'
removeUnusedDefinesM (t:ts) = fmap (t :) $ removeUnusedDefinesM ts
removeUnusedDefinesM [] = return []




