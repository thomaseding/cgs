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
import Hoops
import Language.Cpp.Lex
import Language.Cpp.SyntaxToken
import Match
import UsedKeys


defOpenCloseSeg :: Matcher WildsRest
defOpenCloseSeg = match "DEFINE(HC_Open_Segment($str),$key);HC_Close_Segment();"

openCloseSeg :: Matcher WildsRest
openCloseSeg = match "HC_Open_Segment($str);HC_Close_Segment();"

openCloseSegByKey :: Matcher Rest
openCloseSegByKey = match "HC_Open_Segment_By_Key(LOOKUP($key));HC_Close_Segment();"

openCloseGeom :: Matcher Rest
openCloseGeom = match "HC_Open_Geometry(LOOKUP($key));HC_Close_Geometry();"

openCloseFace :: Matcher Rest
openCloseFace = match "HC_Open_Face($int);HC_Close_Face();"

openCloseVertex :: Matcher Rest
openCloseVertex = match "HC_Open_Vertex($int);HC_Close_Vertex();"

openCloseEdge :: Matcher Rest
openCloseEdge = match "HC_Open_Edge($int,$int);HC_Close_Edge();"

openCloseLod :: Matcher Rest
openCloseLod = match "HC_Open_LOD(LOOKUP($key));HC_Close_LOD();"

openCloseRegion :: Matcher Rest
openCloseRegion = match "HC_Open_Region($int);HC_Close_Region();"

openCloseTrim :: Matcher Rest
openCloseTrim = match "HC_Open_Trim($int);HC_Close_Trim();"

defVarByArgs :: Matcher PrefixWildsRest
defVarByArgs = match "DEFINE($var($args),$key)"


i :: String -> SyntaxToken Hoops
i = Identifier

p :: String -> SyntaxToken Hoops
p = Punctuation . punc


data Status = Changed | Unchanged


removeNopPairs :: [SyntaxToken Hoops] -> [SyntaxToken Hoops]
removeNopPairs toks = case flip runState Unchanged $ removeNopPairsM toks of
    (newToks, Changed) -> removeNopPairs newToks
    (newToks, Unchanged) -> newToks


removeNopPairsM :: [SyntaxToken Hoops] -> State Status [SyntaxToken Hoops]
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
removeNopPairsM (openCloseLod -> Rest ts) = put Changed >> removeNopPairsM ts
removeNopPairsM (openCloseRegion -> Rest ts) = put Changed >> removeNopPairsM ts
removeNopPairsM (openCloseTrim -> Rest ts) = put Changed >> removeNopPairsM ts
removeNopPairsM (t:ts) = fmap (t :) $ removeNopPairsM ts
removeNopPairsM [] = return []


removeUnusedDefines :: [SyntaxToken Hoops] -> [SyntaxToken Hoops]
removeUnusedDefines toks = flip runReader (Set.fromList $ usedKeys toks) $ removeUnusedDefinesM toks


removeUnusedDefinesM :: [SyntaxToken Hoops] -> Reader (Set Key) [SyntaxToken Hoops]
removeUnusedDefinesM (defVarByArgs -> PrefixWildsRest prefix wilds rest) = do
    let Ext (Key key) = last wilds
    used <- asks $ Set.member key
    rest' <- removeUnusedDefinesM rest
    if used
        then return $ prefix ++ rest'
        else return $ (init $ init $ init $ drop 2 prefix) ++ rest'
removeUnusedDefinesM (t:ts) = fmap (t :) $ removeUnusedDefinesM ts
removeUnusedDefinesM [] = return []




