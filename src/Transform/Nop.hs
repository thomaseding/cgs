{-# LANGUAGE ViewPatterns #-}

module Transform.Nop (
      removeNopPairs
    , removeUnusedDefines
    ) where


import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.Set (Set)
import qualified Data.Set as Set
import Hoops.Match
import Hoops.SyntaxToken
import Query.UsedKeys




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
removeNopPairsM = let
    defOpenCloseSeg = match "DEFINE(HC_Open_Segment($path),$key);HC_Close_Segment();"
    openCloseSeg = match "HC_Open_Segment($path);HC_Close_Segment();"
    openCloseSegByKey = match "HC_Open_Segment_By_Key(LOOKUP($key));HC_Close_Segment();"
    openCloseGeom = match "HC_Open_Geometry(LOOKUP($key));HC_Close_Geometry();"
    openCloseFace = match "HC_Open_Face($int);HC_Close_Face();"
    openCloseVertex = match "HC_Open_Vertex($int);HC_Close_Vertex();"
    openCloseEdge = match "HC_Open_Edge($int,$int);HC_Close_Edge();"
    openCloseLod = match "HC_Open_LOD(LOOKUP($key));HC_Close_LOD();"
    openCloseRegion = match "HC_Open_Region($int);HC_Close_Region();"
    openCloseTrim = match "HC_Open_Trim($int);HC_Close_Trim();"
    in \toks -> case toks of
        (defOpenCloseSeg -> CapturesRest [seg, key] ts) -> do
            put Changed
            ts' <- removeNopPairsM ts
            return $ i "DEFINE" : p "(" : i "HC_Create_Segment" : p "(" : seg : p ")" : p "," : key : p ")" : p ";" : ts'
        (openCloseSeg -> CapturesRest [seg] ts) -> do
            put Changed
            ts' <- removeNopPairsM ts
            return $ i "HC_Create_Segment" : p "(" : seg : p ")" : p ";" : ts'
        (openCloseSegByKey -> Rest ts) -> put Changed >> removeNopPairsM ts
        (openCloseGeom -> Rest ts) -> put Changed >> removeNopPairsM ts
        (openCloseFace -> Rest ts) -> put Changed >> removeNopPairsM ts
        (openCloseVertex -> Rest ts) -> put Changed >> removeNopPairsM ts
        (openCloseEdge -> Rest ts) -> put Changed >> removeNopPairsM ts
        (openCloseLod -> Rest ts) -> put Changed >> removeNopPairsM ts
        (openCloseRegion -> Rest ts) -> put Changed >> removeNopPairsM ts
        (openCloseTrim -> Rest ts) -> put Changed >> removeNopPairsM ts
        t : ts -> fmap (t :) $ removeNopPairsM ts
        [] -> return []


removeUnusedDefines :: [SyntaxToken Hoops] -> [SyntaxToken Hoops]
removeUnusedDefines toks = flip runReader (Set.fromList $ usedKeys toks) $ removeUnusedDefinesM toks


removeUnusedDefinesM :: [SyntaxToken Hoops] -> Reader (Set Key) [SyntaxToken Hoops]
removeUnusedDefinesM = let
    defVarByArgs = match "DEFINE($var($args),$key)"
    in \toks -> case toks of
        (defVarByArgs -> PrefixCapturesRest prefix captures rest) -> do
            let Ext (Key key) = last captures
            used <- asks $ Set.member key
            rest' <- removeUnusedDefinesM rest
            if used
                then return $ prefix ++ rest'
                else return $ (init $ init $ init $ drop 2 prefix) ++ rest'
        t : ts -> fmap (t :) $ removeUnusedDefinesM ts
        [] -> return []




