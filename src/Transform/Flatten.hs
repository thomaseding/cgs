{-# LANGUAGE ViewPatterns #-}

module Transform.Flatten (
      flatten
    ) where


import Control.Monad.State.Lazy
import Data.Maybe
import Hoops.Match
import Hoops.SegPath
import Hoops.SyntaxToken


i :: String -> SyntaxToken Hoops
i = Identifier


p :: String -> SyntaxToken Hoops
p = Punctuation . punc


type Flattener = State FlattenState


data Segment
    = SegByPath SegPath
    | SegByKey Key
    | SegByKeyByPath Key SegPath


data Open
    = OpenBySeg Segment


data FlattenState = FlattenState {
      opens :: [Open]
    }


flatten :: [SyntaxToken Hoops] -> [SyntaxToken Hoops]
flatten = flip evalState st . flattenM
    where
        st = FlattenState {
              opens = []
            }


flattenM :: [SyntaxToken Hoops] -> Flattener [SyntaxToken Hoops]
flattenM = let
    defOpenSegment = match "DEFINE(HC_Open_Segment($path),$!key);"
    defOpenSegmentKeyByKey = match "DEFINE(HC_Open_Segment_Key_By_Key($key,$path),$!key);"
    openSegment = match "HC_Open_Segment($path);"
    openSegmentByKey = match "HC_Open_Segment_By_Key($key);"
    openSegmentKeyByKey = match "HC_Open_Segment_Key_By_Key($key, $path);"
    closeSegment = match "HC_Close_Segment();"
    advance rest front = do
        rest' <- flattenM rest
        return $ front ++ rest'
    in \toks -> let
        in case toks of
            (defOpenSegment -> PrefixCapturesRest prefix [Ext (SegPath path)] rest) -> do
                needsClose <- fmap (isAbsolute path &&) $ gets $ not . null . opens
                withOpens $ (:) $ OpenBySeg $ SegByPath path
                advance rest $ if needsClose
                    then closeSegmentToks ++ prefix
                    else prefix
            (defOpenSegmentKeyByKey -> PrefixCapturesRest prefix [Ext (Key key), Ext (SegPath path)] rest) -> do
                needsClose <- gets $ not . null . opens
                withOpens $ (:) $ OpenBySeg $ SegByKeyByPath key path
                advance rest $ if needsClose
                    then closeSegmentToks ++ prefix
                    else prefix
            (openSegment -> PrefixCapturesRest prefix [Ext (SegPath path)] rest) -> do
                needsClose <- fmap (isAbsolute path &&) $ gets $ not . null . opens
                withOpens $ (:) $ OpenBySeg $ SegByPath path
                advance rest $ if needsClose
                    then closeSegmentToks ++ prefix
                    else prefix
            (openSegmentByKey -> PrefixCapturesRest prefix [Ext (Key key)] rest) -> do
                needsClose <- gets $ not . null . opens
                withOpens $ (:) $ OpenBySeg $ SegByKey key
                advance rest $ if needsClose
                    then closeSegmentToks ++ prefix
                    else prefix
            (openSegmentKeyByKey -> PrefixCapturesRest prefix [Ext (Key key), Ext (SegPath path)] rest) -> do
                needsClose <- gets $ not . null . opens
                withOpens $ (:) $ OpenBySeg $ SegByKeyByPath key path
                advance rest $ if needsClose
                    then closeSegmentToks ++ prefix
                    else prefix
            (closeSegment -> PrefixRest prefix rest) -> do
                mOldOpen <- gets $ listToMaybe . opens
                let wasRelative = case mOldOpen of
                        Just (OpenBySeg seg) -> case seg of
                            SegByPath path -> isRelative path
                            _ -> False
                        Nothing -> False
                withOpens $ drop 1
                mCurrOpen <- gets $ listToMaybe . opens
                advance rest $ case mCurrOpen of
                    Just (OpenBySeg seg) -> (prefix ++) $ case seg of
                        SegByPath path -> if wasRelative
                            then []
                            else openSegmentToks path
                        SegByKey key -> openSegmentByKeyToks key
                        SegByKeyByPath key path -> openSegmentKeyByKeyToks key path
                    Nothing -> prefix
            t : ts -> advance ts [t]
            [] -> return []


openSegmentToks :: SegPath -> [SyntaxToken Hoops]
openSegmentToks path = i "HC_Open_Segment" : p "(" : Ext (SegPath path) : p ")" : p ";" : []


openSegmentByKeyToks :: Key -> [SyntaxToken Hoops]
openSegmentByKeyToks key = i "HC_Open_Segment_By_Key" : p "(" : Ext (Key key) : p ")" : p ";" : []


openSegmentKeyByKeyToks :: Key -> SegPath -> [SyntaxToken Hoops]
openSegmentKeyByKeyToks key path = i "HC_Open_Segment_Key_By_Key" : p "(" : Ext (Key key) : p "," : Ext (SegPath path) : p ")" : p ";" : []


closeSegmentToks :: [SyntaxToken Hoops]
closeSegmentToks = i "HC_Close_Segment" : p "(" : p ")" : p ";" : []


withOpens :: ([Open] -> [Open]) -> Flattener ()
withOpens f = modify $ \st -> st { opens = f $ opens st }



