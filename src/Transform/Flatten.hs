{-# LANGUAGE ViewPatterns #-}

module Transform.Flatten (
      flatten
    ) where


import Control.Monad.State.Lazy
import Data.List
import Data.Maybe
import Hoops.Match
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


type NonSegmentKind = String


data OpenKind
    = OpenSeg Segment
    | OpenNonSeg NonSegmentKind


data FlattenState = FlattenState {
      openStack :: [OpenKind]
    }


flatten :: [SyntaxToken Hoops] -> [SyntaxToken Hoops]
flatten = flip evalState st . flattenM
    where
        st = FlattenState {
              openStack = []
            }


flattenM :: [SyntaxToken Hoops] -> Flattener [SyntaxToken Hoops]
flattenM = let
    defOpenSegment = match "DEFINE(HC_Open_Segment($path),$!key);"
    defOpenSegmentKeyByKey = match "DEFINE(HC_Open_Segment_Key_By_Key(LOOKUP($key),$path),$!key);"
    openSegment = match "HC_Open_Segment($path);"
    openSegmentByKey = match "HC_Open_Segment_By_Key(LOOKUP($key));"
    openSegmentKeyByKey = match "HC_Open_Segment_Key_By_Key(LOOKUP($key), $path);"
    closeSegment = match "HC_Close_Segment();"
    advance rest front = do
        rest' <- flattenM rest
        return $ front ++ rest'
    in \tokens -> let
        in case tokens of
            (defOpenSegment -> PrefixCapturesRest prefix [Ext (SegPath path)] rest) -> do
                needsCloseSeg <- fmap (isAbsolute path &&) hasOpenedSeg
                withOpenStack $ (:) $ OpenSeg $ SegByPath path
                advance rest $ if needsCloseSeg
                    then closeSegmentToks ++ prefix
                    else prefix
            (defOpenSegmentKeyByKey -> PrefixCapturesRest prefix [Ext (Key key), Ext (SegPath path)] rest) -> do
                needsCloseSeg <- hasOpenedSeg
                withOpenStack $ (:) $ OpenSeg $ SegByKeyByPath key path
                advance rest $ if needsCloseSeg
                    then closeSegmentToks ++ prefix
                    else prefix
            (openSegment -> PrefixCapturesRest prefix [Ext (SegPath path)] rest) -> do
                needsCloseSeg <- fmap (isAbsolute path &&) hasOpenedSeg
                withOpenStack $ (:) $ OpenSeg $ SegByPath path
                advance rest $ if needsCloseSeg
                    then closeSegmentToks ++ prefix
                    else prefix
            (openSegmentByKey -> PrefixCapturesRest prefix [Ext (Key key)] rest) -> do
                needsCloseSeg <- hasOpenedSeg
                withOpenStack $ (:) $ OpenSeg $ SegByKey key
                advance rest $ if needsCloseSeg
                    then closeSegmentToks ++ prefix
                    else prefix
            (openSegmentKeyByKey -> PrefixCapturesRest prefix [Ext (Key key), Ext (SegPath path)] rest) -> do
                needsCloseSeg <- hasOpenedSeg
                withOpenStack $ (:) $ OpenSeg $ SegByKeyByPath key path
                advance rest $ if needsCloseSeg
                    then closeSegmentToks ++ prefix
                    else prefix
            (closeSegment -> PrefixRest prefix rest) -> do
                mOldSeg <- gets $ viewOpenSeg <=< listToMaybe . openStack
                advance rest =<< case mOldSeg of
                    Just oldSeg -> do
                        let mOldPath = case oldSeg of
                                SegByPath oldPath -> Just oldPath
                                _ -> Nothing
                        withOpenStack tail
                        mCurrOpen <- gets $ listToMaybe . openStack
                        case mCurrOpen of
                            Just (OpenSeg seg) -> return $ (prefix ++) $ case seg of
                                SegByPath path -> if fmap isRelative mOldPath == Just True
                                    then []
                                    else openSegmentToks path
                                SegByKey key -> openSegmentByKeyToks key
                                SegByKeyByPath key path -> openSegmentKeyByKeyToks key path
                            _ -> return prefix
                    _ -> return prefix
            t : ts -> case t of
                Identifier ('H':'C':'_':name) -> let
                    mOpenType = stripPrefix "Open_" name
                    mCloseType = stripPrefix "Close_" name
                    in case mOpenType of
                        Just openType -> do
                            withOpenStack (OpenNonSeg openType :)
                            advance ts [t]
                        Nothing -> case mCloseType of
                            Just closeType -> do
                                withOpenStack $ \opens -> case opens of
                                    OpenNonSeg openType : rest -> if openType == closeType
                                        then rest
                                        else opens
                                    _ -> opens
                                advance ts [t]
                            Nothing -> advance ts [t]
                _ -> advance ts [t]
            [] -> return []


viewOpenSeg :: OpenKind -> Maybe Segment
viewOpenSeg kind = case kind of
    OpenSeg seg -> Just seg
    _ -> Nothing


hasOpenedSeg :: Flattener Bool
hasOpenedSeg = gets $ isJust . (viewOpenSeg <=< listToMaybe . openStack)


openSegmentToks :: SegPath -> [SyntaxToken Hoops]
openSegmentToks path = i "HC_Open_Segment" : p "(" : Ext (SegPath path) : p ")" : p ";" : []


openSegmentByKeyToks :: Key -> [SyntaxToken Hoops]
openSegmentByKeyToks key = i "HC_Open_Segment_By_Key" : p "(" : i "LOOKUP" : p "(" : Ext (Key key) : p ")" : p ")" : p ";" : []


openSegmentKeyByKeyToks :: Key -> SegPath -> [SyntaxToken Hoops]
openSegmentKeyByKeyToks key path = i "HC_Open_Segment_Key_By_Key" : p "(" : i "LOOKUP" : p "(" : Ext (Key key) : p ")" : p "," : Ext (SegPath path) : p ")" : p ";" : []


closeSegmentToks :: [SyntaxToken Hoops]
closeSegmentToks = i "HC_Close_Segment" : p "(" : p ")" : p ";" : []


withOpenStack :: ([OpenKind] -> [OpenKind]) -> Flattener ()
withOpenStack f = modify $ \st -> st { openStack = f $ openStack st }



