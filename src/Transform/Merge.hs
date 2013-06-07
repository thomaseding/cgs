{-# LANGUAGE ViewPatterns #-}

module Transform.Merge (
      merge
    ) where


import Control.Monad.State.Lazy
import Data.List
import Data.Maybe
import Hoops.Match
import Hoops.SyntaxToken


data Segment
    = SegByPath SegPath
    | SegByKey Key
    | SegByKeyByPath Key SegPath
    deriving (Show, Eq)


type NonSegmentKind = String


data OpenKind
    = OpenSeg Segment
    | OpenNonSeg NonSegmentKind
    deriving (Show, Eq)


data MergeState = MergeState {
        openStack :: [OpenKind]
    }


type Merger = State MergeState


withOpenStack :: ([OpenKind] -> [OpenKind]) -> Merger ()
withOpenStack f = modify $ \st -> st { openStack = f $ openStack st }


data PrefixOpenRest
    = PrefixOpenRest [SyntaxToken Hoops] OpenKind [SyntaxToken Hoops]
    | NoPrefixOpenRest


merge :: [SyntaxToken Hoops] -> [SyntaxToken Hoops]
merge = flip evalState st . mergeM
    where st = MergeState {
          openStack = []
        }


matchOpenSeg :: [SyntaxToken Hoops] -> PrefixOpenRest
matchOpenSeg = let
    defOpenSegment = match "DEFINE(HC_Open_Segment($path),$!key);"
    defOpenSegmentKeyByKey = match "DEFINE(HC_Open_Segment_Key_By_Key(LOOKUP($key),$path),$!key);"
    openSegment = match "HC_Open_Segment($path);"
    openSegmentByKey = match "HC_Open_Segment_By_Key(LOOKUP($key));"
    openSegmentKeyByKey = match "HC_Open_Segment_Key_By_Key(LOOKUP($key), $path);"
    in \tokens -> case tokens of
        (defOpenSegment -> PrefixCapturesRest prefix [Ext (SegPath path)] rest) -> let
            seg = SegByPath path
            in PrefixOpenRest prefix (OpenSeg seg) rest
        (openSegment -> PrefixCapturesRest prefix [Ext (SegPath path)] rest) -> let
            seg = SegByPath path
            in PrefixOpenRest prefix (OpenSeg seg) rest
        (defOpenSegmentKeyByKey -> PrefixCapturesRest prefix [Ext (Key key), Ext (SegPath path)] rest) -> let
            seg = SegByKeyByPath key path
            in PrefixOpenRest prefix (OpenSeg seg) rest
        (openSegmentKeyByKey -> PrefixCapturesRest prefix [Ext (Key key), Ext (SegPath path)] rest) -> let
            seg = SegByKeyByPath key path
            in PrefixOpenRest prefix (OpenSeg seg) rest
        (openSegmentByKey -> PrefixCapturesRest prefix [Ext (Key key)] rest) -> let
            seg = SegByKey key
            in PrefixOpenRest prefix (OpenSeg seg) rest
        _ -> NoPrefixOpenRest


advance :: [SyntaxToken Hoops] -> [SyntaxToken Hoops] -> Merger [SyntaxToken Hoops]
advance toksToExpand prependToks = do
    endToks <- mergeM toksToExpand
    return $ prependToks ++ endToks


mergeM :: [SyntaxToken Hoops] -> Merger [SyntaxToken Hoops]
mergeM = let
    closeSegment = match "HC_Close_Segment();"
    in \tokens -> case tokens of
        (matchOpenSeg -> PrefixOpenRest prefix open rest) -> do
            withOpenStack (open :)
            advance rest prefix
        (closeSegment -> PrefixRest closePrefix (matchOpenSeg -> PrefixOpenRest openPrefix open rest)) -> do
            mOpen <- gets $ listToMaybe . openStack
            advance rest =<< if mOpen == Just open
                then return []
                else do
                    withOpenStack $ (open :) . \opens -> case opens of
                        OpenSeg _ : opens' -> opens'
                        _ -> opens
                    return $ closePrefix ++ openPrefix
        (closeSegment -> PrefixRest prefix rest) -> do
            withOpenStack $ \opens -> case opens of
                OpenSeg _ : opens' -> opens'
                _ -> opens
            advance rest prefix
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







