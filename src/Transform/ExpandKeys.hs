{-# LANGUAGE ViewPatterns #-}

module Transform.ExpandKeys (
      expandKeys
    ) where


import Control.Monad.State.Lazy
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Hoops.Match
import Hoops.SyntaxToken
import Prelude hiding (lookup, pred)


i :: String -> SyntaxToken Hoops
i = Identifier


p :: String -> SyntaxToken Hoops
p = Punctuation . punc


merge :: SegPath -> SegPath -> SegPath
p1 `merge` p2 = if isAbsolute p1
    then p1 `mappend` p2
    else p2


type Expander = State ExpandState


data Scope
    = Global
    | LocalKP Key SegPath
    | LocalK Key
    | LocalP SegPath
    deriving (Show, Eq, Ord)


data NonSegKind
    = Geometry
    deriving (Show, Eq, Ord)


data CloseKind
    = CloseSeg
    | CloseNonSeg NonSegKind
    deriving (Show, Eq, Ord)


data OpenKind
    = OpenSeg Segment
    | OpenNonSeg NonSegKind
    deriving (Show, Eq, Ord)


data Segment
    = SegmentKeyPath Key SegPath
    | SegmentKey Key
    | SegmentPath SegPath
    deriving (Show, Eq, Ord)


data ExpandState = ExpandState {
      openStack :: [OpenKind]
    , keyMap :: Map (Key, Scope) SegPath
    , aliasMap :: Map SegPath SegPath
    , anonymousNames :: [String]
    }


expandKeys :: [SyntaxToken Hoops] -> [SyntaxToken Hoops]
expandKeys = flip evalState st . expandKeysM
    where
        st = ExpandState {
              openStack = []
            , keyMap = Map.empty
            , aliasMap = Map.empty
            , anonymousNames = ["$" ++ show n | n <- [(0::Int) .. ]]
            }


expandKeysM :: [SyntaxToken Hoops] -> Expander [SyntaxToken Hoops]
expandKeysM = let
    defCreateSegment = match "DEFINE(HC_Create_Segment($path),$key)"
    defOpenSegment = match "DEFINE(HC_Open_Segment($path),$key)"
    defOpenSegmentKeyByKey = match "DEFINE(HC_Open_Segment_Key_By_Key(LOOKUP($key),$path),$key)"
    openSegmentByKey = match "HC_Open_Segment_By_Key(LOOKUP($key))"
    closeSegment = match "HC_Close_Segment()"
    lookup = match "LOOKUP($key)"
    --defineAlias = match "HC_Define_Alias($path,$path)"
    renumberKey = match "HC_Renumber_Key(LOOKUP($key),$int,$str)"
    --moveByKey = match "HC_Move_By_Key(LOOKUP($key),$str)"
    --moveByKeyByKey = match "HC_Move_By_Key(LOOKUP($key),LOOKUP($key))"
    --moveSegment = match "HC_Move_Segment($path,$path)"
    --renameSegment = match "HC_Rename_Segment($path,$path)"
    pickByFirst xs = case xs of
        (bool, kind) : rest -> if bool
            then Just kind
            else pickByFirst rest
        [] -> Nothing
    openNonSeg = let
        openers = map (\(str, kind) -> (match str, kind)) [
              ("HC_Open_Geometry", Geometry)
            ]
        in \toks -> pickByFirst $ map (\(m, kind) -> (m toks, kind)) openers
    closeNonSeg = let
        closers = map (\(str, kind) -> (match str, kind)) [
              ("HC_Close_Geometry", Geometry)
            ]
        in \toks -> pickByFirst $ map (\(m, kind) -> (m toks, kind)) closers
    in \toks -> let
        continue = case toks of
            t : ts -> do
                ts' <- expandKeysM ts
                return $ t : ts'
            [] -> return []
        in case toks of
            (defCreateSegment -> Captures [Ext (SegPath path), Ext (Key key)]) -> do
                recordDef path key Global
                continue
            (defOpenSegment -> Captures [Ext (SegPath path), Ext (Key key)]) -> do
                recordDef path key Global
                open $ OpenSeg $ SegmentKeyPath key path
                continue
            (defOpenSegmentKeyByKey -> CapturesRest [Ext (Key k1), Ext (SegPath path), Ext (Key k2)] rest) -> do
                mExpanded <- expandOpenSegmentKeyByKey k1 path
                case mExpanded of
                    Nothing -> do
                        open $ OpenSeg $ SegmentKey k2
                        continue
                    Just expanded -> do
                        let ts = i "DEFINE" : p "(" : expanded ++ [p ",", Ext $ Key k2, p ")"]
                        expandKeysM $ ts ++ rest
            (openSegmentByKey -> CapturesRest [Ext (Key key)] rest) -> do
                mExpanded <- expandOpenSegmentByKey key
                case mExpanded of
                    Nothing -> do
                        open $ OpenSeg $ SegmentKey key
                        continue
                    Just (expanded, path) -> do
                        open $ OpenSeg $ SegmentKeyPath key path
                        ts <- expandKeysM rest
                        return $ expanded ++ ts
            (closeSegment -> True) -> do
                close CloseSeg
                continue
            (lookup -> CapturesRest [Ext (Key key)] rest) -> do
                mPath <- lookupPath key
                case mPath of
                    Nothing -> continue
                    Just path -> do
                        ts <- expandKeysM rest
                        return $ i "K" : p "(" : Ext (SegPath path) : p ")" : ts
            (renumberKey -> CapturesRest [Ext (Key oldKey), Integer intKey, String scopeStr] rest) -> do
                let newKey = mkKey intKey
                mScope <- case scopeStr of
                    'g' : _ -> return $ Just Global
                    'l' : _ -> do
                        mSeg <- currentlyOpenedSeg
                        return $ case mSeg of
                            Nothing -> Nothing
                            Just seg -> Just $ case seg of
                                SegmentKeyPath key path -> LocalKP key path
                                SegmentKey key -> LocalK key
                                SegmentPath path -> LocalP path
                    _ -> return Nothing
                case mScope of
                    Nothing -> continue
                    Just scope -> do
                        mPath <- recordRenumberedKey oldKey newKey scope
                        case mPath of
                            Nothing -> continue
                            Just path -> let
                                expanded = i "HC_Renumber_Key" : p "("
                                    : i "K" : p "(" : Ext (SegPath path) : p ")"  : p ","
                                    : Integer intKey : p ","
                                    : String scopeStr : p ")"
                                    : []
                                in do
                                    ts <- expandKeysM rest
                                    return $ expanded ++ ts
            (openNonSeg -> Just kind) -> do
                open $ OpenNonSeg kind
                continue
            (closeNonSeg -> Just kind) -> do
                close $ CloseNonSeg kind
                continue
            _ -> continue


open :: OpenKind -> Expander ()
open kind = case kind of
    OpenSeg seg -> case seg of
        SegmentKeyPath key path -> openWithPath path $ Just key
        SegmentPath path -> openWithPath path Nothing
        SegmentKey {} -> openWithoutPath
    _ -> openWithoutPath
    where
        openWithoutPath = modify $ \st -> st { openStack = kind : openStack st }
        openWithPath path mKey = do
            mCurrPath <- currentlyOpenedPath
            case mCurrPath of
                Nothing -> modify $ \st -> st { openStack = kind : openStack st }
                Just currPath -> let
                    path' = currPath `mappend` path
                    kind' = OpenSeg $ maybe SegmentPath SegmentKeyPath mKey path'
                    in modify $ \st -> st { openStack = kind' : openStack st }


close :: CloseKind -> Expander ()
close closeKind = do
    let pred = (/= closeKind) . openKindToCloseKind
    modify $ \st -> st { openStack = drop 1 $ dropWhile pred $ openStack st }
    where
        openKindToCloseKind openKind = case openKind of
            OpenSeg {} -> CloseSeg
            OpenNonSeg kind -> CloseNonSeg kind


currentlyOpenedSeg :: Expander (Maybe Segment)
currentlyOpenedSeg = do
    mOpenItem <- gets $ listToMaybe . openStack
    case mOpenItem of
        Nothing -> return Nothing
        Just openItem -> case openItem of
            OpenSeg seg -> return $ Just seg
            _ -> return Nothing


currentlyOpenedPath :: Expander (Maybe SegPath)
currentlyOpenedPath = do
    mSeg <- currentlyOpenedSeg
    return $ case mSeg of
        Just seg -> case seg of
            SegmentKeyPath _ path -> Just path
            SegmentPath path -> Just path
            SegmentKey {} -> Nothing
        Nothing -> Nothing


lookupPath :: Key -> Expander (Maybe SegPath)
lookupPath = \key -> do
    mCurrSeg <- currentlyOpenedSeg
    case mCurrSeg of
        Nothing -> gets $ Map.lookup (key, Global) . keyMap
        Just currSeg -> case currSeg of
            SegmentKeyPath currKey currPath -> do
                mPath1 <- gets $ Map.lookup (key, LocalKP currKey currPath) . keyMap
                case mPath1 of
                    Just path -> return $ Just path
                    Nothing -> do
                        mPath2 <- gets $ Map.lookup (key, LocalK currKey) . keyMap
                        case mPath2 of
                            Just path -> return $ Just path
                            Nothing -> do
                                mPath3 <- gets $ Map.lookup (key, LocalP currPath) . keyMap
                                case mPath3 of
                                    Just path -> return $ Just path
                                    Nothing -> gets $ Map.lookup (key, Global) . keyMap
            SegmentPath currPath -> do
                mPath <- gets $ Map.lookup (key, LocalP currPath) . Map.mapKeys discardScopeKey . keyMap
                case mPath of
                    Just path -> return $ Just path
                    Nothing -> gets $ Map.lookup (key, Global) . keyMap
            SegmentKey currKey -> do
                mPath <- gets $ Map.lookup (key, LocalK currKey) . Map.mapKeys discardScopePath . keyMap
                case mPath of
                    Just path -> return $ Just path
                    Nothing -> gets $ Map.lookup (key, Global) . keyMap
    where
        discardScopeKey (key, scope) = case scope of
            LocalKP _ path -> (key, LocalP path)
            _ -> (key, scope)
        discardScopePath (key, scope) = case scope of
            LocalKP localKey _ -> (key, LocalK localKey)
            _ -> (key, scope)


recordDef :: SegPath -> Key -> Scope -> Expander ()
recordDef path key scope = let
    anonPath = mkSegPath ""
    in do
        mCurrPath <- currentlyOpenedPath
        let currPath = maybe mempty id mCurrPath
        path' <- if path == anonPath
            then do
                anonName <- gets $ head . anonymousNames
                modify $ \st -> st { anonymousNames = tail $ anonymousNames st }
                return $ currPath `merge` mkSegPath anonName
            else return $ currPath `merge` path
        modify $ \st -> st {
              keyMap = Map.insert (key, scope) path' $ keyMap st
            }


recordRenumberedKey :: Key -> Key -> Scope -> Expander (Maybe SegPath)
recordRenumberedKey oldKey newKey scope = do
    mPath <- lookupPath oldKey
    case mPath of
        Nothing -> return Nothing
        Just path -> do
            recordDef path newKey scope
            return $ Just path


expandOpenSegmentKeyByKey :: Key -> SegPath -> Expander (Maybe [SyntaxToken Hoops])
expandOpenSegmentKeyByKey parentKey childPath = do
    mParentPath <- lookupPath parentKey
    return $ case mParentPath of
        Nothing -> Nothing
        Just parentPath -> let
            path = parentPath `merge` childPath
            in Just $ i "HC_Open_Segment" : p "(" : Ext (SegPath path) : p ")" : []


expandOpenSegmentByKey :: Key -> Expander (Maybe ([SyntaxToken Hoops], SegPath))
expandOpenSegmentByKey key = do
    mPath <- lookupPath key
    return $ case mPath of
        Nothing -> Nothing
        Just path -> Just (i "HC_Open_Segment" : p "(" : Ext (SegPath path) : p ")" : [], path)









