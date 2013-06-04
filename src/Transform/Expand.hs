{-# LANGUAGE ViewPatterns #-}

module Transform.Expand (
      expand
    ) where


import Control.Monad.State.Lazy
import Data.List hiding (lookup)
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


data Scope
    = Global
    | LocalKP Key SegPath
    | LocalK Key
    | LocalP SegPath
    deriving (Show, Eq, Ord)


type NonSegmentKind = String


data CloseKind
    = CloseSeg
    | CloseNonSeg NonSegmentKind
    deriving (Show, Eq, Ord)


data OpenKind
    = OpenSeg Segment
    | OpenNonSeg NonSegmentKind
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


type Expander = State ExpandState


expand :: [SyntaxToken Hoops] -> [SyntaxToken Hoops]
expand = flip evalState st . expandM
    where
        st = ExpandState {
              openStack = []
            , keyMap = Map.empty
            , aliasMap = Map.empty
            , anonymousNames = ["$" ++ show n | n <- [(0::Int) .. ]]
            }


expandM :: [SyntaxToken Hoops] -> Expander [SyntaxToken Hoops]
expandM = let
    defCreateSegment = match "DEFINE(HC_Create_Segment($path),$key)"
    defCreateSegmentKeyByKey = match "DEFINE(HC_Create_Segment_Key_By_Key(LOOKUP($key),$path),$key)"
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
    segpath = match "$path"
    advance rest front = do
        rest' <- expandM rest
        return $ front ++ rest'
    in \tokens -> let
        continue = case tokens of -- [continue]'s purpose over [advance] is that it allows the match prefix to continue to be developed by the Expander
            t : ts -> do
                ts' <- expandM ts
                return $ t : ts'
            [] -> return []
        in case tokens of
            (defCreateSegment -> Captures [Ext (SegPath path), Ext (Key key)]) -> do
                recordDef path key Global
                continue
            (defCreateSegmentKeyByKey -> PrefixCapturesRest prefix [Ext (Key parentKey), Ext (SegPath childPath), Ext (Key defKey)] rest) -> do
                mParentPath <- lookupPath parentKey
                case mParentPath of
                    Just parentPath -> do
                        let path' = parentPath `merge` childPath
                        recordDef path' defKey Global
                        advance prefix rest -- TODO: This case can be simplified down to a normal [DEFINE(HC_Create_Segment(path)),key]
                    Nothing -> do
                        -- I'm not bothering to even attempt to store the defined key in the Expander as a (parentKey, childPath).
                        -- The worst that can happen is that later occurrences of the defined key won't be expanded. Whoop-dee-do.
                        advance prefix rest
            (defOpenSegment -> Captures [Ext (SegPath path), Ext (Key key)]) -> do
                recordDef path key Global
                openSeg $ SegmentKeyPath key path
                continue
            (defOpenSegmentKeyByKey -> PrefixCapturesRest prefix [Ext (Key parentKey), Ext (SegPath childPath), Ext (Key defKey)] rest) -> do
                mExpanded <- expandOpenSegmentKeyByKey parentKey childPath
                case mExpanded of
                    Nothing -> do
                        openSeg $ SegmentKey defKey
                        advance prefix rest
                    Just expanded -> do
                        let ts = i "DEFINE" : p "(" : expanded ++ [p ",", Ext $ Key defKey, p ")"]
                        expandM $ ts ++ rest
            (openSegmentByKey -> CapturesRest [Ext (Key key)] rest) -> do
                mExpanded <- expandOpenSegmentByKey key
                case mExpanded of
                    Nothing -> do
                        openSeg $ SegmentKey key
                        continue
                    Just (expanded, path) -> do
                        openSeg $ SegmentKeyPath key path
                        ts <- expandM rest
                        return $ expanded ++ ts
            (closeSegment -> True) -> do
                withOpenStack $ \opens -> case opens of
                    OpenSeg _ : rest -> rest
                    _ -> opens
                continue
            (lookup -> CapturesRest [Ext (Key key)] rest) -> do
                mPath <- lookupPath key
                case mPath of
                    Nothing -> continue
                    Just path -> do
                        ts <- expandM rest
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
                                    ts <- expandM rest
                                    return $ expanded ++ ts
            (segpath -> CapturesRest [Ext (SegPath path)] rest) -> do
                path' <- expandPath path
                advance rest [Ext $ SegPath path']
            (Identifier ('H':'C':'_':name) : _) -> do
                let mOpenType = stripPrefix "Open_" name
                    mCloseType = stripPrefix "Close_" name
                case mOpenType of
                    Just openType -> do
                        withOpenStack (OpenNonSeg openType :)
                    Nothing -> case mCloseType of
                        Just closeType -> withOpenStack $ \opens -> case opens of
                            OpenNonSeg openType : rest -> if openType == closeType
                                then rest
                                else opens
                            _ -> opens
                        Nothing -> return ()
                continue
            _ -> continue


withOpenStack :: ([OpenKind] -> [OpenKind]) -> Expander ()
withOpenStack f = modify $ \st -> st { openStack = f $ openStack st }


openSeg :: Segment -> Expander ()
openSeg seg = case seg of
    SegmentKeyPath key path -> openWithPath path $ Just key
    SegmentPath path -> openWithPath path Nothing
    SegmentKey {} -> withOpenStack (OpenSeg seg :)
    where
        openWithPath path mKey = do
            mCurrPath <- currentlyOpenedPath
            case mCurrPath of
                Nothing -> withOpenStack (OpenSeg seg :)
                Just currPath -> let
                    path' = currPath `mappend` path
                    seg' = maybe SegmentPath SegmentKeyPath mKey path'
                    in withOpenStack (OpenSeg seg' :)


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


expandPath :: SegPath -> Expander SegPath
expandPath path = let
    anonPath = mkSegPath ""
    in do
        mCurrPath <- currentlyOpenedPath
        let currPath = maybe mempty id mCurrPath
        if path == anonPath
            then do
                anonName <- gets $ head . anonymousNames
                modify $ \st -> st { anonymousNames = tail $ anonymousNames st }
                return $ currPath `merge` mkSegPath anonName
            else return $ currPath `merge` path
    

recordDef :: SegPath -> Key -> Scope -> Expander ()
recordDef path key scope = do
    path' <- expandPath path
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









