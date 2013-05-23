{-# LANGUAGE ViewPatterns #-}

module ExpandKeys (
      expandKeys
    ) where


import Control.Monad.State.Strict
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Hoops
import Language.Cpp.SyntaxToken
import Match
import SegPath


i :: String -> SyntaxToken Hoops
i = Identifier


p :: String -> SyntaxToken Hoops
p = Punctuation . punc


type Expander = State ExpandState


data Scope
    = Global
    | Local SegPath
    deriving (Show, Eq, Ord)


data NonSegKind
    = Geometry
    deriving (Show, Eq, Ord)


data Close
    = CloseSeg
    | CloseNonSeg NonSegKind
    deriving (Show, Eq, Ord)


data Open
    = OpenSegByPath SegPath
    | OpenSegByKey Key
    | OpenNonSeg NonSegKind
    deriving (Show, Eq, Ord)


data ExpandState = ExpandState {
      openStack :: [Open]
    , keyMap :: Map (Key, Scope) SegPath
    , aliasMap :: Map SegPath SegPath
    , anonymousNames :: [String]
    }


expandKeys :: [SyntaxToken Hoops] -> [SyntaxToken Hoops]
expandKeys = flip evalState st . expandKeysM
    where
        st = ExpandState {
              openStack = []
            , keyMap = M.empty
            , aliasMap = M.empty
            , anonymousNames = ["$" ++ show n | n <- [(0::Int) .. ]]
            }


expandKeysM :: [SyntaxToken Hoops] -> Expander [SyntaxToken Hoops]
expandKeysM = let
    defCreateSegment = match "DEFINE(HC_Create_Segment($seg),$key)"
    defOpenSegment = match "DEFINE(HC_Open_Segment($seg),$key)"
    defOpenSegmentKeyByKey = match "DEFINE(HC_Open_Segment_Key_By_Key(LOOKUP($key),$seg),$key)"
    openSegmentByKey = match "HC_Open_Segment_By_Key(LOOKUP($key))"
    closeSegment = match "HC_Close_Segment()"
    lookup = match "LOOKUP($key)"
    --defineAlias = match "HC_Define_Alias($seg,$seg)"
    renumberKey = match "HC_Renumber_Key(LOOKUP($key),$int,$str)"
    --moveByKey = match "HC_Move_By_Key(LOOKUP($key),$str)"
    --moveByKeyByKey = match "HC_Move_By_Key(LOOKUP($key),LOOKUP($key))"
    --moveSegment = match "HC_Move_Segment($seg,$seg)"
    --renameSegment = match "HC_Rename_Segment($seg,$seg)"
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
                open $ OpenSegByPath path
                continue
            (defOpenSegmentKeyByKey -> CapturesRest [Ext (Key k1), Ext (SegPath path), Ext (Key k2)] rest) -> do
                mExpanded <- expandOpenSegmentKeyByKey k1 path
                case mExpanded of
                    Nothing -> do
                        open $ OpenSegByKey k2
                        continue
                    Just expanded -> do
                        let ts = i "DEFINE" : p "(" : expanded ++ [p ",", Ext $ Key k2, p ")"]
                        expandKeysM $ ts ++ rest
            (openSegmentByKey -> CapturesRest [Ext (Key key)] rest) -> do
                mExpanded <- expandOpenSegmentByKey key
                case mExpanded of
                    Nothing -> do
                        open $ OpenSegByKey key
                        continue
                    Just (expanded, path) -> do
                        open $ OpenSegByPath path
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
                        mPath <- currentlyOpenedPath
                        return $ case mPath of
                            Nothing -> Nothing
                            Just path -> Just $ Local path
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
            _ -> continue


open :: Open -> Expander ()
open kind = modify $ \st -> st { openStack = kind : openStack st }


close :: Close -> Expander ()
close closeKind = do
    mOpenKind <- gets $ listToMaybe . openStack
    case mOpenKind of
        Nothing -> return ()
        Just openKind -> let
            closeKind' = openKindToCloseKind openKind
            in if closeKind == closeKind'
                then pop
                else popTill
    where
        pop = modify $ \st -> st { openStack = tail $ openStack st }
        popTill = let
            pred = (/= closeKind) . openKindToCloseKind
            in modify $ \st -> st { openStack = drop 1 $ dropWhile pred $ openStack st }
        openKindToCloseKind openKind = case openKind of
            OpenSegByPath {} -> CloseSeg
            OpenSegByKey {} -> CloseSeg
            OpenNonSeg kind -> CloseNonSeg kind


currentlyOpenedPath :: Expander (Maybe SegPath)
currentlyOpenedPath = do
    mOpen <- gets $ listToMaybe . openStack
    case mOpen of
        Nothing -> return Nothing
        Just open -> case open of
            OpenSegByPath path -> return $ Just path
            _ -> return Nothing


lookupPath :: Key -> Expander (Maybe SegPath)
lookupPath key = do
    mCurrPath <- currentlyOpenedPath
    case mCurrPath of
        Nothing -> gets $ M.lookup (key, Global) . keyMap
        Just currPath -> do
            mPath <- gets $ M.lookup (key, Local currPath) . keyMap
            case mPath of
                Just path -> return $ Just path
                Nothing -> gets $ M.lookup (key, Global) . keyMap


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
                return $ currPath `mappend` mkSegPath anonName
            else return $ currPath `mappend` path
        modify $ \st -> st {
              keyMap = M.insert (key, scope) path' $ keyMap st
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
            path = parentPath `mappend` childPath
            in Just $ i "HC_Open_Segment" : p "(" : Ext (SegPath path) : p ")" : []


expandOpenSegmentByKey :: Key -> Expander (Maybe ([SyntaxToken Hoops], SegPath))
expandOpenSegmentByKey key = do
    mPath <- lookupPath key
    return $ case mPath of
        Nothing -> Nothing
        Just path -> Just (i "HC_Open_Segment" : p "(" : Ext (SegPath path) : p ")" : [], path)









