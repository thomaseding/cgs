{-# LANGUAGE ViewPatterns #-}

module Transform.Flatten (
    flatten
) where


import Control.Monad ((<=<), guard)
import Control.Monad.Reader (ReaderT, runReaderT, local, ask)
import Control.Monad.State.Lazy (State, evalState, modify, gets)
import Data.List (nub, stripPrefix)
import Data.Map (Map)
import qualified Data.Map as Map (empty, insertWith, lookup)
import Data.Maybe (isJust, listToMaybe)
import Hoops.Match
import Hoops.SyntaxToken


i :: String -> SyntaxToken Hoops
i = Identifier


p :: String -> SyntaxToken Hoops
p = Punctuation . punc


newtype Continue = Continue { runContinue :: Flattener [SyntaxToken Hoops] }


type Flattener = ReaderT Continue (State FlattenState)


data Segment
    = SegByPath SegPath
    | SegByKey Key
    | SegByKeyByPath Key SegPath


type NonSegmentKind = String


type OpenSegInfo = (Segment, Maybe Segment) -- (Maybe Segment) is the segment closed when opening (Segment)


data OpenKind
    = OpenSeg OpenSegInfo
    | OpenNonSeg NonSegmentKind


data Scope = Local | Global
    deriving (Eq)


data FlattenState = FlattenState {
      openStack :: [OpenKind]
    , userKeys :: Map Key [Scope]
    }


data KeyKind
    = SystemKey
    | UndefinedKey
    | UserKey Scope
    deriving (Eq)


flatten :: [SyntaxToken Hoops] -> [SyntaxToken Hoops]
flatten ts = flip evalState st . flip runReaderT cont . flattenM $ ts
    where
        cont = Continue $ flattenM ts
        st = FlattenState {
              openStack = []
            , userKeys = Map.empty
            }


continue :: Flattener [SyntaxToken Hoops]
continue = ask >>= runContinue


advance :: [SyntaxToken Hoops] -> [SyntaxToken Hoops] -> Flattener [SyntaxToken Hoops]
advance toksToExpand prependToks = do
    endToks <- flattenM toksToExpand
    return $ prependToks ++ endToks


flattenM :: [SyntaxToken Hoops] -> Flattener [SyntaxToken Hoops]
flattenM = let
    defOpenSegment = match "DEFINE(HC_Open_Segment($path),$!key);"
    defOpenSegmentKeyByKey = match "DEFINE(HC_Open_Segment_Key_By_Key(LOOKUP($key),$path),$!key);"
    openSegment = match "HC_Open_Segment($path);"
    openSegmentByKey = match "HC_Open_Segment_By_Key(LOOKUP($key));"
    openSegmentKeyByKey = match "HC_Open_Segment_Key_By_Key(LOOKUP($key),$path);"
    closeSegment = match "HC_Close_Segment();"
    renumberKey = match "HC_Renumber_Key($!key,$int,$str);"
    in \tokens -> let
        cont = Continue $ case tokens of
            t : ts -> do
                ts' <- flattenM ts
                return $ t : ts'
            [] -> return []
        in local (const cont) $ case tokens of
            (defOpenSegment -> PrefixCapturesRest prefix [Ext (SegPath path)] rest) -> do
                handleOpenSegment prefix path rest
            (defOpenSegmentKeyByKey -> PrefixCapturesRest prefix [Ext (Key key), Ext (SegPath path)] rest) -> do
                handleOpenSegmentKeyByKey prefix key path rest
            (openSegment -> PrefixCapturesRest prefix [Ext (SegPath path)] rest) -> do
                handleOpenSegment prefix path rest
            (openSegmentByKey -> PrefixCapturesRest prefix [Ext (Key key)] rest) -> do
                handleOpenSegmentByKey prefix key rest
            (openSegmentKeyByKey -> PrefixCapturesRest prefix [Ext (Key key), Ext (SegPath path)] rest) -> do
                handleOpenSegmentKeyByKey prefix key path rest
            (closeSegment -> PrefixRest prefix rest) -> do
                handleCloseSegment prefix rest
            (renumberKey -> Captures [Integer intKey, String scopeStr]) -> do
                handleRenumberKey intKey scopeStr
            (Identifier ('H':'C':'_':name)) : _ -> do
                handleHcCall name
            _ -> continue


handleOpenSegment :: [SyntaxToken Hoops] -> SegPath -> [SyntaxToken Hoops] -> Flattener [SyntaxToken Hoops]
handleOpenSegment prefix path rest = do
    mOpenSegInfo <- getOpenSegInfo
    let needsCloseSeg = isAbsolute path && isJust mOpenSegInfo
    withOpenStack $ (:) $ OpenSeg (SegByPath path, guard needsCloseSeg >> fmap fst mOpenSegInfo)
    advance rest $ if needsCloseSeg
        then closeSegmentToks ++ prefix
        else prefix


handleOpenSegmentKeyByKey :: [SyntaxToken Hoops] -> Key -> SegPath -> [SyntaxToken Hoops] -> Flattener [SyntaxToken Hoops]
handleOpenSegmentKeyByKey prefix key path rest = do
    mOpenSegInfo <- getOpenSegInfo
    keyKind <- getKeyKind key
    let needsCloseSeg = canOpenAnywhere && isJust mOpenSegInfo
        canOpenAnywhere = case keyKind of
            SystemKey -> True
            UserKey scope -> case scope of
                Global -> True
                Local -> False
            UndefinedKey -> False
    withOpenStack $ (:) $ OpenSeg (SegByKeyByPath key path, guard needsCloseSeg >> fmap fst mOpenSegInfo)
    advance rest $ if needsCloseSeg
        then closeSegmentToks ++ prefix
        else prefix


handleOpenSegmentByKey :: [SyntaxToken Hoops] -> Key -> [SyntaxToken Hoops] -> Flattener [SyntaxToken Hoops]
handleOpenSegmentByKey prefix key rest = do
    mOpenSegInfo <- getOpenSegInfo
    keyKind <- getKeyKind key
    let needsCloseSeg = canOpenAnywhere && isJust mOpenSegInfo
        canOpenAnywhere = case keyKind of
            SystemKey -> True
            UserKey scope -> case scope of
                Global -> True
                Local -> False
            UndefinedKey -> False
    withOpenStack $ (:) $ OpenSeg (SegByKey key, guard needsCloseSeg >> fmap fst mOpenSegInfo)
    advance rest $ if needsCloseSeg
        then closeSegmentToks ++ prefix
        else prefix


handleCloseSegment :: [SyntaxToken Hoops] -> [SyntaxToken Hoops] -> Flattener [SyntaxToken Hoops]
handleCloseSegment prefix rest = do
    mOldSegInfo <- getOpenSegInfo
    advance rest =<< case mOldSegInfo of
        Just (_, mSegToOpen) -> do
            withOpenStack tail
            case mSegToOpen of
                Just seg -> return $ (prefix ++) $ case seg of
                    SegByPath path -> openSegmentToks path
                    SegByKey key -> openSegmentByKeyToks key
                    SegByKeyByPath key path -> openSegmentKeyByKeyToks key path
                Nothing -> return prefix
        Nothing -> return prefix


handleHcCall :: String -> Flattener [SyntaxToken Hoops]
handleHcCall name = let
    mOpenKind = stripPrefix "Open_" name
    mCloseKind = stripPrefix "Close_" name
    in case mOpenKind of
        Just openKind -> handleNonSegOpen openKind
        Nothing -> case mCloseKind of
            Just closeKind -> handleNonSegClose closeKind
            Nothing -> continue


handleNonSegOpen :: NonSegmentKind -> Flattener [SyntaxToken Hoops]
handleNonSegOpen nonSegKind = do
    withOpenStack (OpenNonSeg nonSegKind :)
    continue


handleNonSegClose :: NonSegmentKind -> Flattener [SyntaxToken Hoops]
handleNonSegClose nonSegKind = do
    withOpenStack $ \opens -> case opens of
        OpenNonSeg openKind : rest -> if openKind == nonSegKind
            then rest
            else opens
        _ -> opens
    continue


handleRenumberKey :: Integer -> String -> Flattener [SyntaxToken Hoops]
handleRenumberKey intKey scopeStr = do
    case mScope of
        Just scope -> modify $ \st -> let
            f = Map.insertWith (\new old -> nub $ new ++ old) key [scope]
            in st { userKeys = f $ userKeys st  }
        Nothing -> return ()
    continue
    where
        key = mkKey intKey
        mScope = case scopeStr of
            'g' : _ -> Just Global
            'l' : _ -> Just Local
            _ -> Nothing


getKeyKind :: Key -> Flattener KeyKind
getKeyKind key = if isUserKey key
    then do
        mScopes <- gets $ Map.lookup key . userKeys
        return $ case mScopes of
            Just scopes -> UserKey $ if Local `elem` scopes
                then Local
                else Global
            Nothing -> UndefinedKey
    else return SystemKey


viewOpenSegInfo :: OpenKind -> Maybe OpenSegInfo
viewOpenSegInfo kind = case kind of
    OpenSeg openInfo -> Just openInfo
    _ -> Nothing


getOpenSegInfo :: Flattener (Maybe OpenSegInfo)
getOpenSegInfo = gets $ viewOpenSegInfo <=< listToMaybe . openStack


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



