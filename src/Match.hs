{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Match (
      Match(..)
    , Matcher
    , Captures(..)
    , Rest(..)
    , PrefixRest(..)
    , PrefixCaptures(..)
    , CapturesRest(..)
    , PrefixCapturesRest(..)
    , Detailed(..)
    , DetailedRest(..)
    ) where


import Control.Monad.State.Strict
import Data.Char
import Data.DList (DList)
import qualified Data.DList as DL
import Data.Function
import Data.List
import Hoops
import Language.Cpp.Lex
import Language.Cpp.SyntaxToken


data Captures
    = Captures [SyntaxToken Hoops]
    | NoCaptures
    deriving (Show)


data Rest
    = Rest [SyntaxToken Hoops]
    | NoRest
    deriving (Show)


data PrefixRest
    = PrefixRest [SyntaxToken Hoops] [SyntaxToken Hoops]
    | NoPrefixRest
    deriving (Show)


data PrefixCaptures
    = PrefixCaptures [SyntaxToken Hoops] [SyntaxToken Hoops]
    | NoPrefixCaptures


data CapturesRest
    = CapturesRest [SyntaxToken Hoops] [SyntaxToken Hoops]
    | NoCapturesRest
    deriving (Show)


data PrefixCapturesRest
    = PrefixCapturesRest [SyntaxToken Hoops] [SyntaxToken Hoops] [SyntaxToken Hoops]
    | NoPrefixCapturesRest
    deriving (Show)


type Matcher a = [SyntaxToken Hoops] -> a


class Match a where
    match :: Code -> Matcher a


data Detailed a
    = Lit a
    | Wild Int a
    deriving (Show)


data DetailedRest
    = DetailedRest [Detailed (SyntaxToken Hoops)] [SyntaxToken Hoops]
    | NoDetailedRest


instance Match Bool where
    match code = let
        m = matchPrim code
        in \ts -> case m ts of
            Nothing -> False
            Just _ -> True


instance Match Captures where
    match code = let
        m = matchPrim code
        in \ts -> case m ts of
            Nothing -> NoCaptures
            Just (_, st) -> Captures (DL.toList $ captures st)


instance Match Rest where
    match code = let
        m = matchPrim code
        in \ts -> case m ts of
            Nothing -> NoRest
            Just (rest, _) -> Rest rest


instance Match PrefixRest where
    match code = let
        m = matchPrim code
        in \ts -> case m ts of
            Nothing -> NoPrefixRest
            Just (rest, st) -> PrefixRest (DL.toList $ everything st) rest


instance Match PrefixCaptures where
    match code = let
        m = matchPrim code
        in \ts -> case m ts of
            Nothing -> NoPrefixCaptures
            Just (_, st) -> PrefixCaptures (DL.toList $ everything st) (DL.toList $ captures st)


instance Match CapturesRest where
    match code = let
        m = matchPrim code
        in \ts -> case m ts of
            Nothing -> NoCapturesRest
            Just (rest, st) -> CapturesRest (DL.toList $ captures st) rest


instance Match PrefixCapturesRest where
    match code = let
        m = matchPrim code
        in \ts -> case m ts of
            Nothing -> NoPrefixCapturesRest
            Just (rest, st) -> PrefixCapturesRest (DL.toList $ everything st) (DL.toList $ captures st) rest


instance Match DetailedRest where
    match code = let
        m = matchPrim code
        in \ts -> case m ts of
            Nothing -> NoDetailedRest
            Just (rest, st) -> DetailedRest (DL.toList $ details st) rest


matchPrim :: Code -> [SyntaxToken Hoops] -> Maybe ([SyntaxToken Hoops], MatchState)
matchPrim code = let 
    es = lexEquiv code
    initState = MatchState 0 DL.empty DL.empty DL.empty
    in \ts -> case flip runState initState $ matchM es ts of
        (Nothing, _) -> Nothing
        (Just rest, st) -> Just (rest, st)


data MatchState = MatchState {
      wildIndex :: !Int
    , details :: DList (Detailed (SyntaxToken Hoops))
    , captures :: DList (SyntaxToken Hoops)
    , everything :: DList (SyntaxToken Hoops)
    }


matchM :: [EquivClass] -> [SyntaxToken Hoops] -> State MatchState (Maybe [SyntaxToken Hoops])
matchM [] toks = return $ Just toks
matchM (_:_) [] = return Nothing
matchM (e:es) (t:ts) = case e of
    Entity t' -> if t == t'
        then do
            modify $ \st -> st {
                  details = details st `DL.snoc` Lit t
                , everything = everything st `DL.snoc` t
                }
            matchM es ts
        else return Nothing
    Pred ct p -> case p t of
        Fail -> return Nothing
        Consume -> do
            wildIdx <- gets wildIndex
            modify $ \st -> st {
                  wildIndex = wildIndex st + 1
                , details = details st `DL.snoc` Wild wildIdx t
                , everything = everything st `DL.snoc` t
                , captures = case ct of
                    Capture -> captures st `DL.snoc` t
                    NoCapture -> captures st
                }
            matchM es ts
        NoConsume -> matchM es (t:ts)
        Continue p' -> do
            wildIdx <- gets wildIndex
            modify $ \st -> st {
                  wildIndex = wildIndex st + 1
                , details = details st `DL.snoc` Wild wildIdx t
                , everything = everything st `DL.snoc` t
                , captures = case ct of
                    Capture -> captures st `DL.snoc` t
                    NoCapture -> captures st
                }
            matchM (Pred ct p':es) ts


argsPred :: Int -> SyntaxToken Hoops -> PredResult
argsPred balance tok = if balance < 0
    then Fail
    else case tok of
        Punctuation p -> case unpunc p of
            "(" -> Continue $ argsPred (balance + 1)
            ")" -> if balance == 0
                then NoConsume
                else Continue $ argsPred (balance - 1)
            _ -> Continue $ argsPred balance
        _ -> Continue $ argsPred balance


rawEscapes :: [(String, SyntaxToken Hoops -> PredResult)]
rawEscapes = [
      ("any", \_ -> Consume)
    , ("int", \t -> case t of Integer {} -> Consume ; _ -> Fail)
    , ("flt", \t -> case t of Floating {} -> Consume ; _ -> Fail)
    , ("str", \t -> case t of String {} -> Consume ; _ -> Fail)
    , ("var", \t -> case t of Identifier {} -> Consume ; _ -> Fail)
    , ("num", \t -> case t of Integer {} -> Consume ; Floating {} -> Consume ; _ -> Fail)
    , ("key", \t -> case t of Ext (Key {}) -> Consume ; _ -> Fail)
    , ("seg", \t -> case t of Ext (SegPath {}) -> Consume ; _ -> Fail)
    , ("args", argsPred 0)
    ]


data CaptureType
    = Capture
    | NoCapture
    deriving (Show, Eq)


toManglePrefix :: CaptureType -> String
toManglePrefix mt = case mt of
    Capture -> "eW7jpK"
    NoCapture -> "VMxKy1"


fromManglePrefix :: String -> Maybe CaptureType
fromManglePrefix str
    | str == toManglePrefix Capture = Just Capture
    | str == toManglePrefix NoCapture = Just NoCapture
    | otherwise = Nothing


mangledEscapes :: CaptureType -> [(String, SyntaxToken Hoops -> PredResult)]
mangledEscapes ct = flip map rawEscapes $ \(e, p) -> (mangle e, p)
    where
        manglePrefix = toManglePrefix ct
        mangle = (manglePrefix ++)


rawMangleMap :: CaptureType -> [(String, String)]
rawMangleMap ct = on zip (map fst) rawEscapes (mangledEscapes ct)


mangleEscapes :: String -> String
mangleEscapes s = case s of
    "" -> ""
    '$':'$' : rest -> '$' : mangleEscapes rest
    '$':'!': (span isAlphaNum -> (raw, rest)) -> case lookup raw $ rawMangleMap NoCapture of
        Nothing -> "$!" ++ mangleEscapes (raw ++ rest)
        Just mangled -> mangled ++ mangleEscapes rest
    '$': (span isAlphaNum -> (raw, rest)) -> case lookup raw $ rawMangleMap Capture of
        Nothing -> "$" ++ mangleEscapes (raw ++ rest)
        Just mangled -> mangled ++ mangleEscapes rest
    c : rest -> c : mangleEscapes rest


lexEquiv :: Code -> [EquivClass]
lexEquiv code = case runLexer $ mangleEscapes code of
    Left err -> error $ show (code, err)
    Right toks -> flip map toks $ \t -> case t of
        Identifier name -> case lookup name $ mangledEscapes Capture of
            Just p -> Pred Capture p
            Nothing -> case lookup name $ mangledEscapes NoCapture of
                Just p -> Pred NoCapture p
                Nothing -> Entity t
        _ -> Entity t


data PredResult
    = Fail
    | Consume
    | NoConsume
    | Continue (SyntaxToken Hoops -> PredResult)


data EquivClass
    = Entity (SyntaxToken Hoops)
    | Pred CaptureType (SyntaxToken Hoops -> PredResult)












