{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Match (
      Match(..)
    , Matcher
    , Rest(..)
    , PrefixRest(..)
    , WildsRest(..)
    , PrefixWildsRest(..)
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


data Rest
    = Rest [SyntaxToken Hoops]
    | NoRest
    deriving (Show)


data PrefixRest
    = PrefixRest [SyntaxToken Hoops] [SyntaxToken Hoops]
    | NoPrefixRest
    deriving (Show)


data WildsRest
    = WildsRest [SyntaxToken Hoops] [SyntaxToken Hoops]
    | NoWildsRest
    deriving (Show)


data PrefixWildsRest
    = PrefixWildsRest [SyntaxToken Hoops] [SyntaxToken Hoops] [SyntaxToken Hoops]
    | NoPrefixWildsRest
    deriving (Show)


type Matcher a = [SyntaxToken Hoops] -> a


class Match a where
    match :: Code -> Matcher a


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
            Just (rest, st) -> PrefixRest (DL.toList $ matchedEverything st) rest


instance Match WildsRest where
    match code = let
        m = matchPrim code
        in \ts -> case m ts of
            Nothing -> NoWildsRest
            Just (rest, st) -> WildsRest (DL.toList $ matchedWilds st) rest


instance Match PrefixWildsRest where
    match code = let
        m = matchPrim code
        in \ts -> case m ts of
            Nothing -> NoPrefixWildsRest
            Just (rest, st) -> PrefixWildsRest (DL.toList $ matchedEverything st) (DL.toList $ matchedWilds st) rest


matchPrim :: Code -> [SyntaxToken Hoops] -> Maybe ([SyntaxToken Hoops], MatchState)
matchPrim code = let 
    es = lexEquiv code
    initState = MatchState DL.empty DL.empty
    in \ts -> case flip runState initState $ matchM es ts of
        (Nothing, _) -> Nothing
        (Just rest, st) -> Just (rest, st)


data MatchState = MatchState {
      matchedWilds :: DList (SyntaxToken Hoops)
    , matchedEverything :: DList (SyntaxToken Hoops)
    }


matchM :: [EquivClass] -> [SyntaxToken Hoops] -> State MatchState (Maybe [SyntaxToken Hoops])
matchM [] toks = return $ Just toks
matchM (_:_) [] = return Nothing
matchM (e:es) (t:ts) = case e of
    Entity t' -> if t == t'
        then do
            modify $ \st -> st { matchedEverything = matchedEverything st `DL.snoc` t }
            matchM es ts
        else return Nothing
    Pred p -> case p t of
        Fail -> return Nothing
        Consume -> do
            modify $ \st -> st {
                  matchedEverything = matchedEverything st `DL.snoc` t
                , matchedWilds = matchedWilds st `DL.snoc` t
                }
            matchM es ts
        NoConsume -> matchM es (t:ts)
        Continue p' -> do
            modify $ \st -> st {
                  matchedEverything = matchedEverything st `DL.snoc` t
                , matchedWilds = matchedWilds st `DL.snoc` t
                }
            matchM (Pred p':es) ts


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
      ("$any", \_ -> Consume)
    , ("$int", \t -> case t of Integer {} -> Consume ; _ -> Fail)
    , ("$flt", \t -> case t of Floating {} -> Consume ; _ -> Fail)
    , ("$str", \t -> case t of String {} -> Consume ; _ -> Fail)
    , ("$var", \t -> case t of Identifier {} -> Consume ; _ -> Fail)
    , ("$num", \t -> case t of Integer {} -> Consume ; Floating {} -> Consume ; _ -> Fail)
    , ("$args", argsPred 0)
    ]


mangledEscapes :: [(String, SyntaxToken Hoops -> PredResult)]
mangledEscapes = flip map rawEscapes $ \('$':e, p) -> (mangle e, p)
    where
        mangle = ("eW7jpK" ++)


rawMangleMap :: [(String, String)]
rawMangleMap = on zip (map fst) rawEscapes mangledEscapes


mangleEscapes :: String -> String
mangleEscapes s = case s of
    "" -> ""
    '$':'$' : rest -> '$' : mangleEscapes rest
    '$': (span isAlphaNum -> (raw, rest)) -> case lookup ('$':raw) rawMangleMap of
        Nothing -> '$' : mangleEscapes (raw ++ rest)
        Just mangled -> mangled ++ mangleEscapes rest
    c : rest -> c : mangleEscapes rest


lexEquiv :: Code -> [EquivClass]
lexEquiv code = case runLexer $ mangleEscapes code of
    Left err -> error $ show err
    Right toks -> flip map toks $ \t -> case t of
        Identifier name -> maybe (Entity t) Pred $ lookup name mangledEscapes
        _ -> Entity t


data PredResult
    = Fail
    | Consume
    | NoConsume
    | Continue (SyntaxToken Hoops -> PredResult)


data EquivClass
    = Entity (SyntaxToken Hoops)
    | Pred (SyntaxToken Hoops -> PredResult)












