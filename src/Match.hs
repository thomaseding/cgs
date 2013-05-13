{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Match (
      Match(..)
    , Matcher
    , Rest(..)
    , PrefixRest(..)
    , WildsRest(..)
    ) where


import Control.Monad.State.Strict
import Data.DList (DList)
import qualified Data.DList as DL
import Data.Function
import Language.Cpp.Lex
import Language.Cpp.SyntaxToken


data Rest
    = Rest [SyntaxToken]
    | NoRest


data PrefixRest
    = PrefixRest [SyntaxToken] [SyntaxToken]
    | NoPrefixRest


data WildsRest
    = WildsRest [SyntaxToken] [SyntaxToken]
    | NoWildsRest


type Matcher a = [SyntaxToken] -> a


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


matchPrim :: Code -> [SyntaxToken] -> Maybe ([SyntaxToken], MatchState)
matchPrim code = let 
    es = lexEquiv code
    initState = MatchState DL.empty DL.empty
    in \ts -> case flip runState initState $ matchM es ts of
        (Nothing, _) -> Nothing
        (Just rest, st) -> Just (rest, st)


data MatchState = MatchState {
      matchedWilds :: DList SyntaxToken
    , matchedEverything :: DList SyntaxToken
    }


matchM :: [EquivClass SyntaxToken] -> [SyntaxToken] -> State MatchState (Maybe [SyntaxToken])
matchM [] toks = return $ Just toks
matchM (_:_) [] = return Nothing
matchM (e:es) (t:ts) = if e == Entity t
    then do
        modify $ \st -> st { matchedEverything = matchedEverything st `DL.snoc` t }
        case e of
            Pred {} -> modify $ \st -> st { matchedWilds = matchedWilds st `DL.snoc` t }
            _ -> return ()
        matchM es ts
    else return Nothing


rawEscapes :: [(String, SyntaxToken -> Bool)]
rawEscapes = [
      ("$int", \t -> case t of Integer {} -> True ; _ -> False)
    , ("$flt", \t -> case t of Floating {} -> True ; _ -> False)
    , ("$str", \t -> case t of String {} -> True ; _ -> False)
    , ("$var", \t -> case t of Identifier {} -> True ; _ -> False)
    , ("$num", \t -> case t of Integer {} -> True ; Floating {} -> True ; _ -> False)
    ]


mangledEscapes :: [(String, SyntaxToken -> Bool)]
mangledEscapes = flip map rawEscapes $ \('$':e, p) -> (mangle e, p)
    where
        mangle = ("eWj" ++)


rawMangleMap :: [(String, String)]
rawMangleMap = on zip (map fst) rawEscapes mangledEscapes


mangleEscapes :: String -> String
mangleEscapes s = case s of
    "" -> ""
    '$':'$' : rest -> '$' : mangleEscapes rest
    '$':r:a:w : rest -> case lookup ['$',r,a,w] rawMangleMap of
        Nothing -> '$' : mangleEscapes (r:a:w:rest)
        Just mangled -> mangled ++ mangleEscapes rest
    c : rest -> c : mangleEscapes rest


lexEquiv :: Code -> [EquivClass SyntaxToken]
lexEquiv code = case runLexer $ mangleEscapes code of
    Left err -> error $ show err
    Right toks -> flip map toks $ \t -> case t of
        Identifier name -> maybe (Entity t) Pred $ lookup name mangledEscapes
        _ -> Entity t


data EquivClass a = Entity a | Pred (a -> Bool)


instance (Eq a) => Eq (EquivClass a) where
    Entity x == Entity y = x == y
    Entity x == Pred p = p x
    Pred p == Entity x = p x
    Pred _ == Pred _ = False










