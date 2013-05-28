{-# LANGUAGE FlexibleInstances #-}

module Hoops.SyntaxToken (
      module Language.Cpp.SyntaxToken
    , module Hoops.SegPath
    , Hoops(..)
    , expandHoops
    , Key
    , MakeKey(..)
    ) where


import Language.Cpp.SyntaxToken
import Hoops.SegPath


data Hoops
    = Key Key
    | SegPath SegPath
    deriving (Show, Eq, Ord)


expandHoops :: Hoops -> [SyntaxToken ()]
expandHoops hoops = case hoops of
    Key key -> case key of
        Key2 x y -> [Integer x, Punctuation $ punc ",", Integer y]
    SegPath s -> [String $ toString s]


data Key = Key2 Integer Integer
    deriving (Eq, Ord)


instance Show Key where
    show (Key2 x y) = "Key(" ++ show x ++ "," ++ show y ++ ")"


class MakeKey a where
    mkKey :: a -> Key


instance MakeKey Key where
    mkKey = id


instance MakeKey Integer where
    mkKey = flip Key2 (-1)


instance MakeKey (Integer, Integer) where
    mkKey = uncurry Key2


instance MakeKey (Integer, Maybe Integer) where
    mkKey (x, my) = Key2 x $ maybe (-1) id my



