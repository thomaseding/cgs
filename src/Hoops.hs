{-# LANGUAGE FlexibleInstances #-}

module Hoops (
      Hoops(..)
    , expandHoops
    , Key
    , MakeKey(..)
    ) where


import Language.Cpp.SyntaxToken


data Hoops
    = Key Key
    deriving (Show, Eq, Ord)


expandHoops :: Hoops -> [SyntaxToken ()]
expandHoops hoops = case hoops of
    Key key -> case key of
        Key1 x -> [Integer x]
        Key2 x y -> [Integer x, Punctuation $ punc ",", Integer y]


data Key
    = Key1 Integer
    | Key2 Integer Integer
    deriving (Show, Eq, Ord)


class MakeKey a where
    mkKey :: a -> Key


instance MakeKey Key where
    mkKey = id


instance MakeKey Integer where
    mkKey = Key1


instance MakeKey (Integer, Integer) where
    mkKey (x, y) = case y of
        -1 -> Key1 x
        _ -> Key2 x y


instance MakeKey (Integer, Maybe Integer) where
    mkKey (x, my) = case my of
        Nothing -> Key1 x
        Just y -> Key2 x y


