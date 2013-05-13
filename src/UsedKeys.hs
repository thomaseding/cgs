module UsedKeys (
      usedKeys
    ) where


import Data.List
import Hoops
import Language.Cpp.SyntaxToken


hlookup :: [SyntaxToken Hoops]
hlookup = [Identifier "LOOKUP", Punctuation $ punc "("]


usedKeys :: [SyntaxToken Hoops] -> [Integer]
usedKeys tokens = case stripPrefix hlookup tokens of
    Just (Integer n : rest) -> n : usedKeys rest
    Nothing -> case tokens of
        _ : ts -> usedKeys ts
        [] -> []


