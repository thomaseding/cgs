module UsedKeys (
      usedKeys
    ) where


import Data.List
import Hoops
import Language.Cpp.SyntaxToken


hlookup :: [SyntaxToken Hoops]
hlookup = [Identifier "LOOKUP", Punctuation $ punc "("]


usedKeys :: [SyntaxToken Hoops] -> [Key]
usedKeys tokens = case stripPrefix hlookup tokens of
    Just (Ext (Key key) : rest) -> key : usedKeys rest
    Nothing -> case tokens of
        _ : ts -> usedKeys ts
        [] -> []


