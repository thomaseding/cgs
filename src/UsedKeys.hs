module UsedKeys (
      usedKeys
    ) where


import Data.List
import Language.Cpp.SyntaxToken


usedKeys :: [SyntaxToken] -> [Integer]
usedKeys tokens = case stripPrefix [Identifier "LOOKUP", Punctuation $ punc "("] tokens of
    Just (Integer n : rest) -> n : usedKeys rest
    Nothing -> case tokens of
        _ : ts -> usedKeys ts
        [] -> []


