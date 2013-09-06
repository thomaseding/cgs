module Query.UsedKeys (
    usedKeys
) where


import Control.Exception (assert)
import Data.List (stripPrefix)
import Hoops.SyntaxToken
import Prelude hiding (lookup)


lookup :: [SyntaxToken Hoops]
lookup = [Identifier "LOOKUP", Punctuation $ punc "("]


usedKeys :: [SyntaxToken Hoops] -> [Key]
usedKeys tokens = case stripPrefix lookup tokens of
    Just (Ext (Key key) : rest) -> key : usedKeys rest
    Nothing -> case tokens of
        _ : ts -> usedKeys ts
        [] -> []
    Just _ -> assert False undefined

