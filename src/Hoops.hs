module Hoops (
      Hoops(..)
    , expandHoops
    ) where


import Language.Cpp.SyntaxToken


data Hoops = Hoops
    deriving (Show, Eq, Ord)


expandHoops :: Hoops -> [SyntaxToken ()]
expandHoops = const []


