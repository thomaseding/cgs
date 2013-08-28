module Transform.Extract.Shell (
      extractor
    ) where


import Hoops.Match
import Hoops.SyntaxToken
import Transform.Extract.Common


extractor :: BlockKind -> ExtractFunc
extractor kind = case kind of
    "HC_Insert_Shell" -> Just . extract
    _ -> const Nothing


extract :: [SyntaxToken Hoops] -> Extractor [SyntaxToken Hoops]
extract = let
    --point = match "points[$int].x = $num; points[$int].y = $num; points[$int].z = $num;"
    in return


