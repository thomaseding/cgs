{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Transform.Extract.Util (
    cgsReadMetafile,
    double,
    Point(..),
    FaceIndex,
    PointsT,
    FacesT,
    PointsFacesT,
    StringT,
    IntegersT,
    Lists,
    points,
    faces,
    string,
    integers,
    ListsExtractor,
    ListsExtractFunc,
    runListsExtractor,
    evalListsExtractor,
    modifyPoints,
    modifyFaces,
    modifyString,
    modifyIntegers,
    gets,
    lift
) where


import Control.Monad.State.Lazy (MonadState, StateT, runStateT, modify, gets)
import Control.Monad.Trans (MonadTrans(..))
import Hoops.SyntaxToken
import Transform.Extract.Common (Extractor)


cgsReadMetafile :: FilePath -> Maybe Key -> [SyntaxToken Hoops]
cgsReadMetafile path mKey = case mKey of
    Nothing -> [i "CGS_Read_Metafile", p "(", s path, p ")", p ";"]
    Just key -> [i "DEFINE", p "(", i "CGS_Read_Metafile", p "(", s path, p ")", p ",", Ext $ Key key, p ")", p ";"]
    where
        s = String
        i = Identifier
        p = Punctuation . punc


double :: SyntaxToken Hoops -> Maybe Double
double tok = case tok of
    Integer n -> Just $ fromInteger n
    Floating q -> Just $ fromRational q
    _ -> Nothing


data Point = Point Double Double Double
    deriving (Show)
type FaceIndex = Integer


data T
data F

type PointsT'       b c d = (T, b, c, d)
type FacesT'        a c d = (a, T, c, d)
type PointsFacesT'    c d = (T, T, c, d)
type StringT'       a b d = (a, b, T, d)
type IntegersT'     a b c = (a, b, c, T)

type PointsT        = PointsT' F F F
type FacesT         = FacesT' F F F
type PointsFacesT   = PointsFacesT' F F
type StringT        = StringT' F F F
type IntegersT      = IntegersT' F F F


data Lists phantom = Lists {
    listPoints :: [Point],
    listFaces :: [FaceIndex],
    listString :: String,
    listIntegers :: [Integer]
} deriving (Show)

points :: Lists (PointsT' a b c) -> [Point]
points = listPoints

faces :: Lists (FacesT' a b c) -> [FaceIndex]
faces = listFaces

string :: Lists (StringT' a b c) -> String
string = listString

integers :: Lists (IntegersT' a b c) -> [Integer]
integers = listIntegers


type ListsExtractor phantom a = ListsStateT phantom Extractor a
type ListsExtractFunc phantom = [SyntaxToken Hoops] -> ListsExtractor phantom (Maybe [SyntaxToken Hoops])


newtype ListsStateT phantom m a = ListsStateT {
    unListsStateT :: StateT (Lists phantom) m a
} deriving (Functor, Monad, MonadTrans, MonadState (Lists phantom))


runListsExtractor :: ListsExtractor phantom a -> Extractor (a, Lists phantom)
runListsExtractor = flip runStateT st . unListsStateT
    where
        st = Lists [] [] [] []


evalListsExtractor :: ListsExtractor phantom a -> Extractor a
evalListsExtractor = fmap fst . runListsExtractor


modifyPoints :: ([Point] -> [Point]) -> ListsExtractor (PointsT' a b c) ()
modifyPoints f = modify $ \st -> st { listPoints = f $ listPoints st }

modifyFaces :: ([FaceIndex] -> [FaceIndex]) -> ListsExtractor (FacesT' a b c) ()
modifyFaces f = modify $ \st -> st { listFaces = f $ listFaces st }

modifyString :: (String -> String) -> ListsExtractor (StringT' a b c) ()
modifyString f = modify $ \st -> st { listString = f $ listString st }

modifyIntegers :: ([Integer] -> [Integer]) -> ListsExtractor (IntegersT' a b c) ()
modifyIntegers f = modify $ \st -> st { listIntegers = f $ listIntegers st }













