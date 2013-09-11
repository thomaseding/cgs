{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Transform.Extract.Util (
    cgsReadMetafile,
    double,
    Point(..),
    Vector(..),
    FaceIndex,
    PointsT,
    FacesT,
    PointsFacesT,
    StringT,
    IntegersT,
    VectorsT,
    Lists,
    points,
    faces,
    string,
    integers,
    vectors,
    ListsExtractor,
    ListsExtractFunc,
    runListsExtractor,
    evalListsExtractor,
    modifyPoints,
    modifyFaces,
    modifyString,
    modifyIntegers,
    modifyVectors,
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
data Vector = Vector Double Double Double
    deriving (Show)

type FaceIndex = Integer


data T
data F

type PointsT'       b c d e = (T, b, c, d, e)
type FacesT'        a c d e = (a, T, c, d, e)
type PointsFacesT'    c d e = (T, T, c, d, e)
type StringT'       a b d e = (a, b, T, d, e)
type IntegersT'     a b c e = (a, b, c, T, e)
type VectorsT'      a b c d = (a, b, c, d, T)

type PointsT        = PointsT' F F F F
type FacesT         = FacesT' F F F F
type PointsFacesT   = PointsFacesT' F F F
type StringT        = StringT' F F F F
type IntegersT      = IntegersT' F F F F
type VectorsT       = VectorsT' F F F F


data Lists phantom = Lists {
    listPoints :: [Point],
    listFaces :: [FaceIndex],
    listString :: String,
    listIntegers :: [Integer],
    listVectors :: [Vector]
} deriving (Show)

points :: Lists (PointsT' a b c d) -> [Point]
points = listPoints

faces :: Lists (FacesT' a b c d) -> [FaceIndex]
faces = listFaces

string :: Lists (StringT' a b c d) -> String
string = listString

integers :: Lists (IntegersT' a b c d) -> [Integer]
integers = listIntegers

vectors :: Lists (VectorsT' a b c d) -> [Vector]
vectors = listVectors


type ListsExtractor phantom a = ListsStateT phantom Extractor a
type ListsExtractFunc phantom = [SyntaxToken Hoops] -> ListsExtractor phantom (Maybe [SyntaxToken Hoops])


newtype ListsStateT phantom m a = ListsStateT {
    unListsStateT :: StateT (Lists phantom) m a
} deriving (Functor, Monad, MonadTrans, MonadState (Lists phantom))


runListsExtractor :: ListsExtractor phantom a -> Extractor (a, Lists phantom)
runListsExtractor = flip runStateT st . unListsStateT
    where
        st = Lists [] [] [] [] []


evalListsExtractor :: ListsExtractor phantom a -> Extractor a
evalListsExtractor = fmap fst . runListsExtractor


modifyPoints :: ([Point] -> [Point]) -> ListsExtractor (PointsT' a b c d) ()
modifyPoints f = modify $ \st -> st { listPoints = f $ listPoints st }

modifyFaces :: ([FaceIndex] -> [FaceIndex]) -> ListsExtractor (FacesT' a b c d) ()
modifyFaces f = modify $ \st -> st { listFaces = f $ listFaces st }

modifyString :: (String -> String) -> ListsExtractor (StringT' a b c d) ()
modifyString f = modify $ \st -> st { listString = f $ listString st }

modifyIntegers :: ([Integer] -> [Integer]) -> ListsExtractor (IntegersT' a b c d) ()
modifyIntegers f = modify $ \st -> st { listIntegers = f $ listIntegers st }

modifyVectors :: ([Vector] -> [Vector]) -> ListsExtractor (VectorsT' a b c d) ()
modifyVectors f = modify $ \st -> st { listVectors = f $ listVectors st }












