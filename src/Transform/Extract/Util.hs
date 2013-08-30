{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Transform.Extract.Util (
    cgsReadMetafile,
    double,
    Point,
    FaceIndex,
    PointsT,
    FacesT,
    PointsFacesT,
    StringT,
    Lists,
    points,
    faces,
    string,
    ListsExtractor,
    runListsExtractor,
    evalListsExtractor,
    getList,
    modifyPoints,
    modifyFaces,
    modifyString,
    lift
) where


import Control.Monad.State.Lazy
import Hoops.SyntaxToken
import Transform.Extract.Common


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


type Point = (Double, Double, Double)
type FaceIndex = Integer


data X


type PointsT'       b c = (X, b, c)
type FacesT'        a c = (a, X, c)
type PointsFacesT'    c = (X, X, c)
type StringT'       a b = (a, b, X)


type PointsT        = PointsT' () ()
type FacesT         = FacesT' () ()
type PointsFacesT   = PointsFacesT' ()
type StringT        = StringT' () ()


data Lists phantom = Lists {
    listPoints :: [Point],
    listFaces :: [FaceIndex],
    listString :: String
} deriving (Show)


points :: Lists (PointsT' a b) -> [Point]
points = listPoints


faces :: Lists (FacesT' a b) -> [FaceIndex]
faces = listFaces


string :: Lists (StringT' a b) -> String
string = listString


type ListsExtractor phantom a = ListsStateT phantom Extractor a


newtype ListsStateT phantom m a = ListsStateT {
    unListsStateT :: StateT (Lists phantom) m a
} deriving (Functor, Monad, MonadTrans)


runListsExtractor :: ListsExtractor phantom a -> Extractor (a, Lists phantom)
runListsExtractor = flip runStateT st . unListsStateT
    where
        st = Lists [] [] []


evalListsExtractor :: ListsExtractor phantom a -> Extractor a
evalListsExtractor = fmap fst . runListsExtractor


getList :: (Lists phantom -> a) -> ListsExtractor phantom a
getList f = ListsStateT $ gets f


modifyPoints :: ([Point] -> [Point]) -> ListsExtractor (PointsT' a b) ()
modifyPoints f = ListsStateT $ modify $ \st -> st { listPoints = f $ listPoints st }


modifyFaces :: ([FaceIndex] -> [FaceIndex]) -> ListsExtractor (FacesT' a b) ()
modifyFaces f = ListsStateT $ modify $ \st -> st { listFaces = f $ listFaces st }


modifyString :: (String -> String) -> ListsExtractor (StringT' a b) ()
modifyString f = ListsStateT $ modify $ \st -> st { listString = f $ listString st }








