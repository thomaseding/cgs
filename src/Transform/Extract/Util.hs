{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Transform.Extract.Util (
    cgsReadMetafile,
    double,
    Point,
    FaceIndex,
    PointT,
    FaceT,
    PointsT,
    FacesT,
    PointsFacesT,
    Lists,
    points,
    faces,
    ListsExtractor,
    runListsExtractor,
    evalListsExtractor,
    getList,
    modifyPoints,
    modifyFaces,
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


data PointT
data FaceT


type PointsT = (PointT, ())
type FacesT = ((), FaceT)
type PointsFacesT = (PointT, FaceT)


data Lists phantom = Lists {
    listPoints :: [Point],
    listFaces :: [FaceIndex]
} deriving (Show)


points :: Lists (PointT, face) -> [Point]
points = listPoints


faces :: Lists (point, FaceT) -> [FaceIndex]
faces = listFaces


type ListsExtractor phantom a = ListsStateT phantom Extractor a


newtype ListsStateT phantom m a = ListsStateT {
    unListsStateT :: StateT (Lists phantom) m a
} deriving (Functor, Monad, MonadTrans)


runListsExtractor :: ListsExtractor phantom a -> Extractor (a, Lists phantom)
runListsExtractor = flip runStateT st . unListsStateT
    where
        st = Lists [] []


evalListsExtractor :: ListsExtractor phantom a -> Extractor a
evalListsExtractor = fmap fst . runListsExtractor


getList :: (Lists phantom -> a) -> ListsExtractor phantom a
getList f = ListsStateT $ gets f


modifyPoints :: ([Point] -> [Point]) -> ListsExtractor (PointT, face) ()
modifyPoints f = ListsStateT $ modify $ \st -> st { listPoints = f $ listPoints st }


modifyFaces :: ([FaceIndex] -> [FaceIndex]) -> ListsExtractor (point, FaceT) ()
modifyFaces f = ListsStateT $ modify $ \st -> st { listFaces = f $ listFaces st }












