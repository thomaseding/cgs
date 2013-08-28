{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Transform.Extract.Common (
      BlockKind
    , ExtractFunc
    , Extractor
    , runExtractor
    , Entry
    , entryPath
    , scopedEntry
    , tellEntry
    , cgsReadMetafile
    ) where


import Control.Monad.State.Lazy
import Hoops.SyntaxToken
import System.FilePath
import System.IO


type BlockKind = String
type NamePrefix = String
type Extension = String
type ExtractFunc = [SyntaxToken Hoops] -> Extractor (Maybe [SyntaxToken Hoops])


type EntryId = Int


data ExtractorState = ExtractorState {
      entryBasePath :: FilePath
    , nextEntryId :: EntryId
    }


newtype Extractor a = Extractor {
      unExtractor :: StateT ExtractorState IO a
    }
    deriving (Functor, Monad)


runExtractor :: FilePath -> Extractor a -> IO a
runExtractor basePath extractor = do
    (res, _) <- flip runStateT st $ unExtractor extractor
    return res
    where
        st = ExtractorState {
              entryBasePath = basePath
            , nextEntryId = 0
            }


data Entry = Entry {
      entryHandle :: Handle
    , entryPath :: FilePath
    }
    deriving (Show)


newEntry :: NamePrefix -> Extension -> Extractor Entry
newEntry entryPrefix ext = Extractor $ do
    basePath <- gets entryBasePath
    entId <- gets nextEntryId
    modify $ \st -> st { nextEntryId = entId + 1 }
    let path = basePath </> (entryPrefix ++ "-" ++ show entId ++ "." ++ ext)
    handle <- liftIO $ openFile path WriteMode
    let entry = Entry { entryHandle = handle, entryPath = path }
    return entry


scopedEntry :: NamePrefix -> Extension -> (Entry -> Extractor a) -> Extractor a
scopedEntry entryPrefix ext f = do
    entry <- newEntry entryPrefix ext
    res <- f entry
    Extractor $ liftIO $ hClose $ entryHandle entry
    return res


tellEntry :: Entry -> String -> Extractor ()
tellEntry entry str = Extractor $ do
    liftIO $ hPutStr (entryHandle entry) str


cgsReadMetafile :: FilePath -> Maybe Key -> [SyntaxToken Hoops]
cgsReadMetafile path mKey = case mKey of
    Nothing -> [i "CGS_Read_Metafile", p "(", s path, p ")", p ";"]
    Just key -> [i "DEFINE", p "(", i "CGS_Read_Metafile", p "(", s path, p ")", p ",", Ext $ Key key, p ")", p ";"]
    where
        s = String
        i = Identifier
        p = Punctuation . punc





