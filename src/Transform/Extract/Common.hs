{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Transform.Extract.Common (
      BlockKind
    , ExtractFunc
    , Extractor
    , runExtractor
    , Entry
    , scopedEntry
    , tellEntry
    ) where


import Control.Monad.State.Lazy
import Hoops.SyntaxToken
import System.FilePath
import System.IO


type BlockKind = String
type ExtractFunc = [SyntaxToken Hoops] -> Maybe (Extractor [SyntaxToken Hoops])


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
    }
    deriving (Show)


newEntry :: String -> Extractor Entry
newEntry entryPrefix = Extractor $ do
    basePath <- gets entryBasePath
    entId <- gets nextEntryId
    modify $ \st -> st { nextEntryId = entId + 1 }
    let path = basePath </> (entryPrefix ++ "-" ++ show entId)
    handle <- liftIO $ openFile path WriteMode
    let entry = Entry { entryHandle = handle }
    return entry


scopedEntry :: String -> (Entry -> Extractor a) -> Extractor a
scopedEntry entryPrefix f = do
    entry <- newEntry entryPrefix
    res <- f entry
    Extractor $ liftIO $ hClose $ entryHandle entry
    return res


tellEntry :: Entry -> String -> Extractor ()
tellEntry (Entry handle) str = Extractor $ do
    liftIO $ hPutStr handle str






