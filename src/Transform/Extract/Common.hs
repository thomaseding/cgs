{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Transform.Extract.Common (
    BlockKind,
    ExtractFunc,
    Extractor,
    ExtractOptions(..),
    runExtractor,
    Entry,
    entryPath,
    scopedEntry,
    tellEntry,
    getOptions
) where


import Control.Monad.State.Lazy (StateT, evalStateT, gets, modify)
import Control.Monad.Trans (liftIO)
import Hoops.SyntaxToken
import System.FilePath ((</>))
import System.IO (openFile, IOMode(WriteMode), hClose, Handle, hPutStr)


type BlockKind = String
type NamePrefix = String
type Extension = String
type ExtractFunc = [SyntaxToken Hoops] -> Extractor (Maybe [SyntaxToken Hoops])


type EntryId = Int


data ExtractOptions = ExtractOptions {
    minStmtRequirement :: Int }


data ExtractorState = ExtractorState {
    entryBasePath :: FilePath,
    nextEntryId :: EntryId,
    options :: ExtractOptions
}


newtype Extractor a = Extractor {
    unExtractor :: StateT ExtractorState IO a
} deriving (Functor, Monad)


runExtractor :: ExtractOptions -> FilePath -> Extractor a -> IO a
runExtractor opts basePath = flip evalStateT st . unExtractor
    where
        st = ExtractorState {
            entryBasePath = basePath,
            nextEntryId = 0,
            options = opts }


data Entry = Entry {
    entryHandle :: Handle,
    entryPath :: FilePath
} deriving (Show)


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


getOptions :: Extractor ExtractOptions
getOptions = Extractor $ gets options



