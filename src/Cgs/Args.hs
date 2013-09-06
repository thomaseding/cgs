module Cgs.Args (
    CgsOptions(..),
    parseArgs,
    parseArgsIO
) where


import Control.Monad.Identity (Identity(Identity))
import Control.Monad.State (State, execState, modify)
import System.Environment (getArgs)
import Transform.Extract (ExtractOptions(..))


data CgsOptions f = CgsOptions {
    cgsFile :: f FilePath,
    cgsExtractOpts :: ExtractOptions }


type ParseState = CgsOptions Maybe


collapse :: ParseState -> Maybe (CgsOptions Identity)
collapse CgsOptions {
    cgsFile = Just file,
    cgsExtractOpts = opts } = Just $ CgsOptions {
        cgsFile = Identity file,
        cgsExtractOpts = opts }
collapse _ = Nothing


parseArgsIO :: IO (Maybe (CgsOptions Identity))
parseArgsIO = fmap parseArgs getArgs


parseArgs :: [String] -> Maybe (CgsOptions Identity)
parseArgs = collapse . flip execState st . parseArgsM
    where
        st = CgsOptions {
            cgsFile = Nothing,
            cgsExtractOpts = extractOpts }
        extractOpts = ExtractOptions {
            minStmtRequirement = 0 }


parseArgsM :: [String] -> State ParseState ()
parseArgsM args = case args of
    [file] -> modify $ \st -> st { cgsFile = Just file }
    _ -> return ()



