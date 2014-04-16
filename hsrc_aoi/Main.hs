{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | a REPL for AO
module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Strict

--import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Sequence as S
import qualified Data.Map as M
import qualified Data.List as L

import qualified System.IO as Sys
import qualified System.IO.Error as Err
-- import System.IO.Unsafe (unsafeInterleaveIO)
import qualified System.Exit as Sys
import qualified System.Environment as Env
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FS
import qualified System.Console.Haskeline as HKL

-- AO imports
import AO.AOFile
import AO.Dict
import AO.Imperative.AORT

-- LOCAL UTILITIES (from ../hsrc_aort)
import ShowEnv

-- a dubiously useful help message
helpMsg :: String
helpMsg =
    "The `aoi` executable implements an interactive AO REPL. It does\n\
    \not take any arguments, but can be configured somewhat:\n\
    \\n\
    \Environment Variables:\n\
    \    AO_PATH: where to search for '.ao' files \n\
    \    AO_HOME: where to keep persistent state; default ./aostate \n\
    \    AOI_DICT: root dictionary text; default \"aoi\" \n\
    \\n\
    \Haskeline Configuration (history, edit mode, etc.): \n\
    \    see http://trac.haskell.org/haskeline/wiki/UserPrefs \n\
    \\n\
    \This REPL does not allow defining words. Developers instead edit \n\
    \the dictionary then hit ctrl+c (interrupt) to reload it. Limited \n\
    \undo is available using command `@undo`. \n\
    \"

-- | The AOI monad is above AORT but below Haskeline.
-- It primarily manages the dictionaries and histories.
type AOI = StateT AOI_CONTEXT RT
data AOI_CONTEXT = AOI_CONTEXT 
    { aoi_dict    :: !Dict  -- loaded and compiled dictionary
    , aoi_hist    :: !Hist  -- previous states 
    , aoi_histLen :: !Int   -- how much history to keep
    }

type Dict = AODict AOFMeta  -- the current dictionary
type RT = AORT ()           -- not using extended context (yet)
type RTVal = V RT           -- a value in our AO runtime
type Hist = S.Seq RTVal     -- a history of values for @undo
type HKL = HKL.InputT AOI   -- haskeline input monad

emptyContext :: AOI_CONTEXT
emptyContext = AOI_CONTEXT emptyAODict emptyHist 0 where
    emptyHist = S.empty

main :: IO ()
main = Env.getArgs >>= runArgs

runArgs :: [String] -> IO ()
runArgs [] = runAOI
runArgs ["help"] = Sys.putStrLn helpMsg 
runArgs _ = 
    putErrLn "arguments not recognized; try `aoi help`" >>
    Sys.exitFailure

putErrLn :: String -> IO ()
putErrLn = Sys.hPutStrLn Sys.stderr
    
-- get AOI_DICT environment variable (default 'aoi')
getAOI_DICT :: IO String
getAOI_DICT = Err.catchIOError (Env.getEnv "AOI_DICT") (const (pure "aoi"))

-- runAOI will manage initial configurations, then enter a
-- permanent 'recovery loop'
runAOI :: IO ()
runAOI = 
    getAOI_DICT >>= \ rootDict ->
    getHistoryFile >>= \ histFile ->
    let hklSettings = HKL.Settings
            { HKL.complete = aoiCompletion
            , HKL.historyFile = histFile
            , HKL.autoAddHistory = True
            }
    in
    error "TODO: main AOI loop!"

tryIO :: IO a -> IO (Maybe a)
tryIO = liftM eitherToMaybe . Err.tryIOError where
    eitherToMaybe = either (const Nothing) Just

getHistoryFile :: IO (Maybe Sys.FilePath)
getHistoryFile = tryIO $
    FS.getAppDataDirectory (T.pack "aoi") >>= \ appDir ->
    FS.createTree appDir >>
    let fp = appDir FS.</> FS.fromText (T.pack "hist.haskeline") in
    FS.appendTextFile fp T.empty >> -- create file if it does not exist
    return (FS.encodeString fp)


-- for now, using a trivial prefix search on dictionary. I would
-- prefer a 'fuzzy find' but Haskeline doesn't support it
aoiCompletion :: HKL.CompletionFunc AOI
aoiCompletion = quotedFiles prefixedWords where
    quotedFiles = HKL.completeQuotedWord Nothing "\"" HKL.listFiles
    prefixedWords = HKL.completeWord Nothing " \n[](|)" findWords
    findWords s = 
        gets (readAODict . aoi_dict) >>= \ d ->
        return (dictCompletions d s)

dictCompletions :: M.Map Word val -> String -> [HKL.Completion]
dictCompletions _ [] = []
dictCompletions _ (_:[]) = []
dictCompletions dc str = 
    let ws = fmap T.unpack $ M.keys dc in
    let wsP = L.filter (str `L.isPrefixOf`) ws in
    L.map HKL.simpleCompletion wsP

