{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | a REPL for AO
module Main (main) where

import Control.Applicative
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import qualified Control.Exception as Err

-- import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Sequence as S
import qualified Data.Map as M
import qualified Data.List as L

import qualified System.IO as Sys
-- import System.IO.Unsafe (unsafeInterleaveIO)
import qualified System.Exit as Sys
import qualified System.Environment as Env
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FS
import qualified System.Console.Haskeline as HKL

import qualified Text.Parsec as P

-- AO imports
import AO.AOFile
import AO.Dict
import AO.Code
import AO.Parser
import AO.Compile
import ABC.Operators
import ABC.Simplify
import ABC.Imperative.Value
import ABC.Imperative.Interpreter

-- LOCAL UTILITIES (from ../hsrc_aort)
import AORT
import Util
import ShowEnv

-- a dubiously useful help message
helpMsg :: String
helpMsg =
    "The `aoi` executable implements an interactive AO REPL. It does\n\
    \not take any arguments, but can be configured somewhat:\n\
    \\n\
    \Environment Variables:\n\
    \    AO_PATH: where to search for '.ao' files \n\
    \    AOI_DICT: root dictionary text; default \"aoi\" \n\
    \    \n\
    \    AO_TEMP: directory for temporaries; default \"aotmp\" \n\
    \\n\
    \Haskeline Configuration (history, edit mode, etc.): \n\
    \    see http://trac.haskell.org/haskeline/wiki/UserPrefs \n\
    \\n\
    \This REPL does not allow definition of words. Developers can \n\
    \edit their dictionary then use the `@reload` meta command. \n\
    \"
-- when persistence is working, perhaps add an AO_HOME or similar.

-- | The AOI monad is above AORT but below Haskeline.
-- It primarily manages the dictionaries and histories.
type AOI = StateT AOICX IO
data AOICX = AOICX 
    { aoi_dict     :: !Dict  -- loaded and compiled dictionary
    , aoi_dictSrc  :: !String -- root dictionary text
    , aoi_rtval    :: !RtVal -- active runtime value
    , aoi_hist     :: !(S.Seq RtVal)
    , aoi_histLen  :: !Int
    , aoi_rtcx     :: !RTCX  -- current runtime context
    }

type Dict = AODict AOFMeta  -- the current dictionary
type RT = AORT              -- not using extended context (yet)
type RTCX = AORT_CX 
type RtVal = V RT           -- a value in our AO runtime
type HKL = HKL.InputT AOI   -- haskeline input monad

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
getAOI_DICT = maybe "aoi" id <$> tryJust (Env.getEnv "AOI_DICT")

-- runAOI will manage initial configurations, then enter a
-- permanent 'recovery loop'
runAOI :: IO ()
runAOI = 
    newAOIContext >>= \ cx ->
    getHistoryFile >>= \ histFile ->
    let hklSettings = HKL.Settings
            { HKL.complete = aoiCompletion
            , HKL.historyFile = histFile
            , HKL.autoAddHistory = True
            }
    in
    let p = aoiInit >> queryLoop >> aoiFini in
    let app = HKL.runInputT hklSettings p in
    evalStateT app cx

aoiInit :: HKL ()
aoiInit =
    lift (gets aoi_dictSrc) >>= \ dsrc ->
    loadAODict dsrc (liftIO . putErrLn) >>= \ d ->
    lift (modify $ \ cx -> cx { aoi_dict = d }) >>
    let nDictSize = M.size $ readAODict d in
    let msg = "\nWelcome to aoi, an AO REPL!\n    " ++ 
              show nDictSize ++ " words loaded\n" ++
              "  ctrl+d to exit; also try @reload or @undo"
    in 
    HKL.outputStrLn msg

aoiFini :: HKL ()
aoiFini = return ()

queryLoop :: HKL ()
queryLoop = loop True where
    loop False = return ()
    loop True = wi (report >> query) >>= loop
    wi = HKL.handleInterrupt onInterrupt . HKL.withInterrupt
    onInterrupt = 
        HKL.outputStrLn " (ctrl+c interrupt)" >>
        return True
    report =
        lift (gets aoi_rtval) >>= \ v ->
        HKL.outputStrLn ('\n' : showEnv v [])
    query = 
        HKL.getInputLine "                        " >>= \ ln ->
        case fmap trimSP ln of
            Nothing -> return False -- done with loop
            Just [] -> query -- skip blank lines
            Just cmd -> aoiCommand cmd >> return True

aoiCommand :: String -> HKL ()
aoiCommand "@undo" =
    lift get >>= \ cx ->
    case S.viewl (aoi_hist cx) of
        S.EmptyL -> HKL.outputStrLn "(cannot undo; at limit of history)"
        (v S.:< h) ->
            let cx' = cx { aoi_rtval = v, aoi_hist = h } in
            lift (put cx')
aoiCommand "@reload" = aoiInit
aoiCommand aoStr =
    case P.parse parseAO "" (' ':aoStr) of
        Left err -> HKL.outputStrLn (show err)
        Right aoCode ->
            lift (gets aoi_dict) >>= \ dict ->
            case compileAOtoABC dict aoCode of
                Left mws -> 
                    let txtWords = (L.unwords . fmap T.unpack) mws in
                    let emsg = "undefined words: " ++ txtWords in
                    HKL.outputStrLn emsg
                Right abc -> aoiRunABC abc


aoiRunABC :: [Op] -> HKL ()
aoiRunABC abc =
    lift (gets aoi_rtval) >>= \ v ->
    lift (gets aoi_rtcx) >>= \ rt ->
    let runProg = runRT rt $ interpret (simplify abc) v in
    liftIO (tryIO runProg) >>= \ evf ->
    case evf of
        Left  e  -> HKL.outputStrLn (show e)
        Right vf -> lift (updateEnv vf)

updateEnv :: V AORT -> AOI ()
updateEnv v = modify $ \ cx ->
    let hist' = aoi_rtval cx S.<| aoi_hist cx in
    let trimmedHist = S.take (aoi_histLen cx) hist' in
    cx { aoi_rtval = v, aoi_hist = trimmedHist }

-- eliminate unnecessary spaces
trimSP :: String -> String
trimSP = t0 . dropWhile (== ' ') where
    t0 (' ':s) = t1 s
    t0 (c:s) = c:(t0 s)
    t0 [] = []
    t1 (' ':s) = t1 s
    t1 (c:s) = ' ':c:(t0 s)
    t1 [] = []

newAOIContext :: IO AOICX
newAOIContext =
    newDefaultRuntime >>= \ cx ->
    runRT cx newDefaultEnvironment >>= \ v0 -> 
    getAOI_DICT >>= \ dsrc ->
    return $ AOICX { aoi_dict = emptyAODict
                   , aoi_dictSrc = dsrc
                   , aoi_rtval = v0
                   , aoi_hist = S.empty
                   , aoi_histLen = 12
                   , aoi_rtcx = cx 
                   }



getHistoryFile :: IO (Maybe Sys.FilePath)
getHistoryFile = tryJust $
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

-- special commands
allCmds :: [String]
allCmds = ["@undo","@reload"]

dictCompletions :: M.Map Word val -> String -> [HKL.Completion]
dictCompletions _ [] = [] -- empty input
dictCompletions _ (_:[]) = [] -- wait for at least two characters
dictCompletions _ cmd@('@':_) = -- special commands
    let matched = L.filter (cmd `L.isPrefixOf`) allCmds in
    fmap HKL.simpleCompletion matched
dictCompletions dc str = 
    let ws = fmap T.unpack $ M.keys dc in
    let wsP = L.filter (str `L.isPrefixOf`) ws in
    fmap HKL.simpleCompletion wsP
