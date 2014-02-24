{-# LANGUAGE FlexibleContexts, ViewPatterns #-}

-- | The `ao` command line executable
--
-- `ao` follows the modern convention of taking a second word
-- as its command, followed by any options for that word.
--
--     ao test dict   -- run all 'test.' words in named dict
--
-- currently, there aren't many options! I might need to add some
-- verbosity options to testing, later.
--
-- I'm just using Parsec to handle the command line.
module Main 
    ( main, runMode
    , parseCmdLine, cmdLineHelp
    ) where

import Control.Applicative
--import Control.Concurrent
--import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Monad
import Data.IORef
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Sequence as S
import qualified Data.Foldable as S
import qualified Text.Parsec as P
-- import qualified Text.Parsec.Pos as P
-- import qualified Text.Parsec.Token as P
import qualified System.IO as Sys
import qualified System.Environment as Env
import qualified System.IO.Error as Err
import AO.AO
import AO.ABC

-- not many modes to parse at the moment.
data Mode 
    = Test DictName -- run all `test.` words
    | Type DictName [W] -- report type for a set of words
    | Help
type DictName = Import
data TestState = TestState 
    { ts_emsg :: S.Seq Text -- explicit warnings or errors
    }
data TestStatus = Pass | Warn | Fail deriving(Eq)

type TestResult = (TestState, Maybe (V IO))
ts0 :: TestState
ts0 = TestState S.empty

cmdLineHelp :: String
cmdLineHelp = 
    "ao test foo             typecheck words in foo, run `test.` words\n" ++
    "ao type foo word        print type for a given (list of) word(s)\n" ++
    "ao help (or -?)         these options\n"

main :: IO ()
main = getMode >>= runMode

-----------------------------
-- Initial Setup 
-----------------------------

getMode :: IO Mode
getMode =
    Env.getArgs >>= \ args ->
    case P.parse parseCmdLine "" args of
        Left parseErr ->
            putErrLn (show parseErr) >>
            return Help -- default to help mode
        Right mode -> return mode


putErrLn :: String -> IO ()
putErrLn = Sys.hPutStrLn Sys.stderr

runMode :: Mode -> IO ()
runMode Help = runHelp
runMode (Test dict) = runTests dict
runMode (Type d ws) = runType d ws

runHelp :: IO ()
runHelp = putErrLn cmdLineHelp >> return ()

--------------------------------
-- Running Tests
--------------------------------

runTests :: Import -> IO ()
runTests d0 =
    loadDictionary d0 >>= \ dict ->
    let dc = compileDictionary dict in
    let tests = M.filterWithKey isTestWord dc in
    mapM (onSnd runTest) (M.toList tests) >>= \ testResults ->
    mapM_ (uncurry reportTest) testResults >> 
    let nPass = L.length $ L.filter (testPass . snd) testResults in
    let nWarn = L.length $ L.filter (testWarn . snd) testResults in
    let nFail = L.length $ L.filter (testFail . snd) testResults in
    let summary = show nPass ++ " PASS; " 
               ++ show nWarn ++ " WARN; " 
               ++ show nFail ++ " FAIL"
    in
    Sys.putStrLn summary

isTestWord :: W -> a -> Bool
isTestWord w _ = T.isPrefixOf (T.pack "test.") w

onSnd :: (Applicative ap) => (b -> ap c) -> ((a,b) -> ap (a,c))
onSnd f (a,b) = ((,) a) <$> f b

---------------------------
-- Reporting Results
---------------------------

reportTest :: W -> TestResult -> IO ()
reportTest w tsr =
    case testStatus tsr of
        Pass -> Sys.putStrLn ("(pass) " ++ T.unpack w)
        Warn -> Sys.putStrLn ("(Warn) " ++ T.unpack w) >>
                Sys.putStrLn (indent "  " $ tsrFailureMsg tsr)
        Fail -> Sys.putStrLn ("(FAIL) " ++ T.unpack w) >>
                Sys.putStrLn (indent "  " $ tsrFailureMsg tsr)

tsrFailureMsg :: TestResult -> String
tsrFailureMsg = T.unpack . T.unlines . S.toList . ts_emsg . fst

testStatus :: TestResult -> TestStatus
testStatus (ts, Just _) = if (S.null (ts_emsg ts)) then Pass else Warn
testStatus (_, Nothing) = Fail

testPass, testWarn, testFail :: TestResult -> Bool
testPass = (Pass ==) . testStatus
testWarn = (Warn ==) . testStatus
testFail = (Fail ==) . testStatus

indent :: String -> String -> String
indent ws = L.unlines . map (ws ++) . L.lines 

---------------------
-- Running One Test
--
--  each test runs with a fresh standard environment & state.
--  the powerblock is confined to simple state manipulations.
---------------------

runTest :: S.Seq Op -> IO TestResult
runTest code = 
    newIORef ts0 >>= \ rf ->
    let env0 = stdEnv (testPower rf) in
    let action = runABC invNull code env0 in
    Err.tryIOError action >>= \ result ->
    case result of
        Left ioe ->
            addErrMsg rf (T.pack (show ioe)) >>
            readIORef rf >>= \ tsf ->
            return (tsf, Nothing)
        Right vf ->
            readIORef rf >>= \ tsf ->
            return (tsf, Just vf)

{-
-- fork test in a thread; lazily obtain result from MVar
--
-- TODO: consider introducing a thread worker pool so at 
-- most K tests run at any given time.
runTestAsync :: S.Seq Op -> IO TestResult
runTestAsync code =
    newEmptyMVar >>= \ mvResult ->
    forkIO (runTest code >>= putMVar mvResult) >>
    unsafeInterleaveIO (readMVar mvResult)
-}

-- standard environment with given powerblock.
stdEnv :: (Monad m) => V m -> V m
stdEnv pb = (P U (P U (P pb (P (P (N 3) U) U))))
           -- stack hand power     sn   ns ex

addErrMsg :: IORef TestState -> Text -> IO ()
addErrMsg rf msg =
    readIORef rf >>= \ ts ->
    let emsg' = (ts_emsg ts) S.|> msg in
    let ts' = ts { ts_emsg = emsg' } in
    writeIORef rf ts'

testPower :: IORef TestState -> V IO
testPower rf = B (KF False False) (ABC ops action) where
    ops = (S.singleton . Invoke . T.pack) "~test power~"
    vt = liftA T.unpack . valToText
    action (P (vt -> Just cmd) msg) = 
        runCmd cmd msg >>= \ result ->
        return (P (testPower rf) result)
    action v = fail ("unrecognized power: " ++ show v)
    runCmd "warn" (valToText -> Just msg) = addErrMsg rf msg >> return U
    runCmd "error" (vt -> Just msg) = fail msg
    runCmd s msg = fail $ "unrecognized command: " 
                          ++ s ++ " with " ++ show msg

--------------------------------------
-- Running Pass0 typecheck
--------------------------------------

runType :: DictName -> [W] -> IO ()
runType dictName dictWords =
    loadDictionary dictName >>= \ dictAO ->
    let dc = compileDictionary dictAO in
    mapM_ (printTypeOfWord dc) dictWords

printTypeOfWord :: DictC -> W -> IO ()
printTypeOfWord dc w = case M.lookup w dc of
    Nothing -> Sys.putStrLn (T.unpack w ++ " :: (ERROR: WORD NOT FOUND!)")
    Just _ops -> 
        let typeString = "? → ?; type currently disabled" in
        let msg = T.unpack w ++ " :: " ++ typeString in
        Sys.putStrLn msg

---------------------------------------
-- Command Line Parser (Tok is command line argument)
---------------------------------------
type Tok = String

tok :: (P.Stream s m Tok) => (Tok -> Bool) -> P.ParsecT s u m Tok
tok match = P.tokenPrim id nextPos matchTok where
    nextPos pos _ _ = P.incSourceColumn pos 1
    matchTok t = if (match t) then Just t else Nothing

anyOneArg :: (P.Stream s m Tok) => P.ParsecT s u m Tok
anyOneArg = tok (const True)

parseTestMode :: (P.Stream s m Tok) => P.ParsecT s u m Mode
parseTestMode =
    tok (== "test") >>
    anyOneArg >>= 
    return . Test . T.pack

parseTypeMode :: (P.Stream s m Tok) => P.ParsecT s u m Mode
parseTypeMode =
    tok (== "type") >>
    liftM (T.pack) anyOneArg >>= \ dictName ->
    P.many1 (anyOneArg) >>= \ targetWords ->
    return (Type dictName (map T.pack targetWords))

parseHelpMode :: (P.Stream s m Tok) => P.ParsecT s u m Mode
parseHelpMode = tok anyHelp >> P.eof >> return Help where
    anyHelp s = ("help" == s) || ("-help" == s) || ("-?" == s)

parseCmdLine :: P.Stream s m Tok => P.ParsecT s u m Mode
parseCmdLine = 
    parseTestMode P.<|> 
    parseTypeMode P.<|>
    parseHelpMode
