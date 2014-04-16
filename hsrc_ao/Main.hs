{-# LANGUAGE PatternGuards, ViewPatterns #-}

-- | The `ao` command line executable, with many utilities for
-- non-interactive programming. 
module Main 
    ( main
    ) where

import Control.Applicative
import qualified Data.List as L
import qualified System.IO as Sys
import qualified System.IO.Error as Err
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified System.Exit as Sys
import qualified System.Environment as Env
import AO.Imperative.AORT

helpMsg :: String
helpMsg =
    "The `ao` executable provides many utilities for non-interactive\n\
    \operations involving AO or ABC code. Usage:\n\
    \\n\
    \    ao help               print this message \n\
    \    \n\
    \    ao abc command        dump ABC for AO command \n\
    \    ao exec command       execute AO command \n\
    \    ao exec.abc command   execute ABC code \n\
    \    \n\
    \    ao abc.s              dump ABC for AO on input stream \n\
    \    ao exec.s             execute AO command from input stream \n\
    \    ao exec.abc.s         execute ABC from input stream \n\
    \    \n\
    \    ao test               run all `test.` words in test environment \n\
    \    ao type               attempt to detect type errors \n\
    \\n\
    \All 'exec' operations use the same powers and environment as `aoi`.\n\
    \The input stream is stdin, processed one paragraph at a time.\n\
    \\n\
    \Environment Variables: \n\
    \    AO_PATH: where to search for '.ao' files \n\
    \    AO_DICT: root dictionary text; default \"ao\" \n\
    \"
-- when persistence is working, perhaps add an AO_HOME or similar.

main :: IO ()
main = Env.getArgs >>= runMode

-- very simple command line processing
runMode :: [String] -> IO ()
runMode ["help"]         = Sys.putStrLn helpMsg
runMode ["abc",cmd]      = mkCmdS cmd >>= dumpABC 
runMode ["exec",cmd]     = mkCmdS cmd >>= execAO
runMode ["exec.abc",cmd] = mkCmdS cmd >>= execABC
runMode ["abc.s"]        = stdCmdS >>= dumpABC
runMode ["exec.s"]       = stdCmdS >>= execAO
runMode ["exec.abc.s"]   = stdCmdS >>= execABC
runMode ["test"]         = runAOTests
runMode ["type"]         = runAOType
runMode _ = putErrLn eMsg >> Sys.exitFailure where
    eMsg = "arguments not recognized; try `ao help`"

getAO_DICT :: IO String
getAO_DICT = Err.catchIOError (Env.getEnv "AO_DICT") (const (pure "ao"))

putErrLn :: String -> IO ()
putErrLn = Sys.hPutStrLn Sys.stderr

-- for now, modeling input stream as list of strings
mkCmdS :: String -> IO [String]
mkCmdS s = Sys.hClose Sys.stdin >> return [s]

-- obtain a paragraph at a time from 
stdCmdS :: IO [String]
stdCmdS = hGetParagraphs Sys.stdin

-- obtain paragraphs from input (lazy IO)
hGetParagraphs :: Sys.Handle -> IO [String]
hGetParagraphs h =
    hGetParagraph h >>= \ p ->
    if (null p) then return [] else
    unsafeInterleaveIO (hGetParagraphs h) >>= \ ps ->
    return (p:ps)

-- read the return one non-empty paragraph.
-- at end-of-file, will return empty string.
hGetParagraph :: Sys.Handle -> IO String
hGetParagraph h = L.reverse <$> getc [] where
    getc cs = 
        Err.tryIOError (Sys.hGetChar h) >>=
        either (onErr cs) (onChr cs)
    onChr cs@('\n':_) ('\n') = return cs -- non-empty
    onChr cs c = getc (c:cs)
    onErr cs e | Err.isEOFError e = return cs -- done
               | otherwise = ioError e

-- dump ABC code, paragraph at a time, to standard output
dumpABC :: [String] -> IO ()
dumpABC _s = fail "todo abc"

execAO :: [String] -> IO ()
execAO _s = fail "todo exec"

execABC :: [String] -> IO ()
execABC _s = fail "todo exec.abc"

runAOTests :: IO ()
runAOTests = fail "todo test"

runAOType :: IO ()
runAOType = fail "todo type"

{-

data TestState = TestState 
    { ts_emsg :: S.Seq Text -- explicit warnings or errors
    }
data TestStatus = Pass | Warn | Fail deriving(Eq)

type TestResult = (TestState, Maybe (V IO))
ts0 :: TestState
ts0 = TestState S.empty

--------------------------------
-- Running Tests
--------------------------------

runTests :: IO ()
runTests =
    loadDictionary >>= \ dict ->
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

runType :: [W] -> IO ()
runType [] = 
    loadDictionary >>= \ dictA0 ->
    let dc = compileDictionary dictA0 in
    mapM_ (uncurry runTypeW) (M.toList dc)
runType ws =
    loadDictionary >>= \ dictA0 ->
    let dc = compileDictionary dictA0 in
    mapM_ (findAndType dc) ws

findAndType :: DictC -> W -> IO ()
findAndType dc w = maybe notFound (runTypeW w) (M.lookup w dc) where
    notFound = Sys.putStrLn $ T.unpack w ++ " :: (WORD NOT FOUND!)"

runTypeW :: W -> S.Seq Op -> IO ()
runTypeW w code = Sys.putStrLn (T.unpack w ++ " :: " ++ msg) where
    msg = case typeOfABC code of
            Left etxt -> "(ERROR!)\n" ++ indent "  " (T.unpack etxt)
            Right (tyIn, tyOut) -> show tyIn ++ " → " ++ show tyOut

--------------------------------------
-- Dump bytecode for the compiler
-- (may fail if one or more words is undefined)
--------------------------------------

runDumpABC :: Bool -> AODef -> IO ()
runDumpABC bSimp = compile >=> simplify >=> printABC where
    exitFailure = Exit.exitWith $ Exit.ExitFailure 1
    compile aoSrc = 
        loadDictionary >>= \ dict ->
        let dc = compileDictionary dict in
        case compileActions dc aoSrc of
            Left err -> putErrLn (T.unpack err) >> exitFailure
            Right abc -> return abc
    simplify = return . if bSimp then simplifyABC else id
    printABC = Sys.putStrLn . T.unpack . showOps

compileActions :: DictC -> AODef -> Either Text (S.Seq Op)
compileActions dc actions = 
    let wNeed = aoWordsRequired actions in
    let wMissed = Set.filter (`M.notMember` dc) wNeed in
    if Set.null wMissed
        then Right $ aoToABC dc actions
        else Left $ 
            T.pack "undefined words: " `T.append` 
            T.unwords (Set.toList wMissed)

-}


