{-# LANGUAGE FlexibleContexts, ViewPatterns #-}

-- | The `ao` command line executable
--
-- `ao` follows the modern convention of taking a second word
-- as its command, followed by any options for that word.
--
--     ao test -- run all 'test.' words in named dict
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
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Sequence as S
import qualified Data.Foldable as S
import qualified Text.Parsec as P
-- import qualified Text.Parsec.Pos as P
-- import qualified Text.Parsec.Token as P
import qualified System.IO as Sys
import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.IO.Error as Err
import AO.AO
import AO.ParseAO
import AO.ABC
import AO.TypeABC
import AO2HS (runAO2HS, runDict2HS, runProg2HS)

-- not many modes to parse at the moment.
data Mode 
    = Test -- run all `test.` words
    | Type [W] -- report type for a set of words
    | DumpABC Bool AODef -- dump bytecode for a word or command
    | AO2HS AODef -- generate Haskell code for a given command
    | Prog2HS AODef -- generate Haskell program for given command
    | Dict2HS     -- generate Haskell code for each word in dictionary
    | Help
data TestState = TestState 
    { ts_emsg :: S.Seq Text -- explicit warnings or errors
    }
data TestStatus = Pass | Warn | Fail deriving(Eq)

type TestResult = (TestState, Maybe (V IO))
ts0 :: TestState
ts0 = TestState S.empty

cmdLineHelp :: String
cmdLineHelp = 
    "ao test                 run all `test.` words in dict\n\
    \ao type                 typecheck all words in dict\n\
    \ao type x y z           print type for a given list of words\n\
    \ao abc command*         dump weakly simplified ABC for given command\n\
    \  Each command must parse independently as AO code. Output is\n\
    \  the trivial, concatenative composition of these subprograms.\n\
    \ao help (or -?)         print these options\n\
    \n\
    \Emitting Haskell Code:\n\
    \  ao dict2hs              emits AODict module containing full dictionary\n\
    \  ao ao2hs command*       emits code for command *assuming* AODict\n\
    \  ao prog2hs command*     emits AOProg module for given command\n\
    \\n\
    \Environment Variables:\n\
    \  AO_PATH: list of directories, searched for .ao files\n\
    \  AO_DICT: root import, e.g. `std` or `aoi`. Default `lang`.\n\
    \\n\
    \NOTE: AODict and AOProg depend on user-provided AOPrelude.\n\
    \"

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
runMode Test = runTests 
runMode (Type ws) = runType ws
runMode (DumpABC bSimp aoDef) = runDumpABC bSimp aoDef
runMode (AO2HS action) = runAO2HS action
runMode (Dict2HS) = runDict2HS
runMode (Prog2HS action) = runProg2HS action

runHelp :: IO ()
runHelp = putErrLn cmdLineHelp >> return ()

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
parseTestMode = tok (== "test") >> return Test

parseTypeMode :: (P.Stream s m Tok) => P.ParsecT s u m Mode
parseTypeMode =
    tok (== "type") >>
    liftM (map T.pack) (P.many anyOneArg) >>= \ targetWords ->
    return (Type targetWords)

parseDumpABCMode :: (P.Stream s m Tok) => P.ParsecT s u m Mode
parseDumpABCMode = rawABC P.<|> simpABC where
    simpABC = tok (== "abc") >> mode True
    rawABC  = tok (== "abcRaw") >> mode False
    mode bSimp = DumpABC bSimp <$> parseCmd

parseCmd :: (P.Stream s m Tok) => P.ParsecT s u m AODef
parseCmd = concatCmds <$> P.manyTill parseOneCmd P.eof where
    concatCmds = foldr (S.><) S.empty
    parseOneCmd = 
        anyOneArg >>= \ str ->
        case P.parse parseAODef "" str of
            Left pe -> P.unexpected (show pe)
            Right def -> return def

parseDict2HSMode :: (P.Stream s m Tok) => P.ParsecT s u m Mode
parseDict2HSMode = tok (== "dict2hs") >> return Dict2HS

parseAO2HSMode :: (P.Stream s m Tok) => P.ParsecT s u m Mode
parseAO2HSMode = tok (== "ao2hs") >> (AO2HS <$> parseCmd)

parseProg2HSMode :: (P.Stream s m Tok) => P.ParsecT s u m Mode
parseProg2HSMode = tok (== "prog2hs") >> (Prog2HS <$> parseCmd)

parseHelpMode :: (P.Stream s m Tok) => P.ParsecT s u m Mode
parseHelpMode = (P.eof P.<|> helpTok) >> return Help where
    helpTok = tok anyHelp >> return ()
    anyHelp s = ("help" == s) || ("-help" == s) || ("-?" == s)

parseCmdLine :: P.Stream s m Tok => P.ParsecT s u m Mode
parseCmdLine = parseMode >>= \ m -> P.eof >> return m where
    parseMode = 
        parseTestMode    P.<|>
        parseTypeMode    P.<|>
        parseDumpABCMode P.<|>
        parseAO2HSMode   P.<|>
        parseDict2HSMode P.<|>
        parseProg2HSMode P.<|>
        parseHelpMode
