{-# LANGUAGE PatternGuards, ViewPatterns #-}

-- | The `ao` command line executable, with many utilities for
-- non-interactive programming. 
module Main 
    ( main
    ) where

import Control.Applicative
import Control.Monad

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Sequence as S

import qualified Data.IORef as IORef

import qualified System.IO as Sys
-- import System.IO.Unsafe (unsafeInterleaveIO)
import qualified System.Exit as Sys
import qualified System.Environment as Env
import qualified Text.Parsec as P

import ABC.Simplify (simplify)
import ABC.Operators
import ABC.Imperative.Value
import ABC.Imperative.Interpreter
import AO.Dict
import AO.Code
import AO.AOFile
import AO.Parser
import AO.Compile
import JIT
import AORT
import Util

helpMsg :: String
helpMsg =
    "The `ao` executable provides many utilities for non-interactive\n\
    \operations involving AO or ABC code. Usage:\n\
    \\n\
    \    ao help               print this message \n\
    \    \n\
    \    ao test               run all `test.` words in test environment \n\
    \    \n\
    \    ao abc command        dump simplified ABC for AO command \n\
    \    ao abc.raw command    dump raw ABC for AO command \n\
    \    ao exec command       execute AO command \n\
    \    ao exec.abc command   execute ABC code \n\
    \    \n\
    \    ao abc.s              dump simplified ABC for AO on input stream \n\
    \    ao abc.raw.s          dump raw ABC for AO on input stream \n\
    \    ao exec.s             execute AO command from input stream \n\
    \    ao exec.abc.s         execute ABC from input stream \n\
    \    \n\
    \    ao jit command        print haskell code for imperative JIT \n\
    \    \n\
    \    ao list pattern       list words matching pattern (e.g. test.*) \n\
    \    ao defs pattern       find definitions of words matching pattern \n\
    \    ao uses pattern       find uses of words matching pattern \n\
    \    ao def  word          print full accepted definition of word \n\
    \\n\
    \All 'exec' operations use the same powers and environment as `aoi`. \n\
    \The 'exec.abc' utilities will escape any {tokens} not valid in AO. \n\
    \The '.s' input stream is stdin, processed one paragraph at a time. \n\
    \\n\
    \Environment Variables: \n\
    \    AO_PATH: where to search for '.ao' files \n\
    \    AO_DICT: root dictionary text; default \"ao\" \n\
    \    \n\
    \    AO_TEMP: directory for temporaries; default \"aotmp\" \n\
    \"
-- todo: typechecking! 
-- when persistence is working, perhaps add an AO_HOME or similar.

main :: IO ()
main = Env.getArgs >>= runMode

-- very simple command line processing
runMode :: [String] -> IO ()
runMode ["help"]         = Sys.putStrLn helpMsg
runMode ["abc",cmd]      = mkCmdS cmd >>= dumpABC simplify
runMode ["abc.raw",cmd]  = mkCmdS cmd >>= dumpABC id
runMode ["exec",cmd]     = mkCmdS cmd >>= execAO
runMode ["exec.abc",cmd] = mkCmdS cmd >>= execABC
runMode ["abc.s"]        = stdCmdS >>= dumpABC simplify
runMode ["abc.raw.s"]    = stdCmdS >>= dumpABC id
runMode ["exec.s"]       = stdCmdS >>= execAO
runMode ["exec.abc.s"]   = stdCmdS >>= execABC
runMode ["jit",cmd]      = printImperativeJIT cmd
runMode ["list",ptrn]    = listWords ptrn
runMode ["uses",ptrn]    = listUses ptrn
runMode ["defs",ptrn]    = listDefs ptrn
runMode ["def",word]     = printDef word
runMode ["test"]         = runAOTests
runMode _ = putErrLn eMsg >> Sys.exitFailure where
    eMsg = "arguments not recognized; try `ao help`"

-- extract paragraphs from command string
mkCmdS :: String -> IO [String]
mkCmdS s = Sys.hClose Sys.stdin >> return (paragraphs s)

-- lazily obtain paragraphs from stdin
-- (I kind of hate lazy IO, but it's convenient here)
stdCmdS :: IO [String]
stdCmdS = paragraphs <$> Sys.hGetContents Sys.stdin

getAO_DICT :: IO String
getAO_DICT = maybe "ao" id <$> tryJust (Env.getEnv "AO_DICT")

-- obtain a list of paragraphs. Each paragraph is recognized
-- by two or more sequential '\n' characters.
paragraphs :: String -> [String]
paragraphs = pp0 where
    pp0 ('\n':ss) = pp0 ss
    pp0 (c:ss) = pp1 [c] ss
    pp0 [] = []
    pp1 ('\n':p) ('\n':ss) = L.reverse p : pp0 ss
    pp1 p (c:ss) = pp1 (c:p) ss
    pp1 p [] = L.reverse p : []

-- getDict will always succeed, but might return an empty
-- dictionary... and might complain a lot on stderr
getDict :: IO (AODict ())
getDict = 
    getAO_DICT >>= \ root ->
    loadAODict root putErrLn >>= \ dict ->
    return (fmap (const ()) dict)

putErrLn :: String -> IO ()
putErrLn = Sys.hPutStrLn Sys.stderr

-- dump ABC code, paragraph at a time, to standard output
type SimplifyFn = [Op] -> [Op]

dumpABC :: SimplifyFn -> [String] -> IO ()
dumpABC f ss = 
    getDict >>= \ d ->
    let nss = L.zip [1..] ss in
    mapM_ (uncurry (dumpABC' d f)) nss

dumpABC' :: AODict md -> SimplifyFn -> Int -> String -> IO ()
dumpABC' dict fSimp nPara aoStr = 
    when (nPara > 1) (Sys.putChar '\n') >>
    compilePara dict nPara aoStr >>= \ ops ->
    Sys.putStr (show (fSimp ops)) >>
    Sys.putChar '\n' >> Sys.hFlush Sys.stdout 

compilePara :: AODict md -> Int -> String -> IO [Op]
compilePara dict nPara aoStr =
    case compileAOString dict aoStr of
        Left err -> putErrLn ("paragraph " ++ show nPara) >>
                    putErrLn err >> Sys.exitFailure
        Right ops -> return ops

compileAOString :: AODict md -> String -> Either String [Op]
compileAOString dict aoString = 
    case P.parse parseAO "" aoString of
        Left err -> Left $ show err
        Right ao -> case compileAOtoABC dict ao of
            Left mw -> Left $ undefinedWordsMsg mw
            Right abc -> Right abc

undefinedWordsMsg :: [Word] -> String
undefinedWordsMsg mw = "undefined words: " ++ mwStr where
    mwStr = L.unwords $ fmap T.unpack mw

execAO :: [String] -> IO ()
execAO ss = 
    getDict >>= \ d -> 
    let compile (n,s) = simplify <$> compilePara d n s in
    execOps $ fmap compile $ L.zip [1..] ss

-- note that execABC will scrub the ABC input to ensure 
-- security and expression equivalence to AO inputs.
execABC :: [String] -> IO ()
execABC = execOps . fmap (return . scrubABC . simplify . read)  

type CX = AORT_CX
type RtVal = V AORT

execOps :: [IO [Op]] -> IO ()
execOps ppOps =
    newDefaultRuntime >>= \ cx ->
    runRT cx newDefaultEnvironment >>= \ v0 -> 
    void (execOps' cx v0 ppOps)

-- the toplevel will simply interpret operations
-- (leave JIT for inner loops!)
execOps' :: CX -> RtVal -> [IO [Op]] -> IO RtVal
execOps' _ v [] = return v
execOps' cx v (readPara:more) =
    readPara >>= \ ops -> 
    let prog = interpret ops in
    runRT cx (prog v) >>= \ v' ->
    execOps' cx v' more

-- pattern with simple wildcards.
-- may escape '*' using '\*'
type Pattern = String
matchStr :: Pattern -> String -> Bool
matchStr ('*':[]) _ = True
matchStr pp@('*':pp') ss@(_:ss') = matchStr pp' ss || matchStr pp ss'
matchStr ('\\':'*':pp) (c:ss) = (c == '*') && matchStr pp ss
matchStr (c:pp) (c':ss) = (c == c') && (matchStr pp ss)
matchStr pp ss = null pp && null ss

-- List words starting with a given regular expression.
listWords :: String -> IO ()
listWords pattern = 
    (fmap T.unpack . M.keys . readAODict) <$> getDict >>= \ allWords ->
    let matchingWords = L.filter (matchStr pattern) allWords in
    mapM_ Sys.putStrLn matchingWords

getDictFiles :: IO [AOFile]
getDictFiles = getAO_DICT >>= \ root -> loadAOFiles root putErrLn

-- find all uses of a given word (modulo entries that fail to compile)
listUses :: Pattern -> IO ()
listUses ptrn = getDictFiles >>= mapM_ (fileUses ptrn)

fileUses :: Pattern -> AOFile -> IO ()
fileUses p file = 
    let defs = L.filter (codeUsesWord p . fst . snd) (aof_defs file) in
    if null defs then return () else
    Sys.putStrLn (show (aof_path file)) >>
    mapM_ (Sys.putStrLn . ("  " ++) . defSummary) defs

codeUsesWord :: Pattern -> AO_Code -> Bool
codeUsesWord = L.any . actionUsesWord

actionUsesWord :: Pattern -> AO_Action -> Bool
actionUsesWord p (AO_Word w) = (matchStr p (T.unpack w))
actionUsesWord p (AO_Block code) = codeUsesWord p code
actionUsesWord _ _ = False

defSummary :: AODef Int -> String
defSummary (defWord,(code,line)) = showsSummary [] where
    showsSummary = 
        shows line . showChar ' ' . showChar '@' .
        showsCode (AO_Word defWord : code)
    showsCode = shows . fmap cutBigText
    cutBigText (AO_Text txt) | isBigText txt = AO_Word (T.pack "(TEXT)")
    cutBigText (AO_Block ops) = AO_Block (fmap cutBigText ops)
    cutBigText op = op
    isBigText txt = (L.length txt > 14) || (L.any isMC txt)
    isMC c = ('"' == c) || ('\n' == c)


listDefs :: Pattern -> IO ()
listDefs ptrn = getDictFiles >>= mapM_ (fileDefines ptrn)

fileDefines :: Pattern -> AOFile -> IO ()
fileDefines p file = 
    let defs = L.filter (matchStr p . T.unpack . fst) (aof_defs file) in
    if null defs then return () else
    Sys.putStrLn (show (aof_path file)) >>
    mapM_ (Sys.putStrLn . ("  " ++) . defSummary) defs

printDef :: String -> IO ()
printDef word =
    getDict >>= \ d ->
    case M.lookup (T.pack word) (readAODict d) of
        Nothing -> putErrLn "undefined" >> Sys.exitFailure
        Just (def,_) -> Sys.putStrLn (show def) 

runAOTests :: IO ()
runAOTests = getDict >>= \ d -> mapM_ (runTest d) (testWords d) 

-- obtain words in dictionary starting with "test."
testWords :: AODict md -> [Word]
testWords = filter hasTestPrefix . M.keys . readAODict where
    hasTestPrefix = (T.pack "test." `T.isPrefixOf`)

-- assumes word is in dictionary
runTest :: AODict md -> Word -> IO ()
runTest d w = 
    let (Right ops) = compileAOtoABC d [AO_Word w] in 
    newDefaultRuntime >>= \ rt ->
    IORef.newIORef [] >>= \ rfW ->
    let fwarn s = liftIO $ IORef.modifyIORef rfW (s:) in
    runRT rt (newTestPB d fwarn) >>= \ testPB ->
    let testEnv = aoStdEnv testPB in
    let prog = interpret (simplify ops) in
    let runProg = runRT rt (prog testEnv) in
    try runProg >>= \ evf ->
    IORef.readIORef rfW >>= \ warnings ->
    reportTest w (L.reverse warnings) evf

type Warning = String

reportTest :: (Show err) => Word -> [Warning] -> Either err (V AORT) -> IO ()
reportTest w [] (Right _) = Sys.putStrLn ("(pass) " ++ T.unpack w)
reportTest w ws (Right _) = 
    Sys.putStrLn ("(Warn) " ++ T.unpack w) >>
    mapM_ reportWarning ws
reportTest w ws (Left err) = 
    Sys.putStrLn ("(FAIL) " ++ T.unpack w) >>
    mapM_ reportWarning ws >>
    Sys.putStrLn (indent "  " (show err))

reportWarning :: Warning -> IO ()
reportWarning = Sys.putStrLn . indent "  "


-- test powerblock is linear, and is not serialized at the moment
newTestPB :: AODict md -> (Warning -> AORT ()) -> AORT (Block AORT)
newTestPB d fwarn = return b where
    b = Block { b_aff = True, b_rel = True, b_code = code, b_prog = prog }
    code = S.singleton $ Tok "test powers" 
    prog (P (valToText -> Just cmd) arg) = 
        runCmd cmd arg >>= \ result ->
        newTestPB d fwarn >>= \ tpb ->
        return (P (B tpb) result)
    prog v = fail $ "not structured as a command: " ++ show v
    runCmd "warn" (valToText -> Just msg) = fwarn msg >> return U
    runCmd "error" (valToText -> Just msg) = fwarn msg >> fail "error command"
    runCmd s v = fail $ "unrecognized command: " ++ s ++ " with arg " ++ show v

-- for now, I just emit code. I might later need to emit a context
-- that is held by the runtime and recovered dynamically.
printImperativeJIT :: String -> IO ()
printImperativeJIT aoStr =
    getDict >>= \ d -> 
    case compileAOString d aoStr >>= abc2hs . simplify of
        Left err -> putErrLn err >> Sys.exitFailure
        Right hsCode -> Sys.putStrLn hsCode


{-

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

-}


