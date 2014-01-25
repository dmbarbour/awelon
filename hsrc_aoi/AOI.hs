{-# LANGUAGE PatternGuards, FlexibleContexts, FlexibleInstances #-}

-- | AOI describes a simplistic, imperative REPL for language AO.
--
-- AOI will start by importing the `aoi` dictionary, though another
-- dictionary may be specified on the command line via a `-dict=foo`
-- option, in which case the `foo` dictionary file (foo.ao) will be
-- selected. All dictionaries are selected based on the AO_PATH
-- environment variable, including the initial dictionary.
--
-- Unlike conventional REPLs, AOI does not permit defining new words
-- via the REPL itself. Developers are instead encouraged to modify
-- their dictionary file, save it, then reload the REPL via Ctrl+C.
-- This approach creates a more persistent environment, and avoids
-- need for reactivity in this simple implementation.
--
-- AOI starts in the standard environment:
--
--    (stack * (hands * (power * ((stackName * namedStacks) * 1))))
--
-- The powerblock, in this case, ensures a trivial, linear effects
-- model. Developers can't do much in AOI other than read and write
-- files, and grab some random numbers. 
--
-- AOI maintains a call stack for debugging purposes, and additionally
-- can report some history from the call stack. 
--
-- Note: AOI does not represent the intended vision for AO interactive
-- programming. AOI is simply a stopgap for pre-bootstrap testing.
module AOI
    ( main
    -- stuff that should probably be in separate modules
    , newDefaultContext
    , makePowerBlock, defaultPowers
    , randomBytes, newSecret
    ) where

import Control.Monad
import Control.Applicative
import Control.Arrow (first)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.IORef

import qualified System.IO as Sys
import qualified System.IO.Error as Err
import qualified System.Environment as Env
import qualified System.Random as R
import qualified Crypto.Random.AESCtr as CR
import qualified System.Console.Haskeline as HKL
--import qualified Data.ByteString.Base64.URL as B64
import Data.Ratio
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.List as L
import qualified Data.Map as M
import qualified Filesystem.Path.CurrentOS as FS
import qualified Filesystem as FS
--import qualified Text.Parsec as P
import Data.Text (Text)
import Data.ByteString (ByteString)

import AO.AO
import AO.ABC
import HLS
import AOICX

putErrLn :: String -> IO ()
putErrLn = Sys.hPutStrLn Sys.stderr

-- create a powerblock given a map of powers
--
-- the powerblock receives (label * message) 
--            then outputs (power * result).
--
-- The set of powers is closed once constructed.
type Power = V AOI -> AOI (V AOI)
makePowerBlock :: M.Map Text Power -> V AOI
makePowerBlock powerMap = powerBlock where
    powerBlock = B kfLinear abc where
    kfLinear = KF False False
    abc = ABC { abc_code = falseCode, abc_comp = power }
    falseCode = (Prim . Invoke . T.pack) "*power*" -- never invoked
    run frame action = 
        pushFrame frame >> -- simplify error discovery
        action >>= \ result ->
        popFrame' frame >>
        return (P kfLinear powerBlock result)
    power v@(P _ label message) =
        case valToText label of
            Nothing -> powerFail v
            Just txt -> case M.lookup txt powerMap of
                Nothing -> powerFail v
                Just op -> run label (op message)
    power v = powerFail v
    powerFail v = fail $ "{*power*} message not understood: " ++ show v

-- very few powers for now...
defaultPowers :: M.Map Text Power
defaultPowers = M.fromList $ map (first T.pack) $
    [("destroy", const (return U))
    ,("switchAOI", switchAOI)
    -- debugging support
    ,("debugOut", (\ v -> debugOut v >> return v))
    -- secure random bytes
    ,("randomBytes", aoiRandomBytes)
    -- read and write text files
    ,("getOSEnv", aoiGetOSEnv )
    ,("readFile", aoiReadFile)
    ,("writeFile", aoiWriteFile)
    ]

defaultPowerBlock :: V AOI
defaultPowerBlock = makePowerBlock defaultPowers

switchAOI :: V -> AOI V
switchAOI (P _ (B kf abc) eIC) | may_copy kf && may_drop kf = 
    getCX >>= \ cx ->
    let ifnOld = aoi_ifn cx in
    let cx' = cx { aoi_ifn = IFN abc eIC } in
    putCX cx' >>
    return (prod (B kf0 (ifn_op ifnOld)) (ifn_eIC ifnOld))
switchAOI v = fail ("switchAOI @ " ++ show v)

debugOut :: (MonadIO c) => V c -> c ()
debugOut = liftIO . putErrLn . show


aoiRandomBytes :: (MonadIO c) => V c -> c (V c)
aoiRandomBytes (N r) | naturalNum r =
    liftIO (randomBytes (numerator r)) >>= 
    return . bytesToVal
aoiRandomBytes v = fail ("randomBytes @ " ++ show v)

naturalNum :: Rational -> Bool
naturalNum r = (1 == denominator r) && (r >= 0)

randomBytes :: Integer -> IO ByteString
randomBytes n = liftM toBytes CR.makeSystem where
    toBytes = B.pack . L.take (fromInteger n) . R.randoms

bytesToVal :: ByteString -> V c
bytesToVal bs =
    case B.uncons bs of
        Nothing -> N 8
        Just (w, bs') -> P kf0 (N $ fromIntegral w) (bytesToVal bs')

aoiGetOSEnv :: (MonadIO c) => V c -> c (V c)
aoiGetOSEnv v = 
    case valToText v of
        Nothing -> fail ("getOSEnv @ " ++ show v)
        Just txt -> 
            liftIO (getOSEnv txt) >>= \ result ->
            return (textToVal result)

getOSEnv :: Text -> IO Text
getOSEnv = liftM (maybe T.empty T.pack) . tryIO . Env.getEnv . T.unpack

aoiReadFile :: (MonadIO c) => V c -> c (V c)
aoiReadFile fileName =
    case valToText fileName of
        Nothing -> fail $ ("readFile @ " ++ show fileName)
        Just fntxt -> 
            let op = FS.readTextFile $ FS.fromText fntxt in
            maybe (L U) (R . valToText) <$> liftIO (tryIO op)

aoiWriteFile :: (MonadIO c) => V c -> c (V c)
aoiWriteFile v@(P _ fn ftext) =
    case (,) <$> valToText fn <*> valToText ftext of
        Nothing -> fail $ ("writeFile @ " ++ show v)
        Just (file, fdata) ->
            let fp = FS.fromText file in
            let op = FS.createTree (FS.directory fp) >>
                     FS.writeTextFile fp fdata 
            in
            maybe (L U) (const (R U)) <$> liftIO (tryIO op)

selectSource :: IO Import
selectSource = sfa <$> Env.getArgs where
    sfa [] = T.pack "aoi"
    sfa (arg:args) = 
        case L.stripPrefix "-dict=" arg of
            Nothing -> sfa args
            Just foo -> (T.pack foo)

-- initial AOI_CONTEXT
newDefaultContext :: IO AOI_CONTEXT
newDefaultContext =
    selectSource >>= \ source ->
    let s = U in -- initial stack
    let h = U in -- initial hand
    let p = defaultPowerBlock in -- initial powerblock
    let rns = (prod (N 3) U) in -- initial named stacks
    let ex = U in -- initial extension area
    let eU0 = (prod s (prod h (prod p (prod rns ex)))) in
    let cx0 = AOI_CONTEXT 
            { aoi_dict = M.empty -- loaded on init or ctrl+c
            , aoi_source = source
            , aoi_frames = hls_init []
            , aoi_step = hls_init (0, eU0) 
            , aoi_ifn = defaultIFN
            }
    in
    return cx0

tryIO :: IO a -> Maybe (IO a)
tryIO = liftM (either (const Nothing) (Just)) . Err.tryIOError

getHistoryFile :: IO (Maybe Sys.FilePath)
getHistoryFile = tryIO $
    FS.getAppDataDirectory (T.pack "aoi") >>= \ appDir ->
    FS.createTree appDir >>
    let fp = appDir FS.</> FS.fromText (T.pack "hist.haskeline") in
    FS.appendFile fp B.empty >>
    return (FS.encodeString fp)

main :: IO ()
main = 
    newDefaultContext >>= \ cx ->
    getHistoryFile >>= \ hfile ->
    let hklSettings = HKL.Settings
            { HKL.complete = aoiCompletion 
            , HKL.historyFile = hfile
            , HKL.autoAddHistory = True
            }
    in
    let hkl = initAOI >> aoiHaskelineLoop >> finiAOI in
    let hklwi = HKL.withInterrupt hkl in
    recoveryLoop (HKL.runInputT hklSettings hklwi) cx

-- recovery loop will handle state errors (apart from parse errors)
-- by reversing the ABC streaming state to the prior step. If this
-- happens, a stack trace is also printed.
recoveryLoop :: AOI () -> AOI_CONTEXT -> IO ()
recoveryLoop aoi cx0 = 
    runAOI aoi cx0 >>= \ (cx', r) ->
    case r of
        Right () -> return () -- clean exit
        Left eTxt ->
            putErrLn (T.unpack eTxt) >>
            readIORef (aoi_frames cx') >>= \ stack ->
            writeIORef (aoi_frames cx') 
            reportContext stack >> -- STACK TRACE, VALUE, ETC.
            let cxcln = cx' { aoi_frames = hls_init [] } in
            recoveryLoop aoi cxcln

-- reportContext currently just prints a stack trace
-- todo: print recent history of stacks, too. 
reportContext :: HLS [Text] -> IO ()
reportContext hls = stackTrace >> histTrace where
    stack = hls_get hls
    stackTrace = unless (L.null stack) $
        putErrLn "== CALL STACK ==" >>
        mapM_ (putErrLn . T.unpack) stack 
    hist = L.map L.nub $ L.reverse (hls_getHist hls)
    diffs = L.filter hasDiff $ L.map snd $ 
            L.scanl stackDiff ([],([],[])) hist 
    hasDiff ([],[]) = False
    hasDiff _ = True
    histTrace = unless (L.null diffs) $ -- simplistic history trace
        putStrLn "== INCOMPLETE STACK HISTORY ==" >>
        mapM_ showStackDiff diffs
    stackDiff (sPrev,_) sNew = 
        let added = L.filter (`L.notElem` sPrev) sNew in
        let removed = L.filter (`L.notElem` sNew) sPrev in
        (sNew, (added, removed))
    showStackDiff (lAdd, lRem) = 
        mapM_ putStrLn (L.map (("- " ++) . T.unpack) lRem) >>
        mapM_ putStrLn (L.map (("+ " ++) . T.unpack) (L.reverse lAdd))

dictCompletions :: DictC -> String -> [HKL.Completion]
dictCompletions _ [] = []
dictCompletions _ (_:[]) = []
dictCompletions dc str = 
    let ws = L.map T.unpack $ M.keys dc in
    let wsP = L.filter (str `L.isPrefixOf`) ws in
    let basicCompletions = L.map HKL.simpleCompletion wsP in
    basicCompletions
  
-- for now, using a trivial prefix search on dictionary. I would
-- prefer a 'fuzzy find' but Haskeline doesn't support it
aoiCompletion :: HKL.CompletionFunc AOI
aoiCompletion = quotedFiles prefixedWords where
    quotedFiles = HKL.completeQuotedWord Nothing "\"" HKL.listFiles
    prefixedWords = HKL.completeWord Nothing " \n[](|)" findWords
    findWords s = 
        (aoi_dict <$> getCX) >>= \ dc ->
        return (dictCompletions dc s)

initAOI :: HKL.InputT AOI ()
initAOI = greet >> lift aoiReload where
    greet =  
        HKL.haveTerminalUI >>= \ bTerm ->
        when bTerm $
            HKL.outputStrLn "Welcome to aoi!" >>
            HKL.outputStrLn "  ctrl+d to exit, ctrl+c to abort & reload"

finiAOI :: HKL.InputT AOI ()
finiAOI = 
    HKL.haveTerminalUI >>= \ bTerm ->
    when bTerm $ HKL.outputStrLn "goodbye!"

aoiHaskelineLoop :: HKL.InputT AOI ()
aoiHaskelineLoop =
    aoiHklPrint >>
    lift resetFrameHist >>
    (fst <$> lift aoiGetStep) >>= \ n ->
    let prompt = show (n + 1) ++ ": " in
    foi (HKL.getInputLine prompt) >>= \ sInput ->
    case sInput of
        Nothing -> return ()
        Just str -> -- TODO: catch HKL ctrl+c interrupt and fail... 
            foi (lift (aoiStep (T.pack (' ':str)))) >>
            aoiHaskelineLoop

foi :: HKL.InputT AOI a -> HKL.InputT AOI a
foi = HKL.handleInterrupt (fail "ctrl+c")

-- a colorful printing function
aoiHklPrint :: HKL.InputT AOI ()
aoiHklPrint = printIfTerminalUI where
    printIfTerminalUI = 
        HKL.haveTerminalUI >>= \ bTerm ->
        when bTerm $
            HKL.outputStrLn "" >>
            lift (snd <$> aoiGetStep) >>= printENV >>
            HKL.outputStrLn ""
    printENV v = maybe (atypical v) typical (fromABCV v)
    stackCount :: V -> Integer
    stackCount (P _ ss) = 1 + stackCount ss
    stackCount U = 0
    stackCount _ = 1
    atypical env =
        HKL.outputStrLn "--(atypical environment)--" >>
        HKL.outputStrLn (show env)
    typical (s, (h, (p, ((sn, rns), ex)))) =
        printPower (b_code p) >>
        printExtendedEnv ex >>
        printNamedStacks rns >>
        printHand h >>
        printStack sn s
    printStack sn U =
        HKL.outputStrLn ("---" ++ T.unpack sn ++ "---")
    printStack sn (P e ss) =
        printStack sn ss >>
        HKL.outputStrLn ("| " ++ show e)
    printStack sn v =
        printStack (sn `T.append` T.pack "(atypical)") U >>
        HKL.outputStrLn ("$ " ++ show v)
    printHand h =
        let n = stackCount h in
        when (n > 0) $
            HKL.outputStrLn ("hand: " ++ show n)
    printPower (ABC _) = return ()
    printNamedStacks U = return ()
    printNamedStacks (P s ss) = 
        printNamedStack s >> 
        printNamedStacks ss
    printNamedStacks v =
        HKL.outputStrLn ("(atypical rns term): " ++ show v)
    printNamedStack ns =
        case fromABCV ns of
            Just (name,stack) ->
                let ct = stackCount stack in
                let label = T.unpack name ++ ": " in
                HKL.outputStrLn (label ++ show ct)
            Nothing -> 
                HKL.outputStrLn ("(?): " ++ show ns)
    printExtendedEnv U = return ()
    printExtendedEnv ex =
        HKL.outputStrLn "--(extended environment)--" >>
        HKL.outputStrLn (show ex)

-- the primary interpreter step for AOI
aoiStep :: Text -> AOI ()
aoiStep txt = 
    aoiGetIfn >>= \ ifn ->
    let vInputIC = toABCV (txt, ifn_eIC ifn) in
    runABC aoiInvoker vInputIC ((b_code . ifn_block) ifn) >>= \ vParsed ->
    case fromABCV vParsed of
        Just (Right (B b, eIC')) -> 
            let ifn' = ifn { ifn_eIC = eIC' } in
            aoiPutIfn ifn' >>
            aoiGetStepV >>= \ v0 ->
            runABC aoiInvoker v0 (b_code b) >>= \ vf ->
            aoiPutStepV vf
        Just (Left errText) -> liftIO $ 
            putErrLn ("READ ERROR: " ++ T.unpack errText) >>
            putErrLn ("dropping this input...")
        _ -> liftIO $
            putErrLn ("READER TYPE ERROR; received: " ++ show vParsed) >>
            putErrLn ("dropping this input...")


