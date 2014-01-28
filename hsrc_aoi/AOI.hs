{-# LANGUAGE PatternGuards, ViewPatterns, FlexibleContexts, FlexibleInstances #-}

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
    , defaultPowers
    , randomBytes
    ) where

import Control.Monad
import Control.Applicative
import Control.Arrow (first)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.IORef

import qualified System.IO as Sys
import qualified System.Exit as Sys
import qualified System.IO.Error as Err
import qualified System.Environment as Env
import qualified System.Random as R
import qualified Crypto.Random.AESCtr as CR
import qualified System.Console.Haskeline as HKL
import Data.Ratio
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Map as M
--import qualified Data.Sequence as S
import qualified Filesystem.Path.CurrentOS as FS
import qualified Filesystem as FS
import Data.Text (Text)
import Data.ByteString (ByteString)

import AO.AO
import HLS
import AOICX

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

newDefaultContext :: IO AOI_CONTEXT
newDefaultContext = Env.getArgs >>= foldM p defaultContext where
    p _ "-?" = runHelp
    p _ "-help" = runHelp
    p cx (dictArg -> Just d) = return $ cx { aoi_source = T.pack d }
    p cx "-frames" = 
        newIORef (hls_init []) >>= \ rf ->
        return $ cx { aoi_frames = Just rf }
    p cx a = putErrLn ("unknown arg: " ++ a) >> return cx
    dictArg = L.stripPrefix "-dict="

runHelp :: IO a
runHelp = putErrLn helpMsg >> Sys.exitSuccess where
    helpMsg = "-? -help      this message\n"
           ++ "-dict=foo     set AO dictionary (foo.ao on AO_PATH)\n"
           ++ "-frames       enable debug frames\n"

-- recovery loop will handle state errors (apart from parse errors)
-- by reversing the ABC streaming state to the prior step. If this
-- happens, a stack trace is also printed.
recoveryLoop :: AOI () -> AOI_CONTEXT -> IO ()
recoveryLoop aoi cx0 = 
    runAOI aoi cx0 >>= \ (r, cx') ->
    case r of
        Right () -> return () -- clean exit
        Left (E eTxt) ->
            putErrLn (T.unpack eTxt) >>
            printCX (aoi_frames cx') >>
            recoveryLoop aoi cx' -- continue the loop

printCX :: Maybe (IORef FrameHist) -> IO ()
printCX Nothing = return ()
printCX (Just rf) = readIORef rf >>= reportContext

initAOI :: HKL.InputT AOI ()
initAOI = greet >> lift aoiReload where
    greet =  
        HKL.haveTerminalUI >>= \ bTerm ->
        when bTerm $
            HKL.outputStrLn "Welcome to aoi!" >>
            HKL.outputStrLn "  ctrl+d to exit, ctrl+c to reload"

finiAOI :: HKL.InputT AOI ()
finiAOI = 
    HKL.haveTerminalUI >>= \ bTerm ->
    when bTerm $ HKL.outputStrLn "goodbye!"

aoiHaskelineLoop :: HKL.InputT AOI ()
aoiHaskelineLoop =
    aoiHklPrint >>
    lift resetFrameHist >> -- debug history per paragraph
    (fst <$> lift aoiGetStep) >>= \ n ->
    let prompt = show (n + 1) ++ ": " in
    foi (HKL.getInputLine prompt) >>= \ sInput ->
    case sInput of
        Nothing -> return ()
        Just str -> -- TODO: catch HKL ctrl+c interrupt and fail... 
            foi (lift (aoiStep (T.pack (' ':str)))) >>
            aoiHaskelineLoop

-- fail on interrupt
foi :: HKL.InputT AOI a -> HKL.InputT AOI a
foi = HKL.handleInterrupt (fail "ctrl+c")

putErrLn :: String -> IO ()
putErrLn = Sys.hPutStrLn Sys.stderr

-- The primary interpreter step for AOI
--
-- In each step we receive some text and process it through the
-- current interpreter. The result is a block value, which may
-- then be executed on the current environment. 
aoiStep :: Text -> AOI ()
aoiStep txt = 
    aoiGetIfn >>= \ ifn ->
    let vIn = (P (textToVal txt) (ifn_eIC ifn)) in
    abc_comp (ifn_op ifn) vIn >>= \ vParsed ->
    case vParsed of
        (R (P (B _ action) eIC')) ->
            let ifn' = ifn { ifn_eIC = eIC' } in
            (snd <$> aoiGetStep) >>= \ v0 ->
            abc_comp action v0 >>= \ vf ->
            aoiPutIfn ifn' >>
            aoiPushStep vf
        (L vErr) -> liftIO $ putErrLn ("read error: " ++ show vErr)
        v -> liftIO $ putErrLn ("read type error: " ++ show v)

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

switchAOI :: V AOI -> AOI (V AOI)
switchAOI (P (B kf abc) eIC) | may_copy kf && may_drop kf = 
    getCX >>= \ cx ->
    let ifnOld = aoi_ifn cx in
    let cx' = cx { aoi_ifn = IFN abc eIC } in
    putCX cx' >>
    return (P (B kf0 (ifn_op ifnOld)) (ifn_eIC ifnOld))
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
        Just (w, bs') -> P (N $ fromIntegral w) (bytesToVal bs')

aoiGetOSEnv :: (MonadIO c) => V c -> c (V c)
aoiGetOSEnv v = 
    case valToText v of
        Nothing -> fail ("getOSEnv @ " ++ show v)
        Just txt -> 
            liftIO (getOSEnv txt) >>= \ result ->
            return (textToVal result)

getOSEnv :: Text -> IO Text
getOSEnv = liftM (maybe T.empty T.pack) . tryIO . Env.getEnv . T.unpack

aoiReadFile :: (MonadIO c, Functor c) => V c -> c (V c)
aoiReadFile fileName =
    case valToText fileName of
        Nothing -> fail $ ("readFile @ " ++ show fileName)
        Just fntxt -> 
            let op = FS.readTextFile $ FS.fromText fntxt in
            maybe (L U) (R . textToVal) <$> liftIO (tryIO op)

aoiWriteFile :: (MonadIO c, Functor c) => V c -> c (V c)
aoiWriteFile v@(P fn ftext) =
    case (,) <$> valToText fn <*> valToText ftext of
        Nothing -> fail $ ("writeFile @ " ++ show v)
        Just (file, fdata) ->
            let fp = FS.fromText file in
            let op = FS.createTree (FS.directory fp) >>
                     FS.writeTextFile fp fdata 
            in
            maybe (L U) (const (R U)) <$> liftIO (tryIO op)
aoiWriteFile v = fail $ ("writeFile @ " ++ show v)


tryIO :: IO a -> IO (Maybe a)
tryIO = liftM (either (const Nothing) (Just)) . Err.tryIOError

getHistoryFile :: IO (Maybe Sys.FilePath)
getHistoryFile = tryIO $
    FS.getAppDataDirectory (T.pack "aoi") >>= \ appDir ->
    FS.createTree appDir >>
    let fp = appDir FS.</> FS.fromText (T.pack "hist.haskeline") in
    FS.appendFile fp B.empty >>
    return (FS.encodeString fp)

-- reportContext prints a stack trace and an incomplete stack history
-- (the stack history is based on state with a halflife)
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

-- a printing function
aoiHklPrint :: HKL.InputT AOI ()
aoiHklPrint = printIfTerminalUI where
    printIfTerminalUI = 
        HKL.haveTerminalUI >>= \ bTerm ->
        when bTerm $
            HKL.outputStrLn "" >>
            lift (snd <$> aoiGetStep) >>= 
            printENV >>
            HKL.outputStrLn ""


-- print the typical environment
--   print up to 7 items on current stack, and stack name
--   print count of items in hand, named stacks
--   print ex if not U
printENV :: V AOI -> HKL.InputT AOI ()
printENV (P s (P h (P (B _ p) (P (P sn ns) ex)))) =
    printPower p >>
    printExt ex >>
    printNS ns >>
    printHand h >>
    printStackHdr sn >>
    printStack 7 s
printENV env =  -- print atypical environment value
    HKL.outputStrLn "--(atypical environment)--" >>
    HKL.outputStrLn (show env)

-- currently not printing powerblock
printPower :: ABC AOI -> HKL.InputT AOI ()
printPower = const $ return ()

printExt :: V AOI -> HKL.InputT AOI ()
printExt U = return ()
printExt ex =
    HKL.outputStrLn "--(extended environment)--" >>
    HKL.outputStrLn (show ex)

-- count items in a stack
stackCount :: V c -> Integer
stackCount (P _ s) = 1 + stackCount s
stackCount U = 0
stackCount _ = 1

printNS :: V c -> HKL.InputT AOI ()
printNS U = return ()
printNS (P s ss) =
    printNamedStack s >>
    printNS ss
printNS v = 
    HKL.outputStrLn "--(atypical named stacks)--" >>
    HKL.outputStrLn (show v)

printNamedStack :: V c -> HKL.InputT AOI ()
printNamedStack v@(P sn ss) =
    case valToText sn of
        Just label ->
            let ct = stackCount ss in
            HKL.outputStrLn (T.unpack label ++ ": " ++ show ct)
        Nothing ->
            HKL.outputStrLn ("(?): " ++ show v)
printNamedStack v = HKL.outputStrLn ("(?): " ++ show v)

printHand :: V c -> HKL.InputT AOI ()
printHand h =
    let n = stackCount h in
    when (n > 0) $
        HKL.outputStrLn ("hand: " ++ show n)

printStackHdr :: V c -> HKL.InputT AOI ()
printStackHdr sn =
    case valToText sn of
        Just txt -> HKL.outputStrLn ("---" ++ T.unpack txt ++ "---")
        Nothing -> HKL.outputStrLn ("---" ++ show sn ++ "---")

printStack :: Int -> V c -> HKL.InputT AOI ()
printStack _ U = return ()
printStack n v | n < 1 =
    let ct = stackCount v in
    when (ct > 0) $
        HKL.outputStrLn ("(" ++ show ct ++ " more)")
printStack n (P v ss) =
    printStack (n-1) ss >>
    HKL.outputStrLn ("| " ++ show v) 
printStack _ v = HKL.outputStrLn ("~ " ++ show v)


