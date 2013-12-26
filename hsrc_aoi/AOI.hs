{-# LANGUAGE PatternGuards, FlexibleContexts, FlexibleInstances #-}

-- | AOI describes a simplistic, imperative REPL for language AO.
-- AOI will start by importing the "aoi" dictionary unless a .ao 
-- file is specified on the command line. Developers cannot modify
-- definitions from within aoi. However, devs may tweak original 
-- files and use 'reload' however often they wish. (This design 
-- helps minimize tendency to lose work performed in REPL.)
--
--   TODO: leverage haskeline, possibly ansi-terminal
--         consider support for acid-state persistent sessions
-- 
-- AOI has a trivial effects model documented in the standard aoi 
-- dictionary file. AOI also enables reprogramming the interpreter
-- from within, with the 'switchAOI' word and command.
--
-- AOI does keep track of historical states, using an exponential
-- decay algorithm. In addition, it keeps track of frames in the
-- current state. Between these attributes, AOI should be decent 
-- for debugging (providing both a stack trace and a history). 
--
-- Note: AOI does not represent the intended vision for AO interactive
-- programming. AOI is simply a stopgap for pre-bootstrap testing.
module AOI
    ( main
    -- stuff that should probably be in separate modules
    , HLS(..), hls_init, hls_put
    , randomBytes, newSecret
    ) where

import Control.Monad
import Control.Applicative
import Control.Arrow (first)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified System.IO as Sys
import qualified System.IO.Error as Err
import qualified System.Environment as Env
import qualified System.Random as R
import qualified Crypto.Random.AESCtr as CR
import qualified System.Console.Haskeline as HKL
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.List as L
import qualified Data.Map as M
import qualified Filesystem.Path.CurrentOS as FS
import qualified Filesystem as FS
import qualified Text.Parsec as P
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Maybe (catMaybes)
import AO
import ABC

putErrLn :: String -> IO ()
putErrLn = Sys.hPutStrLn Sys.stderr

-- AOI_CONTEXT holds state for running AO
data AOI_CONTEXT = AOI_CONTEXT 
    { aoi_dict    :: DictC  -- loaded dictionary
    , aoi_secret  :: Text   -- secret held by powerblock
    , aoi_power   :: M.Map Text (V -> AOI V) -- common powers
    , aoi_source  :: Either [Import] FS.FilePath -- for reloads
    , aoi_frames  :: !(HLS [Text]) -- stack frame history (for debugging)
    , aoi_step    :: !(HLS (Integer,V)) -- recovery values 
    , aoi_ifn     :: !IFN 
    }

type Error = Text
newtype AOI a = AOI { runAOI :: AOI_CONTEXT -> IO (AOI_CONTEXT, Either Error a) }

instance Monad AOI where
    return a = AOI $ \ cx -> return (cx, (Right a))
    (>>=) m f = AOI $ \ cx ->
        runAOI m cx >>= \ (cx',r) ->
        cx' `seq`
        case r of
            Left e -> return (cx', Left e)
            Right a -> runAOI (f a) cx' 
    fail msg = AOI $ \ cx -> return (cx, Left (T.pack msg))

instance MonadIO AOI where
    liftIO m = AOI $ \ cx ->
        Err.tryIOError m >>= \ ea ->
        case ea of
            Left e -> return (cx, Left (T.pack (show e)))
            Right a -> return (cx, Right a)

-- MonadException is a Haskeline class for bracketed IO. The purpose
-- isn't entirely clear to me, but Haskeline isn't something I want
-- to spend much time learning.
instance HKL.MonadException AOI where
    -- I don't really grok the control flow here so I'm borrowing from 
    -- source examples in System.Console.Haskeline.MonadException
    controlIO f = AOI $ \ cx -> HKL.controlIO $ \(HKL.RunIO run) -> let
        run' = HKL.RunIO (fmap (AOI . const) . run . flip runAOI cx) 
        in fmap (flip runAOI cx) $ f run'

-- AOI supports a reprogrammable interpreter to support bootstrap.
-- The interpreter is really an incremental compiler of rough type:
--
--    (text * eIC) -> (error + ([eU -> eU'] * eIC')
-- 
-- This is probably very inefficient, but it's useful. The 
data IFN = IFN
    { ifn_action :: ABC
    , ifn_bt     :: BT
    , ifn_eIC    :: !V
    }

defaultIFN :: IFN
defaultIFN = IFN abc btRel U where
    abc = ABC [Invoke (T.singleton 'i')]
    btRel = BT { bt_rel = True, bt_aff = False }

instance ToABCV IFN where 
    toABCV ifn = B (ifn_bt ifn) (ifn_action ifn) `P` ifn_eIC ifn
instance FromABCV IFN where
    fromABCV (P (B bt act) eIC) = Just $ IFN act bt eIC
    fromABCV _ = Nothing

aoiInvoker :: Text -> V -> AOI V
aoiInvoker t v = 
    case T.uncons t of
        Just ('&',anno) -> aoiAnno anno >> return v
        Just ('!',s) -> aoiTrySecret s (aoiCommand v)
        Just ('i',m) | T.null m -> f1 (uncurry aoiInterpret) v
        _ -> fail ("unknown invocation {" ++ T.unpack t ++ "}")

-- aoi ignores most annotations, except for frames
aoiAnno :: Text -> AOI ()
aoiAnno t = 
    case T.uncons t of
        Just('@',f) | f == fDrop -> popFrame
                    | otherwise -> pushFrame f
        _ -> return ()
    where fDrop = T.singleton '-'

-- aoiInterpret corresponds to an ABC function of type:
--    (text * eIC) -> (errorText + ((eU -> eU') * eIC)
-- This is really an incremental compilation for the given text.
-- Parse failures are reported in-band. Input type failures are
-- reported out-of-band.
aoiInterpret :: Text -> V -> AOI (Either Error (V,V))
aoiInterpret txt eIC = liftM tov (aoiInterpret' txt) where
    tov (Left e) = Left e
    tov (Right abc) = Right (B bt0 abc, eIC)
    bt0 = BT { bt_rel = False, bt_aff = False }

aoiInterpret' :: Text -> AOI (Either Error ABC)
aoiInterpret' = either parseErr build . readAO where
    parseErr = return . Left . T.pack . show
    build ao = aoiGetDict >>= \ dc -> buildDC dc ao
    buildDC dc ao = either buildErr buildOK $ compileAO dc ao
    buildErr = return . Left . T.unwords . (uw :) . L.nub
    uw = T.pack "unrecognized words: "
    buildOK = return . Right

readAO :: Text -> Either P.ParseError AO
readAO = liftM AO . P.parse pma "" where
    pma = P.manyTill parseAction P.eof

-- execute only if a secret matches, fail otherwise
aoiTrySecret :: Text -> AOI a -> AOI a 
aoiTrySecret s action = 
    aoiGetSecret >>= \ secret ->
    let badMsg = "invalid secret " ++ T.unpack s in
    if (s /= secret) then fail badMsg else
    action

aoiCommand :: V -> AOI V
aoiCommand msg = case fromABCV msg of
    Nothing -> fail ("unrecognized command: " ++ show msg)
    Just (label, message) -> 
        aoiDispatch label message >>= \ response ->
        aoiGetSecret >>= \ secret ->
        secret `seq` 
        return (pb secret `P` response)

aoiDispatch :: Text -> V -> AOI V
aoiDispatch label msg =
    aoiGetPowers >>= \ mpw ->
    case M.lookup label mpw of
        Nothing -> fail ("unknown action: " ++ T.unpack label)
        Just action -> action msg

aoiGetContext :: AOI AOI_CONTEXT
aoiGetContext = AOI $ \ cx -> return (cx, Right cx)

aoiPutContext :: AOI_CONTEXT -> AOI ()
aoiPutContext cx' = AOI $ \ _ -> return (cx', Right ())

aoiGetSecret :: AOI Text
aoiGetSecret = liftM aoi_secret aoiGetContext

aoiGetPowers :: AOI (M.Map Text (V -> AOI V))
aoiGetPowers = liftM aoi_power aoiGetContext

aoiGetDict :: AOI DictC
aoiGetDict = liftM aoi_dict aoiGetContext

aoiGetIfn :: AOI IFN
aoiGetIfn = liftM aoi_ifn aoiGetContext

aoiPutIfn :: IFN -> AOI ()
aoiPutIfn ifn = AOI $ \ cx ->
    let cx' = cx { aoi_ifn = ifn } in 
    return (cx', Right ())

aoiGetStepCt :: AOI Integer
aoiGetStepCt = liftM (fst . hls_state . aoi_step) aoiGetContext

aoiGetStepV :: AOI V
aoiGetStepV = liftM (snd . hls_state . aoi_step) aoiGetContext

aoiPutStepV :: V -> AOI ()
aoiPutStepV v = AOI $ \ cx ->
    let hls = aoi_step cx in
    let (n,_) = hls_state hls in
    let n' = n + 1 in
    n' `seq` v `seq`
    let hls' = hls_put (n',v) hls in
    let cx' = cx { aoi_step = hls' } in
    return (cx', Right ())

aoiGetFrames :: AOI [Text]
aoiGetFrames = liftM (hls_state . aoi_frames) aoiGetContext

aoiPutFrames :: [Text] -> AOI ()
aoiPutFrames fs = AOI $ \ cx ->
    let s' = hls_put fs (aoi_frames cx) in
    let cx' = cx { aoi_frames = s' } in
    s' `seq` return (cx', Right ())

pushFrame :: Text -> AOI ()
pushFrame text = aoiGetFrames >>= aoiPutFrames . (text :) 
    -- >> liftIO (putErrLn (T.unpack text))

popFrame :: AOI ()
popFrame =
    aoiGetFrames >>= \ fs ->
    case fs of
        [] -> fail "debug frame underflow!"
        (f:fs) -> aoiPutFrames fs
               -- >> liftIO (putErrLn (T.unpack ('-' `T.cons` f)))
    
randomBytes :: Integer -> IO ByteString
randomBytes n = liftM toBytes CR.makeSystem where 
    toBytes = B.pack . L.take (fromInteger n) . R.randoms

-- AOI context secret (in case of open systems)
newSecret :: IO Text
newSecret = liftM toText (randomBytes 12) where
    toText = T.decodeUtf8 . B64.encode

-- AOI powerblock from a secret
pb :: Text -> V
pb secret = B (BT True True) (ABC [Invoke (T.cons '!' secret)])

-- load a specified dictionary, print errors and return whatever
-- dictionary we manage to build.
aoiLoadDict :: Either [Import] FS.FilePath -> IO DictC
aoiLoadDict src = 
    either importDictC loadDictC src >>= \ (errors,dictC) ->
    mapM_ (putErrLn . T.unpack) errors >>
    return dictC

-- load dictionary from file; print errors
--  (AOI word 'reload' will also repeat this)
aoiReload :: AOI ()
aoiReload = source >>= load >>= set where
    source = liftM aoi_source aoiGetContext
    load = liftIO . aoiLoadDict 
    set dc = AOI $ \ cx ->
        let cx' = cx { aoi_dict = dc } in
        return (cx', Right ())

aoiSourceFromArgs :: IO (Either [Import] FS.FilePath)
aoiSourceFromArgs = 
    Env.getArgs >>= \ args ->
    let textArgs = L.map T.pack args in
    let aoSuffix = T.pack ".ao" in
    let aoFileArgs = L.filter (aoSuffix `T.isSuffixOf`) textArgs in
    case aoFileArgs of
        [] -> return (Left [T.pack "aoi"])
        (f:[]) -> 
            FS.canonicalizePath (FS.fromText f) >>= \ fc ->
            return (Right fc)
        _ -> fail ("args not understood: " ++ show args)

-- default interpreter for code in ABC is just [{i}]
-- this will invoke the AO to ABC compiler using the AO dictionary
-- (Developers cannot define new words; all edits must be performed
-- via the filesystem, using "reload" if needed.)
defaultInterpreter :: V
defaultInterpreter = B bt (ABC [invokeI]) where
    bt = BT { bt_rel = True, bt_aff = False }
    invokeI = Invoke (T.singleton 'i')

-- 
newDefaultContext :: IO AOI_CONTEXT
newDefaultContext =
    newSecret >>= \ secret ->
    aoiSourceFromArgs >>= \ source ->
    aoiLoadDict source >>= \ dictC ->
    let s = U in -- initial stack
    let h = U in -- initial hand
    let p = pb secret in -- initial powerblock
    let rns = (T.empty, U) in -- record of named stacks
    let ex = U in -- extension area
    let eU0 = (s, (h, (p, (rns, ex)))) in
    let cx0 = AOI_CONTEXT 
            { aoi_dict = dictC
            , aoi_secret = secret
            , aoi_power = defaultPower
            , aoi_source = source
            , aoi_frames = hls_init []
            , aoi_step = hls_init (0, toABCV eU0) 
            , aoi_ifn = defaultIFN
            }
    in
    return cx0

defaultPower :: M.Map Text (V -> AOI V)
defaultPower = M.fromList $ map (first T.pack) lPowers where

lPowers :: [(String, V -> AOI V)]
lPowers = 
    [("switchAOI", f1 switchAOI)
    ,("reloadDictionary", f1 (\ () -> aoiReload))
    ,("loadWord",  f1 loadWord)
    ,("getOSEnv",  f1 (liftIO . getOSEnv))
    ,("loadRandomBytes", f1 (liftIO . randomBytes))
    ,("destroy", const (return U))
    -- debugging support
    ,("debugOut", (\ v -> debugOut v >> return v))
    ,("pushFrame", f1 pushFrame)
    ,("popFrame",  f1 (\() -> popFrame))
    -- read and write files
    ,("readFile", f1 (liftIO . readFileT))
    ,("writeFile", f1 (liftIO . uncurry writeFileT))
    ,("readFileB", f1 (liftIO . readFileB))
    ,("writeFileB", f1 (liftIO . uncurry writeFileB))
    ]

f1 :: (FromABCV arg, ToABCV res) => (arg -> AOI res) -> V -> AOI V
f1 action v = case fromABCV v of
    Nothing -> fail ("invalid command argument: " ++ show v)
    Just arg -> action arg >>= return . toABCV

switchAOI :: IFN -> AOI IFN
switchAOI ifn = 
    aoiGetIfn >>= \ ifn0 ->
    aoiPutIfn ifn >>
    return ifn0

debugOut :: V -> AOI ()
debugOut = liftIO . putErrLn . show

loadWord :: Text -> AOI (Maybe V)
loadWord w =
    let bt0 = BT { bt_rel = False, bt_aff = False } in
    aoiGetDict >>= \ dc ->
    return (B bt0 <$> M.lookup w dc)

getOSEnv :: Text -> IO Text
getOSEnv = liftM tt . Env.lookupEnv . T.unpack where
    tt = maybe T.empty T.pack

-- tryIO returns error condition inline, but only as a minimal
-- (1 + result). 
tryIO :: IO a -> IO (Maybe a)
tryIO m = 
    Err.tryIOError m >>= \ ea ->
    case ea of
        Left err -> putErrLn (show err) >> return Nothing 
        Right a -> return (Just a)

readFileT :: FS.FilePath -> IO (Maybe Text)
readFileT = tryIO . FS.readTextFile

writeFileT :: FS.FilePath -> Text -> IO (Maybe ())
writeFileT fp txt = tryIO $ FS.writeTextFile fp txt
 
readFileB :: FS.FilePath -> IO (Maybe ByteString)
readFileB = tryIO . FS.readFile

writeFileB :: FS.FilePath -> ByteString -> IO (Maybe ())
writeFileB fp bytes = tryIO $ FS.writeFile fp bytes

instance ToABCV FS.FilePath where 
    toABCV = toABCV . either id id . FS.toText 
instance FromABCV FS.FilePath where 
    fromABCV = liftM FS.fromText . fromABCV

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
    let hkl = greet >> failOnInterrupt aoiHaskelineLoop in
    recoveryLoop (HKL.runInputT hklSettings hkl) cx

-- recovery loop will handle state errors (apart from parse errors)
-- by reversing the ABC streaming state to the prior step. If this
-- happens, a stack trace is also printed.
recoveryLoop :: AOI () -> AOI_CONTEXT -> IO ()
recoveryLoop aoi cx = 
    runAOI aoi cx >>= \ (cx', r) ->
    case r of
        Right () -> return () -- clean exit
        Left eTxt ->
            putErrLn ("STATE ERROR: " ++ T.unpack eTxt) >>
            reportContext (aoi_frames cx') >> -- STACK TRACE, VALUE, ETC.
            putErrLn "\nRECOVERING!\n" >>
            let cxClean = cx' { aoi_frames = hls_init [] } in
            recoveryLoop aoi cxClean

-- reportContext currently just prints a stack trace
-- todo: print recent history of stacks, too. 
reportContext :: HLS [Text] -> IO ()
reportContext hls = stackTrace >> histTrace where
    stack = hls_state hls
    stackTrace = 
        putErrLn "== CALL STACK ==" >>
        mapM_ (putErrLn . T.unpack) stack
    hist = L.map L.nub $ L.reverse (hls_getHist hls)
    diffs = L.map snd $ L.scanl stackDiff ([],([],[])) hist 
    histTrace = -- simplistic history trace
        putStrLn "== INCOMPLETE STACK HISTORY ==" >>
        mapM_ showStackDiff diffs
    stackDiff (sPrev,_) sNew = 
        let added = L.filter (`L.notElem` sPrev) sNew in
        let removed = L.filter (`L.notElem` sNew) sPrev in
        (sNew, (added, removed))
    showStackDiff (lAdd, lRem) = 
        showDiff "-- " lRem >> 
        showDiff "++ " lAdd 
    showDiff _ [] = return ()
    showDiff s xs = putStrLn . (s ++) . T.unpack . T.unwords $ xs


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
    findWords s = aoiGetDict >>= \ dc -> return (dictCompletions dc s)

greet :: HKL.InputT AOI ()
greet = 
    HKL.haveTerminalUI >>= \ bTerm ->
    when bTerm (liftIO $ Sys.putStrLn greetMsg)
  where 
    greetMsg = "Welcome to aoi! ctrl+d to exit"


aoiHaskelineLoop :: HKL.InputT AOI ()
aoiHaskelineLoop = 
    lift aoiGetStepCt >>= \ n ->
    let prompt = show (n + 1) ++ ": " in
    HKL.getInputLine prompt >>= \ sInput ->
    case sInput of
        Nothing -> return ()
        Just str -> -- TODO: catch HKL ctrl+c interrupt and fail... 
            lift (aoiStep (T.pack (' ':str))) >>
            aoiHaskelineLoop

failOnInterrupt :: HKL.InputT AOI a -> HKL.InputT AOI a
failOnInterrupt action = HKL.withInterrupt $
    HKL.handleInterrupt (fail "ctrl+c interrupt") $ 
    action

-- clean up some debug info between steps
aoiClearStepHist :: AOI ()
aoiClearStepHist = AOI $ \ cx ->
    let fr0 = (hls_state . aoi_frames) cx in
    let cx' = cx { aoi_frames = hls_init fr0 } in
    return (cx', Right ())

-- the primary interpreter step for AOI
aoiStep :: Text -> AOI ()
aoiStep txt = 
    aoiClearStepHist >> 
    aoiGetIfn >>= \ ifn ->
    let vInputIC = toABCV (txt, ifn_eIC ifn) in
    runABC aoiInvoker vInputIC (ifn_action ifn) >>= \ vParsed ->
    case fromABCV vParsed of
        Just (Right (B _ abcAction, eIC')) -> 
            let ifn' = ifn { ifn_eIC = eIC' } in
            aoiPutIfn ifn' >>
            aoiGetStepV >>= \ v0 ->
            runABC aoiInvoker v0 abcAction >>= \ vf ->
            aoiPutStepV vf >> 
            liftIO (reportStepV vf) >>
            liftIO (Sys.putStrLn "")
        Just (Left errText) -> liftIO $ 
            putErrLn ("READ ERROR: " ++ T.unpack errText) >>
            putErrLn ("dropping this input...")
        _ -> liftIO $
            putErrLn ("READER TYPE ERROR; received: " ++ show vParsed) >>
            putErrLn ("dropping this input...")

-- for now, just print the values on the data stack.
reportStepV :: V -> IO ()
reportStepV (P s@(P _ _) _) = reportStack s where
    reportStack U = Sys.putStrLn "\n------"
    reportStack (P v s) = reportStack s >> Sys.putStrLn ("| " ++ show v)
    reportStack v = Sys.putStrLn ("\n--(atypical stack)-- \n~ " ++ show v)
reportStepV (P U _) = Sys.putStrLn "\n--(empty stack)--"
reportStepV v = 
    Sys.putStrLn "\n--(atypical environment)--" >>
    Sys.putStrLn (show v)


-- states with exponential decay of history
data HLS s = HLS 
    { hls_halflife :: Double 
    , hls_rgen     :: R.StdGen
    , hls_state    :: s
    , hls_hist     :: [s]
    , hls_join     :: s {-newer-} -> s {-older-} -> s {-joined-}
    }

-- defaults to a halflife of 100 steps.
hls_init :: s -> HLS s
hls_init s0 = s0 `seq` HLS
    { hls_halflife = 31.4159 -- arbitrary default halflife
    , hls_rgen = R.mkStdGen 60091 -- flight number of our galactic sun
    , hls_state = s0
    , hls_hist = []
    , hls_join = const
    }

hls_getHist :: HLS s -> [s]
hls_getHist hls = hls_state hls : hls_hist hls

-- hls_put is strict in state and history
hls_put :: s -> HLS s -> HLS s
hls_put s hls =
    let (r,g') = R.random (hls_rgen hls) in
    let n = negate $ round $ log (1.0 - r) * hls_halflife hls in
    let h' = dropHLS (hls_join hls) n (hls_state hls : hls_hist hls) in
    h' `seq` s `seq`
    hls { hls_rgen = g', hls_state = s, hls_hist = h' }

hls_modify :: (s -> s) -> HLS s -> HLS s
hls_modify f hls = hls_put (f (hls_state hls)) hls 

-- dropHLS will join two items in a list at the given position
-- This operation is strict in the join function because it is
-- important to avoid a memory leak. 
dropHLS :: (s -> s -> s) -> Integer -> [s] -> [s]
dropHLS jf n (s:ss) | (n > 0) = 
    let ss' = dropHLS jf (n - 1) ss in
    ss' `seq` (s : ss')
dropHLS jf _ (sNewer:sOlder:ss) = 
    let s' = jf sNewer sOlder in
    s' `seq` (s' : ss)
dropHLS _ _ ss = ss


